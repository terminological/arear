#' Create a catchment area map
#'
#' This implements the label propagation algorithm described in our paper:
#' R. J. Challen, G. J. Griffith, L. Lacasa, and K. Tsaneva-Atanasova,
#' ‘Algorithmic hospital catchment area estimation using label propagation’, BMC
#' Health Services Research, vol. 22, no. 1, p. 828, June 2022, doi:
#' 10.1186/s12913-022-08127-7.
#'
#' @param supplyShape a sf object containing a list of the locations of supply
#'   points, with a column containing supply capacity, for example NHS hospital
#'   sites, with a bed capacity count
#' @param supplyIdVar the variable name of the identifier of the supplier or
#'   group of suppliers. For example this could be an NHS trust (multiple sites)
#' @param supplyVar the column name of the supply parameter. This could be
#'   number of beds in a hospital.
#' @param supplyOutputVars (optional - defaults to grouping) the columns from
#'   the input that are to be retained in the output
#' @param demandShape the sf object with the geographical map of the demand
#'   surface. For example the geographical distribution of the population
#'   served,
#' @param demandIdVar the column name of the unique identifier of the areas,
#' @param demandVar the column name of the demand parameter. This could be the
#'   population in each region
#' @param growthConstant a growth parameter which defines how quickly each
#'   label propagates
#' @param bridges a named list containing extra linkages beyond those inferred
#'   by the demandShape topology. These are used to add in bridges
#' @param outputMap should we export a shape file or just the mapping file
#' @return a
#' @importFrom rlang .data
#' @export
createCatchment = function(
  supplyShape,
  supplyIdVar = "code",
  supplyVar,
  supplyOutputVars = supplyShape %>% dplyr::groups(),
  demandShape,
  demandIdVar = "code",
  demandVar,
  growthConstant = 1.2,
  #distanceModifier = function(distanceToSupply) {return(2/(1+distanceToSupply/min(0.1,mean(distanceToSupply))))},
  bridges = arear::ukconnections,
  outputMap = TRUE
) {
  # set up column aliases so that supply and demand can be specified in a flexible manner
  supplyIdVar = rlang::ensym(supplyIdVar)
  supplyVar = rlang::ensym(supplyVar)
  demandIdVar = rlang::ensym(demandIdVar)
  demandVar = rlang::ensym(demandVar)

  .forceGeos({
    # rename key columns from demand shape and define $n$, $V_N$ and $D_V_n$ to follow terminology as per catchment areas paper
    V_N = demandShape %>% #V_N
      dplyr::ungroup() %>%
      dplyr::rename(demandCode = !!demandIdVar, D_V_n = !!demandVar) %>%
      dplyr::mutate(n = dplyr::row_number())

    # renaming for consistency with published algorithm
    if (growthConstant <= 1) {
      stop("growthConstant must be > 1")
    }
    C_growth = growthConstant

    # preserve the features we want to output. Aggregate them to the level of
    # the output
    supplyFeatures = supplyShape %>%
      dplyr::ungroup() %>%
      tibble::as_tibble() %>%
      dplyr::group_by(!!!unique(c(supplyIdVar, supplyOutputVars))) %>%
      dplyr::summarise(!!supplyVar := sum(!!supplyVar)) %>%
      dplyr::distinct()

    # rename key columns from supply shape and define $m$, $P_M$, and $S_P_m$ to
    # follow terminology as per catchment areas paper in this implementation
    # there can be many suppliers with the same label (supplyId)
    P_M = supplyShape %>% #P
      dplyr::rename(supplyCode = !!supplyIdVar, S_P_m = !!supplyVar) %>%
      dplyr::group_by(supplyCode) %>%
      dplyr::mutate(m = dplyr::cur_group_id()) %>%
      dplyr::ungroup()

    # define the neighborhood network $nu(V_x)$ - and connect non neighbouring
    # areas that have transport links defined createNeighbourNetwork creates a
    # directed edgelist but with symmetrical edges, and the edges are defined in
    # terms of $n$
    E_N = V_N %>% arear::createNeighbourNetwork(idVar = n, bridges = bridges)

    # find the regions containing the supply points: define $V_M$ as the
    # geographic region in $G$ (i.e. $V_N$) containing point $P_M$
    # getContainedIn constructs a mapping from $P_m$ to $V_n$ retaining
    # inputVars and outputVars
    V_M = P_M %>%
      arear::getContainedIn(
        V_N,
        inputVars = dplyr::vars(supplyCode, m, S_P_m),
        outputVars = dplyr::vars(demandCode, n, D_V_n)
      )

    # No more than one P_M can be found in a given V_M. This constraint is
    # sometimes violated in real life but can be achieved by merging P_M's when
    # they are found in a region. if there are multiple providers in one area we
    # merge them (creating a new supplier id).

    # merge multiple providers in same $V_M$ area into single $P_m$ in $V_n$

    V_M = V_M %>%
      tibble::as_tibble() %>%
      dplyr::select(demandCode, D_V_n, n, supplyCode, S_P_m, m)

    V_M_merged = NULL
    if (
      any(
        V_M %>%
          dplyr::group_by(demandCode, n) %>%
          dplyr::summarise(count = dplyr::n()) %>%
          dplyr::pull(count) >
          1
      )
    ) {
      warning(
        "More than one supplier was found in a single region. These the first value will be picked, and the total capacity combined, but as a result the catchment map will be missing some values from the supplier list."
      )
      V_M_merged = V_M %>%
        dplyr::group_by(demandCode, n) %>%
        dplyr::filter(dplyr::n() > 1)
    }

    V_M = V_M %>%
      dplyr::group_by(demandCode, n, D_V_n) %>%
      dplyr::summarise(
        m = dplyr::first(m),
        S_P_m = sum(S_P_m),
        # for tracking purposes we keep the original ids for merged areas.
        supplyCode = paste0(unique(supplyCode), collapse = "|")
      ) %>%
      dplyr::ungroup()
    # TODO: investigate possibility of reassigning a hospital to a neighbouring
    # region when more than one are in a given small area

    V_N = V_N %>%
      tibble::as_tibble() %>%
      dplyr::select(demandCode, D_V_n, n)

    # define the initial labelled areas ($V^new_M_k$) as those with a $P_M$ currently
    # this is an implementation detail and differs slightly from the published algorithm
    k_count = 0
    Vnew_M_k = V_M %>% dplyr::mutate(k = k_count)
    G_M_k_minus_1 = tibble::tibble()
    G_M_k = Vnew_M_k # %>% dplyr::select(n,k) %>% dplyr::left_join(V_N, by="n")

    # define the initial unlabelled set of vertices when k=0:
    U_k = V_N %>%
      dplyr::anti_join(V_M, by = "n") %>%
      dplyr::mutate(k = k_count, S_P_m = 0) # unlsuppied areas contribute zero 0 supply)

    # define the un-labelled neighbours of $G_{M,k-1}$ as $U_{M,k}$ initially empty:
    U_M_k = tibble::tibble(
      m = integer(),
      n = integer(),
      k = integer(),
      S_P_m = numeric(),
      D_V_n = numeric()
    )

    # remaining = Inf
    repeat {
      # stop if unsupplied area is empty, all areas have been labelled
      if (nrow(U_k) == 0) {
        break
      }

      if (k_count > 0 & nrow(U_M_k) == 0 & nrow(Vnew_M_k) == 0) {
        if (nrow(U_k) > 0) {
          warning(
            "No futher areas to grow into. Terminating early with missing areas - it looks like ",
            nrow(U_k) - nrow(Vnew_M_k),
            " areas are not connected."
          )
        }
        break
      }

      # The simplifying assumption that G is connected is not always true in
      # real life due to islands. this can lead to some areas being impossible
      # to reach and the algorithm never terminating. here we track the progress
      # of the algorithm and terminate it early with a warning, if no more areas
      # are labelled in 10 iterations.
      # N.B. this should not be needed
      # if (sum(remaining == nrow(U_k)) > 10) {
      #   warning("terminating early with missing areas - it looks like ",nrow(U_k), " areas are not connected")
      #   break;
      # }
      # remaining = c(remaining,nrow(U_k))

      k_count = k_count + 1
      Vnew_M_k_minus_1 = Vnew_M_k
      G_M_k_minus_1 = G_M_k
      U_M_k_minus_1 = U_M_k

      #  define the un-labelled vertices as the set of V not contained in any of G_M_k−1 :
      U_k = V_N %>%
        dplyr::anti_join(G_M_k_minus_1, by = "n") %>%
        dplyr::mutate(k = k_count, S_P_m = 0)
      message("areas remaining: ", nrow(U_k), "; ", appendLF = FALSE)

      # define the unlabelled neighbours of $G_M_K-1$ as $U_M_k$
      # these are the areas where there is potential growth
      # first get the
      nu_G_M_k_minus_1 = Vnew_M_k_minus_1 %>%
        dplyr::inner_join(E_N, by = c("n" = "from")) %>%
        dplyr::anti_join(U_M_k_minus_1, by = c("to" = "n")) %>% # prevent duplicates with already existing U_M_k's that have
        dplyr::select(to, m, supplyCode) %>%
        dplyr::distinct()

      U_M_k = dplyr::bind_rows(
        U_M_k_minus_1,
        U_k %>%
          dplyr::inner_join(nu_G_M_k_minus_1, by = c("n" = "to")) %>%
          dplyr::mutate(
            A_U_m_k = 0 # the accumulated growth score
          )
      ) %>%
        dplyr::mutate(
          k = k_count
        )
      message("growing into: ", nrow(U_M_k))

      # define the reserve capacity to supply existing labelled, $G_M_k−1$ , and
      # un-labelled neighbours, RM , as:
      R_M = dplyr::bind_rows(
        U_M_k,
        G_M_k_minus_1
      ) %>%
        dplyr::group_by(m) %>%
        dplyr::summarise(
          R = sum(S_P_m) / sum(D_V_n),
          .groups = "drop"
        ) %>%
        dplyr::ungroup()

      # update the accumulated growth score (only for areas which are growing),
      # A_U_M,k , with the normalised rank of the reserve capacity and
      # multiplied by a constant C_growth > 1 representing the speed at which
      # the accumulated growth score increases in all areas:
      R_M_k = R_M %>%
        dplyr::semi_join(
          U_M_k %>% dplyr::select(m) %>% dplyr::distinct(),
          by = "m"
        ) %>%
        dplyr::mutate(
          delta_A_U_m_k = C_growth * rank(R) / dplyr::n()
        )

      U_M_k = U_M_k %>%
        dplyr::left_join(R_M_k, by = "m") %>%
        dplyr::mutate(A_U_m_k = A_U_m_k + delta_A_U_m_k) %>%
        dplyr::select(-R, -delta_A_U_m_k)

      # for all the un-labelled vertices, select the label M , with the highest
      # score, and if the accumulated score has reached the threshold of 1,
      # incorporate it into the labelled sub-graph, $G_M_k−1$:
      Vnew_M_k = U_M_k %>%
        dplyr::filter(A_U_m_k > 1) %>%
        dplyr::group_by(n) %>%
        dplyr::arrange(dplyr::desc(A_U_m_k)) %>%
        dplyr::filter(dplyr::row_number() == 1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-A_U_m_k)

      # incorporate it into the labelled sub-graph, $G_M_k−1$:
      G_M_k = dplyr::bind_rows(G_M_k_minus_1, Vnew_M_k)
      # TODO: add: and remove from the unlabelled neighbourhood nodes
      U_M_k = U_M_k %>% dplyr::anti_join(Vnew_M_k, by = "n")
    }

    # revert all renaming and assemble output
    if (dplyr::as_label(demandIdVar) == dplyr::as_label(supplyIdVar)) {
      demandIdVar2 = as.symbol(paste0(demandIdVar, ".demand"))
      supplyIdVar2 = as.symbol(paste0(supplyIdVar, ".supply"))
    } else {
      demandIdVar2 = demandIdVar
      supplyIdVar2 = supplyIdVar
    }
    if (dplyr::as_label(demandVar) == dplyr::as_label(supplyVar)) {
      demandVar = as.symbol(paste0(demandVar, ".demand"))
      supplyVar = as.symbol(paste0(supplyVar, ".supply"))
    }
    crossMapping = G_M_k %>%
      dplyr::ungroup() %>%
      dplyr::select(
        !!demandIdVar2 := demandCode,
        !!demandVar := D_V_n,
        !!supplyIdVar2 := supplyCode,
        !!supplyVar := S_P_m,
        k
      )

    suppliedArea = demandShape %>%
      dplyr::inner_join(
        crossMapping %>%
          dplyr::select(
            !!demandIdVar := !!demandIdVar2,
            !!supplyIdVar2,
            !!supplyVar,
            k
          ),
        by = dplyr::as_label(demandIdVar)
      )
    notSuppliedArea = demandShape %>%
      dplyr::anti_join(
        crossMapping %>% dplyr::select(!!demandIdVar := !!demandIdVar2),
        by = dplyr::as_label(demandIdVar)
      )
    if (identical(V_M_merged, NULL)) {
      mergedSuppliers = tibble::tibble()
    } else {
      mergedSuppliers = V_M_merged %>%
        dplyr::ungroup() %>%
        dplyr::select(
          !!demandIdVar2 := demandCode,
          !!demandVar := D_V_n,
          !!supplyIdVar2 := supplyCode,
          !!supplyVar := S_P_m
        )
    }

    out = list(
      suppliers = supplyShape %>% dplyr::ungroup(),
      mergedSuppliers = mergedSuppliers,
      crossMapping = crossMapping,
      suppliedArea = suppliedArea,
      notSuppliedArea = notSuppliedArea
    )

    if (outputMap) {
      # reassemble features preserved from original supply dataframe
      message("assembling catchment area map...")
      tmp = demandShape %>%
        dplyr::inner_join(
          crossMapping %>%
            dplyr::select(!!demandIdVar := !!demandIdVar2, !!supplyIdVar2),
          by = dplyr::as_label(demandIdVar)
        )
      tmp2 = suppressWarnings({
        tmp %>%
          rmapshaper::ms_dissolve(
            field = dplyr::as_label(supplyIdVar2),
            sum_fields = dplyr::as_label(demandVar)
          )
      })
      tmp2 = tmp2 %>%
        dplyr::rename(!!supplyIdVar := !!supplyIdVar2) %>%
        dplyr::left_join(supplyFeatures, by = dplyr::as_label(supplyIdVar))
      out$map = tmp2
    }
  })
  return(out)
}


#' Create and cache a catchment area map
#'
#' @inherit createCatchment
#' @inheritDotParams .cached .nocache .stale
#' @concept analysis
#' @export
catchment = function(
  supplyShape,
  supplyIdVar = "code",
  supplyVar,
  supplyOutputVars = supplyShape %>% dplyr::groups(),
  demandShape,
  demandIdVar = "code",
  demandVar,
  growthConstant = 1.2,
  #distanceModifier = function(distanceToSupply) {return(2/(1+distanceToSupply/min(0.1,mean(distanceToSupply))))},
  bridges = arear::ukconnections,
  outputMap = TRUE,
  ...
) {
  supplyIdVar = rlang::ensym(supplyIdVar)
  supplyVar = rlang::ensym(supplyVar)
  demandIdVar = rlang::ensym(demandIdVar)
  demandVar = rlang::ensym(demandVar)
  .cached(
    {
      arear::createCatchment(
        supplyShape,
        !!supplyIdVar,
        !!supplyVar,
        supplyOutputVars,
        demandShape,
        !!demandIdVar,
        !!demandVar,
        growthConstant,
        bridges,
        outputMap
      )
    },
    hash = list(
      supplyShape,
      supplyIdVar,
      supplyVar,
      supplyOutputVars,
      demandShape,
      demandIdVar,
      demandVar,
      growthConstant,
      bridges,
      outputMap
    ),
    name = "catchment",
    ...
  )
}
