#' @description create a catchment area map from
#' @param supplyShape - a sf object containing a list of the locations of supply points, with a column containing supply capacity, for example NHS hospital sites, with a bed
#' @param supplyIdVar - the variable name of the identifier of the supplier or group of suppliers. For example this could be an NHS trust (multiple sites)
#' @param supplyVar - the column name of the supply parameter. This could be number of beds in a hospital.
#' @param supplyOutputVars - (optional - defaults to gropuing) the columns from the input that are to be retained in the output
#' @param demandShape - the sf object with the geographical map of the demand surface. For example the geographical distribution of the population served,
#' @param demandIdVar - the column name of the unique identifier of the areas,
#' @param demandVar - the column name of the demand parameter. This could be the population in each region
#' @param growthRates - a function to calculate
#' @param tweakNetwork - a named list containing extra linkages beyond those inferred by the demandShape topology. These are used to add in bridges
#' @param outputMap
#' @return a dataframe containing the grouping columns, the outputIdVar and the interpolated value of interpolateVar
createCatchment = function(
  supplyShape,
  supplyIdVar = "code",
  supplyVar,
  supplyOutputVars = supplyShape %>% dplyr::groups(),
  demandShape,
  demandIdVar = "code",
  demandVar,
  growthConstant = 1.1,
  #distanceModifier = function(distanceToSupply) {return(2/(1+distanceToSupply/min(0.1,mean(distanceToSupply))))},
  bridges = arear::ukconnections,
  outputMap = TRUE
) {

  # set up column aliases so that supply and demand can be specified in a flexible manner
  supplyIdVar = ensym(supplyIdVar)
  supplyVar = ensym(supplyVar)
  demandIdVar = ensym(demandIdVar)
  demandVar = ensym(demandVar)

  # rename key columns from demand shape and define $n$, $V_N$ and $D_V_n$ to follow terminology as per catchment areas paper
  V_N = demandShape %>% #V_N
    dplyr::ungroup() %>%
    dplyr::rename(demandCode = !!demandIdVar, D_V_n = !!demandVar) %>%
    dplyr::mutate(n = row_number())

  # renaming for consistency with published algortihm
  C_growth = growthConstant

  # preserve the features we want to output.
  supplyFeatures = supplyShape %>%
    dplyr::ungroup() %>%
    tibble::as_tibble() %>%
    dplyr::select(!!supplyIdVar, !!!supplyOutputVars, !!supplyVar) %>%
    dplyr::distinct()

  # rename key columns from supply shape and define $m$, $P_M$, and $S_P_m$ to follow terminology as per catchment areas paper
  P_M = supplyShape %>% #P
    dplyr::ungroup() %>%
    dplyr::rename(supplyCode = !!supplyIdVar, S_P_m = !!supplyVar) %>%
    dplyr::mutate(m = row_number())

  # find the regions containing the supply points
  # define $V_M$ as the geographic region in $G$ (as $V_M$) containing point $P_M$
  # getContainedIn constructs a mapping from $P_m$ to $V_n$ retaining inputVars and outputVars
  V_M = P_M %>% arear::getContainedIn(
    V_N,
    inputVars = vars(supplyCode,m,S_P_m),
    outputVars = vars(demandCode,n,D_V_n)
  )

  # No more than one P_M can be found in a given V_M. This constraint is sometimes violated in real life
  # but can be achieved by merging P_M's when they are found in a region.
  # if there are multiple providers in one area we merge them (creating a new supplier id).

  # merge multiple providers in same $V_M$ area into single $P_m$ in $V_n$

  V_M = V_M %>%
    tibble::as_tibble() %>%
    dplyr::select(demandCode, D_V_n, n, supplyCode, S_P_m, m)

  V_M_merged = NULL
  if(any(V_M %>% dplyr::group_by(demandCode,n) %>% dplyr::summarise(count=n()) %>% dplyr::pull(count)>1)) {
    warning("More than one supplier was found in a single region. These the first value will be picked, and the total capacity combined, but as a result the catchment map will be missing some values from the supplier list.")
    V_M_merged = V_M %>% dplyr::group_by(demandCode,n) %>% dplyr::filter(dplyr::n()>1)
  }

  V_M = V_M %>%
    dplyr::group_by(demandCode,n,D_V_n) %>%
    dplyr::summarise(
      m = first(m),
      S_P_m = sum(S_P_m),
      supplyCode = paste0(supplyCode,collapse="|") # for tracking purposes we keep the original ids for merged areas.
    ) %>% dplyr::ungroup()
  # TODO: investigate possibility of reassigning a hospital to a neighouring region when more than one are in a given small area

  # define the neighborhood network $nu(V_x)$ - and connect non neighbouring areas that have transport links defined
  # createNeighbourNetwork creates a directed edgelist but with symmetrical edges, and the edges are defined in terms of $n$
  E_N = V_N %>% arear::createNeighbourNetwork(n, bridges)

  V_N = V_N %>%
    tibble::as_tibble() %>%
    dplyr::select(demandCode, D_V_n, n)


  # ensure entire area is connected to at least one other thing - otherwise we get problems with islands
  # TODO: not sure this is required
  # V_N = V_N %>% dplyr::ungroup() %>%
  #   semi_join(
  #     dplyr::bind_rows(
  #       E_N %>% dplyr::select(n=from),
  #       E_N %>% dplyr::select(n=to)
  #     ),
  #     by="n") %>%
  #   tibble::as_tibble() # convert to non sf structure.

  # define the initial labelled areas ($V_M_k$) as those with a $P_M$
  # this is an implementation detail and differs slightly from the published algorithm
  # TODO: update alo
  k_count = 0
  Vnew_M_k = V_M %>% dplyr::mutate(k=k_count)
  G_M_k_minus_1 = tibble::tibble()
  G_M_k = Vnew_M_k # %>% select(n,k) %>% left_join(V_N, by="n")

  # define the initial unlabelled set of vertices:
  U_k = V_N %>% dplyr::anti_join(V_M, by="n") %>% dplyr::mutate(k = k_count, S_P_m = 0) # unlsuppied areas contribute zero 0 supply)

  #  M  define the initial un-labelled neighbours of labelled sub-graphs, G_M :
  # nu_V_M = E_N %>% dplyr::semi_join(V_M, by=c("from"="n"))
  # U_M_k = U_k %>%
  #   dplyr::semi_join(nu_V_M, by=c("n"="to")) %>%
  #   dplyr::mutate(
  #     A_U_m_k = 0, # the accumulated growth score
  #     k = k
  #   )

  U_M_k = tibble::tibble(m=integer(),n=integer(),k=integer(),S_P_m=numeric(),D_V_n=numeric())

  remaining = Inf
  repeat {

    # stop if unsupplied area is empty, all areas have been labelled
    if(nrow(U_k) == 0) break

    # The simplifying assumption that G is connected is not always true in real life due to islands.
    # this can lead to some areas being impossible to reach and the algorithm never terminating.
    # here we track the progress of the algorithm and terminate it early with a warning, if no more areas are
    # labelled in 4 iterations.
    if (sum(remaining == nrow(U_k)) > 4) {
      warning("terminating early with missing areas - it looks like ",nrow(U_k), " areas are not connected")
      break;
    }
    remaining = c(remaining,nrow(U_k))

    k_count=k_count+1
    Vnew_M_k_minus_1 = Vnew_M_k
    G_M_k_minus_1 = G_M_k
    U_M_k_minus_1 = U_M_k

    #  define the un-labelled vertices as the set of V not contained in any of G_M_k−1 :
    U_k = V_N %>% dplyr::anti_join(G_M_k_minus_1, by="n") %>% dplyr::mutate(k = k_count, S_P_m = 0)
    message("areas remaining: ",nrow(U_k))

    # define the unlabelled neighbours of $G_M_K-1$ as $U_M_k$
    # these are the areas where there is potential growth
    nu_G_M_k_minus_1 = Vnew_M_k_minus_1 %>%
      dplyr::inner_join(E_N, by=c("n"="from")) %>%
      dplyr::anti_join(U_M_k_minus_1, by=c("to"="n")) %>% # prevent duplicates with already existing U_M_k's that have
      dplyr::select(to,m,supplyCode) %>%
      dplyr::distinct()

    U_M_k = dplyr::bind_rows(
      U_M_k_minus_1, #TODO: update main paper with this new definition.
      U_k %>% dplyr::inner_join(nu_G_M_k_minus_1, by=c("n"="to")) %>%
        dplyr::mutate(
          A_U_m_k = 0 # the accumulated growth score
        )
      ) %>%
      dplyr::mutate(
        k = k_count
      )


    # define the reserve capacity to supply existing labelled, $G_M_k−1$ , and un-labelled neighbours, RM , as:
    R_M = dplyr::bind_rows(
        U_M_k,
        G_M_k_minus_1
      ) %>%
      dplyr::group_by(m) %>%
      dplyr::summarise(
        R = sum(S_P_m)/sum(D_V_n),
        .groups="drop"
      )

    # update the accumulated growth score, AUM,k , with the normalised rank of the reserve
    # capacity and multiplied by a constant C_growth > 1 representing the speed at which the
    # accumulated growth score increases in all areas:
    R_M = R_M %>%
      dplyr::mutate(
        delta_A_U_m_k = C_growth * rank(R)/dplyr::n()
      )
    U_M_k = U_M_k %>% dplyr::left_join(R_M, by="m") %>%
      dplyr::mutate(A_U_m_k = A_U_m_k + delta_A_U_m_k) %>%
      select(-R,-delta_A_U_m_k)

    #  for all the un-labelled vertices, select the label M , with the highest score, and
    # if the accumulated score has reached the threshold of 1, incorporate it into the
    # labelled sub-graph, $G_M_k−1$:
    Vnew_M_k = U_M_k %>%
      dplyr::filter(A_U_m_k > 1) %>%
      dplyr::group_by(n) %>%
      dplyr::arrange(desc(A_U_m_k)) %>%
      dplyr::filter(row_number()==1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-A_U_m_k)

    # incorporate it into the labelled sub-graph, $G_M_k−1$:
    G_M_k = dplyr::bind_rows(G_M_k_minus_1,Vnew_M_k)
    # TODO: add: and remove from the unlabelled neighbourhood nodes
    U_M_k = U_M_k %>% dplyr::anti_join(Vnew_M_k, by="n")
  }

  # revert all renaming and assemble output
  if(dplyr::as_label(demandIdVar)==dplyr::as_label(supplyIdVar)) {
    demandIdVar2 = as.symbol(paste0(demandIdVar,".demand"))
    supplyIdVar2 = as.symbol(paste0(supplyIdVar,".supply"))
  } else {
    demandIdVar2 = demandIdVar
    supplyIdVar2 = supplyIdVar
  }
  if(dplyr::as_label(demandVar)==dplyr::as_label(supplyVar)) {
    demandVar = as.symbol(paste0(demandVar,".demand"))
    supplyVar = as.symbol(paste0(supplyVar,".supply"))
  }
  crossMapping = G_M_k %>% dplyr::select(!!demandIdVar2 := demandCode, !!demandVar := D_V_n, !!supplyIdVar2 := supplyCode, !!supplyVar := S_P_m, k)

  suppliedArea = demandShape %>% dplyr::inner_join(crossMapping %>% select(!!demandIdVar:=!!demandIdVar2, !!supplyIdVar2, !!supplyVar, k), by=as_label(demandIdVar))
  notSuppliedArea = demandShape %>% dplyr::anti_join(crossMapping %>% select(!!demandIdVar:=!!demandIdVar2), by=as_label(demandIdVar))
  if(identical(V_M_merged,NULL)) {
    mergedSuppliers = tibble::tibble()
  } else {
    mergedSuppliers = V_M_merged %>% dplyr::select(!!demandIdVar2 := demandCode, !!demandVar := D_V_n, !!supplyIdVar2 := supplyCode, !!supplyVar := S_P_m)
  }

  out = list(
    mergedSuppliers = mergedSuppliers,
    crossMapping = crossMapping,
    suppliedArea = suppliedArea,
    notSuppliedArea = notSuppliedArea
  )

  if (outputMap) {
    # reassemble features preserved from original supply dataframe
    message("assembling catchment area map...")
    tmp = demandShape %>% inner_join(
      crossMapping %>% select(!!demandIdVar:=!!demandIdVar2,!!supplyIdVar2),
      by = dplyr::as_label(demandIdVar)
    )
    tmp2 = tmp %>% rmapshaper::ms_dissolve(field = dplyr::as_label(supplyIdVar2), sum_fields = dplyr::as_label(demandVar))
    tmp2 = tmp2 %>% rename(!!supplyIdVar:=!!supplyIdVar2) %>% left_join(supplyFeatures, by=dplyr::as_label(supplyIdVar))
    out$map = tmp2
  }

  return(out)

}
