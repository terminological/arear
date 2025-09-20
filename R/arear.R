.forceGeos = function(expr) {
  sfState = sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  out = eval(expr, envir = rlang::caller_env())
  suppressMessages(sf::sf_use_s2(sfState))
  return(out)
}


#' Intersection of 2 shapes
#'
#' get the intersection between to maps with ids. Caches the result in the working directory.
#'
#' @param inputShape - the input sf
#' @param outputShape - the output sf
#' @param suffix - the suffix of any duplicated columns as per dplyr::inner_join()
#' @param recalcArea - do you need the area of the intersected shape (e.g. for areal interpolation)
#' @param ... passed on to .cached() (cache control) - relevant is nocache = TRUE which prevents this from being precalculated
#'
#' @concept analysis
#' @export
#' @return a sf object representing the intersection of the input and output shapes.
getIntersection = function(
  inputShape,
  outputShape,
  suffix = c(".x", ".y"),
  recalcArea = TRUE,
  ...
) {
  .cached(
    {
      .forceGeos({
        message("calculating intersection ....")
        #ggplot2::ggplot(lad %>% dplyr::mutate(.id = dplyr::row_number()) %>% sf::st_cast(to="POLYGON") %>% dplyr::group_by(.id) %>% dplyr::slice(1) %>% dplyr::ungroup()) + ggplot2::geom_sf()

        #inputShape = inputShape %>% sfheaders::sf_remove_holes() #sf::st_cast(to="POLYGON") %>% dplyr::slice(1)
        if (recalcArea) {
          inputShape$area = inputShape %>% sf::st_area() %>% as.numeric()
        }
        inputShape = inputShape %>%
          dplyr::rename_with(.cols = -geometry, .fn = function(x) {
            paste0(x, suffix[1])
          }) #%>%
        #dplyr::mutate(.input_row = dplyr::row_number())

        #outputShape = outputShape %>% sfheaders::sf_remove_holes() #sf::st_cast(to="POLYGON") %>% dplyr::slice(1)
        if (recalcArea) {
          outputShape$area = outputShape %>% sf::st_area() %>% as.numeric()
        }
        outputShape = outputShape %>%
          dplyr::rename_with(.cols = -geometry, .fn = function(x) {
            paste0(x, suffix[2])
          }) #%>%
        #dplyr::mutate(.output_row = dplyr::row_number())

        tmp = suppressWarnings(suppressMessages(
          inputShape %>% #dplyr::select(.input_row) %>%
            sf::st_intersection(outputShape)
        )) # %>% dplyr::select(.output_row)))
        tmp$intersectionArea = tmp %>% sf::st_area() %>% as.numeric()
        # tmp = tmp %>%
        #   dplyr::left_join(inputShape %>% tibble::as_tibble() %>% dplyr::select(-geometry), by=".input_row", suffix=c("",suffix[1])) %>%
        #   dplyr::left_join(outputShape %>% tibble::as_tibble() %>% dplyr::select(-geometry), by=".output_row", suffix=suffix) %>%
        #   dplyr::select(-.input_row,-.output_row)
      })
      tmp
    },
    hash = list(inputShape, outputShape, suffix, recalcArea),
    name = "intersection",
    ...
  )
}


#' Generate a mapping table between 2 `sf` dataframes.
#'
#' This assumes unique keys defined in input and output shapes through column
#' grouping and outputs a mapping table between input and output groups. The
#' input is related to the output by containment. I.e. the result will be where
#' the input is wholly contained within the output shape
#'
#' @param inputShape - a sf containing points of interest (or shapes)
#' @param outputShape - a sf containing polygons to locate the input in.
#' @param inputVars - defines the columns of the input that you want to retain
#'   (as a dplyr::vars(...) list). This grouping should uniquely identify the
#'   row. If not present will use the current grouping of inputShape.
#' @param outputVars - defines the columns of the output that you want to retain
#'   (as a dplyr::vars(...) list). This grouping should uniquely identify the
#'   row. If not present will use the current grouping of outputShape.
#' @param suffix - the suffix of any duplicated columns as per dplyr::inner_join()
#'
#' @return - a mapping as a dataframe relating the `inputVars` columns and
#' `outputVars` columns
#'
#' @concept analysis
#' @export
#'
#' @examples
#' # find the hospitals in a given area.
#' mapping = getContainedIn(
#'   inputShape = arear::surgecapacity,
#'   outputShape = arear::ukcovidmap(),
#'   inputVars = dplyr::vars(hospitalId),
#'   outputVars = dplyr::vars(code)
#' )
getContainedIn = function(
  inputShape,
  outputShape,
  inputVars = inputShape %>% dplyr::groups(),
  outputVars = outputShape %>% dplyr::groups(),
  suffix = c(".x", ".y")
) {
  .forceGeos({
    if (identical(inputVars, NULL) | length(inputVars) == 0) {
      stop(
        "inputVars must be defined, or inputShape must be grouped to define the unique rows"
      )
    }
    if (identical(outputVars, NULL) | length(inputVars) == 0) {
      stop(
        "outputVars must be defined, or outputShape must be grouped to define the unique rows"
      )
    }

    #browser()
    outputShape = outputShape %>%
      dplyr::ungroup() %>%
      dplyr::mutate(tmp_output_id = dplyr::row_number())
    inputShape = inputShape %>%
      dplyr::ungroup() %>%
      dplyr::mutate(tmp_input_id = dplyr::row_number())

    containment = outputShape %>% sf::st_contains(inputShape)
    if (containment %>% purrr::flatten() %>% length() == 0) {
      mapping = tibble::tibble(
        tmp_input_id = integer(),
        tmp_output_id = integer()
      )
    } else {
      mapping = tibble::tibble(
        tmp_output_id = rep(1:length(containment), sapply(containment, length)),
        tmp_input_id = unlist(containment %>% purrr::flatten())
      )
    }
    mapping = mapping %>%
      dplyr::distinct() %>%
      dplyr::left_join(
        inputShape %>%
          tibble::as_tibble() %>%
          dplyr::select(tmp_input_id, !!!inputVars),
        by = "tmp_input_id"
      ) %>%
      dplyr::left_join(
        outputShape %>%
          tibble::as_tibble() %>%
          dplyr::select(tmp_output_id, !!!outputVars),
        by = "tmp_output_id",
        suffix = suffix
      ) %>%
      dplyr::select(-tmp_input_id, -tmp_output_id)
    return(mapping)
  })
}

#' interpolate a variable from one set of shapes to another
#'
#' @param inputDf - in input data frame containing the variable(s) of interest
#'   to interpolate. Stratification of the variable can be achieved by grouping
#' @param by - the columns to use to join the inputDf to the map provided in
#'   inputShape. This is in the format of a dplyr join specification.
#' @param inputShape - an input sf map,
#' @param interpolateVar - the column that we want to do areal interpolation on,
#' @param outputShape - an output map which may be grouped by the desired
#'   output,
#' @param inputVars - a list of columns from the inputDf (as a
#'   `dplyr::vars(...)` list) that define the stratification of inputDf and are
#'   desired in the output. Defaults to the grouping of inputDf
#' @param outputVars - a list of columns from the outputShape (as a
#'   `dplyr::vars(...)` list) that we want preserved in output, or defined as a
#'   grouping of outputShape
#' @param aggregateFn - a function that will be applied to area weighted
#'   components of interpolateVar - defaults to sum
#' @return a dataframe containing the grouping columns in `inputVars` and
#'   `outputVars`, and the interpolated value of `interpolateVar`
#'
#' @concept analysis
#' @export
interpolateByArea = function(
  inputDf,
  inputShape,
  by,
  interpolateVar,
  outputShape,
  inputVars = inputDf %>% dplyr::groups(),
  outputVars = outputShape %>% dplyr::groups(),
  aggregateFn = sum
) {
  .forceGeos({
    interpolateVar = rlang::ensym(interpolateVar)

    if (identical(outputVars, NULL) | length(outputVars) == 0) {
      stop(
        "outputVars must be defined, or outputShape must be grouped to define the unique rows wanted in the output"
      )
    }
    # browser()
    # TODO: use sf::st_interpolate_aw
    if (is.null(names(by))) {
      lhsCol = by
    } else {
      lhsCol = ifelse(names(by) == "", by, names(by))
    }
    lhsCol = sapply(lhsCol, as.symbol)
    rhsCol = sapply(by, as.symbol)

    if (!rlang::as_label(interpolateVar) %in% colnames(inputDf)) {
      stop(
        "inputDf must contain column defined by interpolateVar: ",
        interpolateVar
      )
    }

    inputDf = inputDf %>%
      dplyr::select(!!!lhsCol, !!interpolateVar, !!!inputVars)
    inputShape = inputShape %>% dplyr::select(!!!rhsCol, geometry)
    # suffix the join columns
    inputShape = inputShape %>%
      dplyr::rename_with(function(x) paste0(x, ".in"), .cols = -geometry)
    inputShape = inputShape %>% dplyr::ungroup() %>% sf::st_as_sf()

    # flip order of join and add the suffix
    by = sapply(rhsCol, dplyr::as_label)
    names(by) = paste0(sapply(lhsCol, dplyr::as_label), ".in")

    if (
      any(
        sapply(outputVars, as_label) %in%
          sapply(c(inputVars, interpolateVar), as_label)
      )
    ) {
      stop(
        "inputs and outputs have the same name: in:",
        paste(sapply(c(inputVars, interpolateVar), as_label), collapse = ","),
        " and out:",
        paste(sapply(outputVars, as_label), collapse = ","),
        " try renaming them"
      )
    }

    outputShape = outputShape %>%
      dplyr::ungroup() %>%
      sf::st_as_sf() %>%
      dplyr::select(!!!outputVars, geometry)
    outputShape$area = outputShape %>% sf::st_area() %>% as.numeric()

    inputShape$area.in = inputShape %>% sf::st_area() %>% as.numeric()

    intersection = getIntersection(
      inputShape,
      outputShape,
      suffix = c("", ""),
      recalcArea = FALSE
    )

    inputMismatch = sum(intersection$intersectionArea) / sum(inputShape$area)
    if (inputMismatch < 0.99) {
      warning(
        "Input does not match intersection: ",
        sprintf("%1.2f%%", (1 - inputMismatch) * 100),
        " of input not captured by outputShape"
      )
    }

    outputMismatch = sum(intersection$intersectionArea) / sum(outputShape$area)
    if (outputMismatch < 0.99) {
      warning(
        "Output does not match intersection: ",
        sprintf("%1.2f%%", (1 - outputMismatch) * 100),
        " of output not represented in inputShape"
      )
    }

    intersection = intersection %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        fracInput = intersectionArea / area.in
      )

    intersection = intersection %>%
      dplyr::inner_join(inputDf, by = by, suffix = c("", ".in"))

    mapping = intersection %>%
      tibble::as_tibble() %>%
      dplyr::select(!!interpolateVar, fracInput, !!!inputVars, !!!outputVars)
    mapping = mapping %>%
      dplyr::mutate(intersectionValue = !!interpolateVar * fracInput)
    mapping = mapping %>%
      dplyr::group_by(!!!inputVars, !!!outputVars) %>%
      dplyr::summarise(.interp = aggregateFn(intersectionValue)) %>%
      dplyr::rename(!!interpolateVar := .interp)

    return(mapping)
  })
}

#' Create a neighbourhood network from touching regions in a map
#'
#' A network of neighbouring map regions including connections due to bridges
#' airports or ferry links, defined in the bridges input.
#'
#' @param shape a `sf` object
#' @param idVar the column containing the coded identifier of the map
#' @param bridges a df with the following columns: `name` `start.lat` `start.long`
#'   `end.lat` `end.long` defining connections between non touching shapes (e.g.
#'   bridges / ferries / etc.)
#' @param queen - include neighbouring areas that only touch at corners,
#'   defaults to false.
#' @inheritDotParams .cached .nocache .stale
#' @return an edge list of ids with from and to columns
#'
#' @concept analysis
#' @export
#' @examples
#'
#' edges = createNeighbourNetwork(
#'   shape = arear::testdata$grid11x11,
#'   idVar = "id"
#' )
#'
#' # in regular grid each cell has 4 neighbours except the edges and corners
#' # we loose 1 per edge
#'
#' nrow(edges) == 11*11*4-4*11
#'
#' queens = createNeighbourNetwork(
#'   shape = arear::testdata$grid11x11,
#'   idVar = "id",
#'   queen = TRUE
#' )
#'
#' # each cell has 8 queen neighbours
#' # except edge pieces which have 3 less, and corners which have 5 less.
#' nrow(queens) == 11*11*8 - 4*9*3 - 4*5
createNeighbourNetwork = function(
  shape,
  idVar = "code",
  bridges = arear::ukconnections,
  queen = FALSE,
  ...
) {
  idVar = rlang::ensym(idVar)

  .cached(
    {
      .forceGeos({
        shape = shape %>%
          dplyr::mutate(tmp_id = 1:nrow(shape), .id = !!idVar)
        bridgeStart = bridges %>%
          sf::st_as_sf(coords = c("start.long", "start.lat"), crs = 4326) %>%
          arear::getContainedIn(
            shape,
            inputVars = dplyr::vars(name),
            outputVars = list(idVar)
          )
        bridgeEnd = bridges %>%
          sf::st_as_sf(coords = c("end.long", "end.lat"), crs = 4326) %>%
          arear::getContainedIn(
            shape,
            inputVars = dplyr::vars(name),
            outputVars = list(idVar)
          )
        bridges = bridgeStart %>%
          dplyr::rename(start = !!idVar) %>%
          dplyr::inner_join(
            bridgeEnd %>% dplyr::rename(end = !!idVar),
            by = "name"
          ) %>%
          dplyr::filter(start != end) %>%
          dplyr::select(-name)
        # browser()

        graph = spdep::poly2nb(shape %>% sf::as_Spatial(), queen = queen)
        #shape %>% sf::st_intersects()
        #browser()

        if (graph %>% purrr::flatten() %>% length() == 0) {
          edges = tibble::tibble(from_tmp_id = integer(), to_tmp_id = integer())
        } else {
          edges = tibble::tibble(
            from_tmp_id = rep(1:length(graph), sapply(graph, length)),
            to_tmp_id = unlist(graph %>% purrr::flatten())
          )
        }

        edges = edges %>%
          dplyr::left_join(
            shape %>%
              tibble::as_tibble() %>%
              dplyr::select(from_tmp_id = tmp_id, from = .id),
            by = "from_tmp_id"
          ) %>%
          dplyr::left_join(
            shape %>%
              tibble::as_tibble() %>%
              dplyr::select(to_tmp_id = tmp_id, to = .id),
            by = "to_tmp_id"
          ) %>%
          dplyr::filter(from != to) %>%
          dplyr::select(-from_tmp_id, -to_tmp_id) %>%
          dplyr::bind_rows(
            bridges %>% dplyr::rename(from = start, to = end)
          ) %>%
          dplyr::bind_rows(bridges %>% dplyr::rename(from = end, to = start))

        return(edges)
      })
    },
    hash = list(shape, idVar, bridges, queen),
    name = "neighbourhood",
    ...
  )
}

# Visualisations ----

#' Preview a map with POI using leaflet
#'
#' @param shape - the map
#' @param shapeLabelGlue - a glue specification for the label for each shape
#' @param shapePopupGlue - a glue specification for the popup for each shape
#' @param poi - a list of points of interest as a sf object
#' @param poiLabelGlue - a glue specification for the label for each poi
#' @param poiPopupGlue - a glue specification for the popup for each poi
#'
#' @return htmlwidget
#'
#' @concept vis
#' @export
preview = function(
  shape,
  shapeLabelGlue = "{name}",
  shapePopupGlue = "{code}",
  poi = NULL,
  poiLabelGlue = "{name}",
  poiPopupGlue = "{code}"
) {
  shape = shape %>%
    dplyr::mutate(
      .label = glue::glue(shapeLabelGlue),
      .popup = glue::glue(shapePopupGlue)
    )
  leaf = leaflet::leaflet(shape) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      data = shape,
      label = ~.label,
      popup = ~.popup
    )
  if (!identical(poi, NULL)) {
    poi = poi %>%
      dplyr::mutate(
        .label = glue::glue(poiLabelGlue),
        .popup = glue::glue(poiPopupGlue)
      )
    leaf = leaf %>%
      leaflet::addCircleMarkers(
        data = poi,
        color = "#FF0000",
        label = ~.label,
        popup = ~.popup
      )
  }
  return(leaf)
}

#' A map theme to remove extraneous clutter
#'
#' @export
#' @concept vis
#'
#' @examples
#'
#' ggplot2::ggplot(arear::testdata$gridDemand)+
#'   ggplot2::geom_sf(ggplot2::aes(fill=demand))+
#'   mapTheme()
mapTheme = function() {
  return(
    ggplot2::theme(
      axis.text.x.top = ggplot2::element_blank(),
      axis.text.x.bottom = ggplot2::element_blank(),
      axis.text.y.right = ggplot2::element_blank(),
      axis.text.y.left = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
  )
}


#' Create a magnified pop-out panel of a map
#'
#' Create a new map with a pop-out panel to show areas where there is a high
#' density of people for example. Defaults work well for London on an England
#' only map.
#'
#' @param shape The original shape
#' @param popoutShape The mask shape. The outer boundary of this shape will be
#'   used as a mask to select the original shape
#' @param popoutScale A factor to grow the popout area by. This is linear scale
#'   so the popout will appear the square of this factor bigger.
#' @param popoutPosition Which corner to place the popout NE,NW,SE or SW
#' @param nudgeX shift the popout panel by a small amount (in coordinate units)
#' @param nudgeY shift the popout panel by a small amount (in coordinate units)
#'
#' @return A new map with the content intersecting the popout area duplicated,
#'   expanded and placed in the specified corner.

#' @concept vis
#' @export
#' @examples
#' tmp = popoutArea(
#'   arear::testdata$diamond11x11 %>% dplyr::mutate(value = x+y),
#'   arear::testdata$offsetBox,
#'   popoutPosition = "NE",
#'   popoutScale = 1.25
#' )
#'
#' ggplot2::ggplot(tmp)+
#'   ggplot2::geom_sf(ggplot2::aes(fill=value))+
#'   ggplot2::scale_fill_gradient2()+
#'   ggplot2::geom_sf(data = arear::testdata$offsetBox, alpha=0)
popoutArea = function(
  shape,
  popoutShape = arear::londonShape,
  popoutPosition = c("NE", "NW", "SE", "SW"),
  popoutScale = 3,
  nudgeX = 0.25,
  nudgeY = 0.25
) {
  popoutPosition = match.arg(popoutPosition)
  popoutShape = popoutShape %>% dplyr::ungroup() %>% dplyr::summarise()
  bigBox = shape %>% sf::st_bbox() %>% as.list()
  smallBox = popoutShape %>% sf::st_bbox() %>% as.list()
  width = (smallBox$xmax - smallBox$xmin) * popoutScale
  height = (smallBox$ymax - smallBox$ymin) * popoutScale

  xmin = switch(
    popoutPosition,
    "NE" = bigBox$xmax - width,
    "NW" = bigBox$xmin,
    "SE" = bigBox$xmax - width,
    "SW" = bigBox$xmin
  ) +
    nudgeX
  xmax = switch(
    popoutPosition,
    "NE" = bigBox$xmax,
    "NW" = bigBox$xmin + width,
    "SE" = bigBox$xmax,
    "SW" = bigBox$xmin + width
  ) +
    nudgeX

  ymin = switch(
    popoutPosition,
    "NE" = bigBox$ymax - height,
    "NW" = bigBox$ymax - height,
    "SE" = bigBox$ymin,
    "SW" = bigBox$ymin
  ) +
    nudgeY
  ymax = switch(
    popoutPosition,
    "NE" = bigBox$ymax,
    "NW" = bigBox$ymax,
    "SE" = bigBox$ymin + height,
    "SW" = bigBox$ymin + height
  ) +
    nudgeY

  newShape = shape %>%
    sf::st_intersection(popoutShape) %>%
    dplyr::select(-tidyselect::ends_with(".1"))
  newCentre = c(
    (smallBox$xmin + smallBox$xmax) / 2,
    (smallBox$ymin + smallBox$ymax) / 2
  )

  movedCentre = c((xmin + xmax) / 2, (ymin + ymax) / 2)
  movedShape = newShape
  movedShape$geometry = (sf::st_geometry(movedShape) - newCentre) *
    popoutScale +
    movedCentre
  movedShape = movedShape %>% sf::st_set_crs(shape %>% sf::st_crs())

  out = dplyr::bind_rows(
    shape %>% dplyr::mutate(inset = FALSE),
    movedShape %>% dplyr::mutate(inset = TRUE)
  )
}


# A simple table as a ggplot patchwork object, no customisation allowed
.gg_simple_table = function(df, pts = 8) {
  p = suppressWarnings(suppressMessages({
    ttheme = gridExtra::ttheme_minimal(
      base_size = pts,
      base_colour = "black",
      parse = FALSE,
      padding = grid::unit(c(4, 1.5), "mm"),
      core = list(
        fg_params = list(hjust = 0, x = 0.1),
        bg_params = list(fill = "#FFFFFF", alpha = 1, col = NA)
      ),
      colhead = list(
        fg_params = list(hjust = 0, x = 0.1),
        bg_params = list(fill = "#FFFFFF", alpha = 1, col = NA)
      )
    )
    g = gridExtra::tableGrob(d = df, rows = NULL, theme = ttheme)
    g <- gtable::gtable_add_grob(
      g,
      grobs = grid::segmentsGrob(
        # line across the bottom
        x0 = grid::unit(0, "npc"),
        y0 = grid::unit(0, "npc"),
        x1 = grid::unit(1, "npc"),
        y1 = grid::unit(0, "npc"),
        gp = grid::gpar(lwd = 2.0)
      ),
      t = nrow(g),
      l = 1,
      r = ncol(g)
    )
    g <- gtable::gtable_add_grob(
      g,
      grobs = grid::grobTree(
        grid::segmentsGrob(
          # line across the top
          x0 = grid::unit(0, "npc"),
          y0 = grid::unit(1, "npc"),
          x1 = grid::unit(1, "npc"),
          y1 = grid::unit(1, "npc"),
          gp = grid::gpar(lwd = 2.0)
        ),
        grid::segmentsGrob(
          # line across the bottom
          x0 = grid::unit(0, "npc"),
          y0 = grid::unit(0, "npc"),
          x1 = grid::unit(1, "npc"),
          y1 = grid::unit(0, "npc"),
          gp = grid::gpar(lwd = 1.0)
        )
      ),
      t = 1,
      l = 1,
      r = ncol(g)
    )
    #if(!unwrapped) return(patchwork::wrap_ggplot_grob(g))
    return(patchwork::wrap_elements(g))
  }))
  return(p)
}

#' Create a map, usually as a chloropleth, with selected areas labelled.
#'
#' This can be used to pick out specific highlighted regions based on a filter,
#' label it on a map using a short code, and provide a tabular lookup of label
#' to full name.
#'
#' @param data A sf object with some data in it. If using facets this should be
#'   grouped. (and if it is grouped faceting will be automatically added)
#' @param mapping the aesthetics as would be passed to `geom_sf`
#' @param ... additional formatting parameters as would be passed to `geom_sf`
#'   (defaults to a thin grey line for the edge of the maps.)
#' @param labelMapping the aesthetics of the label layer. This could include any
#'   aesthetics that apply to `ggrepel::geom_label_repel` other than `x`,`y`..
#'   It must include a label aesthetic (which will go on the map) and a name
#'   aesthetic (which will go in the lookup table)
#' @param labelStyle any additional formatting parameters that would be passed
#'   to `ggrepel::geom_label_repel`. Defaults to a blue label on a light
#'   transparent background which works for dark maps. A
#'   `list(segment.colour = "cyan", colour="cyan", fill=="#000000A0")`
#'   should give a cyan label on a dark transparent background which might work
#'   for lighter maps.
#' @param labelSort (optional) how should we sort the labels . This defaults to
#'   the descending order of the same variable that determines the fill of the
#'   main map. This should be a simple expression that you might use for
#'   `dplyr::arrange` and can include `desc` for descending.
#' @param labels how many labels do you want, per facet. The default 6 is good
#'   for a small number of facets. This will be overridden if `labelFilter` is
#'   specified
#' @param labelSize in points.
#' @param tableSize the labels and the other data from all facets will
#'   be assembled into a table as a ggplot/patchwork object. This defines the
#'   font size (in points) of this table. No other config is allowed.
#' @param labelInset if a map has an zoomed in inset as produced by
#'   `popoutArea()`, for areas which are in both the main map and the inset you
#'   may wish to label only the zoomed area in the "inset", only the unzoomed
#'   area in the "main" map or "both" (the default).
#'
#' @return a list containing 4 items. Plot and legend may be added together to
#'   form a ggplot patchwork. e.g. `p = plotLabelledMap(...)` then
#'   `p$plot+ggplot2::scale_fill_viridis_c()+ggplot2::facet_wrap(dplyr::vars(...))+p$legend+patchwork::plot_annotation(taglevels="A")`
#'   to actually show the map.
#' \describe{
#'   \item{plot}{a ggplot object showing a chloropleth (usually) which is
#'   defined by the main mapping aesthetics, with an overlaid labelling layer
#'   defined by the `labelMapping` label aesthetic. This does not include fill or
#'   colour scales so you will probably want `plot+ggplot2::scale_fill_viridis_c()`
#'   or something similar to define the fill}. If the input data is grouped this
#'   plot will be facetted by group.
#'   \item{legend}{a ggplot patchwork containing the lookup table from labels
#'   to other data (as determined by the `labelMapping` aesthetics)}
#'   \item{labelDf}{the filtered dataframe of the labels appearing in the
#'   labelling layer. The .x and .y columns are added which show where the
#'   label is placed on the main map. the .label and .name show the labels
#'   and names respectively}
#'   \item{labeller}{A function that returns a layer of the labels, formatted
#'   in same way as the main map. the labeller function takes optional xVar
#'   and yVar parameter which are columns in the sf object. These define the
#'   x and y aesthetics of the labeller and default to the same position as
#'   the main map. The labeller function can be used to add a labels layer to
#'   a different map, or to a different graph. This might be useful if you want
#'   to combine cartograms with points of interest and have them consistently
#'   labelled.}
#' }
#'
#' @concept vis
#' @export
#'
#' @examples
#' # create some test data:
#' tmp = dplyr::bind_rows(lapply(1:4,
#'   function(i) testdata$diamond11x11 %>%
#'     dplyr::mutate(set = sprintf("set %d",i), value = runif(x)
#' ))) %>% dplyr::group_by(set) %>%
#'   dplyr::mutate(name = sprintf("%s-%s", letters[x+6], letters[y+6]))
#'
#' ggplot2::ggplot(tmp)+ggplot2::geom_sf(ggplot2::aes(fill = value))+
#'   ggplot2::facet_wrap(~set)
#'
#' p = plotLabelledMap(
#'   data = tmp,
#'   mapping = ggplot2::aes(fill = value),
#'   labelMapping = ggplot2::aes(label=name,percent=sprintf("%1.1f%%",value*100)),
#'   labels = 2
#' )
#'
#' p$plot+p$legend
#'
plotLabelledMap = function(
  data,
  mapping,
  ...,
  labelMapping,
  labelStyle = list(),
  labelSort = NULL,
  labels = 6,
  labelSize = 6,
  tableSize = 6,
  labelInset = c("both", "inset", "main")
) {
  . = .label = .name = .x = .y = inset = label = NULL # keep R CMD check happy

  if (!is.null(labelSort)) {
    labelSort = rlang::enquo(labelSort)
  } else {
    labelSort = rlang::expr(dplyr::desc(!!mapping$fill))
  }
  if (!rlang::is_quosure(labelSort)) {
    labelInset = match.arg(labelInset)
  }

  labelStyle = utils::modifyList(
    list(
      min.segment.length = 0,
      segment.colour = "blue",
      colour = "blue",
      fill = "#F0F0F0A0",
      size = labelSize / 2.845276,
      segment.size = 0.25,
      nudge_x = -1
    ), #2.845276 is the same as ggplot2:::.pt
    labelStyle
  )

  fillDots = rlang::list2(...)

  mapStyle = utils::modifyList(
    list(size = 0.05, colour = "grey"),
    fillDots
  )

  grps = data %>% dplyr::groups()

  if (!exists("label", labelMapping)) {
    stop("label aesthetic must be defined in labelMapping.")
  }

  p2a = ggplot2::ggplot()
  p2a = p2a +
    do.call(ggplot2::geom_sf, c(list(data = data, mapping = mapping), mapStyle))

  if (labels > 0) {
    if (!("inset" %in% colnames(data))) {
      data = data %>% dplyr::mutate(inset = FALSE)
    }

    data$.index_id = 1:nrow(data)

    # Complex. We need to consider only unique labels and rank them according to
    # the sorting specification allowing equal ranks.
    selected_data = data %>%
      tibble::as_tibble() %>%
      dplyr::group_by(!!!grps) %>%
      dplyr::filter(inset == FALSE) %>%
      dplyr::arrange(!!labelSort) %>%
      dplyr::mutate(rank = !!labelSort) %>%
      dplyr::transmute(!!!labelMapping, rank) %>%
      dplyr::group_by(!!!grps, label) %>%
      # deduplicates by label
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::group_by(!!!grps) %>%
      dplyr::arrange(!!!grps, rank) %>%
      # the nth ran allowing for uqal ranks
      dplyr::filter(rank <= dplyr::nth(sort(rank), labels)) %>%
      dplyr::select(-rank)

    # browser()

    mapLabs =
      data %>%
      dplyr::transmute(!!!labelMapping, inset) %>%
      dplyr::semi_join(
        selected_data %>% dplyr::select(!!!grps, label),
        by = dplyr::join_by(!!!grps, label)
      ) %>%
      dplyr::ungroup() %>%
      sf::st_centroid() %>%
      dplyr::mutate(
        .x = sf::st_coordinates(.)[, "X"],
        .y = sf::st_coordinates(.)[, "Y"],
      ) %>%
      tibble::as_tibble()

    if (labelInset == "inset") {
      mapLabs = mapLabs %>%
        dplyr::group_by(!!!grps, label) %>%
        dplyr::filter(dplyr::n() == 1 | inset == TRUE)
    }

    if (labelInset == "main") {
      mapLabs = mapLabs %>%
        dplyr::group_by(!!!grps, label) %>%
        dplyr::filter(dplyr::n() == 1 | inset == FALSE)
    }

    # browser()

    labeller = function(xVar = ".x", yVar = ".y") {
      xVar = rlang::ensym(xVar)
      yVar = rlang::ensym(yVar)
      do.call(
        ggrepel::geom_label_repel,
        c(
          list(
            data = mapLabs,
            #utils::modifyList(
            #labelMapping,
            mapping = ggplot2::aes(x = !!xVar, y = !!yVar, label = label),
            #)
            inherit.aes = FALSE
          ),
          labelStyle
        )
      )
    }

    p2b = p2a + labeller(.x, .y)

    p2 = selected_data %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(!!!grps)) %>%
      dplyr::rename(key = label) %>%
      dplyr::distinct() %>%
      .gg_simple_table(pts = tableSize)
  } else {
    p2b = p2a
    p2 = NA
    mapLabs = NA
    labeller = function(xVar = ".x", yVar = ".y") {
      return(list())
    }
  }

  if (length(grps) == 2) {
    p2b = p2b + ggplot2::facet_grid(rows = grps[[2]], cols = grps[[1]])
  } else if (length(grps) > 0) {
    p2b = p2b + ggplot2::facet_wrap(grps)
  }

  return(list(
    plot = p2b,
    legend = p2,
    labelDf = mapLabs,
    labeller = labeller
  ))
}
