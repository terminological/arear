#' Intersection of 2 shapes
#'
#' get the intersection between to maps with ids. Caches the result in the working directory.
#'
#' @param inputShape - the input sf
#' @param outputShape - the output sf
#'
#' @export
#' @return a sf object representing the intersection of the input and output shapes.
getIntersection = function( inputShape, outputShape, suffix=c(".x",".y"), recalcArea=TRUE,... ) {

  .cached({
    message("calculating intersection ....")
    #ggplot(lad %>% mutate(.id = row_number()) %>% sf::st_cast(to="POLYGON") %>% group_by(.id) %>% slice(1) %>% ungroup()) + geom_sf()

    #inputShape = inputShape %>% sfheaders::sf_remove_holes() #sf::st_cast(to="POLYGON") %>% dplyr::slice(1)
    if(recalcArea) inputShape$area = inputShape %>% sf::st_area() %>% as.numeric()
    inputShape = inputShape %>%
      dplyr::rename_with(.cols=-geometry,.fn=function(x) paste0(x,suffix[1])) #%>%
      #mutate(.input_row = row_number())

    #outputShape = outputShape %>% sfheaders::sf_remove_holes() #sf::st_cast(to="POLYGON") %>% dplyr::slice(1)
    if(recalcArea) outputShape$area = outputShape %>% sf::st_area() %>% as.numeric()
    outputShape = outputShape %>%
      dplyr::rename_with(.cols=-geometry,.fn=function(x) paste0(x,suffix[2])) #%>%
      #mutate(.output_row = row_number())

    tmp = suppressWarnings(suppressMessages(
      inputShape %>% #select(.input_row) %>%
        sf::st_intersection(outputShape))) # %>% select(.output_row)))
    tmp$intersectionArea = tmp %>% sf::st_area() %>% as.numeric()
    # tmp = tmp %>%
    #   left_join(inputShape %>% as_tibble() %>% select(-geometry), by=".input_row", suffix=c("",suffix[1])) %>%
    #   left_join(outputShape %>% as_tibble() %>% select(-geometry), by=".output_row", suffix=suffix) %>%
    #   select(-.input_row,-.output_row)
    tmp

  },hash = list(inputShape,outputShape,suffix,recalcArea),name = "intersection", ...)

}


#' Generate a mapping representing how the input points fit into the output shape
#'
#' This assumes an id column in input and output shapes and
#'
#' @param inputSf - a sf containing points of interest (or shapes)
#' @param outputShape - a sf containing polygons to locate the input in
#' @param inputVar - defines the columns of the input that you want to retain (as a vars(...) list). This grouping should uniquely identify the row. If not present will use the grouping.
#' @param outputIdVar - defines the columns of the input that you want to retain (as a vars(...) list). This grouping should uniquely identify the row. If not present will use the grouping.
#'
#' @return - a mapping as a dataframe relating the input id column and output id columns
#' @export
#'
#' @examples
#' # find the hospitals in a given area.
#' mapping = getContainedIn(inputSf = arear::surgecapacity, outputShape = arear::ukcovidmap, inputIdVar = hospitalId, outputIdVar = code)
getContainedIn = function( inputShape,  outputShape,  inputVars = inputShape %>% dplyr::groups(), outputVars = outputShape %>% dplyr::groups(), suffix=c(".x",".y")) {

  if (identical(inputVars,NULL) | length(inputVars)==0) stop("inputVars must be defined, or inputShape must be grouped to define the unique rows")
  if (identical(outputVars,NULL) | length(inputVars)==0) stop("outputVars must be defined, or outputShape must be grouped to define the unique rows")

  #browser()
  outputShape = outputShape %>% dplyr::ungroup() %>% dplyr::mutate(tmp_output_id = row_number())
  inputShape = inputShape %>% dplyr::ungroup() %>% dplyr::mutate(tmp_input_id = row_number())
  containment = outputShape %>% sf::st_contains(inputShape)
  if(containment %>% purrr::flatten() %>% length() == 0) {
    mapping = tibble::tibble(tmp_input_id=integer(),tmp_output_id=integer())
  } else {
    mapping = tibble::tibble(
      tmp_output_id = rep(1:length(containment),sapply(containment, length)),
      tmp_input_id = unlist(containment %>% purrr::flatten())
    )
  }
  mapping = mapping %>%
    dplyr::distinct() %>%
    dplyr::left_join(inputShape %>% tibble::as_tibble() %>% select(tmp_input_id, !!!inputVars), by="tmp_input_id") %>%
    dplyr::left_join(outputShape %>% tibble::as_tibble() %>% select(tmp_output_id, !!!outputVars), by="tmp_output_id", suffix=suffix) %>%
    dplyr::select(-tmp_input_id,-tmp_output_id)
  return(mapping)
}

#' interpolate a variable from one set of shapes to another
#'
#' @param inputDf - in input data frame containing the variable(s) of interest to interpolate. Stratification of the variable can be achieved by grouping
#' @param by - the columns to use to join the inputDf to the map provided in inputShape. This is in the format of a dplyr join specification.
#' @param inputShape - an input sf map,
#' @param interpolateVar - the column that we want to do areal interpolation on,
#' @param outputShape - an output map which may be grouped by the desired output,
#' @param inputVars - a list of columns from the inputDf (as a vars(...) list) that define the stratification of inputDf and are desired in the output. Defaults to the grouping of inputDf
#' @param outputVars - a list of columns from the outputShape (as a vars(...) list) that we want preserved in output, or defined as a grouping of outputShape
#' @param aggregateFn - a function that will be applied to area weighted components of interpolateVar - defaults to sum
#' @return a dataframe containing the grouping columns, the outputIdVar and the interpolated value of interpolateVar
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

  interpolateVar = ensym(interpolateVar)

  if (identical(outputVars,NULL) | length(outputVars) == 0) stop("outputVars must be defined, or outputShape must be grouped to define the unique rows wanted in the output")
  # browser()
  # TODO: use sf::st_interpolate_aw
  if (is.null(names(by))) {
    lhsCol = by
  } else {
    lhsCol= ifelse(names(by)=="",by,names(by))
  }
  lhsCol = sapply(lhsCol,as.symbol)
  rhsCol = sapply(by,as.symbol)

  if (!as_label(interpolateVar) %in% colnames(inputDf)) stop("inputDf must contain column defined by interpolateVar: ",interpolateVar)

  inputDf = inputDf %>% select(!!!lhsCol,!!interpolateVar,!!!inputVars)
  inputShape = inputShape %>% select(!!!rhsCol,geometry)
  # suffix the join columns
  inputShape = inputShape %>% rename_with(function(x) paste0(x,".in"), .cols=-geometry)
  inputShape = inputShape %>% dplyr::ungroup() %>% sf::st_as_sf()

  # flip order of join and add the suffix
  by = sapply(rhsCol, as_label)
  names(by) =  paste0(sapply(lhsCol, as_label),".in")

  if(any(sapply(outputVars,as_label) %in% sapply(c(inputVars,interpolateVar),as_label))) stop("inputs and outputs have the same name: in:",
      paste(sapply(c(inputVars,interpolateVar),as_label),collapse=",")," and out:",
      paste(sapply(outputVars,as_label),collapse=","), " try renaming them")

  outputShape = outputShape %>% dplyr::ungroup() %>% sf::st_as_sf() %>% select(!!!outputVars,geometry)
  outputShape$area = outputShape %>% sf::st_area() %>% as.numeric()

  inputShape$area.in = inputShape %>% sf::st_area() %>% as.numeric()

  intersection = getIntersection(
    inputShape,
    outputShape,
    suffix=c("",""),
    recalcArea = FALSE
  )

  inputMismatch = sum(intersection$intersectionArea)/sum(inputShape$area)
  if(inputMismatch < 0.99) warning("Input does not match intersection: ",sprintf("%1.2f%%",(1-inputMismatch)*100)," of input not captured by outputShape")

  outputMismatch = sum(intersection$intersectionArea)/sum(outputShape$area)
  if(outputMismatch < 0.99) warning("Output does not match intersection: ",sprintf("%1.2f%%",(1-outputMismatch)*100)," of output not represented in inputShape")

  intersection = intersection %>% tibble::as_tibble() %>% dplyr::mutate(
    fracInput = intersectionArea/area.in
  )

  intersection = intersection %>% inner_join(inputDf, by=by, suffix = c("",".in"))

  mapping = intersection %>% tibble::as_tibble() %>% dplyr::select(!!interpolateVar, fracInput, !!!inputVars, !!!outputVars)
  mapping = mapping %>% dplyr::mutate(intersectionValue = !!interpolateVar * fracInput)
  mapping = mapping %>% dplyr::group_by(!!!inputVars,!!!outputVars) %>% summarise(.interp = aggregateFn(intersectionValue)) %>% dplyr::rename(!!interpolateVar := .interp)
  #
  # mapping = mapping %>% dplyr::group_by(!!!outputVars) %>% dplyr::group_modify(function(d,g,...) {
  #   return(
  #     tibble::tibble(agg = do.call(aggregateFn, list(x=d$intersectionValue))) %>% dplyr::rename(!!interpolateVar := agg)
  #   )
  # })

  return(mapping)
}

#' create a neighbourhood network from a shapefile
#'
#' @param shape - a sf object, if not present will be loaded from cache
#' @param idVar - the column containing the coded identifier of the map
#' @param bridges - a df with the following columns: name start.lat start.long end.lat end.long defining connections between non touching shapes (e.g. bridges / ferries / etc.)
#' @param queen - include neighbouring areas that touch at corners
#' @return an edge list of ids with from and to columns
createNeighbourNetwork = function(shape, idVar="code", bridges = arear::ukconnections, queen=FALSE, ...) {
  idVar = ensym(idVar)

  .cached({

    shape = shape %>% dplyr::mutate(tmp_id = row_number(), .id = !!idVar)
    bridgeStart = bridges %>% sf::st_as_sf(coords=c("start.long","start.lat"), crs=4326) %>% arear::getContainedIn(shape,inputVars = vars(name), outputVars = list(idVar))
    bridgeEnd = bridges %>% sf::st_as_sf(coords=c("end.long","end.lat"), crs=4326) %>% arear::getContainedIn(shape,inputVars = vars(name), outputVars = list(idVar))
    bridges = bridgeStart %>% rename(start = !!idVar) %>% inner_join(bridgeEnd  %>% rename(end = !!idVar),by="name") %>% filter(start != end) %>% select(-name)

    graph = spdep::poly2nb(shape %>% sf::as_Spatial(),queen=queen)
      #shape %>% sf::st_intersects()
    #browser()

    if(graph %>% purrr::flatten() %>% length() == 0) {
      edges = tibble::tibble(from_tmp_id=integer(),to_tmp_id=integer())
    } else {
      edges = tibble::tibble(
        from_tmp_id = rep(1:length(graph),sapply(graph, length)),
        to_tmp_id = unlist(graph %>% purrr::flatten())
      )
    }

    edges = edges %>%
      dplyr::left_join(shape %>% tibble::as_tibble() %>% dplyr::select(from_tmp_id = tmp_id, from = .id), by="from_tmp_id") %>%
      dplyr::left_join(shape %>% tibble::as_tibble() %>% dplyr::select(to_tmp_id = tmp_id, to = .id), by="to_tmp_id") %>%
      dplyr::filter(from != to) %>%
      dplyr::select(-from_tmp_id, -to_tmp_id) %>%
      dplyr::bind_rows(bridges %>% rename(from = start, to=end)) %>%
      dplyr::bind_rows(bridges %>% rename(from = end, to=start))

    edges

  }, hash = list(shape,idVar,bridges),name = "neighbourhood", ...)
}

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
#' @export
preview = function(shape, shapeLabelGlue = "{name}", shapePopupGlue = "{code}", poi=NULL, poiLabelGlue = "{name}", poiPopupGlue = "{code}") {

  shape = shape %>% dplyr::mutate(
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
  if(!identical(poi,NULL)) {
    poi = poi %>% dplyr::mutate(
      .label = glue::glue(poiLabelGlue),
      .popup = glue::glue(poiPopupGlue)
    )
    leaf = leaf %>% leaflet::addCircleMarkers(
      data=poi,
      color="#FF0000",
      label= ~.label,
      popup = ~.popup
    )
  }
  return(leaf)
}


