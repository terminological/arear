% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arear.R
\name{getContainedIn}
\alias{getContainedIn}
\title{Generate a mapping representing how the input points fit into the output shape}
\usage{
getContainedIn(
  inputShape,
  outputShape,
  inputVars = inputShape \%>\% dplyr::groups(),
  outputVars = outputShape \%>\% dplyr::groups(),
  suffix = c(".x", ".y")
)
}
\arguments{
\item{outputShape}{- a sf containing polygons to locate the input in}

\item{inputSf}{- a sf containing points of interest (or shapes)}

\item{inputVar}{- defines the columns of the input that you want to retain (as a vars(...) list). This grouping should uniquely identify the row. If not present will use the grouping.}

\item{outputIdVar}{- defines the columns of the input that you want to retain (as a vars(...) list). This grouping should uniquely identify the row. If not present will use the grouping.}
}
\value{
- a mapping as a dataframe relating the input id column and output id columns
}
\description{
This assumes an id column in input and output shapes and
}
\examples{
# find the hospitals in a given area.
mapping = getContainedIn(inputSf = arear::surgecapacity, outputShape = arear::ukcovidmap, inputIdVar = hospitalId, outputIdVar = code)
}
