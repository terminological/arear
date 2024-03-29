% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catchment-area.R
\name{createCatchment}
\alias{createCatchment}
\title{Create a catchment area map}
\usage{
createCatchment(
  supplyShape,
  supplyIdVar = "code",
  supplyVar,
  supplyOutputVars = supplyShape \%>\% dplyr::groups(),
  demandShape,
  demandIdVar = "code",
  demandVar,
  growthConstant = 1.2,
  bridges = arear::ukconnections,
  outputMap = TRUE
)
}
\arguments{
\item{supplyShape}{- a sf object containing a list of the locations of supply points, with a column containing supply capacity, for example NHS hospital sites, with a bed}

\item{supplyIdVar}{- the variable name of the identifier of the supplier or group of suppliers. For example this could be an NHS trust (multiple sites)}

\item{supplyVar}{- the column name of the supply parameter. This could be number of beds in a hospital.}

\item{supplyOutputVars}{- (optional - defaults to grouping) the columns from the input that are to be retained in the output}

\item{demandShape}{- the sf object with the geographical map of the demand surface. For example the geographical distribution of the population served,}

\item{demandIdVar}{- the column name of the unique identifier of the areas,}

\item{demandVar}{- the column name of the demand parameter. This could be the population in each region}

\item{growthConstant}{- a growth parameter which defines how quickly each label propagates}

\item{bridges}{- a named list containing extra linkages beyond those inferred by the demandShape topology. These are used to add in bridges}

\item{outputMap}{- should we export a shapefile or just the mapping file}
}
\value{
a dataframe containing the grouping columns, the outputIdVar and the interpolated value of interpolateVar
}
\description{
This implements the label propagation algorithm described in our upcoming paper.
}
