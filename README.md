arear
================

<!-- badges: start -->
[![R-CMD-check](https://github.com/terminological/arear/workflows/R-CMD-check/badge.svg)](https://github.com/terminological/arear/actions)
[![DOI](https://zenodo.org/badge/340773310.svg)](https://zenodo.org/badge/latestdoi/340773310)
[![arear status badge](https://terminological.r-universe.dev/badges/arear)](https://terminological.r-universe.dev)
<!-- badges: end -->

This package provides a suite of geo-spatial functions that were useful during the response to the COVID-19 pandemic in the UK. It provides some curated data relevant to hospital capacity in the NHS, and demographic data for the UK at the beginning of the pandemic. Coupled with this are a range of functions to simplify the management of mapping data between different administrative geographies, at different levels of detail that were involved in COVID-19 reporting. For mapping community case data to hospital admissions data the library provides an algorithmic hospital catchment area estimation based on label propagation, the methods for which are described in detail in the paper ["Algortihmic hospital catchment area estimation using label propagation"](https://bmchealthservres.biomedcentral.com/articles/10.1186/s12913-022-08127-7). Many of the operations in the package involve time consuming calculations, which only generally need to be performed once, so the package incorporates a transparent filesystem caching layer to speed it all up.

## Installation

`arear` is distributed via 'r-universe'. `arear` has a dependency on the `sf` package which in turn requires the `gdal`, `geos` and `proj` libraries. These can be installed on MacOS and linux using the instructions below but more details is available in the [`sf` package](https://r-spatial.github.io/sf/)

```BASH
# in debian / ubuntu
sudo add-apt-repository -y ppa:ubuntugis/ubuntugis-unstable
sudo apt-get -q update
sudo apt-get -y install libudunits2-dev libgdal-dev libgeos-dev libproj-dev libprotobuf-dev libjq-dev
# the following may also be requried to install sf
sudo ldconfig

# or on macOS
brew install gdal proj geos protobuf jq
```

With `sf` installed you can then install the development version of `arear` from [GitHub](https://github.com/terminological/arear) with the following commands:

```R
# install.packages("devtools")
devtools::install_github("terminological/arear")
```

or stable releases from `r-universe` by: 

```R
# Enable repository from terminological
options(repos = c(
  terminological = 'https://terminological.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Download and install rmaven in R
install.packages('arear')
```

This can take some time if `sf` has not already been set up.

## Example

Without this package accessing maps relevant to the UK requires knowing where to look for various shape files, and then managing the various differences between nomenclature and naming conventions. The package provides a simple interface to downloading, extracting, standardising, and caching the main UK maps. Getting and plotting a map for the UK based on the administrative code in use is made relatively simple:

```R

library(arear)
library(sf)

# list the available maps ids:
# arear::listStandardMaps()

map = arear::getMap("CTRY19")
nhshospitals = arear::surgecapacity %>% dplyr::filter(sector == "NHS Sector")

ggplot()+
  geom_sf(data=map)+
  geom_sf(data=nhshospitals, aes(colour=tier1))+
  arear::mapTheme()
```

## Next steps

Please check the getting-started vignette for more examples on the available functions, including the catchment area algorithm.

