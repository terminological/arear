
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arear

<!-- badges: start -->

[![R-CMD-check](https://github.com/terminological/arear/workflows/R-CMD-check/badge.svg)](https://github.com/terminological/arear/actions)
[![Codecov test
coverage](https://codecov.io/gh/terminological/arear/branch/main/graph/badge.svg)](https://codecov.io/gh/terminological/arear?branch=main)
<!-- badges: end -->

The goal of arear is to …

## Installation

You can install the released version of arear from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("arear")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("terminological/arear")
```

## Example

There are a set of maps bundled:

``` r
devtools::load_all()
#> Error in get(genname, envir = envir) : object 'testthat_print' not found
#> Loading arear
#> Registered S3 method overwritten by 'geojsonlint':
#>   method         from 
#>   print.location dplyr
#> 
#> Attaching package: 'testthat'
#> The following object is masked from 'package:dplyr':
#> 
#>     matches
#> The following object is masked from 'package:purrr':
#> 
#>     is_null
#> The following object is masked from 'package:tidyr':
#> 
#>     matches
# library(arear)
## basic example code
arear::listStandardMaps()
#>  [1] "WD11"          "WD19"          "LSOA11"        "MSOA11"       
#>  [5] "DZ11"          "CA19"          "HB19"          "LHB19"        
#>  [9] "CTYUA19"       "LAD19"         "LAD20"         "CCG20"        
#> [13] "NHSER20"       "PHEC16"        "CTRY19"        "LGD12"        
#> [17] "OUTCODE"       "GBR_ISO3166_2" "GBR_ISO3166_3"
```

Getting and plotting a map for the UK is simple:

``` r
map = arear::getMap("CTRY19")
#> caching item: /tmp/RtmpezoPiX/CTRY19-18d2521b53cf4da65ad099c6fcf78e0c-d97019db05610cf55de038f3acd898e9.rda
#> Reading layer `Countries_(December_2019)_Boundaries_UK_BGC' from data source `/tmp/RtmpezoPiX/CTRY19' using driver `ESRI Shapefile'
#> Simple feature collection with 4 features and 10 fields
#> geometry type:  MULTIPOLYGON
#> dimension:      XY
#> bbox:           xmin: -116.1928 ymin: 5342.7 xmax: 655653.8 ymax: 1220302
#> proj4string:    +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs
#> Linking to GEOS 3.7.1, GDAL 2.2.2, PROJ 4.9.2
nhshospitals = arear::surgecapacity %>% dplyr::filter(sector == "NHS Sector")
ggplot()+
  geom_sf(data=map)+
  geom_sf(data=nhshospitals,aes(colour=tier1))
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

But the main thing is creating a catchment area:

TODO:
