Arear
================

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
here::i_am("README.Rmd")
#> here() starts at /media/data/Git/arear
options("arear.cache.dir"=here::here("data-raw/cache"))

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
#> using cached item: /media/data/Git/arear/data-raw/cache/CTRY19-18d2521b53cf4da65ad099c6fcf78e0c-d97019db05610cf55de038f3acd898e9.rda
nhshospitals = arear::surgecapacity %>% dplyr::filter(sector == "NHS Sector")
ggplot()+
  geom_sf(data=map)+
  geom_sf(data=nhshospitals,aes(colour=tier1))
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

But the main thing is creating a catchment area:

``` r

apiTrusts = readr::read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsTrust&metric=hospitalCases&format=csv")
#> Parsed with column specification:
#> cols(
#>   areaCode = col_character(),
#>   areaName = col_character(),
#>   areaType = col_character(),
#>   date = col_date(format = ""),
#>   hospitalCases = col_double()
#> )

# There are quite a few mental health, and childrens trusts that we do not as yet have good capacity data for
# apiTrusts %>% select(areaCode,areaName) %>% distinct() %>% anti_join(arear::surgecapacity, by = c("areaCode"="trustId")) %>% View()

# there are however no extra trusts that never appear in the API.
# arear::surgecapacity %>% anti_join(apiTrusts, by = c("trustId"="areaCode"))

sup = arear::surgecapacity %>% semi_join(apiTrusts, by = c("trustId"="areaCode"))
dem = arear::uk2019demographicsmap %>% filter(code %>% stringr::str_starts("E")) %>% left_join(arear::uk2019adultpopulation %>% select(-name,-codeType), by="code")

catchment = arear::createCatchment(
  supplyShape = sup, 
  supplyIdVar = trustId, 
  supplyVar = hduBeds,
  demandShape = dem,
  demandIdVar = code, 
  demandVar = population,
  outputMap = TRUE
)
#> using cached item: /media/data/Git/arear/data-raw/cache/neighbourhood-cb4ea16f88152c75217a53de7485ecbe-1f0469b5931a334156ed70cfed0ab8e2.rda
#> although coordinates are longitude/latitude, st_contains assumes that they are planar
#> Warning in arear::createCatchment(supplyShape = sup, supplyIdVar = trustId, :
#> More than one supplier was found in a single region. These the first value will
#> be picked, and the total capacity combined, but as a result the catchment map
#> will be missing some values from the supplier list.
#> areas remaining: 32614;
#> growing into: 1494
#> areas remaining: 32429; growing into: 1752
#> areas remaining: 31785; growing into: 2621
#> areas remaining: 31038; growing into: 3308
#> areas remaining: 29750; growing into: 4205
#> areas remaining: 28484; growing into: 4935
#> areas remaining: 26451; growing into: 5723
#> areas remaining: 24534; growing into: 6273
#> areas remaining: 22014; growing into: 6664
#> areas remaining: 19220; growing into: 7050
#> areas remaining: 16642; growing into: 6812
#> areas remaining: 13881; growing into: 6416
#> areas remaining: 11436; growing into: 5798
#> areas remaining: 9376; growing into: 4875
#> areas remaining: 7126; growing into: 4019
#> areas remaining: 5577; growing into: 3365
#> areas remaining: 4311; growing into: 2741
#> areas remaining: 3338; growing into: 2133
#> areas remaining: 2565; growing into: 1678
#> areas remaining: 1805; growing into: 1266
#> areas remaining: 1373; growing into: 988
#> areas remaining: 921; growing into: 668
#> areas remaining: 570; growing into: 411
#> areas remaining: 378; growing into: 269
#> areas remaining: 257; growing into: 162
#> areas remaining: 181; growing into: 102
#> areas remaining: 128; growing into: 63
#> areas remaining: 82; growing into: 31
#> areas remaining: 61; growing into: 16
#> areas remaining: 55; growing into: 11
#> areas remaining: 45; growing into: 1
#> areas remaining: 44; growing into: 0
#> Warning in arear::createCatchment(supplyShape = sup, supplyIdVar = trustId, : No
#> futher areas to grow into. Terminating early with missing areas - it looks like
#> 44 areas are not connected.
#> assembling catchment area map...

ggplot(catchment$map)+geom_sf()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r

catchment$map %>% View()
```
