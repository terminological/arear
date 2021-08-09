
test_that("grid test 1", {


  #devtools::load_all()
  library(tidyverse)
  library(sf)

  catchment = arear::createCatchment(
    supplyShape = testdata$gridSupply2,
    supplyIdVar = id,
    supplyVar = supply,
    demandShape = testdata$gridDemand,
    demandIdVar = id,
    demandVar = demand,
    growthConstant = 1.2
  )

  ggplot()+
    geom_sf(data=catchment$suppliedArea, aes(fill=k))+
    geom_sf(data=catchment$map, fill=NA, colour="red")+
    geom_sf(data=catchment$suppliers, aes(size=supply, colour=id))

})

test_that("grid test 2", {

  # devtools::load_all()
  library(tidyverse)
  library(sf)

  catchment = arear::createCatchment(
    supplyShape = testdata$gridSupply,
    supplyIdVar = id,
    supplyVar = supply,
    demandShape = testdata$gridDemand,
    demandIdVar = id,
    demandVar = demand,
    growthConstant = 1.2
  )

  ggplot()+
    geom_sf(data=catchment$suppliedArea, aes(fill=id.supply, alpha=k))+
    geom_sf(data=catchment$map, fill=NA, colour="red")+
    geom_sf(data=catchment$suppliers, aes(size=supply, colour=id))

})

test_that("grid test 3", {

  # devtools::load_all()
  library(tidyverse)
  library(sf)

  testthat::expect_warning({
    catchment = arear::createCatchment(
      supplyShape = testdata$gridSupplyViolateConstraint,
      supplyIdVar = id,
      supplyVar = supply,
      demandShape = testdata$gridDemand,
      demandIdVar = id,
      demandVar = demand,
      growthConstant = 1.2
    )
  })

  ggplot()+
    geom_sf(data=catchment$suppliedArea, aes(fill=id.supply, alpha=k))+
    geom_sf(data=catchment$map, fill=NA, colour="red")+
    geom_sf(data=catchment$suppliers, aes(size=supply, colour=id))

})

test_that("grid test 4", {

  # devtools::load_all()
  library(tidyverse)
  library(sf)

  testthat::expect_warning({
    catchment = arear::createCatchment(
      supplyShape = testdata$gridSupplyDegenerate,
      supplyIdVar = id,
      supplyVar = supply,
      demandShape = testdata$gridDemand,
      demandIdVar = id,
      demandVar = demand,
      growthConstant = 1.2
    )
  })

  ggplot()+
    geom_sf(data=catchment$suppliedArea, aes(fill=id.supply, alpha=k))+
    geom_sf(data=catchment$map, colour="red",fill=NA)+
    geom_sf(data=catchment$suppliers, aes(size=supply, colour=id))

})
