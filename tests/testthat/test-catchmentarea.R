test_that("multiplication works", {

  devtools::load_all()

  catchment = arear::createCatchment(
    supplyShape = testdata$gridSupply,
    supplyIdVar = id,
    supplyVar = supply,
    demandShape = testdata$gridDemand,
    demandIdVar = id,
    demandVar = demand,
    growthConstant = 1.01
  )

  ggplot()+
    geom_sf(data=catchment$suppliedArea, aes(fill=k))+
    geom_sf(data=catchment$map, fill=NA, colour="red")+
    geom_sf(data=testdata$gridSupply, aes(size=supply), colour="magenta")

})
