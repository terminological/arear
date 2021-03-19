test_that("multiplication works", {

  devtools::load_all()
  out = arear::interpolateByArea(
    inputDf = arear::testdata$demand11x11 %>% group_by(type),
    by="id",
    inputShape = arear::testdata$grid11x11,
    interpolateVar = demand,
    outputShape = arear::testdata$diamond11x11 %>% group_by(x,y,id)
  )

})
