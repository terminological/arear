library(tidyverse)

square <- rbind(c(-0.5,-0.5), c(0.5,-0.5), c(0.5,0.5), c(-0.5,0.5),c(-0.5,-0.5))
right = array(c(rep(1,5),rep(0,5)),dim = c(5,2))
up = array(c(rep(0,5),rep(1,5)),dim = c(5,2))

min = -5
max  =5

grid = tibble(x=min:max) %>% tidyr::crossing(tibble(y=min:max))
grid = grid %>% mutate(id=paste0(x,":",y)) %>% rowwise() %>%
  mutate(
    geometry = list(sf::st_polygon(list(square+x*right+y*up)))
  ) %>% sf::st_as_sf(crs=4326)

grid2 = grid %>% mutate(demand = exp(-sqrt(x^2+y^2)))

supply = tibble::tibble(x=c(min,max,max), y=c(min,min,max), supply=c(1,3,2), id=c("bottom_left","bottom_right","top_right") ) %>% sf::st_as_sf(coords = c("x", "y"),crs=4326)

ggplot(grid)+geom_sf(aes(fill=demand))+geom_sf(data=supply,aes(size=supply),colour="red")

testdata=list(
  grid = grid,
  gridDemand = grid2,
  gridSupply = supply
)

usethis::use_data(testdata,overwrite = TRUE)
