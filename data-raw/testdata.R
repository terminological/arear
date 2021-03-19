library(tidyverse)

square <- rbind(c(-0.5,-0.5), c(0.5,-0.5), c(0.5,0.5), c(-0.5,0.5),c(-0.5,-0.5))

.repeated = function(shape, offset_x, offset_y, xlim, ylim) {
  len = dim(shape)[1]
  right = array(c(rep(offset_x[1],len),rep(offset_x[2],len)),dim = c(len,2))
  up = array(c(rep(offset_y[1],len),rep(offset_y[2],len)),dim = c(len,2))
  grid = tibble(x=min(xlim):max(xlim)) %>% tidyr::crossing(tibble(y=min(ylim):max(ylim)))
  grid = grid %>% mutate(id=paste0(x,":",y)) %>% rowwise() %>%
    mutate(
      geometry = list(sf::st_polygon(list(shape+x*right+y*up)))
    ) %>% sf::st_as_sf(crs=4326)
  return(grid)
}


# create a 11x11 grid
grid11x11 = .repeated(square, c(1,0), c(0,1), -5:5, -5:5)
ggplot(grid11x11)+geom_sf()

# create a demand in the grid as exponential decay from centre
grid2 = grid11x11 %>% mutate(demand = exp(-sqrt(x^2+y^2)))

demand = grid11x11 %>% as_tibble() %>% select(x,y,id)
demand = bind_rows(
  demand %>% mutate(demand = exp(-sqrt(x^2+y^2)), type="exp"),
  demand %>% mutate(demand = sqrt(x^2+y^2), type="radial"),
  demand %>% mutate(demand = 1, type="uniform"),
  demand %>% mutate(demand = ifelse((x+y) %% 2 == 0,1,0), type="chessboard")
)



supply = tibble::tibble(x=c(-5,5,5), y=c(-5,-5,5), supply=c(1,3,2), id=c("bottom_left","bottom_right","top_right") ) %>% sf::st_as_sf(coords = c("x", "y"),crs=4326)
ggplot(grid2)+geom_sf(aes(fill=demand))+geom_sf(data=supply,aes(size=supply),colour="red")

# a 5x5 diamond grid
diamond <- rbind(c(-0.5,0), c(0,0.5), c(0.5,0), c(0,-0.5),c(-0.5,0))
diamond5x5 = .repeated(diamond, c(0.5,0.5), c(0.5,-0.5), -2:2, -2:2)
diamond11x11 = .repeated(diamond, c(0.5,0.5), c(0.5,-0.5), -5:5, -5:5)

grid5x5 = .repeated(square, c(1,0), c(0,1), -2:2, -2:2)

ggplot()+
  geom_sf(data=diamond5x5,colour="red",fill=NA)+
  geom_sf(data=grid5x5,colour="blue",fill=NA)

supply2 = tibble::tibble(x=c(-5,5,5), y=c(-5,-5,5), supply=c(1,3,2), id=c("one","two","two") ) %>% sf::st_as_sf(coords = c("x", "y"),crs=4326)

supply3 = tibble::tibble(x=c(-5,5,5,-4.9), y=c(-5,-5,5,-4.9), supply=c(1,3,2,2), id=c("one","two","three","four") ) %>% sf::st_as_sf(coords = c("x", "y"),crs=4326)

supply4 = tibble::tibble(x=c(-5,5,5,-4.9,-5), y=c(-5,-5,5,-4.9,5), supply=c(1,3,2,2,1), id=c("one","two","three","four","four") ) %>% sf::st_as_sf(coords = c("x", "y"),crs=4326)

testdata=list(
  grid5x5 = grid5x5,
  grid11x11 = grid11x11,
  diamond5x5 = diamond5x5,
  diamond11x11 = diamond11x11,
  demand11x11 = demand,
  gridDemand = grid2,
  gridSupply = supply,
  gridSupply2 = supply2,
  gridSupplyViolateConstraint = supply3,
  gridSupplyDegenerate = supply4
)

usethis::use_data(testdata,overwrite = TRUE)
