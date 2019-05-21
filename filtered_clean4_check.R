#check clean4
library(dplyr)
library(lattice)
clean4.hm <- clean4
clean4.hm$AREOCENTRIC_EAST_LONGITUDE <-  round.min(clean4.hm$AREOCENTRIC_EAST_LONGITUDE,2)
clean4.hm$AREOCENTRIC_LATITUDE <-  round.min(clean4.hm$AREOCENTRIC_LATITUDE,2)
clean4.hm$SOLAR_LONGITUDE <-  round.min(clean4.hm$SOLAR_LONGITUDE,2)

clean4.hm <- clean4.hm %>% group_by(AREOCENTRIC_EAST_LONGITUDE,SOLAR_LONGITUDE,AREOCENTRIC_LATITUDE) %>% 
  summarise(
    EPI.f = fuzzy(CAT1_PRISM1_PEAK_AREA)
  )
levelplot(
  EPI.f ~ SOLAR_LONGITUDE * AREOCENTRIC_LATITUDE  , data = clean4.hm
  , col.regions = heat.colors(100)[length(heat.colors(100)):1],
  main ='co2 filtered epithermal'
)



levelplot(
  CAT1_PRISM1_PEAK_AREA ~ SOLAR_LONGITUDE * AREOCENTRIC_LATITUDE  , data = clean3
  , col.regions = heat.colors(100)[length(heat.colors(100)):1],
  main ='unfiltered epithermal'
)

levelplot(
  CAT1_PRISM1_PEAK_AREA ~ SOLAR_LONGITUDE * AREOCENTRIC_LATITUDE  , data = clean4
  , col.regions = heat.colors(100)[length(heat.colors(100)):1],
  main ='co2 filtered epithermal'
)

