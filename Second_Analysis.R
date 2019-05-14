
heatmapData

d <- heatmapData

library(dplyr)
library(lattice)


dg <- d %>% select(TCR,latM,lonM) %>% group_by(latM,lonM) 
dm <- dg %>% summarise( m = mean(TCR)) 
dm %>% with(levelplot(
  m ~ lonM * latM, data = dm
  , col.regions = heat.colors(100)[length(heat.colors(100)):1],
  main ='Hellas Planitia mean'
)
)
plot(dm$latM, dm$m)


levelplot(
  m ~ lonM * latM, data = dm
  , col.regions = heat.colors(100)[length(heat.colors(100)):1],
  main ='Hellas Planitia mean'
)

round.min <- (function(x,m){((x/m)%>%floor())*m})
d3 <- d 
d3$latM  <- round.min(d3$latM, 0.1)
d3$lonM  <- round.min(d3$lonM, 0.1)
plot(d3$latM, d3$TCR, type = "p")

plot(dm$latM, dm$m, type = "l")


#####
detach(package:plyr)
dg <- d %>% select(TCR,latM,lonM) %>% group_by(latM,lonM) %>% summarise( mTCR = mean(TCR))
dg[dg$latM == -60 & dg$lonM == 30.1,]
levelplot(
  mTCR ~ lonM * latM, data = dg
  , col.regions = heat.colors(100)[length(heat.colors(100)):1],
  main ='Hellas Planitia mean'
)

#########################
library(plyr) #plyr:summarise get overall not grouped
d2 <- ddply(d,~latM + lonM ,summarise, mean=mean(TCR),sd=sd(TCR))

plot(d2$latM, d2$mean, type = "l")

levelplot(
  mean ~ lonM * latM, data = d2
  , col.regions = heat.colors(100)[length(heat.colors(100)):1],
  main ='Hellas Planitia mean'
)


#########################
#other plot system shows same results
library(ggplot2)
round.min <- (function(x,m){((x/m)%>%floor())*m})

d3 <- clean3 #data in Hellas
d3$TCR <- d3$THERMAL_COUNT_RATE
d3$lat <- d3$AREOCENTRIC_LATITUDE
d3$lon <- d3$AREOCENTRIC_EAST_LONGITUDE
d3$lat  <- round.min(d3$lat, 1)
d3$lon  <- round.min(d3$lon, 1)
inDate <- "2015-03-30 CEST"
outDate <- "2015-04-13 CEST"
d3 <- d3[d3$UTC > inDate & d3$UTC < outDate,]
d3$TCR %>% length()

ggplot(d3, aes(lat,lon, z = TCR))+ geom_tile(aes(fill = TCR)) + theme_bw()

save(clean3, file = "clean3.RData")

