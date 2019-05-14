#FILTER HEATMAP BY TEMPORAL FRAMES
load("clean3.RDate")
#other plot system shows same results
library(ggplot2)
library(dplyr)

round.min <- (function(x,m){((x/m)%>%floor())*m})

d3 <- clean3 #data in Hellas
d3$TCR <- d3$THERMAL_COUNT_RATE
d3$lat <- d3$AREOCENTRIC_LATITUDE
d3$lon <- d3$AREOCENTRIC_EAST_LONGITUDE
d3$lat  <- round.min(d3$lat, 4) #parameter 
d3$lon  <- round.min(d3$lon, 4) #parameter

plot(d3[d3$lat == -60, ]$UTC,d3[d3$lat == -60, ]$TCR )

plot.ts <- (function(d){plot(d$UTC,d$TCR)})
 



par(mfrow = c(4,4))
for(i in 0:10){
  d3[d3$lat == (-60 + i*2) & d3$UTC > "2015-03-30 CEST" ,] %>%
  # length() %>% print()
  plot.ts()
}


inDate <- "2015-03-30 CEST"     #parameter
outDate <- "2015-04-30 CEST"    #parameter
d3 <- d3[d3$UTC > inDate & d3$UTC < outDate,]
d3$TCR %>% length()

ggplot(d3, aes(lat,lon, z = TCR))+ geom_tile(aes(fill = TCR)) + theme_bw()

