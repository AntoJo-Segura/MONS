#### CAT1_PRISM1_PEAK_AREA########

#3rd EDA: try to emulate MDAP MONS EDAS

latitude.upper <- -30
latitude.lower <- -60
longitud.upper <- 90
longitud.lower <- 30

# grid3 <- getGrid(latitude.upper, latitude.lower, longitud.upper, longitud.lower)
# clean3 <- grid3[grid3$SOLAR_LONGITUDE != 0,]
# plot(clean3$SC_RECV_TIME, clean3$CF_ATM_EPI, type='l')
# plot(clean3$SC_RECV_TIME, clean3$THERMAL_COUNT_RATE, type='l')
# plot(clean3$UTC, clean3$THERMAL_COUNT_RATE, type='l')

#save(grid3,file ='grid3.RData')
clean3b <- get(load(file = 'C:/Users/AntoJo/Documents/R/WorkSpace/MONS/clean3.RData'))
grid3b <- get(load(file = 'C:/Users/AntoJo/Documents/R/WorkSpace/MONS/grid3.RData'))


###############
library(lattice)

# heatmapData <- cbind(grid3$AREOCENTRIC_LATITUDE,grid3$AREOCENTRIC_EAST_LONGITUDE,grid3$THERMAL_COUNT_RATE)


heatmapData <- data.frame(
  lat = grid3$AREOCENTRIC_LATITUDE,
  lon = grid3$AREOCENTRIC_EAST_LONGITUDE,
  TCR = grid3$CAT1_PRISM1_PEAK_AREA  #THERMAL_COUNT_RATE
)
heatmapData <- na.omit(heatmapData)
round.min <- (function(x,m){((x/m)%>%floor())*m})
latM <- round.min(heatmapData$lat,0.1)
lonM <- round.min(heatmapData$lon,0.1)
heatmapData <- cbind(heatmapData, latM, lonM)
# jpeg('3rdEDA.jpg')
levelplot(
  TCR~ lonM * latM, data = heatmapData #head(heatmapData,900) 
  # , cuts = 50
  # , region = TRUE
  #, aspect="fill"
  # ,colorkey = FALSE
  , col.regions = heat.colors(100)[length(heat.colors(100)):1],
  main ='Hellas Planitia'
)
# dev.off()

##### Spacial Analysis ######

#FILTER HEATMAP BY TEMPORAL FRAMES
# load("clean3.RDate")
#other plot system shows same results
library(ggplot2)
library(dplyr)

initialize.d3 <- function(){
  d3 <- clean3 #data in Hellas
  d3$EPI <- d3$CAT1_PRISM1_PEAK_AREA #THERMAL_COUNT_RATE
  d3$lat <- d3$AREOCENTRIC_LATITUDE
  d3$lon <- d3$AREOCENTRIC_EAST_LONGITUDE
  d3$lat  <- round.min(d3$lat, 4) #parameter 
  d3$lon  <- round.min(d3$lon, 4) #parameter
  d3
  
}
d3 <- initialize.d3()

plot(d3[d3$lat == -60, ]$UTC,d3[d3$lat == -60, ]$EPI )


inDate <- "2015-03-30 CEST"     #parameter
outDate <- "2015-04-30 CEST"    #parameter
d3 <- d3[d3$UTC > inDate & d3$UTC < outDate,]
d3$EPI %>% length()

ggplot(d3, aes(lat,lon, z = EPI))+ geom_tile(aes(fill = EPI)) + theme_bw()












##### Time Analysis ######

d3 <- initialize.d3()
inDate <- "2015-03-30 CEST"     #parameter
outDate <- "2018-04-30 CEST"    #parameter
d3 <- d3[d3$UTC > inDate & d3$UTC < outDate,]

plot(d3$UTC, d3$EPI, type = "l")

plot( ( d3$EPI %>% fft() %>% Re() )[2:length(d3$EPI)], type = "l")
lines( ( d3$EPI %>% fft() %>% Im() )[2:length(d3$EPI)] ,  col = 2)
d3$EPI %>% length()

acf(d3$EPI, 200)







