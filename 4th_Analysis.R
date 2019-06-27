#generate pixel images

# clean3b <- get(load(file = paste(getwd(),'/clean3.RData',sep = '')))


clean3ToGrid <- function(lat.p,lon.p){
  d <- clean3 #data in Hellas
  d$EPI <- d$CAT1_PRISM1_PEAK_AREA #THERMAL_COUNT_RATE
  d$lat <- d$AREOCENTRIC_LATITUDE
  d$lon <- d$AREOCENTRIC_EAST_LONGITUDE
  d$lat  <- round.min(d$lat, lat.p) #parameter 
  d$lon  <- round.min(d$lon, lon.p) #parameter
  d
  
}


#subset of ts
dg <- clean3ToGrid(4,4)
plot( dg$UTC, dg$EPI, type= "l")
inDate <- "2013-07-01 CEST"     #parameter
outDate <- "2014-07-01 CEST"    #parameter
dg <- dg[dg$UTC > inDate & dg$UTC < outDate,]
plot( dg$UTC, dg$EPI, type= "l")
min(dg$UTC)
max(dg$UTC)

#-60 band
plot(
  dg[dg$lat == -60, ]$UTC,
  dg[dg$lat == -60, ]$EPI 
  ,type= "l")


#save grided time series
sapply(seq(-60,30,4), function(x){ 
  mapply(
    function(x,y){
      if(length(dg[dg$lat == x & dg$lon == y, ]$UTC) > 0 ){
        jpeg('Hellas_Time_15_18/lat_'%c% x %c% '_lon_'%c% y %c%'.jpeg')
        plot(
          dg[dg$lat == x & dg$lon == y, ]$UTC,
          dg[dg$lat == x & dg$lon == y, ]$EPI 
          ,type= "l")
        dev.off()
      }
    },x,seq(88,32,-4)) 
})
graphics.off()
