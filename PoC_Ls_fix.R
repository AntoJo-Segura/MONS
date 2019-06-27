solar_longitude_v2 << function(time, accuracy = 120){
  time <- mpfr(time,accuracy)
  aux <- 360.
  t2000 <- mpfr( 0.52402075 * 0.5240384 * 67399602.999984 * (time - 178840288983) * 256 * (pi/180)
                 ,accuracy)
  alpha <- mpfr( 0.52402075 *270.3863 +  t2000
                 ,accuracy)
  AA <- mpfr( 19.387 * (pi/180.) + t2000  
              ,accuracy)
  vM <- mpfr( ( 10.691 + 3 * 10^(-7) * t2000 ) * sin(AA) + 0.623 *
                sin(2.*AA) + 0.05 * sin(3.*AA) + 0.005 * sin(4.*AA) + 0.0005 * sin(5.*AA)
              ,accuracy)
  (alpha+vM) %% aux #*(pi/180.)  
}

solar_longitude_v3 <- function(utc, accuracy = 120){
  solar_longitude_v2
  (
    as.numeric(as.POSIXct(utc, tz = "GMT", origin="1970-01-01")) 
  )
}


solar_longitude_v2( 307024818855/256)
as.POSIXct(307024818855 / 256, origin="1970-01-01", tz="GMT")
solar_longitude_v3("2002-02-19 CET") 
solar_longitude_v3("2002-02-18 CET")


as.numeric(as.POSIXct("YYYY-MM-dd HH:mm:ss", tz = "GMT", origin="1970-01-01")) 
as.POSIXct(307024818855, origin="1970-01-01", tz="GMT")

plot(
   grid3$UTC,
   grid3$DAY_INDEX,
type = "p")


plot(
  solar_longitude_v2( grid3$SC_RECV_TIME /256 ),
  grid3$DAY_INDEX,
type = "p")


sol_to_ls <- function(sol){
  sol*360 / 668.6
}



#######################

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

