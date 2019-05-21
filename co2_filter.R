#CO2 filter

co2 <- read.csv(paste(getwd(),"/co2.txt"), sep="")

#EDA confirmation ####
library(lattice)
library(dplyr)

fuzzy <- function(x, threshold = 0){
  if(x > threshold) 1
  else 0
}



co2.hm <- co2
co2.hm$X.Lat.deg. <- round.min(co2.hm$X.Lat.deg.,2)
co2.hm$Lon.deg. <- round.min(co2.hm$Lon.deg.,2)
co2.hm$Ls.deg. <- round.min(co2.hm$Ls.deg.,2)
co2.hm <- co2.hm %>% group_by(X.Lat.deg.,Lon.deg.,Ls.deg.) %>% 
    summarise(
      CO2.f = fuzzy(CO2)
    )
levelplot(
  CO2.f ~ Ls.deg. * X.Lat.deg., data = co2.hm
  , col.regions = heat.colors(100)[length(heat.colors(100)):1],
  main ='co2'
)

# Lat Lon Map
# levelplot(
#   CO2.f ~ Lon.deg. * X.Lat.deg., data = co2.hm
#   , col.regions = heat.colors(100)[length(heat.colors(100)):1],
#   main ='co2'
# )

#Filter ####
clean4 <- clean3[
  co2.hm[
    clean3$SOLAR_LONGITUDE > (co2.hm$Ls.deg. - 2.0) 
    && clean3$SOLAR_LONGITUDE < (co2.hm$Ls.deg. + 2.0) 
  ,]$CO2.f %>% sum() != 0 
,]

#test 
(
  clean3[
    co2.hm[
      co2.hm$CO2.f == 0 
      & clean3$SOLAR_LONGITUDE , co2.hm$Ls.deg. - 2 ,co2.hm$Ls.deg. + 2 
      & clean3$AREOCENTRIC_LATITUDE > co2.hm$X.Lat.deg. - 2
      & clean3$AREOCENTRIC_LATITUDE < co2.hm$X.Lat.deg. + 2
      & clean3$AREOCENTRIC_EAST_LONGITUDE > co2.hm$Lon.deg. - 2
      & clean3$AREOCENTRIC_EAST_LONGITUDE < co2.hm$Lon.deg. + 2
    ,] %>% nrow() != 0
  ,]  %>% nrow() 
)


(
  clean3[
    clean3$SOLAR_LONGITUDE > 300 - 2
    & clean3$SOLAR_LONGITUDE < 320 + 2
    & clean3$AREOCENTRIC_LATITUDE > -55 - 2
    & clean3$AREOCENTRIC_LATITUDE < -50 + 2
    & co2.hm[
        co2.hm$CO2.f != 0
        &  clean3$SOLAR_LONGITUDE  > co2.hm$Lon.deg. - 2
        & 320 + 2 < co2.hm$Lon.deg. + 2
     ,] %>% nrow() == 0
  ,] %>% nrow() 
)

#From grouped data (clean3) filter with kernel (co2)

#Returns False if co2 contains 1
fuzzy_co2 <- function(cl_cursor){
  co2.hm[
    co2.hm$CO2.f == 0 
    & cl_cursor$SOLAR_LONGITUDE > co2.hm$Ls.deg. - 2 
    & cl_cursor$SOLAR_LONGITUDE < co2.hm$Ls.deg. + 2 
    & cl_cursor$AREOCENTRIC_LATITUDE > co2.hm$X.Lat.deg. - 2
    & cl_cursor$AREOCENTRIC_LATITUDE < co2.hm$X.Lat.deg. + 2
    & cl_cursor$AREOCENTRIC_EAST_LONGITUDE > co2.hm$Lon.deg. - 2
    & cl_cursor$AREOCENTRIC_EAST_LONGITUDE < co2.hm$Lon.deg. + 2
    ,] %>% nrow() != 0
}

#filter
'%row%' <- function(f,df){
  result <- data.frame()
  for(i in 1:nrow(df)) {
    if(f(df[i,]))
      result <- rbind(result,df[i,])
    print(paste('avance ',100*i/nrow(df),sep = ''))
  }  
  result
}

'%col%' <-  function(f,df){
  for(i in 1:ncol(df)) f(df[,i])
}

#call
clean4 <- fuzzy_co2 %row% clean3

apply(clean3,1,fuzzy_co2)




(
  clean3[
    co2.hm[
      co2.hm$CO2.f == 0 
      && between(clean3$SOLAR_LONGITUDE, co2.hm$Ls.deg. - 2, co2.hm$Ls.deg. + 2 )
      && between(clean3$AREOCENTRIC_LATITUDE, co2.hm$X.Lat.deg. - 2, co2.hm$X.Lat.deg. + 2 )
      && between(clean3$AREOCENTRIC_EAST_LONGITUDE, co2.hm$Lon.deg. - 2, co2.hm$Lon.deg. + 2)
      ,] %>% nrow() != 0
    ,]  %>% nrow() 
)


(
  co2.hm[
    (clean3 %>% 
       filter(between(SOLAR_LONGITUDE , 300 - 2 , 320 + 2)) %>%
       filter(between(AREOCENTRIC_LATITUDE, co2.hm$X.Lat.deg. - 2, co2.hm$X.Lat.deg. + 2)) %>%
       filter(between(AREOCENTRIC_EAST_LONGITUDE, 30 - 2, 90 + 2)) %>%
       nrow() != 0)
    ,] %>% nrow()
)





clean4[,] %>% (function(x){
  plot(
    
  )
})

levelplot(
   ~ SOLAR_LONGITUDE * AREOCENTRIC_LATITUDE, data = clean4
  , col.regions = heat.colors(100)[length(heat.colors(100)):1],
  main ='co2'
)

