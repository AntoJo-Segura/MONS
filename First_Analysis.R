#needed clean3 from Third_EDA

clean3

# not autocorrelation

acf(clean3$THERMAL_COUNT_RATE,200)

# skip first value because it is very big
fft(clean3$THERMAL_COUNT_RATE)[2:length(clean3)] %>% Im() %>%plot(type = "l")
fft(clean3$THERMAL_COUNT_RATE)[2:length(clean3)] %>% Re() %>% lines(col = 2)

#epithermal case
acf(clean3$CF_ATM_EPI,100)
fft(clean3$CF_ATM_EPI)[2:length(clean3)] %>% Im() %>%plot(type = "l")
fft(clean3$CF_ATM_EPI)[2:length(clean3)] %>% Re() %>% lines(col = 2)


########################



heatmapData <- data.frame(
  lat = grid3$AREOCENTRIC_LATITUDE,
  lon = grid3$AREOCENTRIC_EAST_LONGITUDE,
  TCR = grid3$THERMAL_COUNT_RATE
)
heatmapData <- na.omit(heatmapData)
round.min <- (function(x,m){((x/m)%>%floor())*m})
latM <- round.min(heatmapData$lat,0.1)
lonM <- round.min(heatmapData$lon,0.1)
heatmapData <- cbind(heatmapData, latM, lonM)

levelplot(
  TCR~ lonM * latM, data = heatmapData #head(heatmapData,900) 
  # , cuts = 50
  # , region = TRUE
  #, aspect="fill"
  # ,colorkey = FALSE
  , col.regions = heat.colors(100)[length(heat.colors(100)):1],
  main ='Hellas Planitia'
)

byGrid <- heatmapData %>% group_by( latM, lonM)

