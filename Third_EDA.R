#3rd EDA: try to emulate MDAP MONS EDAS

latitude.upper <- -30
latitude.lower <- -60
longitud.upper <- 90
longitud.lower <- 30

grid3 <- getGrid(latitude.upper, latitude.lower, longitud.upper, longitud.lower)
clean3 <- grid3[grid3$SOLAR_LONGITUDE != 0,]
plot(clean3$SC_RECV_TIME, clean3$CF_ATM_EPI, type='l')
plot(clean3$SC_RECV_TIME, clean3$THERMAL_COUNT_RATE, type='l')
plot(clean3$UTC, clean3$THERMAL_COUNT_RATE, type='l')


###############
library(lattice)

# heatmapData <- cbind(grid3$AREOCENTRIC_LATITUDE,grid3$AREOCENTRIC_EAST_LONGITUDE,grid3$THERMAL_COUNT_RATE)


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

# levelplot(THERMAL_COUNT_RATE~AREOCENTRIC_LATITUDE*AREOCENTRIC_EAST_LONGITUDE, data = heatmapData,col.regions = heat.colors(100)[length(heat.colors(100)):1] )

# library(ggplot2)
# ggplot(head(heatmapData,300), aes(x = lat, y = lon, fill = TCR))+ geom_tile()
# ggplot(heatmapData, aes(x = latM, y = lonM, fill = TCR)) + geom_tile()
# ###########
# 
# #The lattice package provides a dataset named volcano. It's a square matrix looking like that :
# head(volcano)
# 
# # The use of levelplot is really easy then :
# levelplot(volcano)
# 
# #####################
# 
# ## Example data
# x <- seq(1,10, length.out=20)
# y <- seq(1,10, length.out=20)
# data <- expand.grid(X=x, Y=y)
# data$Z <- runif(400, 0, 5)
# 
# ## Try it out
# par(mar=c(3,4,2,2))
# levelplot(Z ~ X*Y, data=data  , xlab="X" , col.regions = heat.colors(100)[length(heat.colors(100)):1]   , main="")
