#LOAD DATA ####

if(!require(mongolite))
  install.packages('mongolite')
library(mongolite)

# mongodb <- function(x){mongo(x, url = "mongodb://192.168.0.104:27017/Mars")}
mongodb <- function(x){mongo(x, url = "mongodb://localhost:27017/Mars")}
Mons <- mongodb('Mons')


selectedColumnNames <- c(
  'SC_RECV_TIME',
  'UTC',
  'ODY_ORBIT_NUMBER',
  'AREOCENTRIC_LATITUDE',
  'AREOCENTRIC_EAST_LONGITUDE',
  'ALTITUDE',
  'DAY_INDEX',
  'LOCAL_HOUR',
  'LOCAL_MINUTE'
  ,'LAT_1'
  ,'LON_1'
  ,'LAT_4_5'
  ,'LON_4_5'
  ,'CAT1_PRISM1_PEAK_AREA'
  ,'CAT1_PRISM1_ERROR'
  ,'THERMAL_COUNT_RATE'
  ,'THERMAL_ERROR'
  ,'CF_ATM_EPI'
  ,'CF_ATM_FAST'
  ,'SOLAR_LONGITUDE'
)
# En principio, de entre todas las columnas, sólo queremos las siguientes: ticks(i),a,UTC(i),a,a,ON(i),lat(i),lon(i),a,a,a,a,a,a,a,a,a,a,a,a,a,DOM(i),hh(i),mm(i),a,a,a,a,a,a,a,a,a,lat2(i),lon2(i),latT(i),lonT(i),ECR(i),SigmaECR(i),a,a,a,a,a,a,a,a, TCR(i),SigmaTCR(i),a,a,a,a,a,a,a,a,a,a,a,a
fieldsString <-  '{"' %c% 
      Reduce((function(x,y){x %c% '" : true, "' %c% y}),selectedColumnNames) %c%
  '" : true }'

m <- Mons$find(
  # query = '{"UTC" : { "$gt" : "2002-02-18 23:00:00.000" } }', #arreglar
  fields = fieldsString,
  limit = 5000
)



# a<- m[m$V3 < '2002-03-04T21:02:06.822',]$V30
# 
# plot(a$V8[1:86400/(20*6)],a$V7[1:86400/(20*6)], type = "l")
# 
# a[a$V8 > 20 && a$V8 < 40 , ]$V30
# 
# mean( a[a$V8 > 20 && a$V8 < 40 , ]$V8  )
# 
# a[a$V6 = 815,]$V7

#RENDERS####

# if(!require(threejs))
#   install.packages('threejs')
# library("threejs")
# 
# earth <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg"
# globejs(img=earth, bg="white")
# 
# ##mars <- "http://planetary.s3.amazonaws.com/assets/images/4-mars/2013/20131025_mars-major-features.jpg"
# mars <- "https://static-2.gumroad.com/res/gumroad/5387571460549/asset_previews/b455aaa72d1482e171f0558c2766cd48/retina/Mars_2k_Color_Preview_v001.jpg"
# ##mars <- "http://static.greatbigcanvas.com/images/print_rolled_posterpaper/science-photo-library/mars-topographical-map-satellite-image,1154339.jpg"
# #mars <- "http://www.msss.com/msss_images/icons/mars_map1.jpg"
# globejs(img = mars, bg = "black")
# 
# globejs(img = mars, bg = "black", lat=cities$lat,     long=cities$long, value=value)


#final render 
library("threejs")
if(!require(threejs))
  install.packages('threejs')
mars <- "https://static-2.gumroad.com/res/gumroad/5387571460549/asset_previews/b455aaa72d1482e171f0558c2766cd48/retina/Mars_2k_Color_Preview_v001.jpg"
globejs(img = mars, bg = "black", 
        lat = m$AREOCENTRIC_LATITUDE,
        long = m$AREOCENTRIC_EAST_LONGITUDE, 
        value = m$THERMAL_COUNT_RATE)


# 
# library("threejs")
# if(!require(threejs))
#   install.packages('threejs')
# library("maptools")
# if(!require(maptools))
#   install.packages('maps')
# library("maps")
# 
# data(world.cities, package="maps")
# cities <- world.cities[order(world.cities$pop,decreasing=TRUE)[1:1000],]
# value  <- 100 * cities$pop / max(cities$pop)
# 
# globejs(bg="black", lat=cities$lat,     long=cities$long, value=value, 
#         rotationlat=-0.34,     rotationlong=-0.38, fov=30)
# 
