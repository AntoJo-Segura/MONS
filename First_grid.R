#functions####

#'
#'@description raw query
#'
#'
getGrid <- function(latitude.upper, latitude.lower, longitude.upper, longitude.lower){
  Mons$find('{    
            "AREOCENTRIC_LATITUDE": {
            "$lt": '%c% latitude.upper %c%',
            "$gt": '%c% latitude.lower %c%'
            },
            "AREOCENTRIC_EAST_LONGITUDE": {
            "$lt": '%c% longitude.upper %c%',
            "$gt": '%c% longitude.lower %c%'    
            } 
}')
}

#'
#'@note select accuracy default:
#'i <-120
#'partial_sl(grid[grid$SOLAR_LONGITUDE == 0,]$SC_RECV_TIME , i) - partial_sl(grid[grid$SOLAR_LONGITUDE == 0,]$SC_RECV_TIME , i+10)
#'@test mpfr check: 
#'j = 1
#'j == ( (mpfr(2.1e+21, 120) + j) %% 360 - mpfr(2.1e+21, 120) %% 360 )
#'
#'
partial_sl<- function(time, accuracy = 120){
  if(!is.numeric(time) || is.null(time)){
    print("time")
    print(time)
    return(0)
  }
  
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

#Implementation####

longitude <- seq(-180,180,1) + 180
latitude <- seq(-90,90,1) 
output.grid <- data.frame(row.names = c('latitude','longitude','lag','correlation'))
i0 = 15
j0 = 62
for(i in i0: (length(latitude) - 1 )){
  for(j in j0: (length(longitude) - 1)) { 
    print("i " %c% i)
    print("j " %c% j)
    
    latitude.lower = latitude[i] 
    latitude.upper = latitude[(i + 1) ]
    longitude.lower = longitude[j]
    longitude.upper = longitude[(j + 1) ]
    output.grid <- output.grid %>% 
      cbind( c(latitude.upper, longitude.upper, GridByIndex(latitude.upper, latitude.lower, longitude.upper, longitude.lower)) )
  }
}

###########

latitude.upper <- 1
latitude.lower <- 0
longitude.upper <- 1
longitude.lower <- 0  

grid <- getGrid(latitude.upper, latitude.lower, longitude.upper, longitude.lower)

grid

print('{    
      "AREOCENTRIC_LATITUDE": {
      "$lt": '%c% latitude.upper %c%',
      "$gt": '%c% latitude.lower %c%'
      },
      "AREOCENTRIC_EAST_LONGITUDE": {
      "$lt": '%c% longitude.upper %c%',
      "$gt": '%c% longitude.lower %c%'    
      } 
      }')



