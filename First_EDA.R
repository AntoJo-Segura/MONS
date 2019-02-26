#First EDA
#Grid of longitud and latitud and temporal serie with solar longitud as time
# take temporal autocorrelation of epi_thermal, thermal y fast.

#TODO R error if number > 8^19 https://stat.ethz.ch/pipermail/r-help/2006-January/087160.html

# Mons$find('{    
#     "AREOCENTRIC_LATITUDE": {
#         "$gt": -29.043,
#         "$lt": -28.0
#     },
#     "AREOCENTRIC_EAST_LONGITUDE": {
#         "$gt": 219.0,
#         "$lt": 220.0    
#     } 
# }')

latitude.upper <- -29.043
latitude.lower <- -28.0
longitud.upper <- 219.0
longitud.lower <- 220.0

grid <- Mons$find('{    
    "AREOCENTRIC_LATITUDE": {
        "$gt": '%c% latitude.upper %c%',
        "$lt": '%c% latitude.lower %c%'
    },
    "AREOCENTRIC_EAST_LONGITUDE": {
        "$gt": '%c% longitud.upper %c%',
        "$lt": '%c% longitud.lower %c%'    
    } 
}')

getGrid <- function(latitude.upper, latitude.lower, longitud.upper, longitud.lower){
  Mons$find('{    
    "AREOCENTRIC_LATITUDE": {
        "$gt": '%c% latitude.upper %c%',
        "$lt": '%c% latitude.lower %c%'
    },
    "AREOCENTRIC_EAST_LONGITUDE": {
        "$gt": '%c% longitud.upper %c%',
        "$lt": '%c% longitud.lower %c%'    
    } 
  }')
}


#grid[grid$SOLAR_LONGITUDE != 0,]
#grid[grid$SOLAR_LONGITUDE == 0,]




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

partial_sl(grid[grid$SOLAR_LONGITUDE == 0,]$SC_RECV_TIME )



clean <- grid[grid$SOLAR_LONGITUDE != 0,]
plot(clean$SC_RECV_TIME, clean$CF_ATM_EPI, type='l')


plot(partial_sl(grid$SC_RECV_TIME), grid$CF_ATM_EPI, type='l')
plot(grid$SC_RECV_TIME, grid$CF_ATM_EPI, type= 'l')
plot(grid$SC_RECV_TIME, grid$CF_ATM_EPI, type= 'b')
acf(grid$CF_ATM_EPI, lag.max = 250)
acf(grid$CF_ATM_EPI, lag.max = 20)
acf(grid$CF_ATM_EPI, lag.max = 20, plot = FALSE) #get values


grid[rep(c(TRUE,FALSE),length(grid)),] %>% # only even
  (function(x){plot(x$SC_RECV_TIME, x$CF_ATM_EPI, type= 'b') })
grid[!rep(c(TRUE,FALSE),length(grid)),] %>% #only odd
  (function(x){points(x$SC_RECV_TIME, x$CF_ATM_EPI, col = 3) })



(grid$SC_RECV_TIME[1:100]-178845137345 ) / (60 * 60 * 1000) # mesures are gruped 2 by 2



#protoGrid####

ls <- as.numeric(partial_sl(grid$SC_RECV_TIME))
epi <- grid$CF_ATM_EPI
protoGrid <- data.frame(ls = ls, epi = epi)
plot(protoGrid[order(ls),]$ls,protoGrid[order(ls),]$epi, type = "b")
acf(protoGrid[order(ls),]$epi, lag.max = 360)


protoGrid[order(ls),][!rep(c(TRUE,FALSE),length(protoGrid)),] %>% #even
  (function(x){plot(x$ls, x$epi, col = 3, type = "b") })

protoGrid[order(ls),][rep(c(TRUE,FALSE),length(protoGrid)),] %>% #odd
  (function(x){points(x$ls, x$epi, col = 2, type = "b") })













#useless part
spectrum(protoGrid[order(ls),]$epi)
spectrum(grid$CF_ATM_EPI)
acf(grid$CF_ATM_EPI)


plot(grid[order(partial_sl(grid$SC_RECV_TIME)),]$CF_ATM_EPI)




