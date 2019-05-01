# save(output.grid, file = "output-grid-15-62.RData")
i <- 15
j <- 62


#'@description it goes over every Grid 
#'@call First_grid
#'
GridByIndex <- function(latitude.upper, latitude.lower, longitude.upper, longitude.lower){

  
  grid <- getGrid(latitude.upper, latitude.lower, longitude.upper, longitude.lower)
  
  ls <- as.numeric(partial_sl(grid$SC_RECV_TIME))
  epi <- grid$CF_ATM_EPI
  
  if(is.null(ls)||is.null(epi)||length(ls) != length(epi) ){
    print("lat and long")
    print(latitude.lower %c% ' , ' %c% longitude.lower)
    print("epi and ls")
    print(epi %c% ls)
    # next # next doesn't work into a function
    return (c(-1, -1)) #default value
  }
  protoGrid <- data.frame(ls = ls, epi = epi)
  
  autocorr <- acf(protoGrid[order(ls),]$epi, lag.max = 360, plot = FALSE)$acf 
  # max.corr <- autocorr[ which.max(autocorr$acf[2:length(autocorr$acf)]) ] 
  
  output.grid <- rbind(
    output.grid, 
    c(latitude,longitude,max.corr$acf,max.corr$lag) 
  )  
  lag <- autocorr %>% which.min()
  c(lag,autocorr[lag]) #output lag min
}





