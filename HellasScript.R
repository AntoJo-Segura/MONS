
#Hellas Planice

latitude.upper <- -30
latitude.lower <- -50
longitud.upper <- 90
longitud.lower <- 45

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
sortedGrid <- protoGrid[order(ls),]
plot(sortedGrid$ls,sortedGrid$epi, type = "b")

autocorr <- acf(protoGrid[order(ls),]$epi, lag.max = 360, plot = FALSE)$acf 
# max.corr <- autocorr[ which.max(autocorr$acf[2:length(autocorr$acf)]) ] 

output.grid <- rbind(
  output.grid, 
  c(latitude,longitude,max.corr$acf,max.corr$lag) 
)  
lag <- autocorr %>% which.min()
c(lag,autocorr[lag]) #output lag min


