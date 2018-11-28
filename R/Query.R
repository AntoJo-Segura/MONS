if(!require(mongolite))
  install.packages('mongolite')
library(mongolite)

# mongodb <- function(x){mongo(x, url = "mongodb://192.168.0.104:27017/Mars")}
mongodb <- function(x){mongo(x, url = "mongodb://localhost:27017/Mars")}
Mons <- mongodb('Mons')
m <- Mons$find()

# a<- m[m$V3 < '2002-03-04T21:02:06.822',]$V30
# 
# plot(a$V8[1:86400/(20*6)],a$V7[1:86400/(20*6)], type = "l")
# 
# a[a$V8 > 20 && a$V8 < 40 , ]$V30
# 
# mean( a[a$V8 > 20 && a$V8 < 40 , ]$V8  )
# 
# a[a$V6 = 815,]$V7



#German email TODO projection ####

# Cada uno de los ficheros que te has bajado y 'desencriptado' (dnd_YYYYMMDD.dat) tiene 61 columnas, pero el Label file que los identifica (adjuntado) tiene 53. 
# La razón es que las columnas 10, 11, 12 y 13 están a su vez divididas en tres columnas (x,y,z), de modo que la columna 14 (Day of Martian Year- DOM) según el documento adjunto es en realidad la 22 (de aquí es defase de '8' posiciones). Más adelante ya no hay desfase.
# 
# En principio, de entre todas las columnas, sólo queremos las siguientes: ticks(i),a,UTC(i),a,a,ON(i),lat(i),lon(i),a,a,a,a,a,a,a,a,a,a,a,a,a,DOM(i),hh(i),mm(i),a,a,a,a,a,a,a,a,a,lat2(i),lon2(i),latT(i),lonT(i),ECR(i),SigmaECR(i),a,a,a,a,a,a,a,a, TCR(i),SigmaTCR(i),a,a,a,a,a,a,a,a,a,a,a,a
# El significado de cada columna viene dado en el archivo adjunto.
# 
# Además, para cada fila, es decir, para cada medida con 61 columnas, queremos añadir un parámetro que se llama Solar Longitude y que mide el tiempo en Marte. Para calcularlo, tienes que hacer la siguiente cuenta para cada
# do i=1,n
# aux=360.
# t2000=67399602.999984*(ticks(i)-178840288983)*256
# alpha=270.3863+0.5240384*t2000
# AA=(19.387+0.52402075*t2000)*(pi/180.)
# vM=(10.691+3*10**(-7)*t2000)*sin(AA)+0.623*sin(2.*AA)+0.05*sin(3.*AA)+0.005*sin(4.*AA)+0.0005*sin(5.*AA)
# SL(i)=(mod(alpha+vM,aux)) !*(pi/180.)               
# 
# end do
# 
# donde tick(i) corresponde a la primera columna de cada fila.   