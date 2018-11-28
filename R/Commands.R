if(!require(mongolite))
  install.packages('mongolite')
library(mongolite)
if(!require(dplyr))
  install.packages('dplyr')
library(dplyr)

'%c%' <- function(x,y){paste(x,y,sep='')}

script_path = 'C:/Users/AntoJo/Documents/TestBench/scripts/'
root_path <- 'E:/HTTrackerMONS/MONS/pds-geosciences.wustl.edu/ody/ody-m-grs-4-dnd-v1/odgd1_xxxx'
db_path <- "mongodb://localhost"

year_path <- '' # need to integrate doc_insert and folder_seeker :(

#data structure ####
#maybe regex: ^(?!foo).*$ could help
columnNames <- c(
  'SC_RECV_TIME',
  'CEB_TIME',
  'UTC',
  'GRS_PIXEL_NUMBER',
  'GRS_ORBIT_NUMBER',
  'ODY_ORBIT_NUMBER',
  'AREOCENTRIC_LATITUDE',
  'AREOCENTRIC_EAST_LONGITUDE',
  'ALTITUDE',
  'SCPOS_INERT_X',
  'SCPOS_INERT_Y',
  'SCPOS_INERT_Z',
  'SCVEL_INERT_X',
  'SCVEL_INERT_Y',
  'SCVEL_INERT_Z',
  'MARSPOS_INSTR_X',
  'MARSPOS_INSTR_Y',
  'MARSPOS_INSTR_Z',
  'MARSVEL_INSTR_X',
  'MARSVEL_INSTR_Y',
  'MARSVEL_INSTR_Z',
  'DAY_INDEX',
  'LOCAL_HOUR',
  'LOCAL_MINUTE',
  'PIXEL_DURATION',
  'POWER_SUPPLY',
  'SWAP_FLAG',
  'POWER_LEVEL',
  'DIRECTION',
  'LIVETIME'
  ,'DEADTIME_PERCENT'
  ,'OVERLOAD_RATE'
  ,'HVPS_MNTR_2'
  ,'LAT_1'
  ,'LON_1'
  ,'LAT_4_5'
  ,'LON_4_5'
  ,'CAT1_PRISM1_PEAK_AREA'
  ,'CAT1_PRISM1_ERROR'
  ,'CAT1_PRISM2_PEAK_AREA'
  ,'CAT1_PRISM2_ERROR'
  ,'CAT1_PRISM3_PEAK_AREA'
  ,'CAT1_PRISM3_ERROR'
  ,'CAT1_PRISM4_PEAK_AREA'
  ,'CAT1_PRISM4_ERROR'
  ,'CAT2_PRISM1_FAST_SUM'
  ,'CAT2_PRISM1_ERROR'
  ,'THERMAL_COUNT_RATE'
  ,'THERMAL_ERROR'
  ,'CAT1_PRISM1_NORM_SLOPE'
  ,'CAT1_PRISM1_NORM_OFFSET'
  ,'CAT1_PRISM2_NORM_SLOPE'
  ,'CAT1_PRISM2_NORM_OFFSET'
  ,'CAT1_PRISM3_NORM_SLOPE'
  ,'CAT1_PRISM3_NORM_OFFSET'
  ,'CAT1_PRISM4_NORM_SLOPE'
  ,'CAT1_PRISM4_NORM_OFFSET'
  ,'CAT2_PRISM1_NORM_SLOPE'
  ,'CAT2_PRISM1_NORM_OFFSET'
  ,'CF_ATM_EPI'
  ,'CF_ATM_FAST'
)

#read.table(colClasses = ) 
#all maped in mongo except date  
columnTypes <- c(
  'integer',
  'integer',
  'POSIXct',#'yyyy-mm-ddThh:mm:ss.sss',
  'integer',
  'integer',
  'integer',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'integer',
  'integer',
  'integer',
  'numeric',
  'integer',
  'integer',
  'integer',
  'integer',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric',
  'numeric'
)

#commands region ####
mongodb <- function(x){mongo(x, db = 'Mars', url = db_path)}
Mons<- mongodb('Mons')
Mons_offset<- mongodb('Mons_offset')

#'
#'@test solar_longitude(as.numeric(a[1,]$V1)) == 274.3732
#'
solar_longitude <- function(time){
  aux = 360.
  t2000 = 67399602.999984 * (time - 178840288983) * 256
  alpha = 270.3863 + 0.5240384 * t2000
  AA = ( 19.387 + 0.52402075 * t2000 ) * (pi/180.)
  vM = ( 10.691 + 3 * 10^(-7) * t2000 ) * sin(AA) + 0.623 *
        sin(2.*AA) + 0.05 * sin(3.*AA) + 0.005 * sin(4.*AA) + 0.0005 * sin(5.*AA)
  (alpha+vM) %% aux #*(pi/180.)  
}

doc_insert <- function(doc, initial_doc = doc, parallel_insert = FALSE){
  
  if(parallel_insert 
     && Mons_offset$find()[1,] > doc) return() #skip if it is inserted
  
  if(doc < initial_doc ) return() #skip if control doc is inserted
  # print(year_path) # debug integration error
  doc_path <- year_path %c% '/' %c% doc %c% '/dnd_' %c% doc %c% '.dat'
  dir <- system('ls ' %c% doc_path, intern = TRUE)
  
  doc_data <- system('java -jar '%c% script_path %c%'grs_code.zip '%c% doc_path, intern = TRUE)
  read.table(text = doc_data, sep= ',', 
             col.names = columnNames, colClasses = columnTypes) %>%
  mutate(SOLAR_LONGITUDE = solar_longitude(SC_RECV_TIME)) %>% 
  Mons$insert()
  #ofset update
  Mons_offset$update(query= '{}', update = '{"$set":{"offset":'%c% doc %c%'}}')
  print(doc)
}

folder_seeker <- function(year,inital_year = year){
  
  if(year < inital_year) return()
  year_path <<- root_path %c% '/' %c% year #needed in doc_insert :(
  docs <- system('ls ' %c% year_path, intern = TRUE)
  docs <- docs[!grepl("html",docs)] #acces only to data folders
  
  sapply(docs, doc_insert)
  print(year)
}

fill_db <- function(script_path, root_path, db_path) {
  
  mongodb <- function(x){mongo(x, db = 'Mars', url = db_path)}
  Mons<- mongodb('Mons')
  Mons_offset<- mongodb('Mons_offset')
  
  if(nrow(Mons_offset$find()) == 0 ){#initialize db if it isnecessary
    initial_offset <- as.data.frame(0)
    names(initial_offset) <- c("offset")
    Mons_offset$insert(initial_offset)
  }
  
  years <- system('ls ' %c% root_path, intern = TRUE)
  years <- years[grepl('[0-9]{4}',years)]
  sapply(years, folder_seeker) 
}

fill_db(script_path, root_path, db_path)

