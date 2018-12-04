
script_path <- 'C:/Users/AntoJo/Documents/TestBench/scripts/'
root_path <-  'C:/Users/AntoJo/Documents/R/WorkSpace/MONS/tests/testthat'
year_path <- root_path 
doc_path <- year_path %c% '/dnd_' %c% doc %c% '.dat'
doc <- '20020628'

doc_data <- system('java -jar '%c% script_path %c%'grs_code.zip '%c% doc_path, intern = TRUE)

if(
  read.table(text = doc_data, sep= ',', 
             col.names = columnNames, colClasses = columnTypes) %>%
    mutate(SOLAR_LONGITUDE = solar_longitude(SC_RECV_TIME), DOC_ID = doc) %>% try()
)
  print('yes')
  
scan(file = doc_data, sep= ',',  what = columnTypes)



