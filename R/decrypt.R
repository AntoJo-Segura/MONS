'%c%' <- function(x,y){paste(x,y,sep='')}

actual_dir <- getwd()
setwd(actual_dir)

#en Unix "system" lanza un comando shell pero se recomienda "shell" en windows que transcribe
#comandos a "system" 

#1st approach
dir <- system("ls", intern = TRUE)
#2nd approach
shell('dir > directory.txt & rm directory.txt') #execute always
shell('dir > directory.txt && rm directory.txt') #execute if first works



#execute -jar decrypcter ####

#get all files names
real_path <- 'E:/HTTrackerMONS/MONS/pds-geosciences.wustl.edu/ody/ody-m-grs-4-dnd-v1/odgd1_xxxx'
test_path <- 'C:/Users/AntoJo/Documents/TestBench/data'

dir <- system('ls ' %c% test_path, intern = TRUE);dir
setwd('C:/Users/AntoJo/Documents/TestBench/scripts')
system('java -jar grs_code.zip '%c% dir[1] %c%' > '%c% 
         substr(dir[1],1,nchar(dir[1])-4)  %c%'_Decrypted.dat')


# grep("(.*?)\\.dat","dnd_20030108.dat")
# 
# m <- regexpr("(.*?)\\.dat","dnd_20030108.dat",perl = TRUE)
# regmatches("dnd_20030108.dat", m)
# 
# library(stringr)
# str_match('dnd_20030108.dat','(.*?)\.dat')

substr(dir[1],1,nchar(dir[1])-4)


script_path = 'C:/Users/AntoJo/Documents/TestBench/scripts/'
data_path = 'C:/Users/AntoJo/Documents/TestBench/data/'

doc <- system('java -jar '%c% script_path %c%'grs_code.zip '%c% data_path %c%'dnd_20030108.dat', intern = TRUE)

doc2 <- gsub( ',', ' ',doc)

doc3 <- read.table(text = doc, sep=',')



library(mongolite)
mongodb <- function(x){mongo(x, db = 'Mars', url = "mongodb://localhost")}
Test <- mongodb('system.users')
Test <- mongodb('InsertTest')
Mons<- mongodb('Mons')
Test$insert(doc3[1,])
Test$find()

###############################
if(!require(mongolite))
  install.packages('mongolite')
library(mongolite)

'%c%' <- function(x,y){paste(x,y,sep='')}

script_path = 'C:/Users/AntoJo/Documents/TestBench/scripts/'
root_path <- 'E:/HTTrackerMONS/MONS/pds-geosciences.wustl.edu/ody/ody-m-grs-4-dnd-v1/odgd1_xxxx'
db_path <- "mongodb://localhost"

year_path <- '' #:(



mongodb <- function(x){mongo(x, db = 'Mars', url = db_path)}
Mons<- mongodb('Mons')
Mons_offset<- mongodb('Mons_offset')

doc_insert <- function(doc, initial_doc = doc, parallel_insert = FALSE){
  
  if(parallel_insert 
     && Mons_offset$find()[1,] > doc) return() #skip if it is inserted
  
  if(doc < initial_doc ) return() #skip if control doc is inserted
  print(year_path)
  doc_path <- year_path %c% '/' %c% doc %c% '/dnd_' %c% doc %c% '.dat'
  dir <- system('ls ' %c% doc_path, intern = TRUE)
  
  doc_data <- system('java -jar '%c% script_path %c%'grs_code.zip '%c% doc_path, intern = TRUE)
  Mons$insert( read.table(text = doc_data, sep= ',') )
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

