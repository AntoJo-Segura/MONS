#SQL EXPRESS####
library(RODBC)
ConnectionString.SQL.Server <- 'Driver={SQL Server};Server=localhost\\SQLEXPRESS;Database=MONS;Trusted_Connection=True;'
channel <- odbcDriverConnect(ConnectionString.SQL.Server)
qry.test <- 'Select * from [dbo].[ConnectionTest]'
df.test <- sqlQuery(channel,qry.test)
#MONGO DB ####
library(mongolite)
mongodb <- function(x){mongo(x, db = 'Mars', url = "mongodb://localhost")}
Test <- mongodb('system.users')
Test$find()

#RODBC ####
library(RODBC)
connectionString <- "Driver={MySQL ODBC 5.2 UNICODE Driver};Server=localhost\\Test01;Database=sys;User=root;Password=44ZwJbFt;Option=3;"
connectionString <- "Driver={MySQL ODBC 5.2 ANSI Driver};Server=localhost;Port=3306;Database=Test01;User=root;Password=44ZwJbFt;Option=3;"

connectionString <- "Provider=MSDASQL;Driver={MySQL ODBC 5.1 Driver};Server=localhost;Port=3306;Database=Test01;User=root;Password=44ZwJbFt;Option=3;"

connectionString <- "Driver={MySQL ODBC};Server=localhost;Port=3306;Database=Test01;User=root;Password=44ZwJbFt;Option=3;"
channel <- odbcDriverConnect(connectionString)

channel <- odbcConnect("localhost\\Test01", uid="root",pwd = "44ZwJbFt")




#My SQL library ####
install.packages('RMySQL')
require(RMySQL) 
con <- dbConnect(RMySQL::MySQL(), host = "localhost",dbname="Test01",user = "root", password = "44ZwJbFt")


