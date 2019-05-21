library(dplyr)
clean4 <- fuzzy_co2 %row% clean3

save(clean4, file ='clean4.RData')