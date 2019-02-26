if(!require(parallel))
  install.packages('parallel')
library(parallel)

#https://www.r-bloggers.com/how-to-go-parallel-in-r-basics-tips/
#https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html


# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

parLapply(cl, 2:4,
          function(exponent)
            2^exponent)


#'Note that you need the clusterExport(cl, "base") in order for the function to see 
#'the base variable. If you are using some special packages you will similarly need 
#'to load those through clusterEvalQ, e.g. I often use the rms package and I therefore
#' use clusterEvalQ(cl, library(rms)). Note that any changes to the variable after 
#' clusterExport are ignored




stopCluster(cl)