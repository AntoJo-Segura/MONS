context("commands")

if(!require(testthat))
  install.packages('testthat')
library(testthat)

#data block was saved for testing
#save(m,file = 'test_data_block.RData')
#load('test_data_block.RData')

test_that("time_to_solar_longitude",{
    time <- 178840288983
    expect_true(
      all.equal(
        solar_longitude(time),
        274.3732,
        tolerance = 0.0001
      )
    )
})

test_that("decript_and_format",{
  script_path <- 'C:/Users/AntoJo/Documents/TestBench/scripts/'
  root_path <-  'C:/Users/AntoJo/Documents/R/WorkSpace/MONS/tests/testthat'
  year_path <- root_path 
  doc <- '20020219'
  expect_equal(
    ncol(load_doc(doc)),
    63
  )
})

