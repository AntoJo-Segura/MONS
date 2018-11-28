context("commands")

#data block was saved for testing
#save(m,file = 'test_data_block.RData')
#load('test_data_block.RData')

test_that("time_to_solar_longitude",{
    time <- 178840288983
    expect_equal(
      solar_longitude(time),
      274.3732
    )
  }
)

