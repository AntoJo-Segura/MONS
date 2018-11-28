context("main_text")

test_that("Difference_between_UTC_and_local_is_correct",{
    localtime <- Sys.time()
    expect_equal(
      format( - hoursUTC() %h+% localtime, format = "%Y-%m-%d %H:%M:%S" ),
      format( toUTC(localtime), format = "%Y-%m-%d %H:%M:%S" )
    )
  }
)

test_that("hoursUTC_at_UTC",{
  localtimeUTC <- Sys.time()
  attr(localtimeUTC,"tzone") <- "UTC"
  expect_equal(
    hoursUTC(localtimeUTC),
    0
  )
}
)
