test_that("Downloaded data is as expected", {
  expect_equal(
    get_alfred_series("INDPRO", "test",
                      observation_start = "2013-03-01", observation_end = "2013-03-01",
                      real_time_start = "2015-02-02", real_time_end = "2015-02-02"),
    data.frame(
      date = as.Date("2013-03-01"),
      realtime_period  = as.Date("2015-02-02"),
      test = 99.488)
  )
  expect_equal(
    get_fred_series("INDPRO", "test",
                    observation_start = "2009-03-01", observation_end = "2009-03-01"),
    data.frame(
      date = as.Date("2009-03-01"),
      test = 89.1913)
  )
})