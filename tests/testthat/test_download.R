test_that("Downloaded data is as expected", {
  expect_equal(
    get_alfred_series("INDPRO", "test",
                      observation_start = "2013-03-01", observation_end = "2013-03-01",
                      realtime_start = "2015-02-02", realtime_end = "2015-02-02"),
    data.frame(
      date = as.Date("2013-03-01"),
      realtime_period  = as.Date("2015-02-02"),
      test = 99.488)
  )
  expect_equal(
    dplyr::filter(get_alfred_series("INDPRO", "test",
                                    realtime_start = "2015-02-02", realtime_end = "2015-02-02"),
                  date == "2013-03-01"),
    data.frame(
      date = as.Date("2013-03-01"),
      realtime_period  = as.Date("2015-02-02"),
      test = 99.488)
  )
  expect_equal(
    dplyr::filter(get_alfred_series("INDPRO", "test",
                                    observation_start = "2009-03-01", observation_end = "2009-03-01"),
                  realtime_period == "2015-02-18"),
    data.frame(
      date = as.Date("2009-03-01"),
      realtime_period = as.Date("2015-02-18"),
      test = 85.6157)
  )
  expect_equal(
    get_fred_series("INDPRO", "test",
                    observation_start = "2009-03-01", observation_end = "2009-03-01"),
    data.frame(
      date = as.Date("2009-03-01"),
      test = 89.0085)
  )
  expect_equal(
    dplyr::filter(get_fred_series("INDPRO", "test"),
                  date == "2009-03-01"),
    data.frame(
      date = as.Date("2009-03-01"),
      test = 89.0085)
  )
  expect_error(
    get_fred_series(1231232, "test",
                    observation_start = "2009-03-01", observation_end = "2009-03-01")
  )
  expect_error(
    get_alfred_series(1231232, "test",
                    observation_start = "2009-03-01", observation_end = "2009-03-01")
  )
})
