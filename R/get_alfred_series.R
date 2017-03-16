# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' This function estimates a multiplicative mixed-frequency GARCH model
#' @param series_id, series_name.
#' @keywords alfred
#' @export get_alfred_series
#' @importFrom magrittr %>%
#' @importFrom tibble data_frame
# @examples likelihood_gjrgarch(0.01, 0.02, 0.9, 0.02, y = rnorm(1:4), mu = 0, g.0 = 0.2)

get_alfred_series <- function(series_id, series_name) {
  length_series_id <- nchar(series_id)

  df_series <-
    xml2::read_xml(paste0("https://api.stlouisfed.org/fred/series/observations?series_id=",
                    series_id,
                    "&realtime_start=1960-01-01&realtime_end=9999-12-31&output_type=2&api_key=98f9f5cad7212e246dc5955e9b744b24"))
  series_freq <- xml2::read_xml("https://api.stlouisfed.org/fred/series/tags?series_id=HOUST&api_key=98f9f5cad7212e246dc5955e9b744b24")

  series_freq <-
    lapply(xml2::xml_children(series_freq), function(x) as.list(xml2::xml_attrs(x))) %>%
    lapply(., unlist) %>%
    lapply(., as.list) %>%
    lapply(., tibble::as_tibble) %>%
    Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2, by = c("name", "group_id", "notes", "created", "popularity", "series_count")), .) %>%
    filter(group_id == "freq") %>%
    select(name)

  suppressMessages(
    df_series <-
      lapply(xml2::xml_children(df_series), function(x) as.list(xml2::xml_attrs(x))) %>%
      lapply(., unlist) %>%
      lapply(., as.list) %>%
      lapply(., tibble::as_tibble) %>%
      Reduce(function(dtf1,dtf2) full_join(dtf1, dtf2), .) %>%
      tidyr::gather(realtime_period, name, -date) %>%
      mutate(realtime_period = paste(substr(realtime_period, start = length_series_id + 2, stop = length_series_id + 5),
                                     substr(realtime_period, start = length_series_id + 6, stop = length_series_id + 7),
                                     substr(realtime_period, start = length_series_id + 8, stop = length_series_id + 9),
                                     sep = "-")) %>%
      dplyr::mutate(realtime_period = lubridate::as_date(realtime_period),
                    date = lubridate::as_date(date),
                    name = as.numeric(name)) %>%
      dplyr::filter(realtime_period != "9999-12-31")
  )

  colnames(df_series)[colnames(df_series) %in% "name"] <- series_name

  df_series
}
