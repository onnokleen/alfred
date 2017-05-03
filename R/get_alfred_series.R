#' This function downloads times series from the alfred dadatabase, https://alfred.stlouisfed.org
#' @param series_id, series_name.
#' @keywords alfred
#' @export get_alfred_series
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_attrs
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom lubridate as_date
# @examples get_alfred_series("INDPRO", "indpro")
# @examples get_alfred_series("INDPRO", "indpro", real_time_start = "2008-10-31", real_time_end = "2009-10-31")

get_alfred_series <- function(series_id, series_name, observation_start = NULL, observation_end = NULL, real_time_start = NULL, real_time_end = NULL) {
  length_series_id <- nchar(series_id)

  if (is.null(real_time_start)  == TRUE) {
    real_time_start <- "1776-07-04"
  }

  if (is.null(real_time_end) == TRUE) {
    real_time_end <- "9999-12-31"
  }

  if (is.null(observation_start) == TRUE) {
    observation_start <- "1776-07-04"
  }

  if (is.null(observation_end) == TRUE) {
    observation_end <- "9999-12-31"
  }

  df_series <-
    read_xml(paste0("https://api.stlouisfed.org/fred/series/observations?series_id=",
                    series_id,
                    "&realtime_start=",
                    real_time_start,
                    "&realtime_end=",
                    real_time_end,
                    "&output_type=2&observation_start=",
                    observation_start,
                    "&observation_end=",
                    observation_end,
                    "&api_key=98f9f5cad7212e246dc5955e9b744b24"))
  # series_freq <-
  #   read_xml(paste0("https://api.stlouisfed.org/fred/series/observations?series_id=",
  #                   series_id,
  #                   "&api_key=98f9f5cad7212e246dc5955e9b744b24"))
  #
  # series_freq <-
  #   lapply(xml_children(series_freq), function(x) as.list(xml_attrs(x))) %>%
  #   lapply(., unlist) %>%
  #   lapply(., as.list) %>%
  #   lapply(., tibble::as_tibble) %>%
  #   Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2, by = c("name", "group_id", "notes", "created", "popularity", "series_count")), .) %>%
  #   filter(group_id == "freq") %>%
  #   select(name)
  df_series <-
    lapply(xml_children(df_series), function(x) as.list(xml_attrs(x))) %>%
    lapply(., unlist) %>%
    lapply(., as.list) %>%
    lapply(., tibble::as_tibble) %>%
    bind_rows() %>%
    tidyr::gather(realtime_period, name, -date) %>%
    na.omit() %>%
    mutate(realtime_period = paste(substr(realtime_period, start = length_series_id + 2, stop = length_series_id + 5),
                                   substr(realtime_period, start = length_series_id + 6, stop = length_series_id + 7),
                                   substr(realtime_period, start = length_series_id + 8, stop = length_series_id + 9),
                                   sep = "-")) %>%
    dplyr::mutate(realtime_period = lubridate::as_date(realtime_period),
                  date = lubridate::as_date(date),
                  name = as.numeric(name)) %>%
    dplyr::filter(realtime_period != "9999-12-31")

  colnames(df_series)[colnames(df_series) %in% "name"] <- series_name

  df_series
}

#' This function downloads times series from the fred dadatabase, https://alfred.stlouisfed.org
#' @param series_id, series_name.
#' @keywords fred
#' @export get_fred_series
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_attrs
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom lubridate as_date
# @examples get_fred_series("INDPRO", "indpro")

get_fred_series <- function(series_id, series_name, observation_start = NULL, observation_end = NULL) {
  length_series_id <- nchar(series_id)

  if (is.null(observation_start) == TRUE) {
    real_time_end <- "1776-07-04"
  }

  if (is.null(observation_end) == TRUE) {
    real_time_end <- "9999-12-31"
  }

  df_series <-
    read_xml(paste0("https://api.stlouisfed.org/fred/series/observations?series_id=",
                    series_id,
                    "&output_type=2&observation_start=",
                    observation_start,
                    "&observation_end=",
                    observation_end,
                    "&api_key=98f9f5cad7212e246dc5955e9b744b24"))

  suppressMessages(
    df_series <-
      lapply(xml_children(df_series), function(x) as.list(xml_attrs(x))) %>%
      lapply(., unlist) %>%
      lapply(., as.list) %>%
      lapply(., tibble::as_tibble) %>%
      bind_rows() %>%
      dplyr::mutate(date = lubridate::as_date(date))
  )

  colnames(df_series)[!(colnames(df_series) %in% "date")] <- series_name

  df_series
}
