#' Accessing ALFRED
#'
#' This function can pull time series from the ALFRED database: \url{https://alfred.stlouisfed.org}
#' Downloading different vintages for performing real-time analysis is provided.
#' @name get_alfred_series
#' @param series_id FRED times series ID.
#' @param series_name Choose a name for the series column in output. Default: series_id.
#' @param observation_start Date of first observation in "yyyy-mm-dd" format. Default: Earliest observation available.
#' @param observation_end Date of last observation in "yyyy-mm-dd" format. Default: Last observation available.
#' @param real_time_start Date of first real time period in "yyyy-mm-dd" format. Default: First vintage date available.
#' @param real_time_end Date of last real time period in "yyyy-mm-dd" format. Default: Last vintage date available.
#' @details FRED time series IDs can be found on the respective site in ALFRED, i.e. \url{https://alfred.stlouisfed.org/series?seid=CPIAUCSL}.
#' @keywords alfred
#' @export get_alfred_series
#' @usage get_alfred_series(series_id, series_name = NULL,
#'     observation_start = NULL, observation_end = NULL,
#'     real_time_start = NULL, real_time_end = NULL)
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_attrs
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather_
#' @importFrom dplyr mutate_
#' @importFrom dplyr filter_
#' @importFrom lubridate as_date
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
#' @importFrom stats na.omit
#' @examples \dontrun{
#'     get_alfred_series("INDPRO", "indpro")
#'     }
#'
#' @examples get_alfred_series("INDPRO", "indpro", real_time_start = "2008-10-31", real_time_end = "2009-10-31")

get_alfred_series <-
  function(series_id, series_name = NULL,
           observation_start = NULL, observation_end = NULL,
           real_time_start = NULL, real_time_end = NULL) {

  if (is.character(series_id) == FALSE) {
    stop("series_id is always in characters")
  }

  length_series_id <- nchar(series_id)

  if (is.null(series_id) == TRUE ) {
    series_name <- series_id
  }

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

  # Form list of xml files to data frame
  df_series <-
    lapply(xml_children(df_series), function(x) as.list(xml_attrs(x))) %>%
    lapply(unlist) %>%
    lapply(as.list) %>%
    lapply(as_tibble) %>%
    bind_rows()

  # Reshape data and convert columns into preferred type
  df_series <-
    df_series %>%
    gather_("realtime_period", "name", setdiff(names(df_series), "date")) %>%
    na.omit() %>%
    mutate_(realtime_period = ~paste(substr(realtime_period, start = length_series_id + 2, stop = length_series_id + 5),
                                   substr(realtime_period, start = length_series_id + 6, stop = length_series_id + 7),
                                   substr(realtime_period, start = length_series_id + 8, stop = length_series_id + 9),
                                   sep = "-")) %>%
    mutate_(realtime_period = ~as_date(realtime_period),
            date = ~as_date(date),
            name = ~as.numeric(name)) %>%
    filter_(.dots = paste0("realtime_period", "!= ", "9999-12-31"))

  colnames(df_series)[!colnames(df_series) %in% c("date", "realtime_period")] <- series_name

  df_series
}

#' Accessing FRED
#'
#' This function can pull time series from the FRED database: \url{https://fred.stlouisfed.org}
#' @name get_fred_series
#' @param series_id FRED times series ID.
#' @param series_name Choose a name for the series column in output. Default: series_id.
#' @param observation_start Date of first observation in "yyyy-mm-dd" format. Default: Earliest observation available.
#' @param observation_end Date of last observation in "yyyy-mm-dd" format. Default: Last observation available.
#' @keywords fred
#' @usage get_fred_series(series_id, series_name = NULL,
#'     observation_start = NULL, observation_end = NULL)
#' @export get_fred_series
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_attrs
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate_
#' @importFrom lubridate as_date
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
#' @examples get_fred_series("INDPRO", "indpro")
get_fred_series <- function(series_id, series_name = NULL, observation_start = NULL, observation_end = NULL) {
  length_series_id <- nchar(series_id)

  if (is.character(series_id) == FALSE) {
    stop("series_id is always in characters")
  }

  if (is.null(series_id) == TRUE ) {
    series_name <- series_id
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
                    "&output_type=2&observation_start=",
                    observation_start,
                    "&observation_end=",
                    observation_end,
                    "&api_key=98f9f5cad7212e246dc5955e9b744b24"))

  suppressMessages(
    df_series <-
      lapply(xml_children(df_series), function(x) as.list(xml_attrs(x))) %>%
      lapply(unlist) %>%
      lapply(as.list) %>%
      lapply(as_tibble) %>%
      bind_rows() %>%
      mutate_(date = ~as_date(date))
  )

  colnames(df_series)[!(colnames(df_series) %in% "date")] <- series_name

  df_series[, 2] <- as.numeric(unlist(df_series[, 2]))

  df_series
}
