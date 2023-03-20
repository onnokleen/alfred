#' Accessing ALFRED
#'
#' This function can pull time series from the ALFRED database: \url{https://alfred.stlouisfed.org}.
#' Downloading different vintages for performing real-time analysis is provided.
#' @name get_alfred_series
#' @param series_id FRED times series ID.
#' @param series_name Choose a name for the series column in output. Default: series_id.
#' @param observation_start Date of first observation in "yyyy-mm-dd" format. Default: Earliest observation available.
#' @param observation_end Date of last observation in "yyyy-mm-dd" format. Default: Last observation available.
#' @param realtime_start Date of first real time period in "yyyy-mm-dd" format. Default: First vintage date available.
#' @param realtime_end Date of last real time period in "yyyy-mm-dd" format. Default: Last vintage date available.
#' @param api_key You can supply your own apikey obtained via \url{https://fredaccount.stlouisfed.org/login/secure/)}
#' if you want to run a large batch of requests. Otherwise you might run into query limits of the API.
#' @details FRED time series IDs can be found on the respective site in ALFRED, e.g. \url{https://alfred.stlouisfed.org/series?seid=CPIAUCSL}.
#' @keywords alfred
#' @export get_alfred_series
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate_
#' @importFrom dplyr mutate_if
#' @importFrom lubridate as_date
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @importFrom tidyselect contains
#' @importFrom dplyr bind_rows
#' @importFrom stats na.omit
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom httr content
#' @examples \dontrun{
#'     get_alfred_series("INDPRO", "indpro")
#'     }
#' @examples \dontrun{
#'     get_alfred_series("INDPRO", "indpro",
#'     realtime_start = "2008-10-31", realtime_end = "2009-10-31")
#'     }

get_alfred_series <-
  function(series_id, series_name = NULL,
           observation_start = NULL, observation_end = NULL,
           realtime_start = NULL, realtime_end = NULL, api_key = NULL) {

  if (is.character(series_id) == FALSE) {
    stop("series_id is always in characters")
  }

  length_series_id <- nchar(series_id)

  if (is.null(series_name)) {
    series_name <- series_id
  }

  if (is.null(realtime_start)) {
    realtime_start <- "1776-07-04"
  }

  if (is.null(realtime_end)) {
    realtime_end <- "9999-12-31"
  }

  if (is.null(observation_start)) {
    observation_start <- "1776-07-04"
  }

  if (is.null(observation_end)) {
    observation_end <- "9999-12-31"
  }

  if (is.null(api_key)) {
    api_key <- "98f9f5cad7212e246dc5955e9b744b24"
  }

  json_api_call <-
    try({
        content(GET( paste0("https://api.stlouisfed.org/fred/series/observations?series_id=",
                            series_id,
                            "&realtime_start=",
                            realtime_start,
                            "&realtime_end=",
                            realtime_end,
                            "&output_type=2&observation_start=",
                            observation_start,
                            "&observation_end=",
                            observation_end,
                            "&api_key=",api_key,"&file_type=json")),
                as = "text")
    }, silent = TRUE)


  if (grepl("error_code", json_api_call, fixed = TRUE)) {
    if (grepl("429", json_api_call, fixed = TRUE)) {
      warning("Too many request have been made to the FRED server via the package's API key.\n Please supply your own API key via the optional argument api_key.")
    }
    if (grepl("400", json_api_call, fixed = TRUE)) {
      warning("Download of specified time-series failed - did you misspell the identifier?")
    }
    return(NULL)
  }

  df_series <-
    jsonlite::fromJSON(json_api_call, simplifyDataFrame = TRUE)$observations

  df_series <-
    df_series %>%
    mutate(date = as_date(df_series[["date"]])) %>%
    gather("realtime_period", "name", contains(c(setdiff(names(df_series), "date")))) %>%
    na.omit() %>%
    mutate(realtime_period =
              paste(substr(.data$realtime_period, start = length_series_id + 2, stop = length_series_id + 5),
                     substr(.data$realtime_period, start = length_series_id + 6, stop = length_series_id + 7),
                     substr(.data$realtime_period, start = length_series_id + 8, stop = length_series_id + 9),
                     sep = "-")) %>%
    mutate(realtime_period = as_date(.data$realtime_period),
            date = as_date(.data$date),
            name = as.numeric(.data$name))

  df_series <- df_series[df_series$realtime_period != "9999-12-31",]

  colnames(df_series)[!colnames(df_series) %in% c("date", "realtime_period")] <- series_name

  df_series
}

#' Accessing FRED
#'
#' This function can pull time series from the FRED database: \url{https://fred.stlouisfed.org}.
#' @name get_fred_series
#' @param series_id FRED times series ID.
#' @param series_name Choose a name for the series column in output. Default: series_id.
#' @param observation_start Date of first observation in "yyyy-mm-dd" format. Default: Earliest observation available.
#' @param observation_end Date of last observation in "yyyy-mm-dd" format. Default: Last observation available.
#' @param api_key You can supply your own apikey obtained via \url{https://fredaccount.stlouisfed.org/login/secure/)}
#' if you want to run a large batch of requests. Otherwise you might run into query limits of the API.
#' @keywords fred
#' @export get_fred_series
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom lubridate as_date
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @examples \dontrun{
#'     get_fred_series("INDPRO", "indpro")
#'     }
get_fred_series <- function(series_id, series_name = NULL,
                            observation_start = NULL, observation_end = NULL,
                            api_key = NULL) {


  length_series_id <- nchar(series_id)

  if (!is.character(series_id)) {
    stop("series_id is always in characters")
  }

  if (is.null(series_name)) {
    series_name <- series_id
  }

  if (is.null(observation_start)) {
    observation_start <- "1776-07-04"
  }

  if (is.null(observation_end)) {
    observation_end <- "9999-12-31"
  }

  if (is.null(api_key)) {
    api_key <- "98f9f5cad7212e246dc5955e9b744b24"
  }

  json_api_call <-
    try({
      content(GET(paste0("https://api.stlouisfed.org/fred/series/observations?series_id=",
               series_id,
               "&observation_start=",
               observation_start,
               "&observation_end=",
               observation_end,
               "&output_type=2",
               "&api_key=",api_key,"&file_type=json")),
               as = "text")
    }, silent = TRUE)

  if ("error_code"  %in% names(json_api_call)) {
    if (json_api_call$error_code == 429) {
      warning("Too many request have been made to the FRED server via the package's API key.\n Please supply your own API key via the optional argument api_key.")
    }
    if (json_api_call$error_code == 429) {
      warning("Download of specified time-series failed - did you misspell the identifier?")
    }
    return(NULL)
  }

  df_series <-
    jsonlite::fromJSON(json_api_call, simplifyDataFrame = TRUE)$observations %>%
    mutate(date = as_date(.data$date))

  colnames(df_series)[!(colnames(df_series) %in% "date")] <- series_name
  df_series[, 2] <- as.numeric(unlist(df_series[, 2]))
  df_series
}
