#' @importFrom httr2 request req_user_agent req_headers req_timeout req_perform resp_status resp_body_json
#' @importFrom utils URLencode
NULL

#' Create ESPN API request
#'
#' @param url Character, the ESPN API URL to request
#' @return httr2 request object
#' @noRd
espn_req <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent("espnRetrieveR/0.1.0") |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_timeout(30)
}

#' Fetch JSON from ESPN API
#'
#' @param url Character, the ESPN API URL
#' @return List, parsed JSON response
#' @noRd
fetch_json <- function(url) {
  resp <- httr2::req_perform(espn_req(url))
  sc <- httr2::resp_status(resp)
  if (sc != 200) stop(sprintf("HTTP %s: %s", sc, url), call. = FALSE)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' Safe fetch for auxiliary data
#'
#' @param url Character, the ESPN API URL
#' @return List or NULL, parsed JSON response or NULL on error
#' @noRd
safe_fetch <- function(url) {
  tryCatch({
    fetch_json(url)
  }, error = function(e) {
    warning(sprintf("Failed to fetch auxiliary data from: %s", url), call. = FALSE)
    NULL
  })
}

#' Build ESPN scoreboard URL
#'
#' @param sport Character, sport name (football, basketball, hockey, baseball)
#' @param league Character, league abbreviation (nfl, nba, nhl, mlb)
#' @param dates Character, date or date range (YYYYMMDD or YYYYMMDD-YYYYMMDD)
#' @param limit Integer, maximum number of results
#' @param week Integer, NFL week number (optional)
#' @param year Integer, season year (optional)
#' @param seasontype Integer, season type (optional)
#' @return Character, complete ESPN scoreboard URL
#' @noRd
build_scoreboard_url <- function(sport, league, dates = NULL, limit = 1000,
                                 week = NULL, year = NULL, seasontype = NULL) {
  base_url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/%s/%s/scoreboard",
                      sport, league)

  params <- list()
  if (!is.null(dates)) params$dates <- dates
  if (!is.null(week)) params$week <- as.character(week)
  if (!is.null(year)) params$year <- as.character(year)
  if (!is.null(seasontype)) params$seasontype <- as.character(seasontype)
  if (!is.null(limit)) params$limit <- as.character(limit)

  if (length(params) > 0) {
    query_string <- paste(names(params), params, sep = "=", collapse = "&")
    base_url <- paste0(base_url, "?", query_string)
  }

  base_url
}

#' Build ESPN core API URL
#'
#' @param sport Character, sport name
#' @param league Character, league abbreviation
#' @param path Character, API path
#' @return Character, complete ESPN core API URL
#' @noRd
build_core_url <- function(sport, league, path) {
  sprintf("https://sports.core.api.espn.com/v2/sports/%s/leagues/%s/%s",
          sport, league, path)
}

#' Map league to sport
#'
#' @param league Character, league abbreviation
#' @return Character, sport name
#' @noRd
map_league_to_sport <- function(league) {
  sport_map <- c(
    "nfl" = "football",
    "nba" = "basketball",
    "nhl" = "hockey",
    "mlb" = "baseball"
  )

  if (!league %in% names(sport_map)) {
    stop(sprintf("Unsupported league: %s. Supported: %s",
                 league, paste(names(sport_map), collapse = ", ")),
         call. = FALSE)
  }

  sport_map[[league]]
}

#' Determine NFL season type based on current date
#'
#' @param date Date, current date (defaults to today)
#' @return Integer, season type (1=preseason, 2=regular, 3=postseason)
#' @noRd
detect_nfl_season_type <- function(date = Sys.Date()) {
  if (is.character(date)) {
    date <- as.Date(date)
  }

  month <- as.integer(format(date, "%m"))
  day <- as.integer(format(date, "%d"))

  # NFL season typically runs:
  # Preseason: August - early September (weeks 1-4)
  # Regular season: September - December (weeks 1-18)
  # Postseason: January - February (wildcard through Super Bowl)

  if (month == 8 || (month == 9 && day <= 10)) {
    return(1L)  # Preseason
  } else if (month %in% c(9, 10, 11, 12) || (month == 1 && day <= 7)) {
    return(2L)  # Regular season (extends into early January)
  } else if (month == 1 || (month == 2 && day <= 14)) {
    return(3L)  # Postseason (through Super Bowl in early February)
  } else {
    # Off-season - default to regular season for data consistency
    return(2L)
  }
}

#' Get NFL season type with graceful fallbacks
#'
#' @param season_type Integer, explicit season type (optional)
#' @param date Date, date for automatic detection
#' @return Integer, season type with fallback logic
#' @noRd
get_nfl_season_type <- function(season_type = NULL, date = Sys.Date()) {
  # If explicitly provided, use it
  if (!is.null(season_type) && !is.na(season_type)) {
    return(as.integer(season_type))
  }

  # Try automatic detection
  detected <- tryCatch({
    detect_nfl_season_type(date)
  }, error = function(e) {
    warning("Failed to detect NFL season type, defaulting to regular season (2)",
            call. = FALSE)
    2L
  })

  return(detected)
}

#' Format date for ESPN API
#'
#' @param date Character or Date, input date
#' @return Character, date formatted as YYYYMMDD
#' @noRd
format_date_for_api <- function(date) {
  if (is.character(date)) {
    # Handle different input formats
    if (nchar(date) == 8 && grepl("^\\d{8}$", date)) {
      return(date)  # Already in YYYYMMDD format
    } else if (nchar(date) == 10 && grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) {
      return(gsub("-", "", date))  # Convert YYYY-MM-DD to YYYYMMDD
    } else {
      stop("Invalid date format. Use YYYY-MM-DD or YYYYMMDD", call. = FALSE)
    }
  } else if (inherits(date, "Date")) {
    return(format(date, "%Y%m%d"))
  } else {
    stop("Date must be character or Date object", call. = FALSE)
  }
}