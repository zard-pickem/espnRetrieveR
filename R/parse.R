#' @importFrom stats setNames
NULL

#' Safely pluck a value from a nested list
#'
#' @param x List, input list
#' @param path Character vector, path to the value
#' @param default Default value if path not found
#' @return Value at path or default
#' @noRd
pluck_safe <- function(x, path, default = NA) {
  if (is.null(x) || length(path) == 0) return(default)

  current <- x
  for (key in path) {
    if (is.null(current)) {
      return(default)
    }

    # Handle atomic vectors or objects without names
    if (is.atomic(current) || is.null(names(current))) {
      return(default)
    }

    if (!key %in% names(current)) {
      return(default)
    }

    current <- current[[key]]
  }

  if (is.null(current)) default else current
}

#' Extract team information from competitor data
#'
#' @param competitor List, competitor data from ESPN API
#' @param prefix Character, prefix for column names (home_ or away_)
#' @return Named list with team information
#' @noRd
extract_team_info <- function(competitor, prefix) {
  if (is.null(competitor)) return(list())

  team_id <- pluck_safe(competitor, c("team", "id"), "")
  team_name <- pluck_safe(competitor, c("team", "displayName"), "")
  score <- as.integer(pluck_safe(competitor, "score", NA))

  stats::setNames(
    list(team_id, team_name, score),
    paste0(prefix, c("team_id", "team", "score"))
  )
}

#' Parse game status from competition data
#'
#' @param competition List, competition data from ESPN API
#' @return List with status information
#' @noRd
parse_game_status <- function(competition) {
  if (is.null(competition)) {
    return(list(
      status = "unknown",
      is_completed = FALSE
    ))
  }

  status_type <- pluck_safe(competition, c("status", "type", "name"), "unknown")
  status_state <- pluck_safe(competition, c("status", "type", "state"), "unknown")

  # Map ESPN status to our simplified status
  status <- switch(status_state,
    "pre" = "pre",
    "in" = "in",
    "post" = "final",
    "unknown"
  )

  is_completed <- status == "final"

  list(
    status = status,
    is_completed = is_completed
  )
}

#' Parse datetime string to POSIXct
#'
#' @param datetime_str Character, datetime string from ESPN API
#' @return POSIXct datetime object
#' @noRd
parse_datetime <- function(datetime_str) {
  if (is.na(datetime_str) || datetime_str == "") {
    return(as.POSIXct(NA))
  }

  # ESPN uses different ISO 8601 formats, try multiple patterns
  formats <- c(
    "%Y-%m-%dT%H:%M:%SZ",    # Full format with seconds
    "%Y-%m-%dT%H:%MZ",       # Format without seconds
    "%Y-%m-%dT%H:%M:%S",     # Format without Z
    "%Y-%m-%dT%H:%M"         # Minimal format
  )

  for (fmt in formats) {
    result <- tryCatch({
      as.POSIXct(datetime_str, format = fmt, tz = "UTC")
    }, error = function(e) NULL)

    if (!is.null(result) && !is.na(result)) {
      return(result)
    }
  }

  # If all formats fail, return NA
  as.POSIXct(NA)
}

#' Extract enrichment data from game summary
#'
#' @param summary List, game summary data from ESPN API
#' @param include Character vector, types of enrichment to include
#' @param event_id Character, event ID for core API calls
#' @param sport Character, sport name for core API calls
#' @param league Character, league abbreviation for core API calls
#' @return List with enrichment data
#' @noRd
extract_enrichment <- function(summary, include = character(), event_id = NULL,
                              sport = NULL, league = NULL) {
  # Initialize enrichment with base structure
  enrichment <- list()

  # Win probabilities from summary (no extra call)
  if ("winprob" %in% include) {
    winprob <- pluck_safe(summary, "winprobability")

    if (!is.null(winprob) && is.list(winprob) && length(winprob) > 0) {
      # Take the latest probability entry
      latest <- winprob[[length(winprob)]]

      if (!is.null(latest)) {
        home_wp <- suppressWarnings(as.numeric(pluck_safe(latest, "homeWinPercentage", NA)))
        away_wp <- suppressWarnings(as.numeric(pluck_safe(latest, "awayWinPercentage", NA)))

        # Convert 0-1 to 0-100 if needed
        if (!is.na(home_wp) && home_wp <= 1) home_wp <- home_wp * 100
        if (!is.na(away_wp) && away_wp <= 1) away_wp <- away_wp * 100

        # If only one side is present, infer complement
        if (is.na(away_wp) && !is.na(home_wp)) away_wp <- 100 - home_wp
        if (is.na(home_wp) && !is.na(away_wp)) home_wp <- 100 - away_wp

        if (!is.na(home_wp)) {
          enrichment$winprob_home_win_pct <- home_wp
          enrichment$winprob_away_win_pct <- away_wp
          enrichment$winprob_source <- "ESPN Win Probability"
        }
      }
    }
  }

  # Predictions from separate core API call
  if ("predictions" %in% include && !is.null(event_id) && !is.null(sport) && !is.null(league)) {
    predictor_url <- build_core_url(sport, league,
                                   sprintf("events/%s/competitions/%s/predictor", event_id, event_id))

    predictor_data <- safe_fetch(predictor_url)

    if (!is.null(predictor_data)) {
      # Extract home/away prediction from core API statistics array
      get_pct <- function(team_node) {
        stats <- pluck_safe(team_node, "statistics")
        if (is.null(stats)) return(NA_real_)

        # Prefer gameProjection, fallback to winProbability
        out <- NA_real_
        for (st in stats) {
          nm <- pluck_safe(st, "name", "")
          if (nm %in% c("gameProjection", "winProbability")) {
            val <- suppressWarnings(as.numeric(pluck_safe(st, "value", NA)))
            if (!is.na(val)) {
              out <- val
              break
            }
          }
        }
        out
      }

      home_proj <- get_pct(pluck_safe(predictor_data, "homeTeam"))
      away_proj <- get_pct(pluck_safe(predictor_data, "awayTeam"))

      # Normalize 0-1 to 0-100
      if (!is.na(home_proj) && home_proj <= 1) home_proj <- home_proj * 100
      if (!is.na(away_proj) && away_proj <= 1) away_proj <- away_proj * 100

      # Complement when needed
      if (is.na(away_proj) && !is.na(home_proj)) away_proj <- max(0, 100 - home_proj)
      if (is.na(home_proj) && !is.na(away_proj)) home_proj <- max(0, 100 - away_proj)

      if (!is.na(home_proj)) {
        enrichment$pred_home_win_pct <- round(home_proj, 1)
        enrichment$pred_away_win_pct <- round(away_proj, 1)
        enrichment$pred_source <- "ESPN Predictor"
      }
    }
  }

  # Odds from summary pickcenter
  if ("odds" %in% include) {
    pickcenter_odds <- pluck_safe(summary, "pickcenter")

    if (!is.null(pickcenter_odds) && is.list(pickcenter_odds) && length(pickcenter_odds) > 0) {
      first_odds <- pickcenter_odds[[1]]

      # Extract spread
      point_spread <- pluck_safe(first_odds, "pointSpread")
      if (!is.null(point_spread)) {
        home_spread_line <- pluck_safe(point_spread, c("home", "close", "line"), NA)

        if (!is.na(home_spread_line) && is.character(home_spread_line)) {
          spread_numeric <- as.numeric(gsub("\\+", "", home_spread_line))
        } else {
          spread_numeric <- as.numeric(home_spread_line)
        }

        if (!is.na(spread_numeric) && abs(spread_numeric) <= 50) {
          enrichment$spread <- spread_numeric
        }
      }

      # Extract over/under
      total <- pluck_safe(first_odds, "total")
      if (!is.null(total)) {
        over_line <- pluck_safe(total, c("over", "close", "line"), NA)

        if (!is.na(over_line) && is.character(over_line)) {
          total_numeric <- as.numeric(gsub("[ou]", "", over_line))
        } else {
          total_numeric <- as.numeric(over_line)
        }

        if (!is.na(total_numeric) && total_numeric > 0 && total_numeric <= 300) {
          enrichment$over_under <- total_numeric
        }
      }

      # Extract book name
      enrichment$book <- pluck_safe(first_odds, c("provider", "name"), NA)
    }
  }

  # Starters from summary
  if ("starters" %in% include) {
    # Try header.competitions for probable starters
    header_comps <- pluck_safe(summary, c("header", "competitions"))
    if (!is.null(header_comps) && length(header_comps) > 0) {
      comp <- header_comps[[1]]
      competitors <- pluck_safe(comp, "competitors")

      if (!is.null(competitors) && length(competitors) >= 2) {
        # Find home and away teams
        home_comp <- NULL
        away_comp <- NULL

        for (competitor in competitors) {
          if (pluck_safe(competitor, "homeAway", "") == "home") {
            home_comp <- competitor
          } else if (pluck_safe(competitor, "homeAway", "") == "away") {
            away_comp <- competitor
          }
        }

        if (!is.null(home_comp) && !is.null(away_comp)) {
          home_starter <- extract_starter_from_competitor(home_comp)
          away_starter <- extract_starter_from_competitor(away_comp)

          enrichment <- c(enrichment,
                         stats::setNames(home_starter, paste0("home_", names(home_starter))),
                         stats::setNames(away_starter, paste0("away_", names(away_starter))))
        }
      }
    }
  }

  # Injuries summary
  if ("injuries" %in% include) {
    # Basic injury counts - detailed implementation would query team endpoints
    enrichment$home_injuries_n <- NA_integer_
    enrichment$away_injuries_n <- NA_integer_
  }

  enrichment
}

#' Extract starter from competitor data
#'
#' @param competitor List, competitor data from header.competitions
#' @return List with starter information
#' @noRd
extract_starter_from_competitor <- function(competitor) {
  starter_info <- list(
    starter_id = NA_character_,
    starter_name = NA_character_,
    starter_role = NA_character_
  )

  # Check for probables field
  probables <- pluck_safe(competitor, "probables")
  if (!is.null(probables) && length(probables) > 0) {
    probable <- probables[[1]]
    starter_info$starter_id <- pluck_safe(probable, c("athlete", "id"), NA)
    starter_info$starter_name <- pluck_safe(probable, c("athlete", "displayName"), NA)
    starter_info$starter_role <- determine_starter_role(pluck_safe(probable, c("athlete", "position", "name"), ""))
  }

  starter_info
}

#' Determine starter role based on position
#'
#' @param position Character, position name
#' @return Character, starter role (pitcher, qb, goalie)
#' @noRd
determine_starter_role <- function(position) {
  if (is.na(position) || position == "") {
    return(NA_character_)
  }

  position_lower <- tolower(position)

  if (grepl("pitch", position_lower)) {
    return("pitcher")
  } else if (grepl("quarter", position_lower) || grepl("qb", position_lower)) {
    return("qb")
  } else if (grepl("goal", position_lower)) {
    return("goalie")
  } else {
    return(position)  # Return original position if not a typical starter
  }
}

#' Flatten statistics from ESPN API response
#'
#' @param stats_data List, statistics data from ESPN API
#' @return Named numeric vector with flattened statistics
#' @noRd
flatten_stats <- function(stats_data) {
  if (is.null(stats_data) || is.null(stats_data$splits) ||
      is.null(stats_data$splits$categories)) {
    return(numeric(0))
  }

  categories <- stats_data$splits$categories
  flat_stats <- numeric(0)

  for (category in categories) {
    if (!is.null(category$stats)) {
      for (stat in category$stats) {
        stat_name <- pluck_safe(stat, "name", "")
        stat_value <- as.numeric(pluck_safe(stat, "value", NA))

        if (stat_name != "" && !is.na(stat_value)) {
          flat_stats[stat_name] <- stat_value
        }
      }
    }
  }

  flat_stats
}