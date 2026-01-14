#' @import data.table
NULL

#' Get games by date with optional enrichment
#'
#' @param date Character, date in YYYY-MM-DD format or YYYYMMDD
#' @param league Character, league abbreviation (nfl, nba, nhl, mlb)
#' @param sport Character, sport name (optional, will be inferred from league)
#' @param limit Integer, maximum number of games to return
#' @param include Character vector, types of enrichment to include. Options:
#'   c("winprob", "predictions", "odds", "starters", "injuries")
#' @param include_team_stats Logical, whether to join team season statistics
#' @param include_power_index Logical, whether to join team power index data
#' @param minimal Logical, if TRUE return only core columns + win probabilities
#' @return data.table with game information
#' @export
#' @examples
#' \dontrun{
#' # Get basic game schedule
#' games <- get_games_by_date("2025-08-25", "nfl")
#'
#' # Get games with enrichment
#' enriched_games <- get_games_by_date(
#'   "2025-08-25", "nfl",
#'   include = c("predictions", "odds", "starters"),
#'   include_team_stats = TRUE
#' )
#' }
get_games_by_date <- function(date, league, sport = NULL, limit = 1000,
                              include = character(),
                              include_team_stats = FALSE,
                              include_power_index = FALSE,
                              minimal = FALSE) {

  # Validate inputs
  if (missing(date) || missing(league)) {
    stop("Both 'date' and 'league' are required", call. = FALSE)
  }

  if (is.null(sport)) {
    sport <- map_league_to_sport(league)
  }

  # Build and fetch scoreboard data
  date_formatted <- format_date_for_api(date)
  scoreboard_url <- build_scoreboard_url(sport, league, dates = date_formatted, limit = limit)
  scoreboard_data <- fetch_json(scoreboard_url)

  # Parse base game data
  games_dt <- parse_scoreboard_data(scoreboard_data, sport, league)

  # For minimal mode, only add win probabilities and return
  if (minimal) {
    if (nrow(games_dt) > 0) {
      # Add minimal enrichment (winprob only)
      games_dt <- enrich_games_data(games_dt, sport, league, "winprob")

      # Return minimal columns
      minimal_cols <- c("event_id", "date_time_utc", "home_team", "away_team",
                       "home_score", "away_score", "winprob_home_win_pct", "winprob_away_win_pct")
      available_cols <- minimal_cols[minimal_cols %in% names(games_dt)]
      games_dt <- games_dt[, available_cols, with = FALSE]
    }
    return(games_dt)
  }

  # Apply enrichment if requested
  if (length(include) > 0 && nrow(games_dt) > 0) {
    games_dt <- enrich_games_data(games_dt, sport, league, include)
  }

  # Join team stats if requested
  if (include_team_stats && nrow(games_dt) > 0) {
    games_dt <- join_team_stats(games_dt, sport, league)
  }

  # Join power index if requested
  if (include_power_index && nrow(games_dt) > 0) {
    games_dt <- join_power_index(games_dt, sport, league)
  }

  # Set key and return
  if (nrow(games_dt) > 0) {
    data.table::setkey(games_dt, date_time_utc, league, event_id)
  }
  games_dt
}

#' Get completed games (scores only)
#'
#' @param date Character, date in YYYY-MM-DD format or YYYYMMDD
#' @param league Character, league abbreviation (nfl, nba, nhl, mlb)
#' @param sport Character, sport name (optional, will be inferred from league)
#' @param limit Integer, maximum number of games to return
#' @param include Character vector, types of enrichment to include (optional)
#' @param minimal Logical, if TRUE return minimal schema with final win probabilities
#' @return data.table with completed game scores
#' @export
#' @examples
#' \dontrun{
#' completed_games <- get_completed_games("2025-08-24", "nfl")
#' }
get_completed_games <- function(date, league, sport = NULL, limit = 1000,
                               include = character(), minimal = FALSE) {

  # Validate inputs
  if (missing(date) || missing(league)) {
    stop("Both 'date' and 'league' are required", call. = FALSE)
  }

  if (is.null(sport)) {
    sport <- map_league_to_sport(league)
  }

  # Format date for ESPN API
  date_formatted <- format_date_for_api(date)

  # Build and fetch scoreboard data
  scoreboard_url <- build_scoreboard_url(sport, league, date_formatted, limit)
  scoreboard_data <- fetch_json(scoreboard_url)

  # Parse and filter for completed games only
  games_dt <- parse_scoreboard_data(scoreboard_data, sport, league)
  completed_dt <- games_dt[is_completed == TRUE]

  # For minimal mode, set final win probabilities
  if (minimal && nrow(completed_dt) > 0) {
    # Set final winprob as 1/0 based on scores
    completed_dt[, winprob_home_win_pct := ifelse(home_score > away_score, 100, 0)]
    completed_dt[, winprob_away_win_pct := ifelse(away_score > home_score, 100, 0)]

    # Return minimal columns
    minimal_cols <- c("event_id", "date_time_utc", "home_team", "away_team",
                     "home_score", "away_score", "winprob_home_win_pct", "winprob_away_win_pct")
    available_cols <- minimal_cols[minimal_cols %in% names(completed_dt)]
    completed_dt <- completed_dt[, available_cols, with = FALSE]
  } else if (length(include) > 0 && nrow(completed_dt) > 0) {
    # Apply enrichment if requested
    completed_dt <- enrich_games_data(completed_dt, sport, league, include)
  }

  # Set key and return
  if (nrow(completed_dt) > 0) {
    data.table::setkey(completed_dt, date_time_utc, league, event_id)
  }
  completed_dt
}

#' Parse scoreboard data into standardized data.table
#'
#' @param scoreboard_data List, raw scoreboard data from ESPN API
#' @param sport Character, sport name
#' @param league Character, league abbreviation
#' @return data.table with parsed game data
#' @noRd
parse_scoreboard_data <- function(scoreboard_data, sport, league) {

  if (is.null(scoreboard_data$events) || length(scoreboard_data$events) == 0) {
    # Return empty data.table with correct structure
    return(create_empty_games_dt())
  }

  events <- scoreboard_data$events
  games_list <- list()

  for (i in seq_along(events)) {
    event <- events[[i]]

    # Safely extract competitions
    competitions <- pluck_safe(event, "competitions", NULL)
    if (is.null(competitions) || length(competitions) == 0) {
      next
    }

    # Handle case where competitions might be atomic or list
    if (is.atomic(competitions)) {
      next
    }

    competition <- competitions[[1]]

    # Extract basic event information
    event_id <- pluck_safe(event, "id", "")
    date_time_str <- pluck_safe(event, "date", "")

    date_time_utc <- parse_datetime(date_time_str)

    # Parse season information
    season <- as.integer(pluck_safe(event, c("season", "year"), NA))
    season_type <- as.integer(pluck_safe(event, c("season", "type"), NA))

    # Extract teams and status
    competitors <- pluck_safe(competition, "competitors", NULL)
    if (is.null(competitors) || length(competitors) < 2) next

    # Determine home/away (ESPN uses homeAway field)
    home_idx <- which(sapply(competitors, function(x) pluck_safe(x, "homeAway", "") == "home"))
    away_idx <- which(sapply(competitors, function(x) pluck_safe(x, "homeAway", "") == "away"))

    if (length(home_idx) == 0 || length(away_idx) == 0) {
      # Fallback: assume first is home, second is away
      home_idx <- 1
      away_idx <- 2
    } else {
      home_idx <- home_idx[1]
      away_idx <- away_idx[1]
    }

    # Extract team information
    home_info <- extract_team_info(competitors[[home_idx]], "home_")
    away_info <- extract_team_info(competitors[[away_idx]], "away_")

    # Extract status
    status_info <- parse_game_status(competition)

    # Combine all information
    game_row <- c(
      list(
        event_id = event_id,
        league = league,
        sport = sport,
        season = season,
        season_type = season_type,
        date_time_utc = date_time_utc
      ),
      status_info,
      home_info,
      away_info
    )

    games_list[[length(games_list) + 1]] <- game_row
  }

  if (length(games_list) == 0) {
    return(create_empty_games_dt())
  }

  # Convert to data.table
  games_dt <- data.table::rbindlist(games_list, fill = TRUE)
  games_dt
}

#' Create empty games data.table with correct structure
#'
#' @return Empty data.table with games structure
#' @noRd
create_empty_games_dt <- function() {
  data.table::data.table(
    event_id = character(0),
    league = character(0),
    sport = character(0),
    season = integer(0),
    season_type = integer(0),
    date_time_utc = as.POSIXct(character(0)),
    status = character(0),
    is_completed = logical(0),
    home_team_id = character(0),
    home_team = character(0),
    home_score = integer(0),
    away_team_id = character(0),
    away_team = character(0),
    away_score = integer(0)
  )
}

#' Enrich games data with auxiliary information
#'
#' @param games_dt data.table, base games data
#' @param sport Character, sport name
#' @param league Character, league abbreviation
#' @param include Character vector, types of enrichment
#' @return data.table with enriched games data
#' @noRd
enrich_games_data <- function(games_dt, sport, league, include) {

  if (nrow(games_dt) == 0) return(games_dt)

  # Get unique event IDs for enrichment
  event_ids <- unique(games_dt$event_id)

  # Process each event for enrichment
  for (event_id in event_ids) {
    # Fetch game summary (single try, non-blocking)
    summary_url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/%s/%s/summary?event=%s",
                          sport, league, event_id)

    summary_data <- safe_fetch(summary_url)

    if (!is.null(summary_data)) {
      enrichment <- extract_enrichment(summary_data, include, event_id, sport, league)

      # Apply enrichment to matching rows
      for (col_name in names(enrichment)) {
        if (!col_name %in% names(games_dt)) {
          # Initialize column type based on enrichment value
          val <- enrichment[[col_name]]
          if (is.numeric(val)) {
            games_dt[[col_name]] <- rep(NA_real_, nrow(games_dt))
          } else if (is.logical(val)) {
            games_dt[[col_name]] <- rep(NA, nrow(games_dt))
          } else {
            games_dt[[col_name]] <- rep(NA_character_, nrow(games_dt))
          }
        }

        # Set value for current event
        current_event_id <- event_id
        val <- enrichment[[col_name]]
        games_dt[event_id == current_event_id, (col_name) := val]
      }
    }
  }

  games_dt
}

#' Join team statistics to games data
#'
#' @param games_dt data.table, base games data
#' @param sport Character, sport name
#' @param league Character, league abbreviation
#' @return data.table with team statistics joined
#' @noRd
join_team_stats <- function(games_dt, sport, league) {

  if (nrow(games_dt) == 0) return(games_dt)

  # Get unique team IDs and season
  unique_teams <- unique(c(games_dt$home_team_id, games_dt$away_team_id))
  unique_seasons <- unique(games_dt$season[!is.na(games_dt$season)])

  if (length(unique_seasons) == 0) {
    warning("No season information available for team stats join", call. = FALSE)
    return(games_dt)
  }

  season <- unique_seasons[1]  # Use the first available season

  # Use previous year if current year stats aren't available yet
  if (season >= 2025) {
    season <- 2024
  }

  # Fetch team stats for each unique team (single try per team)
  team_stats_list <- list()

  for (team_id in unique_teams) {
    if (is.na(team_id) || team_id == "") next

    # Try with season_type=2 (regular season) for current season
    stats_url <- build_core_url(sport, league, sprintf("seasons/%d/types/2/teams/%s/statistics", season, team_id))
    team_stats <- safe_fetch(stats_url)

    if (!is.null(team_stats)) {
      flat_stats <- flatten_stats(team_stats)

      if (length(flat_stats) > 0) {
        # Keep only a few key stats to avoid column explosion
        key_stats <- head(flat_stats, 5)  # Top 5 stats
        team_stats_list[[team_id]] <- as.list(key_stats)
      }
    }
  }

  # Join stats to games data if we have any
  if (length(team_stats_list) > 0) {
    # Create home and away stat columns
    stat_names <- unique(unlist(lapply(team_stats_list, names)))

    for (stat_name in stat_names) {
      home_col <- paste0("home_", stat_name)
      away_col <- paste0("away_", stat_name)

      games_dt[[home_col]] <- NA_real_
      games_dt[[away_col]] <- NA_real_

      # Fill in values where available
      for (team_id in names(team_stats_list)) {
        if (stat_name %in% names(team_stats_list[[team_id]])) {
          games_dt[home_team_id == team_id, (home_col) := team_stats_list[[team_id]][[stat_name]]]
          games_dt[away_team_id == team_id, (away_col) := team_stats_list[[team_id]][[stat_name]]]
        }
      }
    }
  }

  games_dt
}

#' Join power index data to games data
#'
#' @param games_dt data.table, base games data
#' @param sport Character, sport name
#' @param league Character, league abbreviation
#' @return data.table with power index data joined
#' @noRd
join_power_index <- function(games_dt, sport, league) {

  if (nrow(games_dt) == 0) return(games_dt)

  # Try to get standings as power index proxy
  standings_url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/%s/%s/standings",
                          sport, league)

  standings_data <- safe_fetch(standings_url)
  power_lookup <- list()

  if (!is.null(standings_data) && !is.null(standings_data$children)) {
    # Extract win percentage as power proxy
    for (division in standings_data$children) {
      if (!is.null(division$standings) && !is.null(division$standings$entries)) {
        for (entry in division$standings$entries) {
          team_id <- pluck_safe(entry, c("team", "id"), NA)
          wins <- as.numeric(pluck_safe(entry, c("stats", "0", "value"), 0))
          losses <- as.numeric(pluck_safe(entry, c("stats", "1", "value"), 0))

          if (!is.na(team_id) && !is.na(wins) && !is.na(losses) && (wins + losses) > 0) {
            win_pct <- wins / (wins + losses) * 100  # Convert to 0-100 scale
            power_lookup[[as.character(team_id)]] <- win_pct
          }
        }
      }
    }
  }

  # Apply power index to games
  if (length(power_lookup) > 0) {
    games_dt$home_power_index <- NA_real_
    games_dt$away_power_index <- NA_real_
    games_dt$pi_source <- "win_percentage"

    for (i in 1:nrow(games_dt)) {
      home_id <- games_dt$home_team_id[i]
      away_id <- games_dt$away_team_id[i]

      if (!is.na(home_id) && as.character(home_id) %in% names(power_lookup)) {
        games_dt$home_power_index[i] <- power_lookup[[as.character(home_id)]]
      }

      if (!is.na(away_id) && as.character(away_id) %in% names(power_lookup)) {
        games_dt$away_power_index[i] <- power_lookup[[as.character(away_id)]]
      }
    }
  } else {
    # Add placeholder columns
    games_dt$home_power_index <- NA_real_
    games_dt$away_power_index <- NA_real_
    games_dt$pi_source <- NA_character_
  }

  games_dt
}