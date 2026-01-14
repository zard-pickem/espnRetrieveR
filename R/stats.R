#' @import data.table
NULL

#' Get team season statistics
#'
#' @param team_id Character, ESPN team ID
#' @param season Integer, season year (e.g., 2025)
#' @param league Character, league abbreviation (nfl, nba, nhl, mlb)
#' @param sport Character, sport name (optional, will be inferred from league)
#' @param season_type Integer, season type (1=preseason, 2=regular, 3=postseason).
#'   Required for NFL, optional for others.
#' @return data.table with team season statistics (one row per team)
#' @export
#' @examples
#' \dontrun{
#' # Get NBA team stats
#' lakers_stats <- get_team_stats("1610612747", 2025, "nba")
#'
#' # Get NFL team stats (requires season_type)
#' patriots_stats <- get_team_stats("17", 2025, "nfl", season_type = 2)
#' }
get_team_stats <- function(team_id, season, league, sport = NULL, season_type = NULL) {

  # Validate inputs
  if (missing(team_id) || missing(season) || missing(league)) {
    stop("team_id, season, and league are required", call. = FALSE)
  }

  if (is.null(sport)) {
    sport <- map_league_to_sport(league)
  }

  # Handle NFL season_type with graceful fallbacks
  if (league == "nfl") {
    season_type <- get_nfl_season_type(season_type)
    if (is.null(season_type)) {
      warning("Could not determine NFL season type, attempting without season_type",
              call. = FALSE)
    }
  }

  # Build URL
  if (!is.null(season_type)) {
    stats_url <- build_core_url(sport, league,
                               sprintf("seasons/%d/types/%d/teams/%s/statistics",
                                      season, season_type, team_id))
  } else {
    stats_url <- build_core_url(sport, league,
                               sprintf("seasons/%d/teams/%s/statistics",
                                      season, team_id))
  }

  # Fetch statistics
  stats_data <- fetch_json(stats_url)

  # Parse statistics into data.table
  stats_dt <- parse_team_stats(stats_data, team_id, season, season_type, league, sport)

  stats_dt
}

#' Get player season statistics
#'
#' @param athlete_id Character, ESPN athlete ID
#' @param season Integer, season year (e.g., 2025)
#' @param league Character, league abbreviation (nfl, nba, nhl, mlb)
#' @param sport Character, sport name (optional, will be inferred from league)
#' @param season_type Integer, season type (1=preseason, 2=regular, 3=postseason).
#'   Required for NFL, optional for others.
#' @return data.table with player season statistics (one row per player)
#' @export
#' @examples
#' \dontrun{
#' # Get NBA player stats
#' lebron_stats <- get_player_stats("1966", 2025, "nba")
#'
#' # Get NFL player stats (requires season_type)
#' mahomes_stats <- get_player_stats("3139477", 2025, "nfl", season_type = 2)
#' }
get_player_stats <- function(athlete_id, season, league, sport = NULL, season_type = NULL) {

  # Validate inputs
  if (missing(athlete_id) || missing(season) || missing(league)) {
    stop("athlete_id, season, and league are required", call. = FALSE)
  }

  if (is.null(sport)) {
    sport <- map_league_to_sport(league)
  }

  # Handle NFL season_type with graceful fallbacks
  if (league == "nfl") {
    season_type <- get_nfl_season_type(season_type)
    if (is.null(season_type)) {
      warning("Could not determine NFL season type, attempting without season_type",
              call. = FALSE)
    }
  }

  # Build URL
  if (!is.null(season_type)) {
    stats_url <- build_core_url(sport, league,
                               sprintf("seasons/%d/types/%d/athletes/%s/statistics",
                                      season, season_type, athlete_id))
  } else {
    stats_url <- build_core_url(sport, league,
                               sprintf("seasons/%d/athletes/%s/statistics",
                                      season, athlete_id))
  }

  # Fetch statistics
  stats_data <- fetch_json(stats_url)

  # Parse statistics into data.table
  stats_dt <- parse_player_stats(stats_data, athlete_id, season, season_type, league, sport)

  stats_dt
}

#' Parse team statistics into data.table
#'
#' @param stats_data List, raw statistics data from ESPN API
#' @param team_id Character, team ID
#' @param season Integer, season year
#' @param season_type Integer, season type (can be NULL)
#' @param league Character, league abbreviation
#' @param sport Character, sport name
#' @return data.table with parsed team statistics
#' @noRd
parse_team_stats <- function(stats_data, team_id, season, season_type, league, sport) {

  # Extract basic team information
  team_name <- pluck_safe(stats_data, c("team", "displayName"), NA)

  # Flatten statistics
  flat_stats <- flatten_stats(stats_data)

  if (length(flat_stats) == 0) {
    warning(sprintf("No statistics found for team %s", team_id), call. = FALSE)
    flat_stats <- list()
  }

  # Create base row with metadata
  base_row <- list(
    team_id = team_id,
    team = team_name,
    season = season,
    season_type = season_type,
    league = league,
    sport = sport
  )

  # Combine with statistics
  stats_row <- c(base_row, as.list(flat_stats))

  # Convert to data.table
  stats_dt <- data.table::as.data.table(stats_row)

  stats_dt
}

#' Parse player statistics into data.table
#'
#' @param stats_data List, raw statistics data from ESPN API
#' @param athlete_id Character, athlete ID
#' @param season Integer, season year
#' @param season_type Integer, season type (can be NULL)
#' @param league Character, league abbreviation
#' @param sport Character, sport name
#' @return data.table with parsed player statistics
#' @noRd
parse_player_stats <- function(stats_data, athlete_id, season, season_type, league, sport) {

  # Extract basic player information
  athlete_name <- pluck_safe(stats_data, c("athlete", "displayName"), NA)
  team_id <- pluck_safe(stats_data, c("team", "id"), NA)

  # Flatten statistics
  flat_stats <- flatten_stats(stats_data)

  if (length(flat_stats) == 0) {
    warning(sprintf("No statistics found for athlete %s", athlete_id), call. = FALSE)
    flat_stats <- list()
  }

  # Create base row with metadata
  base_row <- list(
    athlete_id = athlete_id,
    athlete = athlete_name,
    team_id = team_id,
    season = season,
    season_type = season_type,
    league = league,
    sport = sport
  )

  # Combine with statistics
  stats_row <- c(base_row, as.list(flat_stats))

  # Convert to data.table
  stats_dt <- data.table::as.data.table(stats_row)

  stats_dt
}

#' Create empty team statistics data.table
#'
#' @return Empty data.table with team stats structure
#' @noRd
create_empty_team_stats_dt <- function() {
  data.table::data.table(
    team_id = character(0),
    team = character(0),
    season = integer(0),
    season_type = integer(0),
    league = character(0),
    sport = character(0)
  )
}

#' Create empty player statistics data.table
#'
#' @return Empty data.table with player stats structure
#' @noRd
create_empty_player_stats_dt <- function() {
  data.table::data.table(
    athlete_id = character(0),
    athlete = character(0),
    team_id = character(0),
    season = integer(0),
    season_type = integer(0),
    league = character(0),
    sport = character(0)
  )
}