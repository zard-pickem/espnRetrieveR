#' @import data.table
NULL

#' Get teams for a league
#'
#' @param league Character, league abbreviation (nfl, nba, nhl, mlb)
#' @param sport Character, sport name (optional, will be inferred from league)
#' @return data.table with team information
#' @export
#' @examples
#' \dontrun{
#' # Get all NBA teams
#' nba_teams <- get_teams("nba")
#'
#' # Get all NFL teams
#' nfl_teams <- get_teams("nfl")
#' }
get_teams <- function(league, sport = NULL) {

  if (missing(league)) {
    stop("league is required", call. = FALSE)
  }

  if (is.null(sport)) {
    sport <- map_league_to_sport(league)
  }

  # Build teams URL
  teams_url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/%s/%s/teams",
                      sport, league)

  # Add limit parameter to get all teams
  teams_url <- paste0(teams_url, "?limit=1000")

  # Fetch teams data (best-effort, single try)
  teams_data <- safe_fetch(teams_url)

  if (is.null(teams_data)) {
    warning(sprintf("Failed to fetch teams for league %s", league), call. = FALSE)
    return(create_empty_teams_dt())
  }

  # Parse teams data
  teams_dt <- parse_teams_data(teams_data, league, sport)

  teams_dt
}

#' Get roster for a team
#'
#' @param team_id Character, ESPN team ID
#' @param league Character, league abbreviation (nfl, nba, nhl, mlb)
#' @param sport Character, sport name (optional, will be inferred from league)
#' @return data.table with roster information
#' @export
#' @examples
#' \dontrun{
#' # Get Lakers roster
#' lakers_roster <- get_roster("1610612747", "nba")
#'
#' # Get Patriots roster
#' patriots_roster <- get_roster("17", "nfl")
#' }
get_roster <- function(team_id, league, sport = NULL) {

  if (missing(team_id) || missing(league)) {
    stop("Both team_id and league are required", call. = FALSE)
  }

  if (is.null(sport)) {
    sport <- map_league_to_sport(league)
  }

  # Build roster URL
  roster_url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/%s/%s/teams/%s/roster",
                       sport, league, team_id)

  # Fetch roster data (best-effort, single try)
  roster_data <- safe_fetch(roster_url)

  if (is.null(roster_data)) {
    warning(sprintf("Failed to fetch roster for team %s", team_id), call. = FALSE)
    return(create_empty_roster_dt())
  }

  # Parse roster data
  roster_dt <- parse_roster_data(roster_data, team_id, league, sport)

  roster_dt
}

#' Parse teams data into data.table
#'
#' @param teams_data List, raw teams data from ESPN API
#' @param league Character, league abbreviation
#' @param sport Character, sport name
#' @return data.table with parsed team data
#' @noRd
parse_teams_data <- function(teams_data, league, sport) {

  if (is.null(teams_data$sports) || length(teams_data$sports) == 0) {
    return(create_empty_teams_dt())
  }

  sport_data <- teams_data$sports[[1]]

  if (is.null(sport_data$leagues) || length(sport_data$leagues) == 0) {
    return(create_empty_teams_dt())
  }

  league_data <- sport_data$leagues[[1]]

  if (is.null(league_data$teams) || length(league_data$teams) == 0) {
    return(create_empty_teams_dt())
  }

  teams <- league_data$teams
  teams_list <- list()

  for (i in seq_along(teams)) {
    team <- teams[[i]]$team  # Teams are nested under 'team' key

    team_row <- list(
      team_id = pluck_safe(team, "id", ""),
      team = pluck_safe(team, "displayName", ""),
      abbreviation = pluck_safe(team, "abbreviation", ""),
      location = pluck_safe(team, "location", ""),
      name = pluck_safe(team, "name", ""),
      color = pluck_safe(team, "color", ""),
      alternate_color = pluck_safe(team, "alternateColor", ""),
      is_active = pluck_safe(team, "isActive", TRUE),
      league = league,
      sport = sport
    )

    teams_list[[i]] <- team_row
  }

  if (length(teams_list) == 0) {
    return(create_empty_teams_dt())
  }

  # Convert to data.table
  teams_dt <- data.table::rbindlist(teams_list, fill = TRUE)
  teams_dt
}

#' Parse roster data into data.table
#'
#' @param roster_data List, raw roster data from ESPN API
#' @param team_id Character, team ID
#' @param league Character, league abbreviation
#' @param sport Character, sport name
#' @return data.table with parsed roster data
#' @noRd
parse_roster_data <- function(roster_data, team_id, league, sport) {

  if (is.null(roster_data$athletes) || length(roster_data$athletes) == 0) {
    return(create_empty_roster_dt())
  }

  athletes <- roster_data$athletes
  roster_list <- list()

  for (i in seq_along(athletes)) {
    athlete_data <- athletes[[i]]

    # Athletes might be grouped by position
    if ("items" %in% names(athlete_data)) {
      # Position-grouped format
      position_name <- pluck_safe(athlete_data, c("position", "name"), "")

      for (j in seq_along(athlete_data$items)) {
        athlete <- athlete_data$items[[j]]

        athlete_row <- create_athlete_row(athlete, team_id, league, sport, position_name)
        roster_list[[length(roster_list) + 1]] <- athlete_row
      }
    } else {
      # Direct athlete format
      athlete_row <- create_athlete_row(athlete_data, team_id, league, sport)
      roster_list[[length(roster_list) + 1]] <- athlete_row
    }
  }

  if (length(roster_list) == 0) {
    return(create_empty_roster_dt())
  }

  # Convert to data.table
  roster_dt <- data.table::rbindlist(roster_list, fill = TRUE)
  roster_dt
}

#' Create athlete row for roster
#'
#' @param athlete List, athlete data from ESPN API
#' @param team_id Character, team ID
#' @param league Character, league abbreviation
#' @param sport Character, sport name
#' @param position_group Character, position group (optional)
#' @return List with athlete information
#' @noRd
create_athlete_row <- function(athlete, team_id, league, sport, position_group = "") {

  list(
    athlete_id = pluck_safe(athlete, "id", ""),
    athlete = pluck_safe(athlete, "displayName", ""),
    first_name = pluck_safe(athlete, "firstName", ""),
    last_name = pluck_safe(athlete, "lastName", ""),
    jersey = pluck_safe(athlete, "jersey", ""),
    position = pluck_safe(athlete, c("position", "abbreviation"), position_group),
    position_name = pluck_safe(athlete, c("position", "displayName"), position_group),
    height = pluck_safe(athlete, "height", NA_real_),
    weight = pluck_safe(athlete, "weight", NA_real_),
    age = pluck_safe(athlete, "age", NA_integer_),
    experience = pluck_safe(athlete, c("experience", "years"), NA_integer_),
    team_id = team_id,
    league = league,
    sport = sport
  )
}

#' Create empty teams data.table
#'
#' @return Empty data.table with teams structure
#' @noRd
create_empty_teams_dt <- function() {
  data.table::data.table(
    team_id = character(0),
    team = character(0),
    abbreviation = character(0),
    location = character(0),
    name = character(0),
    color = character(0),
    alternate_color = character(0),
    is_active = logical(0),
    league = character(0),
    sport = character(0)
  )
}

#' Create empty roster data.table
#'
#' @return Empty data.table with roster structure
#' @noRd
create_empty_roster_dt <- function() {
  data.table::data.table(
    athlete_id = character(0),
    athlete = character(0),
    first_name = character(0),
    last_name = character(0),
    jersey = character(0),
    position = character(0),
    position_name = character(0),
    height = numeric(0),
    weight = numeric(0),
    age = integer(0),
    experience = integer(0),
    team_id = character(0),
    league = character(0),
    sport = character(0)
  )
}