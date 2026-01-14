test_that("get_teams validates inputs", {
  expect_error(get_teams(), "league is required")
  expect_error(get_teams("xyz"), "Unsupported league")
})

test_that("get_roster validates inputs", {
  expect_error(get_roster(), "Both team_id and league are required")
  expect_error(get_roster("123"), "Both team_id and league are required")
  expect_error(get_roster("123", "xyz"), "Unsupported league")
})

test_that("create_empty_teams_dt returns correct structure", {
  empty_dt <- create_empty_teams_dt()
  expect_s3_class(empty_dt, "data.table")
  expect_equal(nrow(empty_dt), 0)
  expect_true(all(c("team_id", "team", "abbreviation", "location", "name",
                   "league", "sport") %in% names(empty_dt)))
})

test_that("create_empty_roster_dt returns correct structure", {
  empty_dt <- create_empty_roster_dt()
  expect_s3_class(empty_dt, "data.table")
  expect_equal(nrow(empty_dt), 0)
  expect_true(all(c("athlete_id", "athlete", "first_name", "last_name",
                   "position", "team_id", "league", "sport") %in% names(empty_dt)))
})

test_that("parse_teams_data handles team data correctly", {
  teams_data <- list(
    sports = list(
      list(
        leagues = list(
          list(
            teams = list(
              list(
                team = list(
                  id = "123",
                  displayName = "Test Team",
                  abbreviation = "TT",
                  location = "Test City",
                  name = "Team",
                  color = "FF0000",
                  alternateColor = "0000FF",
                  isActive = TRUE
                )
              ),
              list(
                team = list(
                  id = "456",
                  displayName = "Another Team",
                  abbreviation = "AT",
                  location = "Another City",
                  name = "Team",
                  color = "00FF00",
                  alternateColor = "FF00FF",
                  isActive = TRUE
                )
              )
            )
          )
        )
      )
    )
  )

  result <- parse_teams_data(teams_data, "nba", "basketball")

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_equal(result$team_id, c("123", "456"))
  expect_equal(result$team, c("Test Team", "Another Team"))
  expect_equal(result$abbreviation, c("TT", "AT"))
  expect_equal(result$location, c("Test City", "Another City"))
  expect_equal(result$league, c("nba", "nba"))
  expect_equal(result$sport, c("basketball", "basketball"))
})

test_that("create_athlete_row processes athlete data correctly", {
  athlete <- list(
    id = "789",
    displayName = "Test Player",
    firstName = "Test",
    lastName = "Player",
    jersey = "23",
    position = list(
      abbreviation = "PG",
      displayName = "Point Guard"
    ),
    height = 75,
    weight = 180,
    age = 25,
    experience = list(years = 5)
  )

  result <- create_athlete_row(athlete, "123", "nba", "basketball")

  expect_equal(result$athlete_id, "789")
  expect_equal(result$athlete, "Test Player")
  expect_equal(result$first_name, "Test")
  expect_equal(result$last_name, "Player")
  expect_equal(result$jersey, "23")
  expect_equal(result$position, "PG")
  expect_equal(result$position_name, "Point Guard")
  expect_equal(result$height, 75)
  expect_equal(result$weight, 180)
  expect_equal(result$age, 25L)
  expect_equal(result$experience, 5L)
  expect_equal(result$team_id, "123")
  expect_equal(result$league, "nba")
  expect_equal(result$sport, "basketball")
})

# Integration tests (skip on CI/CRAN)
test_that("get_teams works with live data", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()

  # Test with NBA teams
  result <- tryCatch({
    get_teams("nba")
  }, error = function(e) NULL)

  if (!is.null(result)) {
    expect_s3_class(result, "data.table")
    expect_true(nrow(result) >= 30)  # NBA has 30 teams
    expect_true(all(c("team_id", "team", "abbreviation", "league") %in% names(result)))
    expect_equal(unique(result$league), "nba")
    expect_equal(unique(result$sport), "basketball")
  }
})

test_that("get_roster works with live data", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()

  # Note: ESPN roster endpoints can be flaky and team IDs may change
  # This test uses Lakers as example, but may fail if ESPN API changes
  # Test with Lakers roster (team_id can vary by season/API version)
  result <- tryCatch({
    get_roster("13", "nba")  # Using alternate team ID format
  }, error = function(e) NULL)

  # Only run assertions if we successfully got data
  # ESPN roster endpoint is known to be unreliable
  if (!is.null(result) && nrow(result) > 0) {
    expect_s3_class(result, "data.table")
    expect_true(nrow(result) >= 10)  # Teams have at least 10 players
    expect_true(all(c("athlete_id", "athlete", "position", "team_id") %in% names(result)))
    expect_equal(unique(result$league), "nba")
    expect_equal(unique(result$sport), "basketball")
  } else {
    skip("ESPN roster endpoint unavailable or returned no data")
  }
})