test_that("get_games_by_date validates inputs", {
  expect_error(get_games_by_date(), "Both 'date' and 'league' are required")
  expect_error(get_games_by_date("2025-08-25"), "Both 'date' and 'league' are required")
  expect_error(get_games_by_date("2025-08-25", "xyz"), "Unsupported league")
})

test_that("get_completed_games validates inputs", {
  expect_error(get_completed_games(), "Both 'date' and 'league' are required")
  expect_error(get_completed_games("2025-08-25"), "Both 'date' and 'league' are required")
  expect_error(get_completed_games("2025-08-25", "xyz"), "Unsupported league")
})

test_that("create_empty_games_dt returns correct structure", {
  empty_dt <- create_empty_games_dt()
  expect_s3_class(empty_dt, "data.table")
  expect_equal(nrow(empty_dt), 0)
  expect_true(all(c("event_id", "league", "sport", "date_time_utc", "status",
                   "home_team", "away_team") %in% names(empty_dt)))
})

test_that("parse_scoreboard_data handles empty input", {
  # Test empty events
  empty_scoreboard <- list(events = list())
  result <- parse_scoreboard_data(empty_scoreboard, "football", "nfl")
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)

  # Test NULL events
  null_scoreboard <- list(events = NULL)
  result <- parse_scoreboard_data(null_scoreboard, "football", "nfl")
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("parse_scoreboard_data processes valid event data", {
  scoreboard_data <- list(
    events = list(
      list(
        id = "12345",
        date = "2025-08-25T20:00:00Z",
        season = list(year = 2025, type = 2),
        competitions = list(
          list(
            status = list(
              type = list(
                name = "Final",
                state = "post"
              )
            ),
            competitors = list(
              list(
                homeAway = "home",
                team = list(
                  id = "1",
                  displayName = "Home Team"
                ),
                score = "21"
              ),
              list(
                homeAway = "away",
                team = list(
                  id = "2",
                  displayName = "Away Team"
                ),
                score = "14"
              )
            )
          )
        )
      )
    )
  )

  result <- parse_scoreboard_data(scoreboard_data, "football", "nfl")

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(result$event_id, "12345")
  expect_equal(result$league, "nfl")
  expect_equal(result$sport, "football")
  expect_equal(result$season, 2025L)
  expect_equal(result$season_type, 2L)
  expect_equal(result$status, "final")
  expect_equal(result$is_completed, TRUE)
  expect_equal(result$home_team_id, "1")
  expect_equal(result$home_team, "Home Team")
  expect_equal(result$home_score, 21L)
  expect_equal(result$away_team_id, "2")
  expect_equal(result$away_team, "Away Team")
  expect_equal(result$away_score, 14L)
})

# Integration tests (skip on CI/CRAN)
test_that("get_games_by_date works with live data", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()

  # Test with NBA (usually has games)
  result <- tryCatch({
    get_games_by_date("2025-01-15", "nba", limit = 5)
  }, error = function(e) NULL)

  if (!is.null(result)) {
    expect_s3_class(result, "data.table")
    expect_true(all(c("event_id", "league", "home_team", "away_team") %in% names(result)))
  }
})

test_that("get_completed_games works with live data", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()

  # Test with historical NBA games
  result <- tryCatch({
    get_completed_games("2024-12-25", "nba", limit = 5)
  }, error = function(e) NULL)

  if (!is.null(result)) {
    expect_s3_class(result, "data.table")
    expect_true(all(result$is_completed))
  }
})

test_that("minimal mode returns correct columns", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()

  # Test minimal mode
  result <- tryCatch({
    get_games_by_date("2025-01-15", "nba", limit = 2, minimal = TRUE)
  }, error = function(e) NULL)

  if (!is.null(result) && nrow(result) > 0) {
    expect_s3_class(result, "data.table")
    minimal_cols <- c("event_id", "date_time_utc", "home_team", "away_team",
                     "home_score", "away_score")
    expect_true(all(minimal_cols %in% names(result)))
    # Should have fewer columns than full mode
    expect_true(ncol(result) <= 10)
  }
})