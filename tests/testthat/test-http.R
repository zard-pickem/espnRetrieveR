test_that("map_league_to_sport works correctly", {
  expect_equal(map_league_to_sport("nfl"), "football")
  expect_equal(map_league_to_sport("nba"), "basketball")
  expect_equal(map_league_to_sport("nhl"), "hockey")
  expect_equal(map_league_to_sport("mlb"), "baseball")
  expect_error(map_league_to_sport("xyz"), "Unsupported league")
})

test_that("format_date_for_api handles different formats", {
  # Test valid formats
  expect_equal(format_date_for_api("2025-08-25"), "20250825")
  expect_equal(format_date_for_api("20250825"), "20250825")
  expect_equal(format_date_for_api(as.Date("2025-08-25")), "20250825")

  # Test invalid formats
  expect_error(format_date_for_api("25/08/2025"), "Invalid date format")
  expect_error(format_date_for_api("2025-8-25"), "Invalid date format")
})

test_that("detect_nfl_season_type works correctly", {
  # Test various dates
  expect_equal(detect_nfl_season_type(as.Date("2024-08-15")), 1L)  # Preseason
  expect_equal(detect_nfl_season_type(as.Date("2024-10-15")), 2L)  # Regular season
  expect_equal(detect_nfl_season_type(as.Date("2024-01-15")), 3L)  # Postseason
  expect_equal(detect_nfl_season_type(as.Date("2024-06-15")), 2L)  # Off-season default
})

test_that("get_nfl_season_type graceful fallback works", {
  season_type <- get_nfl_season_type(NULL, as.Date("2024-10-15"))
  expect_equal(season_type, 2L)

  season_type <- get_nfl_season_type(3L, as.Date("2024-10-15"))
  expect_equal(season_type, 3L)
})

test_that("build_scoreboard_url constructs URLs correctly", {
  # Basic URL
  url <- build_scoreboard_url("football", "nfl")
  expect_true(grepl("site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard", url))

  # URL with dates parameter
  url <- build_scoreboard_url("football", "nfl", dates = "20250825")
  expect_true(grepl("dates=20250825", url))

  # URL with multiple parameters
  url <- build_scoreboard_url("football", "nfl", dates = "20250825", limit = 50)
  expect_true(grepl("dates=20250825", url))
  expect_true(grepl("limit=50", url))
})

test_that("build_core_url constructs URLs correctly", {
  url <- build_core_url("football", "nfl", "events/123/competitions/123/predictor")
  expected <- "https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/events/123/competitions/123/predictor"
  expect_equal(url, expected)
})