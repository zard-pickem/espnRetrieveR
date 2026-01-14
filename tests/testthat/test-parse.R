test_that("pluck_safe handles nested lists safely", {
  test_list <- list(
    a = list(
      b = list(
        c = "value"
      )
    ),
    d = "simple"
  )

  # Test successful path
  expect_equal(pluck_safe(test_list, c("a", "b", "c")), "value")
  expect_equal(pluck_safe(test_list, "d"), "simple")

  # Test missing path
  expect_equal(pluck_safe(test_list, c("a", "b", "missing")), NA)
  expect_equal(pluck_safe(test_list, "missing"), NA)

  # Test custom default
  expect_equal(pluck_safe(test_list, "missing", "default"), "default")

  # Test NULL input
  expect_equal(pluck_safe(NULL, "path"), NA)

  # Test empty path
  expect_equal(pluck_safe(test_list, character(0)), NA)

  # Test atomic vector (should return default)
  expect_equal(pluck_safe(c(1, 2, 3), "key"), NA)

  # Test NULL in middle of path
  test_list_null <- list(a = NULL)
  expect_equal(pluck_safe(test_list_null, c("a", "b")), NA)

  # Test unnamed list
  unnamed <- list(1, 2, 3)
  expect_equal(pluck_safe(unnamed, "key"), NA)

  # Test NULL result should return default
  test_null_result <- list(a = NULL)
  expect_equal(pluck_safe(test_null_result, "a"), NA)
  expect_equal(pluck_safe(test_null_result, "a", "default"), "default")
})

test_that("parse_datetime handles various inputs", {
  # Test valid ISO datetime
  dt <- parse_datetime("2025-08-25T20:00:00Z")
  expect_s3_class(dt, "POSIXct")
  expect_equal(attr(dt, "tzone"), "UTC")

  # Test valid ISO datetime without seconds
  dt <- parse_datetime("2025-08-25T20:00Z")
  expect_s3_class(dt, "POSIXct")

  # Test invalid input
  expect_true(is.na(parse_datetime("")))
  expect_true(is.na(parse_datetime(NA_character_)))
})

test_that("determine_starter_role identifies positions correctly", {
  expect_equal(determine_starter_role("Pitcher"), "pitcher")
  expect_equal(determine_starter_role("Quarterback"), "qb")
  expect_equal(determine_starter_role("QB"), "qb")
  expect_equal(determine_starter_role("Goalie"), "goalie")
  expect_equal(determine_starter_role("Center"), "Center")  # Non-starter position
  expect_equal(determine_starter_role(""), NA_character_)
  expect_equal(determine_starter_role(NA_character_), NA_character_)
})

test_that("extract_team_info handles competitor data", {
  competitor <- list(
    team = list(
      id = "123",
      displayName = "Test Team"
    ),
    score = "21"
  )

  result <- extract_team_info(competitor, "home_")
  expect_equal(result$home_team_id, "123")
  expect_equal(result$home_team, "Test Team")
  expect_equal(result$home_score, 21L)

  # Test NULL input
  result <- extract_team_info(NULL, "home_")
  expect_equal(length(result), 0)
})

test_that("parse_game_status handles competition data", {
  competition <- list(
    status = list(
      type = list(
        name = "Final",
        state = "post"
      )
    )
  )

  result <- parse_game_status(competition)
  expect_equal(result$status, "final")
  expect_equal(result$is_completed, TRUE)

  # Test pre-game status
  competition$status$type$state <- "pre"
  result <- parse_game_status(competition)
  expect_equal(result$status, "pre")
  expect_equal(result$is_completed, FALSE)

  # Test in-game status
  competition$status$type$state <- "in"
  result <- parse_game_status(competition)
  expect_equal(result$status, "in")
  expect_equal(result$is_completed, FALSE)

  # Test unknown status
  competition$status$type$state <- "weird_status"
  result <- parse_game_status(competition)
  expect_equal(result$status, "unknown")
  expect_equal(result$is_completed, FALSE)

  # Test NULL input
  result <- parse_game_status(NULL)
  expect_equal(result$status, "unknown")
  expect_equal(result$is_completed, FALSE)
})

test_that("flatten_stats processes ESPN statistics correctly", {
  stats_data <- list(
    splits = list(
      categories = list(
        list(
          stats = list(
            list(name = "points", value = 25.5),
            list(name = "rebounds", value = 8),
            list(name = "assists", value = 6.2)
          )
        ),
        list(
          stats = list(
            list(name = "steals", value = 1.8)
          )
        )
      )
    )
  )

  result <- flatten_stats(stats_data)
  expect_equal(result[["points"]], 25.5)
  expect_equal(result[["rebounds"]], 8)
  expect_equal(result[["assists"]], 6.2)
  expect_equal(result[["steals"]], 1.8)

  # Test empty/invalid input
  expect_equal(length(flatten_stats(NULL)), 0)
  expect_equal(length(flatten_stats(list())), 0)
})

test_that("extract_starter_from_competitor handles various inputs", {
  # Test with valid probables data
  competitor <- list(
    probables = list(
      list(
        athlete = list(
          id = "12345",
          displayName = "Test Pitcher",
          position = list(
            name = "Pitcher"
          )
        )
      )
    )
  )

  result <- extract_starter_from_competitor(competitor)
  expect_equal(result$starter_id, "12345")
  expect_equal(result$starter_name, "Test Pitcher")
  expect_equal(result$starter_role, "pitcher")

  # Test with NULL competitor
  result <- extract_starter_from_competitor(NULL)
  expect_true(is.na(result$starter_id))
  expect_true(is.na(result$starter_name))
  expect_true(is.na(result$starter_role))

  # Test with empty probables
  competitor_empty <- list(probables = list())
  result <- extract_starter_from_competitor(competitor_empty)
  expect_true(is.na(result$starter_id))

  # Test with no probables field
  competitor_no_prob <- list(team = list(id = "123"))
  result <- extract_starter_from_competitor(competitor_no_prob)
  expect_true(is.na(result$starter_id))
})

test_that("parse_datetime handles all format variations", {
  # Test full ISO format with seconds and Z
  dt1 <- parse_datetime("2025-08-25T20:00:00Z")
  expect_s3_class(dt1, "POSIXct")
  expect_equal(attr(dt1, "tzone"), "UTC")

  # Test ISO format without seconds
  dt2 <- parse_datetime("2025-08-25T20:00Z")
  expect_s3_class(dt2, "POSIXct")

  # Test ISO format without Z suffix
  dt3 <- parse_datetime("2025-08-25T20:00:00")
  expect_s3_class(dt3, "POSIXct")

  # Test with milliseconds
  dt4 <- parse_datetime("2025-08-25T20:00:00.000Z")
  expect_s3_class(dt4, "POSIXct")

  # Test edge cases
  expect_true(is.na(parse_datetime("")))
  expect_true(is.na(parse_datetime(NA_character_)))
  expect_true(is.na(parse_datetime("invalid-date")))
})

test_that("extract_team_info handles edge cases", {
  # Test with missing score
  competitor <- list(
    team = list(
      id = "123",
      displayName = "Test Team"
    )
  )

  result <- extract_team_info(competitor, "away_")
  expect_equal(result$away_team_id, "123")
  expect_equal(result$away_team, "Test Team")
  expect_true(is.na(result$away_score))

  # Test with score as character
  competitor$score = "42"
  result <- extract_team_info(competitor, "home_")
  expect_equal(result$home_score, 42L)

  # Test with NULL team
  competitor_no_team <- list(score = "10")
  result <- extract_team_info(competitor_no_team, "home_")
  expect_equal(result$home_team_id, "")
})