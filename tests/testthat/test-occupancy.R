occupancy_fixture <- function() {
  data.frame(
    subject_id = c("dead", "gap", "gap", "living", "living",
                   "unknown", "unknown", "unknown", "delayed", "delayed"),
    state_label = c("D", "A", "A", "A", "A", "OUT OF COHORT", "A", "D", "START", "A"),
    state_start_date = as.Date("2020-01-01") + c(0, 0, 14, 0, 7, 0, 7, 14, 0, 7),
    state_end_date = as.Date("2020-01-01") + c(0, 6, 20, 6, 9, 6, 13, 14, 0, 13)
  )
}

test_that("occupancy separates recorded states, absorbing carry and missing follow-up", {
  input <- occupancy_fixture()
  result <- summariseOccupancy(input, nCycles = 3, cycleLength = 7,
                               absorbingStates = "D", states = c("A", "D", "U"))
  coverage <- result$coverage
  expect_equal(coverage$observed_known, c(3, 3, 2, 0))
  expect_equal(coverage$unresolved, c(1, 0, 0, 0))
  expect_equal(coverage$carried_absorbing, c(0, 1, 1, 2))
  expect_equal(coverage$missing_within_observation, c(1, 1, 0, 0))
  expect_equal(coverage$censored, c(0, 0, 2, 3))
  expect_equal(coverage$partial_cycle_patients, c(1, 1, 1, 0))
  expect_equal(coverage$denominator, c(3, 4, 3, 2))
  expect_equal(rowSums(coverage[c("observed_known", "unresolved", "carried_absorbing",
                                 "missing_within_observation", "censored")]), rep(5, 4))
  deaths <- subset(result$occupancy, state == "D")
  expect_equal(deaths$known_count, c(1, 1, 2, 2))
  expect_equal(deaths$proportion, c(1 / 3, 1 / 4, 2 / 3, 1))
  expect_equal(deaths$cohort_fraction, c(1, 1, 2, 2) / 5)
  expect_equal(subset(result$occupancy, state == "U")$known_count, rep(0, 4))
  expect_equal(summariseOccupancy(input, cycleLength = 7)$metadata$nCycles, 2)

  living <- input[input$subject_id == "living", ]
  tail <- summariseOccupancy(living, nCycles = 2, cycleLength = 7)$occupancy
  expect_equal(tail$denominator, c(1, 1, 0))
  expect_true(is.na(tail$proportion[3]))
  unresolved <- input[input$state_label == "OUT OF COHORT", ]
  empty <- summariseOccupancy(unresolved, nCycles = 1, cycleLength = 7)
  expect_equal(nrow(empty$occupancy), 0)
  expect_equal(empty$coverage$denominator, c(0, 0))
  expect_equal(empty$coverage$censored, c(0, 1))
  expect_error(summariseOccupancy(living), "cycleLength")
})

test_that("comparison keeps a fixed known-entry population and reports exclusions", {
  prepared <- prepareTrajectories(occupancy_fixture(), cycleLength = 7,
                                   absorbingStates = "D", states = c("A", "D", "U"))
  model <- estimateTransitions(prepared)
  expect_warning(projection <- projectCohort(model, 3), "known|entry|exclude")
  expect_warning(result <- compareOccupancy(prepared, projection), "Excluding 2")
  expect_equal(result$coverage$cohort_patients, rep(3, 4))
  expect_equal(result$coverage$denominator, c(3, 2, 2, 1))
  expect_equal(result$diagnostics$excluded_initial_subjects, 2)
  expect_true(result$diagnostics$initial_distribution_matches)
  expect_true(result$diagnostics$source_subject_count_matches)
  expect_true(result$diagnostics$initial_subject_count_matches)
  expect_false(result$diagnostics$count_scales_match)
  expect_equal(subset(result$comparison, state == "D")$known_count, rep(1, 4))
  expect_warning(all <- compareOccupancy(prepared, projection, population = "all"), "may enter later")
  expect_equal(all$coverage$denominator, c(3, 4, 3, 2))
  expect_equal(subset(all$comparison, state == "D")$known_count, c(1, 1, 2, 2))

  supplied <- projectCohort(model, 3, initial = c(A = 1))
  expect_equal(supplied$metadata$source_n_subjects, 5)
  expect_null(supplied$metadata$initial_denominator)
  warnings <- character()
  different <- withCallingHandlers(compareOccupancy(prepared, supplied), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_false(different$diagnostics$initial_distribution_matches)
  expect_true(any(grepl("starting assumption", warnings)))
})

test_that("complete follow-up agrees with hand propagation and invalid comparisons fail", {
  input <- data.frame(
    subject_id = c("one", "one", "two", "two", "two"),
    state_label = c("A", "D", "A", "A", "A"),
    state_start_date = as.Date("2020-01-01") + c(0, 7, 0, 7, 14),
    state_end_date = as.Date("2020-01-01") + c(6, 7, 6, 13, 20)
  )
  prepared <- prepareTrajectories(input, cycleLength = 7, absorbingStates = "D")
  model <- estimateTransitions(prepared, periodBreaks = c(0, 7, Inf))
  projection <- projectCohort(model, 2, cohortSize = 2)
  result <- compareOccupancy(prepared, projection)
  expect_equal(result$comparison$observed_proportion, result$comparison$projected_probability)
  expect_equal(result$comparison$known_count, result$comparison$projected_expected_count)
  expect_equal(subset(result$comparison, state == "A")$observed_proportion, c(1, 0.5, 0.5))
  expect_true(result$diagnostics$count_scales_match)

  wrongRoles <- prepareTrajectories(input, cycleLength = 7)
  expect_error(compareOccupancy(wrongRoles, projection), "absorbingStates")
  bad <- projection
  bad$metadata$cycleLength <- 1
  bad$occupancy$elapsed_day <- bad$occupancy$cycle
  expect_error(compareOccupancy(prepared, bad), "cycleLength differs")
  bad <- projection
  bad$occupancy[2, ] <- bad$occupancy[1, ]
  expect_error(compareOccupancy(prepared, bad), "exactly once")
  bad <- projection
  bad$occupancy$expected_count[1] <- 99
  expect_error(compareOccupancy(prepared, bad), "expected counts")
})
