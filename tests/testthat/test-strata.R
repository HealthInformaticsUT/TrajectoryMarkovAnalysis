strata_history <- function() {
  data.frame(
    subject_id = c(rep("one", 4), rep("two", 3)),
    state_label = c("A", "A", "B", "Death", "A", "A", "A"),
    state_start_date = as.Date("2020-01-01") + c(0, 7, 14, 21, 0, 7, 14),
    state_end_date = as.Date("2020-01-07") + c(0, 7, 14, 21, 0, 7, 14),
    age_band = c("young", rep("old", 6)), stringsAsFactors = FALSE
  )
}

test_that("changing strata retain source-group crossings and original entries and periods", {
  history <- prepareTrajectories(strata_history(), 7, absorbingStates = "Death",
                                 states = c("A", "B", "Death", "Unobserved"))
  result <- estimateTransitions(history, strata = "age_band")
  expect_s3_class(result, "tma_stratified_transitions")
  expect_identical(names(result$models), result$strata$stratum_id)
  young <- result$models[[result$strata$stratum_id[result$strata$age_band == "young"]]]
  old <- result$models[[result$strata$stratum_id[result$strata$age_band == "old"]]]
  expect_equal(young$matrix["A", "A"], 1)
  expect_equal(sum(young$estimates$count), 1)
  expect_equal(sum(old$estimates$count), 4)
  expect_equal(sum(old$entryCounts$count), 1)
  expect_equal(sum(young$entryCounts$count), 1)
  repeated <- old$estimates[old$estimates$from_state == "A" & old$estimates$to_state == "A", ]
  expect_equal(repeated$count, 2)
  expect_equal(repeated$patients, 1)
  expect_equal(repeated$source_patients, 2)
  expect_equal(old$diagnostics$supporting_patients, 2)
  expect_true(all(is.na(old$matrix["Unobserved", ])))
  expect_equal(unname(old$matrix["Death", ]), c(0, 0, 1, 0))
  expect_true(all(old$estimates$count[old$estimates$structural] == 0))
  pooled <- estimateTransitions(history)
  expect_equal(aggregate(count ~ from_state + to_state, result$estimates, sum),
               aggregate(count ~ from_state + to_state, pooled$estimates, sum))
  periods <- estimateTransitions(history, periodBreaks = c(0, 7, Inf),
                                  periodLabels = c("first", "later"), strata = "age_band")
  oldPeriod <- periods$models[[periods$strata$stratum_id[periods$strata$age_band == "old"]]]
  expect_equal(oldPeriod$matrices$later["A", "B"], 0.5)
  expect_equal(oldPeriod$matrices$first["A", "A"], 1)
  expect_equal(TrajectoryMarkovAnalysis(history, strata = "age_band")$transitions, result)
})

test_that("stratum registries compare each covariate and preserve exact numeric categories", {
  history <- strata_history()[c(1, 2, 5, 6), ]
  history$first <- factor(c("a\rb", "a\rb", "a", "a"))
  history$second <- c("c", "c", "b\rc", "b\rc")
  result <- estimateTransitions(history, 7, strata = c("first", "second"))
  expect_equal(nrow(result$strata), 2)
  expect_identical(result$strata$stratum_id, c("s1", "s2"))
  expect_true(is.factor(result$strata$first))
  expect_equal(vapply(result$models, function(x) sum(x$estimates$count), numeric(1)), c(s1 = 1, s2 = 1))
  expect_equal(estimateTransitions(history[4:1, ], 7, strata = c("first", "second"))$strata,
               result$strata)
  history$category <- c(1, 1, 1 + 1e-12, 1 + 1e-12)
  numericGroups <- estimateTransitions(history, 7, strata = "category")
  expect_equal(nrow(numericGroups$strata), 2)
  history$category <- c(TRUE, TRUE, FALSE, FALSE)
  expect_equal(nrow(estimateTransitions(history, 7, strata = "category")$strata), 2)
})

test_that("stratified costs and days conserve totals through splitting and terminal mapping", {
  history <- data.frame(
    subject_id = c("one", "one", "two", "two"),
    state_label = c("A", "Death", "A", "A"),
    state_start_date = as.Date("2020-01-01") + c(0, 7, 0, 7),
    state_end_date = as.Date("2020-01-01") + c(6, 9, 6, 9),
    age_band = c("young", "old", "old", "old")
  )
  prepared <- prepareTrajectories(history, absorbingStates = "Death")
  events <- data.frame(subject_id = c("one", "one", "one", "two"),
                       event_date = as.Date("2020-01-01") + c(1, 7, 8, 0),
                       amount = c(10, 20, NA, 15), currency_concept_id = c("EUR", "EUR", "EUR", "USD"))
  attr(events, "cost_scope") <- "selected_visits"
  attr(events, "visit_filter") <- list(visit_concept_id = 9201)
  attr(events, "diagnostics") <- list(selected_records = 4L)
  args <- list(trajectories = prepared, costData = events,
               periodBreaks = c(0, 3, 8, Inf), terminalCostMap = c(Death = "A"))
  pooled <- do.call(estimateStateCosts, args)
  result <- do.call(estimateStateCosts, c(args, list(strata = "age_band")))
  expect_true(all(c("stratum_id", "age_band") %in% names(result$intervalCosts)))
  expect_identical(result$metadata$strata, "age_band")
  expect_equal(result$metadata$cost_scope, "selected_visits")
  expect_equal(result$metadata$source_visit_filter, attr(events, "visit_filter"))
  expect_equal(result$diagnostics$extraction, attr(events, "diagnostics"))
  expect_equal(result$unallocatedCosts, pooled$unallocatedCosts)
  measures <- c("observed_days", "total_cost", "known_recorded_cost", "cost_records",
                "missing_amounts", "mapped_terminal_days", "mapped_terminal_intervals")
  summed <- dplyr::summarise(
    dplyr::group_by(result$estimates, .data$period, .data$cost_state, .data$currency_concept_id),
    dplyr::across(dplyr::all_of(measures), sum), .groups = "drop")
  expected <- pooled$estimates[c("period", "cost_state", "currency_concept_id", measures)]
  expected <- expected[order(expected$period, expected$cost_state, expected$currency_concept_id), ]
  rownames(expected) <- NULL
  expect_equal(as.data.frame(summed), expected)
  terminal <- result$intervalCosts[result$intervalCosts$state_label == "Death", ]
  expect_true(all(terminal$age_band == "old" & terminal$cost_state == "A"))
  expect_equal(sum(terminal$observed_days[terminal$currency_concept_id == "EUR"]), 3)
  expect_true(any(duplicated(result$intervalCosts$.row_id)))
  wrapped <- TrajectoryMarkovAnalysis(prepared, analyses = "costs", costData = events,
                                       periodBreaks = c(0, 3, 8, Inf),
                                       terminalCostMap = c(Death = "A"), strata = "age_band")
  expect_equal(wrapped$costs, result)
})

test_that("invalid strata fail early and NULL preserves ordinary results", {
  history <- strata_history()
  events <- data.frame(subject_id = "one", event_date = as.Date("2020-01-02"), amount = 10)
  expect_identical(estimateTransitions(history, 7), estimateTransitions(history, 7, strata = NULL))
  expect_identical(estimateStateCosts(history, events), estimateStateCosts(history, events, strata = NULL))
  expect_false("strata" %in% names(estimateTransitions(history, 7)))
  expect_error(estimateTransitions(history, 7, strata = "missing"), "columns are missing")
  expect_error(estimateStateCosts(history, events, strata = "missing"), "columns are missing")
  expect_error(estimateTransitions(history, 7, strata = c("age_band", "age_band")), "unique")
  expect_error(estimateTransitions(history, 7, strata = "state_label"), "reserved")
  for (bad in list(c(NA, rep("old", 6)), c(" ", rep("old", 6)),
                   c(Inf, rep(1, 6)), as.list(seq_len(7)), rep(1i, 7))) {
    invalid <- history
    invalid$age_band <- bad
    expect_error(estimateTransitions(invalid, 7, strata = "age_band"), "strata column")
    expect_error(estimateStateCosts(invalid, events, strata = "age_band"), "strata column")
  }
  unknown <- history[1:3, ]
  unknown$state_label[2] <- "OUT OF COHORT"
  unknown$age_band[2] <- NA_character_
  result <- estimateTransitions(unknown, 7, strata = "age_band")
  expect_equal(sum(result$estimates$count), 0)
  expect_equal(estimateStateCosts(unknown, events, strata = "age_band")$metadata$strata, "age_band")
})

test_that("registry validation checks every row without clinical state labels", {
  registry <- data.frame(age_band = c("young", "old"))
  expect_equal(nrow(.tmaStrata(registry, "age_band")$registry), 2)
  for (bad in c(NA_character_, " ")) {
    registry$age_band[2] <- bad
    expect_error(.tmaStrata(registry, "age_band"), "missing or blank")
  }
})
