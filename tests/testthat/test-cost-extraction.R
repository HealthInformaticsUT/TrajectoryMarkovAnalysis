cost_test_database <- function(missing_visit = FALSE, unsupported_domain = FALSE) {
  testthat::skip_if_not_installed("CDMConnector")
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  large_id <- "9007199254740993"
  DBI::dbWriteTable(con, "person", data.frame(
    person_id = c(large_id, "2", "3"), gender_concept_id = 0L,
    year_of_birth = 1970L, race_concept_id = 0L, ethnicity_concept_id = 0L
  ))
  DBI::dbExecute(con, "ALTER TABLE person ALTER person_id TYPE BIGINT")
  DBI::dbWriteTable(con, "observation_period", data.frame(
    observation_period_id = 1:3, person_id = c(large_id, "2", "3"),
    observation_period_start_date = as.Date("2020-01-01"),
    observation_period_end_date = as.Date("2020-12-31"), period_type_concept_id = 0L
  ))
  DBI::dbExecute(con, "ALTER TABLE observation_period ALTER person_id TYPE BIGINT")
  DBI::dbWriteTable(con, "drug_exposure", data.frame(
    drug_exposure_id = 1:6, person_id = c(large_id, large_id, "2", "3", "2", "2"),
    drug_concept_id = 0L, drug_type_concept_id = 0L,
    drug_exposure_end_date = as.Date("2020-01-04"),
    drug_exposure_start_date = as.Date(c("2020-01-01", "2020-01-04", "2020-01-02",
                                        "2020-01-01", "2020-01-01", NA))
  ))
  DBI::dbExecute(con, "ALTER TABLE drug_exposure ALTER person_id TYPE BIGINT")
  if (!missing_visit) {
    DBI::dbWriteTable(con, "visit_occurrence", data.frame(
      visit_occurrence_id = 1:2, person_id = c(large_id, "3"),
      visit_concept_id = 0L, visit_type_concept_id = 0L,
      visit_end_date = as.Date("2020-01-04"),
      visit_start_date = as.Date(c("2020-01-02", "2020-01-01"))
    ))
    DBI::dbExecute(con, "ALTER TABLE visit_occurrence ALTER person_id TYPE BIGINT")
  }
  costs <- data.frame(
    cost_id = c(11:16, 21:22, 99L), cost_event_id = c(1:6, 1:2, 999L),
    cost_type_concept_id = 0L,
    cost_domain_id = c(rep("Drug", 6), rep("Visit", 2), "Drug"),
    currency_concept_id = c(rep(1L, 6), 2L, 2L, 1L),
    total_charge = c(10, 20, NA, 99, 5, 6, 100, 200, 50),
    total_cost = c(1, 2, NA, 9, 0.5, 0.6, 10, 20, 5),
    total_paid = c(8, 16, NA, 80, 4, 5, 80, 160, 40)
  )
  if (unsupported_domain) {
    costs$cost_domain_id[[9]] <- "Episode"
  }
  DBI::dbWriteTable(con, "cost", costs)
  cdm <- suppressWarnings(CDMConnector::cdmFromCon(
    con, cdmSchema = "main", cdmVersion = "5.3", cdmName = "cost test",
    .softValidation = TRUE
  ))
  list(con = con, cdm = cdm)
}

cost_test_trajectories <- function() {
  structure(list(
    data = data.frame(),
    patients = data.frame(
      subject_id = c("9007199254740993", "2"),
      index_date = as.Date(c("2020-01-01", "2020-01-02")),
      observation_end = as.Date(c("2020-01-03", "2020-01-02"))
    )
  ), class = "tma_trajectories")
}

testthat::test_that("cost linkage respects domain, patient, exact window, and bigint IDs", {
  db <- cost_test_database()
  on.exit(DBI::dbDisconnect(db$con, shutdown = TRUE), add = TRUE)
  original_tables <- DBI::dbListTables(db$con)
  costs <- extractCosts(db$cdm, cost_test_trajectories())

  testthat::expect_named(costs, c("subject_id", "event_date", "amount", "cost_id",
                                 "cost_domain_id", "cost_event_id", "currency_concept_id", "measure",
                                 "visit_occurrence_id"))
  testthat::expect_equal(nrow(costs), 3L)
  testthat::expect_setequal(costs$cost_id, c("11", "13", "21"))
  testthat::expect_equal(costs$subject_id[costs$cost_id == "11"], "9007199254740993")
  testthat::expect_equal(costs$amount[costs$cost_id == "21"], 100)
  testthat::expect_true(is.na(costs$amount[costs$cost_id == "13"]))
  testthat::expect_setequal(costs$currency_concept_id, c("1", "2"))
  testthat::expect_s3_class(costs$event_date, "Date")
  testthat::expect_equal(attr(costs, "measure"), "total_charge")
  testthat::expect_true(DBI::dbIsValid(db$con))
  testthat::expect_equal(DBI::dbListTables(db$con), original_tables)

  diagnostics <- attr(costs, "diagnostics")
  drug <- diagnostics[diagnostics$cost_domain_id == "Drug", ]
  testthat::expect_equal(drug$unmatched_event_references, 1)
  testthat::expect_equal(drug$selected_patient_cost_rows, 5)
  testthat::expect_equal(drug$missing_event_dates, 1)
  testthat::expect_equal(drug$out_of_window, 2)
  testthat::expect_equal(drug$extracted_rows, 2)
  testthat::expect_equal(drug$missing_amounts, 1)
  testthat::expect_equal(attr(costs, "unmatched_diagnostics_scope"),
                         "database-wide selected domains")
})

testthat::test_that("selected domains and measures are respected", {
  db <- cost_test_database()
  on.exit(DBI::dbDisconnect(db$con, shutdown = TRUE), add = TRUE)
  costs <- extractCosts(db$cdm, cost_test_trajectories(), measure = "total_cost", domains = "Visit")
  testthat::expect_equal(nrow(costs), 1L)
  testthat::expect_equal(costs$amount, 10)
  testthat::expect_equal(costs$measure, "total_cost")
  testthat::expect_error(extractCosts(db$cdm, cost_test_trajectories(), measure = "cost_id"),
                         "measure")
  empty <- extractCosts(db$cdm, cost_test_trajectories(), domains = character())
  testthat::expect_equal(nrow(empty), 0L)
  testthat::expect_s3_class(empty$event_date, "Date")
})

testthat::test_that("duration extraction preserves full events overlapping follow-up", {
  db <- cost_test_database()
  on.exit(DBI::dbDisconnect(db$con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(db$con, paste(
    "UPDATE visit_occurrence SET visit_start_date = DATE '2019-12-30'",
    "WHERE visit_occurrence_id = 1"
  ))
  start <- extractCosts(db$cdm, cost_test_trajectories(), domains = "Visit")
  testthat::expect_equal(nrow(start), 0L)
  testthat::expect_false("event_end_date" %in% names(start))

  duration <- extractCosts(db$cdm, cost_test_trajectories(), domains = "Visit",
                           allocation = "duration")
  testthat::expect_equal(duration$cost_id, "21")
  testthat::expect_equal(duration$event_date, as.Date("2019-12-30"))
  testthat::expect_equal(duration$event_end_date, as.Date("2020-01-04"))
  testthat::expect_equal(duration$amount, 100)
  testthat::expect_equal(attr(duration, "allocation"), "duration")
  testthat::expect_equal(attr(duration, "diagnostics")$in_window_cost_rows, 1)
  history <- data.frame(subject_id = "9007199254740993", state_label = "A",
    state_start_date = as.Date("2020-01-01"), state_end_date = as.Date("2020-01-03"))
  fitted <- estimateStateCosts(history, duration)
  testthat::expect_equal(fitted$estimates$total_cost, 50)
  testthat::expect_equal(fitted$unallocatedCosts$amount, 50)
  wrapped <- TrajectoryMarkovAnalysis(history, analyses = "costs", cdm = db$cdm,
    costDomains = "Visit", costAllocation = "duration")
  testthat::expect_equal(wrapped$costs$estimates, fitted$estimates)
})

testthat::test_that("duration extraction refuses missing or reversed source dates", {
  db <- cost_test_database()
  on.exit(DBI::dbDisconnect(db$con, shutdown = TRUE), add = TRUE)
  for (dates in c(
    "visit_start_date = DATE '2020-01-02', visit_end_date = NULL",
    "visit_start_date = DATE '2020-02-01', visit_end_date = DATE '2020-01-31'",
    "visit_start_date = NULL, visit_end_date = DATE '2020-01-04'"
  )) {
    DBI::dbExecute(db$con, paste("UPDATE visit_occurrence SET", dates,
                                 "WHERE visit_occurrence_id = 1"))
    testthat::expect_error(extractCosts(db$cdm, cost_test_trajectories(), domains = "Visit",
                                       allocation = "duration"), "missing or reversed")
  }
})

testthat::test_that("patients across query chunks and chunks without costs retain correct results", {
  db <- cost_test_database()
  on.exit(DBI::dbDisconnect(db$con, shutdown = TRUE), add = TRUE)
  trajectories <- cost_test_trajectories()
  trajectories$patients <- data.frame(
    subject_id = c(as.character(4:103), "9007199254740993"),
    index_date = as.Date("2020-01-01"), observation_end = as.Date("2020-01-03")
  )
  costs <- extractCosts(db$cdm, trajectories)
  testthat::expect_setequal(costs$cost_id, c("11", "21"))
  testthat::expect_equal(sum(costs$amount), 110)
  diagnostics <- attr(costs, "diagnostics")
  testthat::expect_equal(sum(diagnostics$missing_event_dates), 0)
  testthat::expect_equal(sum(diagnostics$out_of_window), 1)
})

testthat::test_that("missing source tables fail only when their domain is selected", {
  db <- cost_test_database(missing_visit = TRUE)
  on.exit(DBI::dbDisconnect(db$con, shutdown = TRUE), add = TRUE)
  testthat::expect_error(extractCosts(db$cdm, cost_test_trajectories()), "visit_occurrence")
  testthat::expect_equal(nrow(extractCosts(db$cdm, cost_test_trajectories(), domains = "Drug")), 2L)
  testthat::expect_true(DBI::dbIsValid(db$con))
})

testthat::test_that("missing amount columns are explicit errors", {
  db <- cost_test_database()
  on.exit(DBI::dbDisconnect(db$con, shutdown = TRUE), add = TRUE)
  cdm <- db$cdm
  cdm$cost <- dplyr::select(cdm$cost, -"total_charge")
  testthat::expect_error(extractCosts(cdm, cost_test_trajectories()),
                         "missing columns: total_charge")
})

testthat::test_that("unknown observed domains require an explicit inclusion choice", {
  db <- cost_test_database(unsupported_domain = TRUE)
  on.exit(DBI::dbDisconnect(db$con, shutdown = TRUE), add = TRUE)
  testthat::expect_error(extractCosts(db$cdm, cost_test_trajectories()), "Unsupported cost domains: Episode")
  testthat::expect_error(extractCosts(db$cdm, cost_test_trajectories(), domains = "Invalid"),
                         "Unsupported cost domains: Invalid")
  testthat::expect_equal(nrow(extractCosts(db$cdm, cost_test_trajectories(), domains = "Drug")), 2L)
})

testthat::test_that("plain C2T data can be costed without cycle length", {
  db <- cost_test_database()
  on.exit(DBI::dbDisconnect(db$con, shutdown = TRUE), add = TRUE)
  trajectories <- data.frame(
    subject_id = "9007199254740993", state_label = c("START", "Alive", "EXIT"),
    state_id = 1:3, state_start_date = as.Date(c("2019-12-31", "2020-01-01", "2020-01-04")),
    state_end_date = as.Date(c("2019-12-31", "2020-01-03", "2020-01-04")),
    time_in_cohort = c(0, 1, 4), seq_ordinal = 1:3,
    age = 50, gender_concept_id = 0L
  )
  costs <- extractCosts(db$cdm, trajectories)
  testthat::expect_setequal(costs$cost_id, c("11", "21"))
})
