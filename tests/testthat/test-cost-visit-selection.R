visit_cost_database <- function(include_visit_detail = FALSE) {
  testthat::skip_if_not_installed("CDMConnector")
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  person <- "9007199254740993"
  visits <- c("9007199254741001", "9007199254741002", "30", "40", "50")
  write_table <- function(name, data, bigint_columns = character()) {
    DBI::dbWriteTable(con, name, data)
    for (column in bigint_columns) {
      DBI::dbExecute(con, paste("ALTER TABLE", name, "ALTER", column, "TYPE BIGINT"))
    }
  }
  write_table("person", data.frame(
    person_id = c(person, "2"), gender_concept_id = 0L, year_of_birth = 1970L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  ), "person_id")
  write_table("observation_period", data.frame(
    observation_period_id = 1:2, person_id = c(person, "2"),
    observation_period_start_date = as.Date("2020-01-01"),
    observation_period_end_date = as.Date("2020-12-31"), period_type_concept_id = 0L
  ), "person_id")
  write_table("visit_occurrence", data.frame(
    visit_occurrence_id = visits, person_id = c(person, person, "2", person, person),
    visit_concept_id = c(9201L, 9202L, 9201L, 9201L, 9201L),
    visit_type_concept_id = c(999L, 9201L, 999L, 999L, 999L),
    visit_start_date = as.Date("2020-01-01"), visit_end_date = as.Date("2020-01-10")
  ), c("person_id", "visit_occurrence_id"))
  write_table("drug_exposure", data.frame(
    drug_exposure_id = 1:10, person_id = c(rep(person, 9), "2"),
    drug_concept_id = 901:910, drug_type_concept_id = 0L,
    drug_exposure_start_date = as.Date(c(rep("2020-01-02", 7), "2020-01-10",
                                          "2020-01-02", "2020-01-02")),
    drug_exposure_end_date = as.Date("2020-01-10"),
    visit_occurrence_id = c(visits[1], visits[1], visits[2], visits[4], NA, "999",
                             visits[3], visits[1], visits[5], visits[3])
  ), c("person_id", "visit_occurrence_id"))
  write_table("procedure_occurrence", data.frame(
    procedure_occurrence_id = 1:2, person_id = person,
    procedure_concept_id = c(300L, 200L), procedure_type_concept_id = 0L,
    procedure_date = as.Date("2020-01-03"), visit_occurrence_id = visits[1:2]
  ), c("person_id", "visit_occurrence_id"))
  write_table("condition_occurrence", data.frame(
    condition_occurrence_id = 1:5, person_id = person,
    condition_concept_id = c(100L, 100L, 100L, 100L, 101L),
    condition_type_concept_id = 0L,
    condition_start_date = as.Date(c("2020-01-02", "2020-01-03", "2019-12-31",
                                       "2020-01-02", "2020-01-02")),
    condition_end_date = as.Date("2020-01-03"),
    visit_occurrence_id = c(visits[1], visits[1], visits[4], visits[3], visits[5])
  ), c("person_id", "visit_occurrence_id"))
  write_table("specimen", data.frame(
    specimen_id = 1L, person_id = person, specimen_concept_id = 400L,
    specimen_type_concept_id = 0L, specimen_date = as.Date("2020-01-02")
  ), "person_id")
  write_table("cost", data.frame(
    cost_id = c(101:110, 201:202, 301:303),
    cost_event_id = c(as.character(1:10), as.character(1:2), visits[c(1, 2, 4)]),
    cost_domain_id = c(rep("Drug", 10), rep("Procedure", 2), rep("Visit", 3)),
    cost_type_concept_id = 0L, currency_concept_id = 1L,
    total_charge = c(seq(10, 100, 10), 110, 120, 1000, 2000, 3000)
  ), "cost_event_id")
  if (include_visit_detail) {
    write_table("visit_detail", data.frame(
      visit_detail_id = 1:6, person_id = person,
      visit_detail_concept_id = 9202L, visit_detail_type_concept_id = 0L,
      visit_detail_start_date = as.Date(c("2020-01-02", "2020-01-03",
                                          "2020-01-02", "2020-01-10",
                                          "2020-01-02", "2020-01-02")),
      visit_detail_end_date = as.Date("2020-01-10"),
      visit_occurrence_id = c(visits[1], visits[1], visits[2], visits[1], visits[3], NA)
    ), c("person_id", "visit_occurrence_id"))
    DBI::dbAppendTable(con, "cost", data.frame(
      cost_id = c(401:406, 499L), cost_event_id = c(1:6, 999L),
      cost_domain_id = "Visit Detail", cost_type_concept_id = 0L,
      currency_concept_id = 1L, total_charge = c(40, 60, 70, 80, 90, NA, 50)
    ))
  }
  cdm <- suppressWarnings(CDMConnector::cdmFromCon(
    con, cdmSchema = "main", cdmVersion = "5.3", cdmName = "visit costs",
    .softValidation = TRUE
  ))
  history <- data.frame(
    subject_id = c(person, "2"), state_label = "A",
    state_start_date = as.Date("2020-01-01"), state_end_date = as.Date("2020-01-07")
  )
  list(con = con, cdm = cdm, history = history, visits = visits, person = person)
}

testthat::test_that("visit-detail costs use detail dates and filter by the parent visit", {
  db <- visit_cost_database(include_visit_detail = TRUE)
  on.exit(DBI::dbDisconnect(db$con, shutdown = TRUE), add = TRUE)
  all_costs <- extractCosts(db$cdm, db$history)
  details <- subset(all_costs, cost_domain_id == "Visit Detail")
  testthat::expect_setequal(details$cost_id, c("401", "402", "403", "405", "406"))
  testthat::expect_true(is.na(details$amount[details$cost_id == "406"]))

  # Parent visits, not detail concepts, determine inpatient selection.
  selected <- extractCosts(db$cdm, db$history, domains = "Visit Detail",
                            visitFilter = list(visitConceptIds = 9201L))
  testthat::expect_equal(selected$cost_id, c("401", "402"))
  testthat::expect_equal(selected$cost_event_id, c("1", "2"))
  testthat::expect_equal(selected$visit_occurrence_id, rep(db$visits[1], 2))
  testthat::expect_equal(selected$event_date, as.Date(c("2020-01-02", "2020-01-03")))
  testthat::expect_equal(selected$amount, c(40, 60))
  diagnostics <- attr(selected, "diagnostics")
  testthat::expect_equal(unname(unlist(diagnostics[c(
    "unmatched_event_references", "out_of_window", "missing_visit_links",
    "unmatched_visit_references", "visits_not_selected", "extracted_rows"
  )], use.names = FALSE)), c(1, 1, 1, 1, 1, 2))

  DBI::dbExecute(db$con, paste(
    "UPDATE visit_detail SET visit_detail_start_date = DATE '2019-12-30'",
    "WHERE visit_detail_id = 1"
  ))
  duration <- extractCosts(db$cdm, db$history, domains = "Visit Detail",
    visitFilter = list(visitConceptIds = 9201L, eventConcepts = list(Condition = 100L)),
    allocation = "duration")
  testthat::expect_equal(duration$cost_id, c("401", "402"))
  testthat::expect_equal(duration$event_date, as.Date(c("2019-12-30", "2020-01-03")))
  testthat::expect_equal(duration$event_end_date, rep(as.Date("2020-01-10"), 2))
  testthat::expect_equal(duration$amount, c(40, 60))
})

testthat::test_that("concept-linked visits include their other costs without duplication", {
  db <- visit_cost_database()
  on.exit(DBI::dbDisconnect(db$con, shutdown = TRUE), add = TRUE)
  tables <- DBI::dbListTables(db$con)
  filter <- list(eventConcepts = list(Condition = 100L, Procedure = 200L))
  costs <- extractCosts(db$cdm, db$history, visitFilter = filter)

  # Repeated conditions select a visit once. An unrelated procedure and drugs
  # in that visit are included, even though their own concepts do not match.
  testthat::expect_setequal(costs$cost_id, c("101", "102", "103", "201", "202", "301", "302"))
  testthat::expect_equal(nrow(costs), 7L)
  testthat::expect_equal(sum(costs$amount), 3290)
  testthat::expect_setequal(costs$visit_occurrence_id, db$visits[1:2])
  testthat::expect_equal(unique(costs$subject_id), db$person)
  testthat::expect_equal(attr(costs, "visit_filter")$eventConcepts,
                         list(Condition = "100", Procedure = "200"))
  testthat::expect_equal(DBI::dbListTables(db$con), tables)
})

testthat::test_that("visit filters use exact concepts and intersect filter families", {
  db <- visit_cost_database()
  on.exit(DBI::dbDisconnect(db$con, shutdown = TRUE), add = TRUE)
  by_id <- extractCosts(db$cdm, db$history, visitFilter = list(visitIds = db$visits[1:2]))
  testthat::expect_setequal(by_id$cost_id, c("101", "102", "103", "201", "202", "301", "302"))
  by_concept <- extractCosts(db$cdm, db$history,
                            visitFilter = list(visitConceptIds = 9201L))
  testthat::expect_setequal(by_concept$cost_id, c("101", "102", "104", "109", "110",
                                                "201", "301", "303"))
  combined <- extractCosts(db$cdm, db$history, visitFilter = list(
    visitIds = db$visits[1:2], visitConceptIds = c(9201L, 9201L),
    eventConcepts = list(Condition = 100L, Procedure = 200L)
  ))
  testthat::expect_setequal(combined$cost_id, c("101", "102", "201", "301"))
  testthat::expect_equal(attr(combined, "visit_filter")$visitConceptIds, "9201")
})

testthat::test_that("visit exclusions are diagnosed without changing all-cost extraction", {
  db <- visit_cost_database()
  on.exit(DBI::dbDisconnect(db$con, shutdown = TRUE), add = TRUE)
  all_costs <- extractCosts(db$cdm, db$history)
  testthat::expect_equal(nrow(all_costs), 14L)
  testthat::expect_true(all(c("105", "106", "107") %in% all_costs$cost_id))
  testthat::expect_null(attr(all_costs, "visit_filter"))
  selected <- extractCosts(db$cdm, db$history,
                           visitFilter = list(eventConcepts = list(Condition = 100L,
                                                                  Procedure = 200L)))
  drug <- subset(attr(selected, "diagnostics"), cost_domain_id == "Drug")
  testthat::expect_equal(unname(unlist(drug[c("out_of_window", "missing_visit_links",
                                            "unmatched_visit_references", "visits_not_selected",
                                            "extracted_rows")], use.names = FALSE)),
                         c(1, 1, 2, 3, 3))
})

testthat::test_that("visit filtering rejects missing links and malformed selectors", {
  db <- visit_cost_database()
  on.exit(DBI::dbDisconnect(db$con, shutdown = TRUE), add = TRUE)
  filter <- list(visitIds = db$visits[1])
  testthat::expect_error(extractCosts(db$cdm, db$history, domains = "Specimen",
                                     visitFilter = filter), "visit_occurrence_id|Specimen")
  cdm <- db$cdm
  cdm$drug_exposure <- dplyr::select(cdm$drug_exposure, -"visit_occurrence_id")
  testthat::expect_error(extractCosts(cdm, db$history, domains = "Drug",
                                     visitFilter = filter), "visit_occurrence_id")
  testthat::expect_error(extractCosts(db$cdm, db$history,
                                     visitFilter = list(visitTypeConceptIds = 9201L)),
                         "visitFilter")
  testthat::expect_error(extractCosts(db$cdm, db$history,
                                     visitFilter = list(eventConcepts = list(Unknown = 100L))),
                         "eventConcepts")
})

testthat::test_that("selected visits change cost numerators but retain observed days", {
  db <- visit_cost_database()
  on.exit(DBI::dbDisconnect(db$con, shutdown = TRUE), add = TRUE)
  filter <- list(eventConcepts = list(Condition = 100L, Procedure = 200L))
  costs <- extractCosts(db$cdm, db$history, visitFilter = filter)
  result <- estimateStateCosts(db$history, costs)
  testthat::expect_equal(result$estimates$observed_days, 14)
  testthat::expect_equal(result$estimates$total_cost, 3290)
  testthat::expect_equal(result$metadata$source_visit_filter, attr(costs, "visit_filter"))
  wrapper <- TrajectoryMarkovAnalysis(db$history, analyses = "costs", cdm = db$cdm,
                                      costVisitFilter = filter)
  testthat::expect_equal(wrapper$costs$estimates, result$estimates)
  testthat::expect_equal(wrapper$costs$metadata$source_visit_filter,
                         attr(costs, "visit_filter"))
  testthat::expect_error(TrajectoryMarkovAnalysis(db$history, analyses = "costs",
                                                 costData = costs, costVisitFilter = filter),
                         "costVisitFilter|cdm")
  testthat::expect_error(TrajectoryMarkovAnalysis(db$history, cycleLength = 7,
                                                 costVisitFilter = filter),
                         "costVisitFilter|costs|cdm")
})
