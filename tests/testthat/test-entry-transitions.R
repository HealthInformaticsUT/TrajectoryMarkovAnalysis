test_that("entry deaths occur once before ordinary cycle-zero transitions", {
  history <- data.frame(
    subject_id = c("early-death", "survivor", "survivor", "survivor"),
    state_label = c("D", "A", "B", "D"),
    state_start_date = as.Date("2020-01-01") + c(0, 0, 7, 14),
    state_end_date = as.Date("2020-01-01") + c(2, 6, 13, 14)
  )
  prepared <- prepareTrajectories(history, cycleLength = 7,
                                   absorbingStates = "D", states = c("A", "B", "D"))
  entry <- estimateEntryTransitions(prepared, entryState = "A")
  transitions <- estimateTransitions(prepared, periodBreaks = c(0, 7, Inf),
                                      periodLabels = c("First", "Later"))
  result <- projectCohort(transitions, nCycles = 2, entry = entry)
  expect_equal(subset(result$occupancy, state == "A")$probability, c(0.5, 0, 0))
  expect_equal(subset(result$occupancy, state == "B")$probability, c(0, 0.5, 0))
  expect_equal(subset(result$occupancy, state == "D")$probability, c(0.5, 0.5, 1))
  expect_equal(result$steps$source_day, c(0, 7))
  expect_equal(result$steps$period, c("First", "Later"))
  expect_setequal(result$entry$eligible_subject_ids, c("early-death", "survivor"))
  expect_equal(unname(result$entry$baseline_distribution[c("A", "B", "D")]), c(1, 0, 0))
  zero <- projectCohort(transitions, nCycles = 0, entry = entry)
  expect_equal(zero$occupancy$probability, c(0.5, 0, 0.5))
  expect_equal(nrow(zero$steps), 0L)
})

test_that("patient-specific baselines reproduce the eligible outcome marginal", {
  history <- data.frame(
    subject_id = paste0("p", 1:5), state_label = c("A", "B", "D", "B", "A"),
    state_start_date = as.Date("2020-01-01"),
    state_end_date = as.Date("2020-01-01") + c(6, 6, 1, 6, 2)
  )
  prepared <- prepareTrajectories(history, cycleLength = 7,
                                   absorbingStates = "D", states = c("A", "B", "D"))
  baselines <- data.frame(subject_id = c("p4", "p2", "p5", "p1", "p3"),
                          entry_state = c("B", "A", "B", "A", "B"))
  expect_warning(entry <- estimateEntryTransitions(prepared, entryState = baselines),
                  "[Ee]xclud|eligible")
  expect_equal(entry$baselineCounts$count[match(c("A", "B", "D"), entry$baselineCounts$state)],
                c(2, 2, 0))
  expect_true(all(entry$baselineCounts$denominator == 4))
  expect_equal(entry$outcomeCounts$count[match(c("A", "B", "D"), entry$outcomeCounts$state)],
                c(1, 2, 1))
  expect_equal(unname(entry$matrix["A", c("A", "B", "D")]), c(0.5, 0.5, 0))
  expect_equal(unname(entry$matrix["B", c("A", "B", "D")]), c(0, 0.5, 0.5))
  result <- projectCohort(estimateTransitions(prepared), nCycles = 0, entry = entry)
  expect_equal(result$occupancy$probability, c(0.25, 0.5, 0.25))
  expect_setequal(result$entry$eligible_subject_ids, paste0("p", 1:4))
})

test_that("entry exclusions remain excluded from every occupancy comparison cycle", {
  history <- data.frame(
    subject_id = c("alive", "alive", "full-dead", "partial-dead", "partial-alive",
                    "unknown", "unknown", "delayed", "delayed", "delayed"),
    state_label = c("A", "A", "D", "D", "A", "OUT OF COHORT", "D", "START", "A", "A"),
    state_start_date = as.Date("2020-01-01") + c(0, 7, 0, 0, 0, 0, 7, 0, 7, 14),
    state_end_date = as.Date("2020-01-01") + c(6, 13, 6, 1, 2, 6, 7, 0, 13, 20)
  )
  prepared <- prepareTrajectories(history, cycleLength = 7,
                                   absorbingStates = "D", states = c("A", "D"))
  expect_warning(entry <- estimateEntryTransitions(prepared, entryState = "A"),
                  "[Ee]xclud|eligible")
  expect_setequal(entry$subjects$subject_id[entry$subjects$eligible],
                  c("alive", "full-dead", "partial-dead"))
  excluded <- entry$subjects[!entry$subjects$eligible, ]
  expect_setequal(excluded$subject_id, c("partial-alive", "unknown", "delayed"))
  expect_equal(length(unique(excluded$status)), 3L)
  expect_equal(sum(entry$coverage$n), 6L)
  expect_equal(entry$diagnostics$source_subjects, 6L)
  expect_equal(entry$diagnostics$eligible_subjects, 3L)
  expect_equal(entry$diagnostics$excluded_subjects, 3L)
  projection <- projectCohort(estimateTransitions(prepared), nCycles = 2, entry = entry)
  comparison <- suppressWarnings(compareOccupancy(prepared, projection, population = "initial"))
  expect_equal(comparison$coverage$cohort_patients, rep(3, 3))
  expect_equal(comparison$coverage$denominator, c(3, 3, 2))
  expect_equal(subset(comparison$comparison, state == "D")$known_count, rep(2, 3))
  expect_equal(comparison$diagnostics$compared_subjects, 3L)
  expect_true(comparison$diagnostics$initial_distribution_matches)
})

test_that("entry baselines and projection metadata must describe compatible subjects and states", {
  history <- data.frame(
    subject_id = c("p1", "p2"), state_label = c("A", "D"),
    state_start_date = as.Date("2020-01-01"),
    state_end_date = as.Date("2020-01-01") + c(6, 1)
  )
  prepared <- prepareTrajectories(history, cycleLength = 7,
                                   absorbingStates = "D", states = c("A", "D"))
  baselines <- data.frame(subject_id = c("p1", "p2"), entry_state = "A")
  expect_error(estimateEntryTransitions(prepared, baselines[1, , drop = FALSE]),
                "subject|patient|entryState")
  expect_error(estimateEntryTransitions(prepared, rbind(baselines, baselines[1, ])),
                "unique|duplicate|subject")
  extra <- baselines
  extra$subject_id[2] <- "other"
  expect_error(estimateEntryTransitions(prepared, extra), "subject|patient|entryState")
  missing <- baselines
  missing$entry_state[2] <- NA_character_
  expect_error(estimateEntryTransitions(prepared, missing), "missing|entry_state|entryState")
  expect_error(estimateEntryTransitions(prepared, "Undefined"), "state|dictionary")
  expect_error(estimateEntryTransitions(prepared, c("A", "D")), "scalar|single|entryState")
  entry <- estimateEntryTransitions(prepared, "A")
  transitions <- estimateTransitions(prepared)
  expect_error(projectCohort(transitions, 0, initial = c(A = 1), entry = entry), "initial|entry")
  otherRoles <- estimateTransitions(history, cycleLength = 7, states = c("A", "D"))
  expect_error(projectCohort(otherRoles, 0, entry = entry), "absorbing|roles")
  oneDay <- history
  oneDay$state_end_date <- oneDay$state_start_date
  otherLength <- estimateEntryTransitions(oneDay, "A", cycleLength = 1,
                                          absorbingStates = "D", states = c("A", "D"))
  expect_error(projectCohort(transitions, 0, entry = otherLength), "cycleLength|cycle length")
})
