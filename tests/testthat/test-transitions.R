trajectory_fixture <- function(id, state, start, end) {
  data.frame(subject_id = id, state_label = state,
             state_start_date = start, state_end_date = end,
             stringsAsFactors = FALSE)
}

test_that("lowercase trajectories preserve IDs and ignore input row order and seq_ordinal", {
  input <- trajectory_fixture(
    rep(c("000", "9007199254740993123"), each = 4),
    rep(c("START", "A", "B", "EXIT"), 2),
    rep(c("2020-01-01", "2020-01-01", "2020-01-08", "2020-01-15"), 2),
    rep(c("2020-01-01", "2020-01-07", "2020-01-14", "2020-01-15"), 2))
  input$seq_ordinal <- 1L
  prepared <- prepareTrajectories(input[c(8, 3, 1, 5, 2, 7, 4, 6), ], cycleLength = 7)
  expect_identical(prepared$patients$subject_id, c("000", "9007199254740993123"))
  expect_identical(prepared$data$.row_id, 1:4)
  expect_equal(prepared$data$observed_days, rep(7, 4))
  expect_s3_class(prepared$patients$index_date, "Date")
  model <- estimateTransitions(prepared)
  expect_equal(model$matrix["A", "B"], 1)
  expect_equal(model$estimates$count[model$estimates$from_state == "A" &
                                      model$estimates$to_state == "B"], 2)
  expect_equal(prepareTrajectories(trajectory_fixture(0, "A", "2020-01-01", "2020-01-01"))$data$subject_id, "0")
  expect_error(prepareTrajectories(transform(input, subject_id = 2^53)), "exact IDs")
  upper <- input
  names(upper) <- toupper(names(upper))
  expect_error(prepareTrajectories(upper), "lowercase")
})

test_that("unknown cycles, missing dates and administrative boundaries never bridge", {
  input <- trajectory_fixture(c("unknown", "unknown", "unknown", "gap", "gap", "censored"),
                              c("A", "OUT OF COHORT", "B", "A", "B", "A"),
                              c("2020-01-01", "2020-01-08", "2020-01-15",
                                "2020-01-01", "2020-01-15", "2020-01-01"),
                              c("2020-01-07", "2020-01-14", "2020-01-21",
                                "2020-01-07", "2020-01-21", "2020-01-07"))
  model <- estimateTransitions(input, cycleLength = 7)
  expect_equal(sum(model$estimates$count), 0)
  expect_true(all(is.na(model$matrix)))
  expect_equal(model$diagnostics$gap_pairs_excluded, 1)
  expect_equal(model$diagnostics$unresolved_pairs_excluded, 2)
  expect_equal(model$diagnostics$boundary_censored_subjects, 3)
})

test_that("first-cycle deaths remain entry observations and absorbing rows are structural", {
  input <- trajectory_fixture(c("first", "later", "later"), c("Death history", "A", "Death history"),
                              c("2020-01-01", "2020-01-01", "2020-01-08"),
                              c("2020-01-07", "2020-01-07", "2020-01-14"))
  prepared <- prepareTrajectories(input, 7, absorbingStates = "Death history")
  expect_identical(prepareTrajectories(prepared), prepared)
  expect_identical(prepareTrajectories(prepared, states = c("A", "Death history", "Unobserved"))$absorbingStates,
                   "Death history")
  model <- estimateTransitions(prepared)
  expect_equal(model$entryCounts$count[model$entryCounts$state == "Death history"], 1)
  expect_equal(sum(model$estimates$count), 1)
  expect_equal(model$matrix["A", "Death history"], 1)
  expect_equal(model$matrix["Death history", "Death history"], 1)
  terminal <- model$estimates[model$estimates$from_state == "Death history", ]
  expect_true(all(terminal$structural))
  expect_true(all(terminal$count == 0 & terminal$denominator == 0))
  expect_error(prepareTrajectories(input, 7, absorbingStates = "A"), "final observed")
})

test_that("cycle validation permits final truncation and costing intervals without a cycle", {
  input <- trajectory_fixture(c("one", "one", "partial"), c("A", "B", "A"),
                              c("2020-01-01", "2020-01-08", "2020-01-01"),
                              c("2020-01-07", "2020-01-10", "2020-01-02"))
  model <- estimateTransitions(input, 7)
  expect_equal(sum(model$estimates$count), 1)
  expect_equal(model$diagnostics$incomplete_final_cycles, 2)
  expect_error(estimateTransitions(prepareTrajectories(input)), "cycleLength must be supplied")
  expect_equal(sum(estimateTransitions(prepareTrajectories(input), cycleLength = 7)$estimates$count), 1)
  expect_equal(prepareTrajectories(prepareTrajectories(input), cycleLength = 7)$cycleLength, 7)
  intermediate <- input
  intermediate$state_end_date[1] <- "2020-01-03"
  expect_error(prepareTrajectories(intermediate, 7), "intermediate partial")
  misaligned <- input
  misaligned$state_start_date[2] <- "2020-01-09"
  expect_error(prepareTrajectories(misaligned, 7), "align")
  arbitrary <- trajectory_fixture("one", "A", "2020-01-01", "2020-01-10")
  expect_equal(prepareTrajectories(arbitrary)$data$observed_days, 10)
  expect_error(prepareTrajectories(arbitrary, 7), "exceed cycleLength")
  trailing <- rbind(trajectory_fixture("one", "A", "2020-01-01", "2020-01-07"),
                    trajectory_fixture("one", "EXIT", "2020-01-15", "2020-01-15"))
  continuous <- prepareTrajectories(trailing)
  expect_equal(continuous$patients$observation_end, as.Date("2020-01-14"))
  expect_error(prepareTrajectories(continuous, cycleLength = 7), "trailing gaps")
  expect_error(prepareTrajectories(trailing, cycleLength = 7), "day after")
  expect_error(prepareTrajectories(arbitrary, cycleLength = 7 + 1i), "positive integer")
  expect_error(.tmaSubjectIds(1 + 1i), "exact integer identifiers")
})

test_that("unobserved states and invalid dates, intervals and markers remain explicit", {
  input <- trajectory_fixture("one", "A", "2020-01-01", "2020-01-07")
  model <- estimateTransitions(input, 7, absorbingStates = "Dead", states = c("A", "B", "Dead"))
  expect_identical(rownames(model$matrix), c("A", "B", "Dead"))
  expect_true(all(is.na(model$matrix[c("A", "B"), ])))
  expect_equal(unname(model$matrix["Dead", ]), c(0, 0, 1))
  expect_error(prepareTrajectories(rbind(input, input)), "Duplicate")
  expect_error(prepareTrajectories(transform(input, state_end_date = "2019-12-31")), "precede")
  expect_error(prepareTrajectories(transform(input, state_start_date = "2020-1-01")), "strict ISO")
  expect_error(prepareTrajectories(transform(input, state_start_date = "2020-02-30")), "invalid dates")
  overlap <- rbind(input, trajectory_fixture("one", "B", "2020-01-07", "2020-01-14"))
  expect_error(prepareTrajectories(overlap), "overlap")
  wrongExit <- rbind(input, trajectory_fixture("one", "EXIT", "2020-01-07", "2020-01-07"))
  expect_error(prepareTrajectories(wrongExit), "must follow")
  multiple <- rbind(input, trajectory_fixture(rep("one", 2), rep("START", 2),
                                             c("2020-01-01", "2019-12-31"),
                                             c("2020-01-01", "2019-12-31")))
  expect_error(prepareTrajectories(multiple), "one target episode")
})
