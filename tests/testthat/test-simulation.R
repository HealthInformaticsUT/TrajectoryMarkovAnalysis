simulation_model <- function(a = c(1, 0, 0), b = c(0, 1, 0), cycleLength = 7) {
  states <- c("A", "B", "D")
  matrix <- rbind(A = a, B = b, D = c(0, 0, 1))
  colnames(matrix) <- states
  list(matrices = list(All = matrix),
       periods = data.frame(period = "All", start_day = 0, end_day = Inf),
       metadata = list(states = states, absorbingStates = "D", cycleLength = cycleLength))
}

simulation_strata_bundle <- function(groups = NULL) {
  young <- simulation_model()
  older <- simulation_model(a = c(0, 1, 0))
  stratified <- structure(list(models = list(s1 = young, s2 = older),
    strata = data.frame(stratum_id = c("s1", "s2"), age_group = c("young", "older")),
    metadata = list(strata = "age_group")), class = "tma_stratified_transitions")
  createModelBundle(stratified, groups = groups)
}

test_that("simulation selects source periods, preserves IDs, and stops at terminal entry", {
  model <- simulation_model(a = c(0, 1, 0))
  late <- simulation_model(b = c(0, 0, 1))$matrices$All
  model$matrices <- list(Early = model$matrices$All, Late = late)
  model$periods <- data.frame(period = c("Early", "Late"),
    start_day = c(0, 7), end_day = c(7, Inf))
  bundle <- createModelBundle(model)
  input <- data.frame(subject_id = "original", state_label = "A", age = 60)
  result <- simulateTrajectories(bundle, input, nSteps = 9, terminalStates = "D", seed = 4)
  expect_s3_class(result, "tma_simulation")
  expect_equal(result$trajectories$state_label, c("A", "B", "D"))
  expect_equal(result$trajectories$elapsed_day, c(0, 7, 14))
  expect_equal(result$transitions$source_period, c("Early", "Late"))
  expect_equal(result$transitions$destination_period, c("Late", NA_character_))
  expect_equal(result$population$source_subject_id, "original")
  expect_equal(result$population$simulation_id, "sim_1")
  expect_equal(result$metadata$steps, 2)
  expect_equal(result$metadata$stop_reason, "all_terminal")
  expect_equal(input, data.frame(subject_id = "original", state_label = "A", age = 60))
  expect_identical(advanceSimulation(result, 3), result)
  expect_equal(names(result$totals), c("component", "amount"))
})

test_that("calendar age and explicit covariate updates select the next source model", {
  grouping <- list(age_group = list(column = "age", breaks = c(0, 65, Inf),
    labels = c("young", "older")))
  bundle <- simulation_strata_bundle(grouping)
  input <- data.frame(state_label = "A", index_date = as.Date("2025-01-01"),
    birth_date = as.Date("1960-01-08"))
  result <- simulateTrajectories(bundle, input, nSteps = 2, seed = 1)
  expect_equal(result$trajectories$age, c(64, 65, 65))
  expect_equal(result$trajectories$state_label, c("A", "A", "B"))
  expect_equal(result$transitions$source_stratum, c("s1", "s2"))
  expect_equal(result$transitions$destination_stratum, c("s2", "s2"))

  rawAge <- data.frame(state_label = "A", age = 64)
  unchanged <- simulateTrajectories(bundle, rawAge, nSteps = 2)
  expect_equal(unchanged$trajectories$age, rep(64, 3))
  updated <- simulateTrajectories(bundle, rawAge, nSteps = 2,
    updateCovariates = function(current, previous) data.frame(
      simulation_id = rev(current$simulation_id), age = rev(current$age + 1)))
  expect_equal(updated$trajectories$age, c(64, 65, 66))
  expect_equal(updated$trajectories$state_label, c("A", "A", "B"))
  birthday <- simulateTrajectories(bundle, input, nSteps = 1,
    updateCovariates = function(current, previous) {
      expect_false("stratum_id" %in% names(current))
      expect_equal(previous$stratum_id, "s1")
      data.frame(simulation_id = current$simulation_id, eligible = current$age_group == "older")
    })
  expect_true(birthday$population$eligible)
  expect_equal(birthday$population$stratum_id, "s2")
  expect_error(simulateTrajectories(bundle, rawAge,
    updateCovariates = function(current, previous) data.frame(
      simulation_id = current$simulation_id, age_group = "older")), "protected")
  expect_error(simulateTrajectories(bundle, input,
    updateCovariates = function(current, previous) data.frame(
      simulation_id = current$simulation_id, age = 50)), "protected")
})

test_that("initialization samples snapshots and joint profiles with unique simulation IDs", {
  bundle <- simulation_strata_bundle()
  profiles <- data.frame(subject_id = c("one", "two"), state_label = c("A", "B"),
    age_group = c("young", "older"), marker = c(11, 22))
  result <- simulateTrajectories(bundle, profiles, n = 20, replace = TRUE,
    nSteps = 0, seed = 11)
  expect_equal(nrow(result$population), 20)
  expect_equal(anyDuplicated(result$population$simulation_id), 0L)
  expect_setequal(result$population$source_subject_id, c("one", "two"))
  expect_true(all(result$population$marker == ifelse(
    result$population$source_subject_id == "one", 11, 22)))
  override <- simulateTrajectories(bundle, profiles, initial = c(D = 1), nSteps = 0)
  expect_equal(override$population$state_label, c("D", "D"))
  expect_equal(override$population$marker, c(11, 22))
  joint <- data.frame(state_label = c("A", "B"), age_group = c("young", "older"),
    probability = c(0.5, 0.5))
  draws <- simulateTrajectories(bundle, n = 40, initial = joint, nSteps = 0, seed = 3)
  expect_true(all(draws$population$state_label == ifelse(
    draws$population$age_group == "young", "A", "B")))
  expect_false("probability" %in% names(draws$population))
  expect_error(simulateTrajectories(bundle, n = 2, initial = c(A = 1)), "covariates|joint")
  expect_error(simulateTrajectories(bundle, rbind(profiles, profiles[1, ])), "one snapshot")
  expect_error(simulateTrajectories(bundle, profiles, n = 3), "replace")
  expect_error(simulateTrajectories(bundle, n = 2, initial = transform(joint,
    probability = c(0.2, 0.2))), "sum")
  expect_error(simulateTrajectories(bundle, n = 2, initial = transform(joint,
    state_label = c("A", "unknown"), probability = c(1, 0))), "known states")
})

test_that("saved private RNG gives the same random run across resume and leaves caller untouched", {
  bundle <- createModelBundle(simulation_model(a = c(0.7, 0.2, 0.1), b = c(0.2, 0.5, 0.3)))
  population <- data.frame(subject_id = seq_len(30), state_label = "A", score = 0)
  update <- function(current, previous) data.frame(simulation_id = current$simulation_id,
    score = current$score + stats::runif(nrow(current)))
  stopRule <- function(population, step) {
    stats::runif(1)
    FALSE
  }
  set.seed(761)
  originalSeed <- .Random.seed
  originalKind <- RNGkind()
  full <- simulateTrajectories(bundle, population, nSteps = 6, updateCovariates = update,
    terminalStates = "D", stopWhen = stopRule, seed = 41)
  expect_identical(.Random.seed, originalSeed)
  expect_identical(RNGkind(), originalKind)
  partial <- simulateTrajectories(bundle, population, nSteps = 2, updateCovariates = update,
    terminalStates = "D", stopWhen = stopRule, seed = 41)
  checkpoint <- partial
  continued <- advanceSimulation(partial, 4)
  expect_identical(partial, checkpoint)
  expect_identical(continued$population, full$population)
  expect_identical(continued$trajectories, full$trajectories)
  expect_identical(continued$transitions, full$transitions)
  expect_identical(continued$.state$rng, full$.state$rng)
  expect_identical(.Random.seed, originalSeed)
  expect_identical(RNGkind(), originalKind)
})

test_that("stopping checks labelled snapshots and terminal patients need no later models", {
  bundle <- createModelBundle(simulation_model())
  result <- simulateTrajectories(bundle, n = 1, initial = c(A = 1), nSteps = 5,
    stopWhen = function(population, step) step == 2)
  expect_equal(result$metadata$steps, 2)
  expect_equal(result$metadata$stop_reason, "stop_when")
  expect_equal(nrow(result$trajectories), 3)
  expect_identical(advanceSimulation(result), result)
  initialStop <- simulateTrajectories(bundle, n = 1, initial = c(A = 1), nSteps = 3,
    stopWhen = function(population, step) TRUE)
  expect_equal(initialStop$metadata$steps, 0)
  expect_error(simulateTrajectories(bundle, n = 1, initial = c(A = 1),
    stopWhen = function(population, step) NA), "stopWhen")
  selfloop <- simulateTrajectories(bundle, n = 1, initial = c(A = 1), nSteps = 3)
  expect_equal(selfloop$metadata$steps, 3)
  terminal <- simulateTrajectories(simulation_strata_bundle(),
    data.frame(state_label = "D"), nSteps = 4, terminalStates = "D")
  expect_equal(terminal$metadata$steps, 0)
  expect_equal(nrow(terminal$transitions), 0)
  expect_error(simulateTrajectories(bundle, n = 1, initial = c(A = 1), terminalStates = "A"),
    "absorbing")
})

test_that("unsupported strata and reached missing probability rows fail without fallback", {
  bundle <- simulation_strata_bundle()
  expect_error(simulateTrajectories(bundle,
    data.frame(state_label = "A", age_group = "unknown")), "supports|covariate")
  expect_error(simulateTrajectories(bundle, data.frame(state_label = "A", age_group = "young"),
    nSteps = 2,
    updateCovariates = function(current, previous) data.frame(
      simulation_id = current$simulation_id, age_group = "unknown")), "supports|covariate")
  last <- simulateTrajectories(bundle, data.frame(state_label = "A", age_group = "young"),
    nSteps = 1, updateCovariates = function(current, previous) data.frame(
      simulation_id = current$simulation_id, age_group = "unknown"))
  expect_true(is.na(last$population$stratum_id))
  expect_equal(last$metadata$unsupported_current, 1)
  expect_equal(last$transitions$destination_period, "All")
  expect_error(advanceSimulation(last), "supports|covariate")
  model <- simulation_model(a = c(0, 1, 0), b = rep(NA_real_, 3))
  bundle <- createModelBundle(model)
  first <- simulateTrajectories(bundle, n = 1, initial = c(A = 1), nSteps = 1)
  expect_equal(first$population$state_label, "B")
  expect_error(advanceSimulation(first), "Unestimated.*B.*All")
})

test_that("all possible initial profiles are validated before drawing a small sample", {
  bundle <- simulation_strata_bundle()
  joint <- data.frame(state_label = "A", age_group = c("young", "unknown"),
    probability = c(0.999, 0.001))
  expect_error(simulateTrajectories(bundle, n = 1, initial = joint, nSteps = 0, seed = 1),
    "supports|covariate")
  profiles <- joint[c("state_label", "age_group")]
  expect_error(simulateTrajectories(bundle, profiles, n = 1, nSteps = 0, seed = 1),
    "supports|covariate")
  joint$state_label[2] <- "D"
  expect_s3_class(simulateTrajectories(bundle, n = 1, initial = joint, nSteps = 0,
    terminalStates = "D", seed = 1), "tma_simulation")
})

test_that("one-state matrices retain their destination label", {
  model <- list(matrices = list(All = matrix(1, 1, 1, dimnames = list("A", "A"))),
    periods = data.frame(period = "All", start_day = 0, end_day = Inf),
    metadata = list(states = "A", absorbingStates = character(), cycleLength = 7))
  result <- simulateTrajectories(createModelBundle(model), n = 1,
    initial = c(A = 1), nSteps = 2, seed = 1)
  expect_equal(result$trajectories$state_label, rep("A", 3))
  expect_equal(result$transitions$destination_state, rep("A", 2))
})

test_that("unsupported RNG caches are rejected without discarding the caller's next draw", {
  verify <- function() {
    originalKind <- RNGkind()
    hadSeed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    originalSeed <- if (hadSeed) .Random.seed else NULL
    on.exit({
      do.call(RNGkind, as.list(originalKind))
      if (hadSeed) assign(".Random.seed", originalSeed, envir = .GlobalEnv) else {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    bundle <- createModelBundle(simulation_model())
    RNGkind(normal.kind = "Box-Muller")
    set.seed(53)
    stats::rnorm(1)
    expected <- stats::rnorm(1)
    set.seed(53)
    stats::rnorm(1)
    before <- .Random.seed
    expect_error(simulateTrajectories(bundle, n = 1, initial = c(A = 1), seed = 4),
      "Box-Muller")
    expect_identical(.Random.seed, before)
    expect_identical(stats::rnorm(1), expected)
    RNGkind(normal.kind = "Inversion")
    result <- simulateTrajectories(bundle, n = 1, initial = c(A = 1), nSteps = 0, seed = 4)
    result$.state$rng$kind[2] <- "Box-Muller"
    expect_error(advanceSimulation(result), "Box-Muller")
    before <- .Random.seed
    expect_error(simulateTrajectories(bundle, n = 1, initial = c(A = 1), seed = 4,
      updateCovariates = function(current, previous) {
        RNGkind(normal.kind = "Box-Muller")
        data.frame(simulation_id = current$simulation_id)
      }), "must not change RNGkind")
    expect_equal(RNGkind()[2], "Inversion")
    expect_identical(.Random.seed, before)
  }
  verify()
})

test_that("callback patches are keyed, protect state and time, and omit terminal IDs", {
  bundle <- createModelBundle(simulation_model())
  population <- data.frame(state_label = c("A", "B", "D"), marker = c(1, 2, 3))
  update <- function(current, previous) {
    expect_equal(current$simulation_id, c("sim_1", "sim_2"))
    data.frame(simulation_id = rev(current$simulation_id), marker = c(20, 10), added = c(2, 1))
  }
  result <- simulateTrajectories(bundle, population, nSteps = 1, terminalStates = "D",
    updateCovariates = update)
  expect_equal(result$population$marker, c(10, 20, 3))
  expect_equal(result$population$added, c(1, 2, NA))
  expect_true(all(is.na(head(result$trajectories$added, 3))))
  expect_equal(nrow(result$transitions), 2)
  expect_error(simulateTrajectories(bundle, population, updateCovariates = function(current, previous) {
    data.frame(simulation_id = current$simulation_id, cycle = current$cycle + 1)
  }), "protected")
  expect_error(simulateTrajectories(bundle, population, updateCovariates = function(current, previous) {
    data.frame(simulation_id = rep(current$simulation_id[1], 2), marker = 7)
  }), "unique")
})

test_that("resume retains charge memory and does not recharge the checkpoint", {
  bundle <- createModelBundle(simulation_model())
  rules <- list(admission = list(type = "once", amount = 10),
                stay = list(type = "recurring", amount = 2))
  first <- simulateTrajectories(bundle, n = 1, initial = c(A = 1), nSteps = 1,
    costRules = rules, currency = "EUR", priceYear = 2025, seed = 8)
  continued <- advanceSimulation(first, 1)
  full <- simulateTrajectories(bundle, n = 1, initial = c(A = 1), nSteps = 2,
    costRules = rules, currency = "EUR", priceYear = 2025, seed = 8)
  expect_identical(continued$costs, full$costs)
  expect_equal(continued$totals$amount, c(10, 6))
  expect_equal(continued$metadata$currency, "EUR")
  expect_equal(continued$metadata$priceYear, 2025)
  expect_error(simulateTrajectories(bundle, n = 1, initial = c(A = 1), costRules = rules), "currency")
})

test_that("source time alignment and finite costing are checked explicitly", {
  bundle <- createModelBundle(simulation_model())
  later <- simulateTrajectories(bundle, data.frame(state_label = "A", elapsed_day = 28),
    nSteps = 1)
  expect_equal(later$trajectories$elapsed_day, c(28, 35))
  expect_equal(later$trajectories$cycle, c(0, 1))
  expect_error(simulateTrajectories(bundle, data.frame(state_label = "A", elapsed_day = 3)),
    "align")
  expect_error(simulateTrajectories(bundle, data.frame(state_label = "A", cycle = 2^53 - 1)),
    "horizon")
  expect_error(simulateTrajectories(bundle, n = 2, initial = c(A = 1), nSteps = 0,
    costRules = list(stay = list(type = "recurring", amount = 1e308)),
    currency = "EUR", priceYear = 2025), "finite numeric")
})
