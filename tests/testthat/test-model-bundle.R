bundle_model_fixture <- function() {
  states <- c("A", "D")
  matrix <- matrix(c(0.5, 0, 0.5, 1), 2, dimnames = list(states, states))
  list(matrices = list(All = matrix),
       periods = data.frame(period = "All", start_day = 0, end_day = Inf),
       metadata = list(states = states, absorbingStates = "D", cycleLength = 7),
       diagnostics = list(preparation = list(subjects_without_start = "private_id")))
}

test_that("bundles validate models and omit patient-level training diagnostics", {
  model <- bundle_model_fixture()
  bundle <- createModelBundle(model)
  expect_s3_class(bundle, "tma_model_bundle")
  expect_null(bundle$models$s1$diagnostics)
  expect_identical(bundle$metadata$strata, character())
  expect_equal(.tmaBundleModel(bundle, data.frame(state_label = c("A", "D"))), c("s1", "s1"))
  bad <- bundle
  bad$models$s1$matrices$All["A", ] <- c(0.7, 0.7)
  expect_error(.tmaValidateModelBundle(bad), "sum|stochastic")
})

test_that("group definitions retain raw values and use calendar birthdays", {
  groups <- list(age_group = list(column = "age", breaks = c(0, 65, 75, Inf),
                                  labels = c("younger", "middle", "older")))
  data <- data.frame(birth_date = as.Date(rep("1960-02-29", 3)),
                     state_start_date = as.Date(c("2025-02-28", "2025-03-01", "2035-03-01")))
  grouped <- addCovariateGroups(data, groups, dateColumn = "state_start_date")
  expect_equal(grouped$age, c(64, 65, 75))
  expect_equal(grouped$age_group, c("younger", "middle", "older"))
  expect_identical(grouped$birth_date, data$birth_date)
  expect_error(addCovariateGroups(data.frame(age = NA_real_), groups), "finite")
  expect_error(addCovariateGroups(data.frame(age = -1), groups), "cover")
})

test_that("bundle selection uses complete covariate combinations without fallback", {
  model <- bundle_model_fixture()
  registry <- data.frame(stratum_id = c("s1", "s2"), age_group = c("younger", "older"),
                          risk = c("low", "high"))
  fitted <- structure(list(models = list(s1 = model, s2 = model), strata = registry,
                            metadata = list(strata = c("age_group", "risk"))),
                       class = "tma_stratified_transitions")
  groups <- list(age_group = list(column = "age", breaks = c(0, 65, Inf),
                                  labels = c("younger", "older")))
  bundle <- createModelBundle(fitted, groups = groups)
  wrongGroups <- list(age_group = list(column = "age", breaks = c(0, 65, Inf)))
  expect_error(createModelBundle(fitted, groups = wrongGroups), "Grouping labels")
  population <- data.frame(state_label = c("A", "A"), age = c(70, 30), risk = c("high", "low"))
  population <- .tmaSimulationCovariates(bundle, population)
  expect_identical(.tmaBundleModel(bundle, population), c("s2", "s1"))
  population$risk[1] <- "low"
  expect_error(.tmaBundleModel(bundle, population), "No transition model")
  population$risk[1] <- NA_character_
  expect_error(.tmaBundleModel(bundle, population), "missing|blank")
  registry$risk[1] <- NA_character_
  fitted$strata <- registry
  expect_error(createModelBundle(fitted), "missing|blank")
})

test_that("simulation age and fitted grouping use the same calendar rule", {
  model <- bundle_model_fixture()
  bundle <- createModelBundle(model)
  population <- data.frame(state_label = "A", birth_date = as.Date("1960-11-02"),
                            index_date = as.Date("2025-10-31"), elapsed_day = 0)
  expect_equal(.tmaSimulationCovariates(bundle, population)$age, 64)
  population$elapsed_day <- 7
  expect_equal(.tmaSimulationCovariates(bundle, population)$age, 65)
  population$index_date <- NULL
  expect_error(.tmaSimulationCovariates(bundle, population), "index_date")
})
