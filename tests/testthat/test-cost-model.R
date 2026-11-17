cost_model_fixture <- function(currencies = "EUR", terminalMap = NULL) {
  history <- data.frame(subject_id = c("one", "two"), state_label = "Alive",
    state_start_date = as.Date("2025-01-01"), state_end_date = as.Date("2025-01-07"),
    age_band = c("young", "older"))
  prepared <- prepareTrajectories(history, states = c("Alive", "Dead"), absorbingStates = "Dead")
  events <- data.frame(subject_id = c("one", "one", "two", "two"),
    event_date = as.Date("2025-01-01") + c(0, 3, 0, 3), amount = c(30, 80, 90, 160))
  events <- events[rep(seq_len(nrow(events)), times = length(currencies)), ]
  events$currency_concept_id <- rep(currencies, each = 4)
  estimateStateCosts(prepared, events, strata = "age_band",
    periodBreaks = c(0, 3, Inf), periodLabels = c("early", "later"),
    terminalCostMap = terminalMap)
}

cost_model_population <- function(age = c("young", "older"), day = 0) {
  data.frame(simulation_id = paste0("sim_", seq_along(age)), state_label = "Alive",
    cycle = day / 7, elapsed_day = day, age_band = age, stratum_id = "deliberately-unrelated")
}

test_that("daily models retain selected aggregates and integrate full labelled intervals", {
  costs <- cost_model_fixture()
  costs$metadata$cost_scope <- "selected_visits"
  costs$metadata$source_visit_filter <- list(visit_concept_id = 9201L)
  model <- createCostModel(costs, "day", "EUR", 2025, "none")
  expect_s3_class(model, "tma_cost_model")
  expect_equal(model$rates$unit_cost, model$rates$mean_cost_per_day)
  expect_identical(model$metadata$strata, "age_band")
  expect_identical(model$metadata$source_currency_concept_id, "EUR")
  expect_equal(model$metadata$source_visit_filter, list(visit_concept_id = 9201L))
  expect_equal(model$metadata$cost_scope, "selected_visits")
  expect_false(any(c("subject_id", "stratum_id", "intervalCosts", "unallocatedCosts") %in%
                     c(names(model), names(model$rates))))
  population <- cost_model_population()
  ledger <- .tmaModelCostCharge(model, population, cycleLength = 7)
  expect_equal(ledger$cost_period, rep(c("early", "later"), 2))
  expect_equal(ledger$quantity, rep(c(3, 4), 2))
  expect_equal(ledger$amount, c(30, 80, 90, 160))
  expect_equal(ledger$age_band, rep(c("young", "older"), each = 2))
  expect_true(all(ledger$component == "healthcare" & ledger$type == "estimated"))
  later <- .tmaModelCostCharge(model, cost_model_population("older", 7), cycleLength = 7)
  expect_equal(later$amount, 280)
  expect_equal(later$unit_cost, 40)
  expect_equal(later$quantity, 7)
})

test_that("currency selection never pools known or unknown source currencies", {
  costs <- cost_model_fixture(c("EUR", "USD", NA_character_, "0", ""))
  expect_error(createCostModel(costs, "day", "EUR", 2025, "none"), "currencyConceptId")
  selected <- createCostModel(costs, "day", "EUR", 2025, "none", currencyConceptId = "EUR")
  expect_true(all(selected$rates$currency_concept_id == "EUR"))
  unknown <- createCostModel(costs, "day", "asserted EUR", 2025, "none",
                              currencyConceptId = NA_character_)
  expect_true(all(is.na(unknown$rates$currency_concept_id)))
  zero <- createCostModel(costs, "day", "asserted EUR", 2025, "none", currencyConceptId = "0")
  expect_true(all(zero$rates$currency_concept_id == "0"))
  expect_error(createCostModel(costs, "day", "EUR", 2025, "none",
                                currencyConceptId = "missing"), "No estimates")
  expect_error(createCostModel(cost_model_fixture(NA_character_), "day", "EUR", 2025, "none"),
                 "currencyConceptId")
  costs$estimates$mean_cost_per_day[costs$estimates$currency_concept_id %in% "USD"] <- NA_real_
  expect_s3_class(createCostModel(costs, "day", "EUR", 2025, "none", currencyConceptId = "EUR"),
                   "tma_cost_model")
  expect_error(createCostModel(costs, "day", "USD", 2025, "none", currencyConceptId = "USD"),
                 "finite")
})

test_that("cycle models require explicit lengths and aligned cost intervals", {
  costs <- cost_model_fixture()
  expect_error(createCostModel(costs, "day", "EUR", 2025, "none", cycleLength = 7), "Do not supply")
  expect_error(createCostModel(costs, "cycle", "EUR", 2025, "none"), "cycleLength")
  expect_error(createCostModel(costs, "cycle", "EUR", 2025, "none", cycleLength = 7), "align")
  model <- createCostModel(costs, "cycle", "EUR", 2025, "none", cycleLength = 3)
  expect_equal(model$rates$unit_cost, model$rates$mean_cost_per_day * 3)
  ledger <- .tmaModelCostCharge(model, cost_model_population("young"), cycleLength = 3)
  expect_equal(ledger$amount, 30)
  expect_equal(ledger$quantity, 1)
  expect_error(.tmaModelCostCharge(model, cost_model_population("young", 1), cycleLength = 3),
                 "cross a cost-period")
  expect_error(.tmaModelCostCharge(model, cost_model_population(), cycleLength = 7), "cycleLength")
})

test_that("lookup uses cost covariate values and reports absent cells without fallback", {
  model <- createCostModel(cost_model_fixture(), "day", "EUR", 2025, "none")
  population <- cost_model_population(c("older", "young"), 7)
  population$age_band <- factor(population$age_band)
  ledger <- .tmaModelCostCharge(model, population, cycleLength = 7)
  expect_equal(ledger$amount, c(280, 140))
  population$age_band <- c("unfitted", "young")
  expect_error(.tmaModelCostCharge(model, population, cycleLength = 7),
                 "state 'Alive'.*period 'later'.*age_band=unfitted")
  population <- cost_model_population("young", 7)
  population$state_label <- "Dead"
  expect_error(.tmaModelCostCharge(model, population, cycleLength = 7), "state 'Dead'.*later")
  population$age_band <- NULL
  expect_error(.tmaModelCostCharge(model, population, cycleLength = 7), "age_band")
})

test_that("terminal policy requires observed entry and respects the fitted mapping", {
  model <- createCostModel(cost_model_fixture(terminalMap = c(Dead = "Alive")),
                            "day", "EUR", 2025, "entry")
  previous <- cost_model_population(c("young", "older"))
  previous$state_label[2] <- "Dead"
  initial <- .tmaModelCostCharge(model, previous, terminalStates = "Dead", cycleLength = 7)
  expect_equal(unique(initial$simulation_id), "sim_1")
  current <- previous[2:1, ]
  current$state_label <- "Dead"
  current$cycle <- 1
  current$elapsed_day <- 7
  entry <- .tmaModelCostCharge(model, current, previous, "Dead", 7)
  expect_equal(entry$simulation_id, "sim_1")
  expect_equal(entry$amount, 140)
  expect_equal(entry$state_label, "Dead")
  expect_equal(entry$cost_state, "Alive")
  expect_equal(nrow(.tmaModelCostCharge(model, current, current, "Dead", 7)), 0)
  model$metadata$terminal <- "none"
  expect_equal(nrow(.tmaModelCostCharge(model, current, previous, "Dead", 7)), 0)
  expect_error(.tmaModelCostCharge(model, current, previous, cycleLength = 7), "terminalStates")
})

test_that("invalid rates, saved definitions and incompatible bundles fail explicitly", {
  costs <- cost_model_fixture()
  expect_error(createCostModel(costs, currency = "EUR", priceYear = 2025, terminal = "none"), "explicit")
  duplicate <- costs
  duplicate$estimates <- rbind(duplicate$estimates, duplicate$estimates[1, ])
  expect_error(createCostModel(duplicate, "day", "EUR", 2025, "none"), "duplicate")
  invalid <- costs
  invalid$estimates$observed_days[1] <- 0
  expect_error(createCostModel(invalid, "day", "EUR", 2025, "none"), "positive observed_days")
  invalid <- costs
  invalid$metadata$strata <- "component"
  invalid$estimates$component <- "care"
  expect_error(createCostModel(invalid, "day", "EUR", 2025, "none"), "ledger fields")
  model <- createCostModel(costs, "day", "EUR", 2025, "none")
  model$rates$unit_cost[1] <- 999
  expect_error(.tmaValidateCostModel(model), "unit_cost")
  model <- createCostModel(costs, "day", "EUR", 2025, "none")
  bundle <- list(metadata = list(states = c("Alive", "Dead"), absorbingStates = "Dead",
                                  cycleLength = 7, strata = character()))
  expect_s3_class(.tmaValidateCostModel(model, bundle), "tma_cost_model")
  bundle$metadata$states <- "Other"
  expect_error(.tmaValidateCostModel(model, bundle), "Cost states")
  model$rates$mean_cost_per_day <- 1e308
  model$rates$unit_cost <- 1e308
  expect_error(.tmaModelCostCharge(model, cost_model_population(), cycleLength = 7), "finite numeric")
})
