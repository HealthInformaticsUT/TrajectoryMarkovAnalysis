test_that("prices are adjusted before duration allocation without losing nominal charges", {
  events <- data.frame(subject_id = "one", cost_id = c("a", "b"),
    event_date = as.Date(c("2020-12-30", "2021-01-01")),
    event_end_date = as.Date(c("2021-01-02", "2021-01-01")),
    amount = c(100, -10), currency_concept_id = "EUR")
  attr(events, "allocation") <- "duration"
  attr(events, "measure") <- "total_charge"
  attr(events, "diagnostics") <- list(extracted_rows = 2L)
  before <- events
  indices <- data.frame(year = c(2020, 2021, 2025), index = c(100, 110, 120),
                        currency_concept_id = "EUR")
  adjusted <- adjustCostPrices(events, indices, 2025, "event_date", "Synthetic test index")
  expect_identical(events, before)
  expect_equal(adjusted$amount, c(120, -120 / 11))
  expect_equal(adjusted$nominal_amount, c(100, -10))
  expect_equal(adjusted$source_price_year, c(2020, 2021))
  expect_equal(adjusted$price_factor, c(1.2, 12 / 11))
  expect_equal(adjusted$target_price_year, c(2025, 2025))
  expect_identical(attr(adjusted, "allocation"), "duration")
  expect_identical(attr(adjusted, "diagnostics"), attr(events, "diagnostics"))
  history <- data.frame(subject_id = "one", state_label = "Alive",
    state_start_date = as.Date("2021-01-01"), state_end_date = as.Date("2021-01-02"))
  fitted <- estimateStateCosts(history, adjusted)
  expect_equal(fitted$estimates$total_cost, 60 - 120 / 11)
  expect_equal(fitted$estimates$observed_days, 2)
  expect_equal(fitted$unallocatedCosts$amount, 60)
  expect_equal(fitted$allocatedCosts$original_amount, c(120, -120 / 11))
  expect_equal(fitted$allocatedCosts$nominal_amount, c(100, -10))
  expect_identical(fitted$metadata$price_adjustment, attr(adjusted, "price_adjustment"))
  model <- createCostModel(fitted, "day", "EUR", 2025, "none")
  expect_identical(model$metadata$price_adjustment, attr(adjusted, "price_adjustment"))
  expect_error(createCostModel(fitted, "day", "EUR", 2024, "none"), "priceYear.*adjusted")
})

test_that("explicit service years and currencies keep missing amounts and reject incomplete indices", {
  events <- data.frame(subject_id = "one", event_date = as.Date("2020-01-01"),
    service_year = c(2019, 2019, 2025), amount = c(100, 100, NA_real_),
    currency_concept_id = c("EUR", "USD", "EUR"))
  indices <- data.frame(year = c(2019, 2025, 2019, 2025),
    index = c(100, 120, 200, 220), currency_concept_id = c("EUR", "EUR", "USD", "USD"),
    unrelated_field = "must not reach the model")
  adjusted <- adjustCostPrices(events, indices, 2025, "service_year", "Synthetic test index")
  expect_equal(adjusted$amount, c(120, 110, NA_real_))
  expect_equal(adjusted$event_date, events$event_date)
  expect_named(attr(adjusted, "price_adjustment")$index, c("year", "index", "currency_concept_id"))
  expect_error(adjustCostPrices(events, indices[-1, ], 2025, "service_year", "test"), "EUR/2019")
  expect_error(adjustCostPrices(events, indices[-2, ], 2025, "service_year", "test"), "EUR/2025")
  expect_error(adjustCostPrices(events, rbind(indices, indices[1, ]), 2025, "service_year", "test"), "duplicate")
  invalid <- indices
  invalid$index[1] <- 0
  expect_error(adjustCostPrices(events, invalid, 2025, "service_year", "test"), "positive")
  invalid <- events
  invalid$service_year[1] <- NA
  expect_error(adjustCostPrices(invalid, indices, 2025, "service_year", "test"), "nonmissing")
  invalid <- events
  invalid$currency_concept_id[1] <- "0"
  expect_error(adjustCostPrices(invalid, indices, 2025, "service_year", "test"), "known currency")
  expect_error(adjustCostPrices(adjusted, indices, 2025, "service_year", "test"), "already")
  expect_error(adjustCostPrices(events, indices, 2025, source = "test"), "explicitly")
  empty <- adjustCostPrices(events[FALSE, ], indices, 2025, "service_year", "test")
  expect_equal(nrow(empty), 0)
  expect_type(empty$price_factor, "double")
  numericCurrency <- events[1, ]
  numericCurrency$currency_concept_id <- 1e12
  numericIndex <- indices[1:2, ]
  numericIndex$currency_concept_id <- 1e12
  normalized <- adjustCostPrices(numericCurrency, numericIndex, 2025, "service_year", "test")
  expect_identical(normalized$currency_concept_id, "1000000000000")
})

test_that("price provenance survives simulation and inconsistent adjusted rows fail before pooling", {
  history <- data.frame(subject_id = "one", state_label = "Alive",
    state_start_date = as.Date("2020-01-01") + 0:2,
    state_end_date = as.Date("2020-01-01") + 0:2)
  prepared <- prepareTrajectories(history, cycleLength = 1)
  events <- data.frame(subject_id = "one", event_date = as.Date("2020-01-01"),
    amount = 100, currency_concept_id = "EUR")
  index <- data.frame(year = c(2020, 2025), index = c(100, 120), currency_concept_id = "EUR")
  adjusted <- adjustCostPrices(events, index, 2025, "event_date", "Synthetic test index")
  fitted <- estimateStateCosts(prepared, adjusted)
  model <- createCostModel(fitted, "day", "EUR", 2025, "none")
  bundle <- createModelBundle(estimateTransitions(prepared), costs = model)
  run <- simulateTrajectories(bundle, n = 1, initial = c(Alive = 1), nSteps = 1, seed = 1)
  expect_equal(sum(run$costs$amount), 80)
  expect_identical(run$metadata$cost_model$price_adjustment, attr(adjusted, "price_adjustment"))
  invalid <- adjusted
  attr(invalid, "price_adjustment") <- NULL
  expect_error(estimateStateCosts(prepared, invalid), "Incomplete price adjustment")
  invalid <- adjusted
  invalid$amount <- 100
  expect_error(estimateStateCosts(prepared, invalid), "do not match")
  invalid <- adjusted
  invalid$price_factor <- 1
  expect_error(estimateStateCosts(prepared, invalid), "do not match")
  other <- adjustCostPrices(events, index, 2020, "event_date", "Synthetic test index")
  expect_error(estimateStateCosts(prepared, rbind(adjusted, other)), "do not match")
  invalid <- dplyr::bind_rows(adjusted, events)
  expect_error(estimateStateCosts(prepared, invalid), "nonmissing")
  events$accounting_year <- 2025
  byDate <- adjustCostPrices(events, index, 2025, "event_date", "Synthetic test index")
  byYear <- adjustCostPrices(events, index, 2025, "accounting_year", "Synthetic test index")
  expect_error(estimateStateCosts(prepared, rbind(byDate, byYear)), "sourceYear convention")
})
