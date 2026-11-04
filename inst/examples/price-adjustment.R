# Wholly synthetic amounts and indexes. No database or downloads are needed.
library(TrajectoryMarkovAnalysis)

histories <- prepareTrajectories(data.frame(
  subject_id = "one",
  state_label = c("Acute", "Recovery"),
  state_start_date = as.Date(c("2020-12-30", "2021-01-01")),
  state_end_date = as.Date(c("2020-12-31", "2021-01-02"))
))

# The full four-day event crosses New Year. Its original start year supplies
# the price basis for the entire charge under the explicit choice below.
events <- data.frame(
  subject_id = "one", cost_id = "visit-1",
  event_date = as.Date("2020-12-30"),
  event_end_date = as.Date("2021-01-02"),
  amount = 400, currency_concept_id = "44818568", measure = "total_charge"
)
attr(events, "allocation") <- "duration"
original_events <- events

# These invented index values are not CPI estimates or published prices.
price_index <- data.frame(
  year = c(2020L, 2025L),
  index = c(100, 120),
  currency_concept_id = "44818568"
)
priced_events <- adjustCostPrices(
  events, priceIndex = price_index, priceYear = 2025L,
  sourceYear = "event_date",
  source = paste(
    "Synthetic geography and demonstration series; version 1;",
    "annual values; no partial-year observations"
  )
)

# Adjustment precedes allocation: 400 * 120 / 100 = 480, or 120 per day.
# The allocation attribute survives, so no allocation argument is needed here.
costs <- estimateStateCosts(histories, priced_events)
healthcare <- createCostModel(
  costs, unit = "day", currency = "EUR", priceYear = 2025L,
  terminal = "none"
)
# EUR is the analyst's label for the source currency concept, not a conversion.
# Supplying a different model priceYear would contradict the saved adjustment.

stopifnot(
  identical(events, original_events),
  identical(attr(priced_events, "allocation"), "duration"),
  priced_events$source_price_year == 2020L,
  priced_events$target_price_year == 2025L,
  isTRUE(all.equal(priced_events$amount, 480)),
  all(costs$allocatedCosts$original_amount == 480),
  all(costs$allocatedCosts$nominal_amount == 400),
  isTRUE(all.equal(sum(costs$allocatedCosts$amount), 480)),
  all(healthcare$rates$unit_cost == 120),
  identical(costs$metadata$price_adjustment,
            attr(priced_events, "price_adjustment")),
  identical(healthcare$metadata$price_adjustment,
            costs$metadata$price_adjustment)
)

# Full original_amount and nominal_amount repeat on every fragment; sum only
# amount across fragments to reconcile the adjusted charge.
print(costs$allocatedCosts[c(
  "event_date", "allocation_start_date", "allocation_end_date",
  "nominal_amount", "original_amount", "amount", "source_price_year"
)])
print(healthcare$metadata$price_adjustment)
