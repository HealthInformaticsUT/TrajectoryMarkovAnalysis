transition_effect_rule <- function(multiplier = 0.75, ...) {
  .tmaTransitionEffects(list(prevention = c(list(from = "Alive", to = "Pneumonia",
    multiplier = multiplier, redistributeTo = "Alive"), list(...))),
    c("Alive", "Pneumonia", "Dead"), "Dead", 7)
}

test_that("transition effects move only the specified pair's cycle probability", {
  states <- c("Alive", "Pneumonia", "Dead")
  probability <- c(Alive = 0.7, Pneumonia = 0.2, Dead = 0.1)
  changed <- .tmaApplyTransitionEffect(probability, transition_effect_rule()[[1]], states)
  expect_equal(changed, c(Alive = 0.75, Pneumonia = 0.15, Dead = 0.1))
  expect_identical(probability, c(Alive = 0.7, Pneumonia = 0.2, Dead = 0.1))
  expect_identical(changed[["Dead"]], probability[["Dead"]])
  expect_equal(sum(changed), sum(probability))
  reordered <- probability[c("Dead", "Pneumonia", "Alive")]
  expect_equal(.tmaApplyTransitionEffect(reordered, transition_effect_rule()[[1]], states),
                 changed[names(reordered)])
  expect_equal(.tmaApplyTransitionEffect(probability, transition_effect_rule(0)[[1]], states),
                 c(Alive = 0.9, Pneumonia = 0, Dead = 0.1))
  expect_equal(.tmaApplyTransitionEffect(probability, transition_effect_rule(2)[[1]], states),
                 c(Alive = 0.5, Pneumonia = 0.4, Dead = 0.1))
  expect_identical(.tmaApplyTransitionEffect(probability, transition_effect_rule(1)[[1]], states),
                     probability)
  zero <- c(Alive = 0.9, Pneumonia = 0, Dead = 0.1)
  expect_identical(.tmaApplyTransitionEffect(zero, transition_effect_rule(100)[[1]], states), zero)
})

test_that("impossible effects and unavailable rows fail without normalization", {
  states <- c("Alive", "Pneumonia", "Dead")
  probability <- c(Alive = 0.7, Pneumonia = 0.2, Dead = 0.1)
  expect_error(.tmaApplyTransitionEffect(probability, transition_effect_rule(5)[[1]], states),
                 "impossible.*pair mass")
  near <- c(Alive = 0.2, Pneumonia = 0.7, Dead = 0.1)
  endpoint <- .tmaApplyTransitionEffect(near, transition_effect_rule(0.9 / 0.7)[[1]], states)
  expect_equal(endpoint[["Alive"]], 0)
  expect_equal(endpoint[["Pneumonia"]], sum(near[c("Alive", "Pneumonia")]))
  expect_error(.tmaApplyTransitionEffect(near, transition_effect_rule((0.9 + 1e-12) / 0.7)[[1]], states),
                 "impossible")
  missing <- stats::setNames(rep(NA_real_, 3), states)
  expect_error(.tmaApplyTransitionEffect(missing, transition_effect_rule(0)[[1]], states), "complete")
  expect_error(.tmaApplyTransitionEffect(probability * 0.5, transition_effect_rule()[[1]], states),
                 "stochastic")
})

test_that("rule definitions enforce known states, nonabsorbing sources and aligned windows", {
  states <- c("Alive", "Pneumonia", "Dead")
  base <- list(from = "Alive", to = "Pneumonia", multiplier = 0.75, redistributeTo = "Alive")
  validate <- function(rule) .tmaTransitionEffects(list(effect = rule), states, "Dead", 7)
  expect_identical(.tmaTransitionEffects(NULL, states, "Dead", 7), list())
  expect_equal(validate(base)[[1]]$startDay, 0)
  expect_equal(validate(base)[[1]]$endDay, Inf)
  expect_error(.tmaTransitionEffects(list(base), states, "Dead", 7), "unique.*names")
  expect_error(.tmaTransitionEffects(list(a = base, a = base), states, "Dead", 7), "unique.*names")
  bad <- base; bad$from <- "Dead"
  expect_error(validate(bad), "absorbing")
  bad <- base; bad$to <- "Unknown"
  expect_error(validate(bad), "known state")
  bad <- base; bad$redistributeTo <- "Pneumonia"
  expect_error(validate(bad), "distinct")
  bad <- base; bad$to <- "Alive"; bad$redistributeTo <- "Pneumonia"
  expect_equal(validate(bad)[[1]]$to, "Alive")
  for (value in list(-1, Inf, NA_real_, c(0.5, 1))) {
    bad <- base; bad$multiplier <- value
    expect_error(validate(bad), "finite nonnegative")
  }
  expect_error(validate(c(base, list(startDay = 1))), "aligned")
  expect_error(validate(c(base, list(endDay = 7.5))), "whole day")
  expect_error(validate(c(base, list(startDay = 7, endDay = 7))), "greater")
  expect_error(validate(c(base, list(hazardRatio = 0.75))), "optional fields")
})

test_that("eligibility receives matching source snapshots within the half-open window", {
  population <- data.frame(simulation_id = paste0("s", 1:5),
    state_label = c("Alive", "Alive", "Alive", "Pneumonia", "Alive"),
    elapsed_day = c(0, 7, 14, 7, 7), treatment_active = c(NA, TRUE, NA, NA, FALSE))
  calls <- 0L
  rules <- transition_effect_rule(startDay = 7, endDay = 14, eligible = function(p) {
    calls <<- calls + 1L
    p$treatment_active
  })
  selected <- .tmaEligibleEffects(rules, population)
  expect_identical(names(selected), population$simulation_id)
  expect_equal(unname(selected), c(NA_character_, "prevention", NA_character_, NA_character_, NA_character_))
  expect_equal(calls, 1)
  .tmaEligibleEffects(rules, population[c(1, 3, 4), ])
  expect_equal(calls, 1)
  rules$prevention$eligible <- function(p) TRUE
  expect_error(.tmaEligibleEffects(rules, population), "one logical value per candidate")
  rules$prevention$eligible <- function(p) c(TRUE, NA)
  expect_error(.tmaEligibleEffects(rules, population), "without NA")
  rules$prevention$eligible <- function(p) stop("missing covariate")
  expect_error(.tmaEligibleEffects(rules, population), "prevention.*missing covariate")
})

test_that("more than one eligible rule per patient is rejected even for distinct pairs", {
  states <- c("Alive", "Pneumonia", "Dead", "Other")
  first <- list(from = "Alive", to = "Pneumonia", multiplier = 0.75, redistributeTo = "Alive")
  second <- list(from = "Alive", to = "Dead", multiplier = 0.5, redistributeTo = "Other")
  rules <- .tmaTransitionEffects(list(first = first, second = second), states, "Dead", 7)
  population <- data.frame(simulation_id = c("treated", "control"),
    state_label = "Alive", elapsed_day = 0, treated = c(TRUE, FALSE))
  expect_error(.tmaEligibleEffects(rules, population), "Overlapping.*first.*second.*treated")
  rules$first$eligible <- function(p) p$treated
  rules$second$eligible <- function(p) !p$treated
  expect_equal(unname(.tmaEligibleEffects(rules, population)), c("first", "second"))
})
