simulation_cost_population <- function(cycle = 0, active = c(TRUE, FALSE)) {
  data.frame(simulation_id = c("one", "two"), state_label = c("Alive", "Treatment"),
             cycle = cycle, elapsed_day = cycle * 7, treatment_active = active)
}

test_that("once charges preserve individual memory across subsequent snapshots", {
  rules <- .tmaSimulationCostRules(list(
    setup = list(type = "once", amount = 1500, eligible = function(p) p$treatment_active),
    maintenance = list(type = "recurring", amount = 20)), c("Alive", "Treatment"))
  initial <- simulation_cost_population()
  first <- .tmaSimulationCharge(initial, rules = rules)
  expect_equal(first$ledger$simulation_id, c("one", "one", "two"))
  expect_equal(first$ledger$amount, c(1500, 20, 20))
  expect_equal(first$ledger$cycle, c(0, 0, 0))
  expect_equal(first$charged$setup, "one")

  later <- simulation_cost_population(1, c(TRUE, TRUE))[2:1, ]
  second <- .tmaSimulationCharge(later, initial, rules, first$charged)
  expect_equal(second$ledger$simulation_id[second$ledger$component == "setup"], "two")
  expect_setequal(second$charged$setup, c("one", "two"))
  resumed <- .tmaSimulationCharge(simulation_cost_population(2, c(TRUE, TRUE)),
                                  later, rules, second$charged)
  expect_equal(resumed$ledger$component, rep("maintenance", 2))
  expect_equal(resumed$charged, second$charged)
})

test_that("destination states, covariates and half-open day windows refine charges", {
  rules <- .tmaSimulationCostRules(list(maintenance = list(
    type = "recurring", amount = -20, states = "Treatment", startDay = 7, endDay = 14,
    eligible = function(p) p$treatment_active)), c("Alive", "Treatment"))
  initial <- simulation_cost_population(0, c(TRUE, TRUE))
  atStart <- simulation_cost_population(1, c(TRUE, TRUE))
  atEnd <- simulation_cost_population(2, c(TRUE, TRUE))
  expect_equal(nrow(.tmaSimulationCharge(initial, rules = rules)$ledger), 0)
  charged <- .tmaSimulationCharge(atStart, initial, rules)$ledger
  expect_equal(charged$simulation_id, "two")
  expect_equal(charged$amount, -20)
  expect_equal(charged$elapsed_day, 7)
  expect_equal(nrow(.tmaSimulationCharge(atEnd, atStart, rules)$ledger), 0)
  atStart$treatment_active <- FALSE
  expect_equal(nrow(.tmaSimulationCharge(atStart, initial, rules)$ledger), 0)
})

test_that("terminal entry charges actual changes but never repeated terminal residency", {
  rules <- .tmaSimulationCostRules(list(
    startup = list(type = "once", amount = 10),
    care = list(type = "recurring", amount = 20),
    terminal = list(type = "transition", amount = 500, from = "Alive", to = "Dead")),
    c("Alive", "Treatment", "Dead"))
  initial <- simulation_cost_population()
  initial$state_label <- c("Alive", "Dead")
  first <- .tmaSimulationCharge(initial, rules = rules, terminalStates = "Dead")
  expect_equal(first$ledger$simulation_id, c("one", "one"))
  expect_equal(first$ledger$component, c("startup", "care"))
  later <- initial[2:1, ]
  later$cycle <- 1
  later$elapsed_day <- 7
  later$state_label <- "Dead"
  second <- .tmaSimulationCharge(later, initial, rules, first$charged, "Dead")
  expect_equal(second$ledger$simulation_id, "one")
  expect_equal(second$ledger$component, "terminal")
  expect_equal(second$ledger$amount, 500)
  repeated <- later
  repeated$cycle <- 2
  repeated$elapsed_day <- 14
  expect_equal(nrow(.tmaSimulationCharge(repeated, later, rules, second$charged, "Dead")$ledger), 0)

  # A declared absorbing state is still costable unless explicitly terminal.
  nonterminal <- .tmaSimulationCharge(later, initial, rules, first$charged)
  expect_equal(sum(nonterminal$ledger$component == "care"), 2)
  expect_equal(nonterminal$ledger$simulation_id[nonterminal$ledger$component == "startup"], "two")
})

test_that("invalid cost rules fail before simulation", {
  states <- c("Alive", "Dead")
  rule <- list(type = "recurring", amount = 1)
  expect_identical(.tmaSimulationCostRules(NULL, states), list())
  expect_error(.tmaSimulationCostRules(list(rule), states), "component names")
  expect_error(.tmaSimulationCostRules(list(a = rule, a = rule), states), "component names")
  expect_error(.tmaSimulationCostRules(list(a = c(rule, list(rate = 2))), states), "optional fields")
  expect_error(.tmaSimulationCostRules(list(a = list(type = "daily", amount = 1)), states), "type")
  expect_error(.tmaSimulationCostRules(list(a = list(type = "once", amount = Inf)), states), "finite")
  expect_error(.tmaSimulationCostRules(list(a = c(rule, list(states = "Missing"))), states), "known state")
  expect_error(.tmaSimulationCostRules(list(a = c(rule, list(startDay = 7, endDay = 7))), states), "greater")
  expect_error(.tmaSimulationCostRules(list(a = list(type = "transition", amount = 1, from = "Alive")),
                                       states), "known state")
  expect_error(.tmaSimulationCostRules(list(a = list(type = "transition", amount = 1,
                                                     from = "Dead", to = "Dead")), states), "self-pair")
})

test_that("eligibility callbacks must return complete logical vectors", {
  population <- simulation_cost_population()
  check <- function(callback) {
    rules <- .tmaSimulationCostRules(list(care = list(type = "recurring", amount = 1,
                                                      eligible = callback)),
                                      c("Alive", "Treatment"))
    .tmaSimulationCharge(population, rules = rules)
  }
  expect_error(check(function(p) TRUE), "care.*logical value per candidate population row")
  expect_error(check(function(p) c(TRUE, NA)), "without NA")
  expect_error(check(function(p) c(1, 0)), "logical value")
  expect_error(check(function(p) p$missing), "care.*logical value")
  expect_error(check(function(p) stop("missing covariate")), "care.*missing covariate")
})

test_that("eligibility sees only charge candidates and skips already charged patients", {
  population <- data.frame(
    simulation_id = c("paid", "live", "dead", "outside", "other-state"),
    state_label = c("Alive", "Alive", "Dead", "Alive", "Treatment"),
    cycle = c(0, 0, 0, 2, 0), elapsed_day = c(0, 0, 0, 14, 0),
    age = c(NA, 70, NA, NA, NA))
  calls <- 0L
  rules <- .tmaSimulationCostRules(list(care = list(
    type = "once", amount = 5, states = "Alive", endDay = 7,
    eligible = function(p) {
      calls <<- calls + 1L
      p$age > 65
    })), c("Alive", "Treatment", "Dead"))
  first <- .tmaSimulationCharge(population, rules = rules,
                                 charged = list(care = "paid"), terminalStates = "Dead")
  expect_equal(first$ledger$simulation_id, "live")
  expect_equal(first$charged$care, c("paid", "live"))
  second <- .tmaSimulationCharge(population, rules = rules,
                                  charged = first$charged, terminalStates = "Dead")
  expect_equal(nrow(second$ledger), 0)
  expect_equal(calls, 1L)

  transition <- .tmaSimulationCostRules(list(terminal = list(
    type = "transition", amount = 10, from = "Alive", to = "Dead",
    eligible = function(p) stop("No transition candidates should be evaluated."))),
    c("Alive", "Treatment", "Dead"))
  expect_equal(nrow(.tmaSimulationCharge(population, rules = transition,
                                          terminalStates = "Dead")$ledger), 0)
})
