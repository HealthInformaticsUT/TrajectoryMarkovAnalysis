# TrajectoryMarkovAnalysis

TrajectoryMarkovAnalysis estimates transitions and recorded healthcare costs
from patient histories. It projects expected cohort outcomes and simulates
patient trajectories under explicitly supplied intervention and mortality
assumptions. Transition and cost estimation can be used independently.

## Install

From the cloned repository, install the current checkout in R:

```r
install.packages(".", repos = NULL, type = "source")
```

Install the packages listed under `Imports` in `DESCRIPTION` first.
Install `CDMConnector` as well when using a connected database. Restart R
if an older TMA build was already loaded. This checkout is version 1.1.0.9000.
The automatic C2T definition handoff requires Cohort2Trajectory 1.3.0.9000 or later.

## Start with a small example

```r
source(system.file("examples", "quickstart.R", package = "TrajectoryMarkovAnalysis"))
```

The example uses synthetic histories and costs. It needs no database connection.
For your study, use the data frame returned by
`Cohort2Trajectory::createTrajectories()`:

```r
library(TrajectoryMarkovAnalysis)

histories <- prepareTrajectories(trajectories)
transitions <- estimateTransitions(histories)
transitions$estimates

# Cost estimation can be run independently.
costs <- estimateStateCosts(histories, cost_records)
costs$estimates
```

Current C2T output carries its cycle length, state labels and absorbing states.
TMA uses them without repeating the settings. RDS preserves this handoff. For a
plain data frame or CSV, supply the definitions explicitly:

```r
histories <- prepareTrajectories(
  rows, cycleLength = 7, states = c("Stable", "Complication", "Dead"),
  absorbingStates = "Dead"
)
```

Explicit arguments that conflict with an attached C2T definition raise an
error. Required trajectory columns are `subject_id`,
`state_label`, `state_start_date` and `state_end_date`. Local cost records need
`subject_id`, `event_date` and `amount`. You can also read costs through an
existing CDMConnector reference with `extractCosts(cdm, histories, ...)`.
TMA leaves that connection open.

## Inspect results

Print the object itself for a short summary:

```r
histories
transitions
costs
```

The full tables and settings stay on the object. Printing does not remove or
suppress any data. Use `transitions$estimates` for counts and probabilities,
`simulation$trajectories` for patient histories and `comparison$outcomes` for
cohort totals and per-patient results.

Use `transitionMatrix()` to inspect probabilities without navigating nested
lists. A fitted model returns its original probabilities. A projection returns
the probabilities actually used at the selected source cycle:

```r
transitionMatrix(transitions)
transitionMatrix(stage_fit, stage = "onset")
transitionMatrix(treated_projection, stage = "onset", cycle = 0)
```

Cycle zero is the first transition step. Select a `period` or `stratum` by its
exact name when a fit contains more than one. See `?transitionMatrix` for the
supported model types.

For a connected example from C2T intervals to a TMA ICER, install
Cohort2Trajectory and run:

```r
example_results <- source(system.file(
  "examples", "c2t-to-icer.R", package = "TrajectoryMarkovAnalysis"
))$value
example_results$comparison
```

This five-patient example runs C2T twice on the same weekly grid: a pneumonia
history view preserves events before death, and a final-outcome view records
death. `prepareStageObservations()` aligns these views for onset followed by
mortality within each week. State definitions and this stage order are analyst
choices; the helper does not infer clinical events. Complete final weeks count
by default, while incomplete censored weeks are excluded from both stages.
Prevention changes only onset. The example uses invented costs and effects,
with the same completed-interval accounting for projected and simulated outcomes.

## Define economic assumptions once

Create rewards for the reference scenario, then change only what differs:

```r
reference_rewards <- createRewardModel(
  stateCosts = c(Stable = 100, Complication = 200, Dead = 0),
  aliveStates = c("Stable", "Complication"),
  utilities = c(Stable = 0.8, Complication = 0.6, Dead = 0),
  currency = "EUR", priceYear = 2026,
  costDiscountRate = 0.035, healthDiscountRate = 0.035
)
intervention_rewards <- updateRewardModel(
  reference_rewards, upfrontCosts = c(programme = 50)
)
reference_outcomes <- summariseOutcomes(reference_projection, reference_rewards)
intervention_outcomes <- summariseOutcomes(intervention_projection, intervention_rewards)

outcomeTotals(intervention_outcomes)  # discounted cohort cost, life-years, QALYs
outcomeTotals(intervention_outcomes, perPatient = TRUE)
comparison <- compareScenarios(reference_outcomes, intervention_outcomes)
comparison
```

The projections above must use your defined states, treatment effects and a
common horizon. These costs and utilities are illustrative. `updateRewardModel()`
validates the changed definition and leaves the original unchanged. Each named
argument replaces that entire field, so supply all components you want to keep.
`outcomeTotals(..., discounted = FALSE)` reads undiscounted results. Uncomputed
outcomes stay `NA`. Detailed tables remain available on the result objects.

## Choose the modelling path

| Task | Public functions |
| --- | --- |
| Validate histories and estimate adjacent-cycle transitions | `prepareTrajectories()`, `estimateTransitions()` |
| Align C2T views and estimate ordered events within a cycle | `prepareStageObservations()`, `estimateStageTransitions()` |
| Estimate first-cycle outcomes from declared starting states | `estimateEntryTransitions()` |
| Extract, adjust and allocate recorded costs | `extractCosts()`, `adjustCostPrices()`, `estimateStateCosts()` |
| Combine supplied stages and project expected outcomes | `composeTransitionStages()`, `projectCohort()` |
| Simulate patients with changing covariates | `createModelBundle()`, `simulateTrajectories()`, `advanceSimulation()` |
| Account for interval costs, life-years and QALYs | `createRewardModel()`, `updateRewardModel()`, `summariseOutcomes()` |
| Read and compare economic outcomes | `outcomeTotals()`, `compareScenarios()` |
| Calibrate and extend mortality | `createMortalityTransitions()`, `calibrateExcessMortality()`, `extrapolateMortality()` |

Clinical state definitions, event order, eligibility, treatment effects,
utilities and external evidence remain study inputs. The package returns tables
and model objects. It does not write a study report or apply a disclosure policy.

For ordered models, intervention effects can target a named stage. The same
reward definition can then account for deterministic and simulated outcomes.
With `summariseOutcomes()`, 52 completed weekly steps represent 52 weeks. The
separate labelled-cycle functions, `projectCosts()` and `summariseSimulation()`,
retain their documented initial-label accounting.

## Examples and reference

- [Quickstart with synthetic trajectories and local costs](inst/examples/quickstart.R)
- [Ordered stages, intervention and economic outcomes](inst/examples/ordered-stage-economics.R)
- [Full workflow reference](docs/workflow.md)
- [Dynamic covariates and supplied future mortality](inst/examples/future-mortality-simulation.R)
- [Prices adjusted to an explicit reference year](inst/examples/price-adjustment.R)

Examples are also installed under
`system.file("examples", package = "TrajectoryMarkovAnalysis")`.

The supported interface replaces the older GUI, continuous-time and
database-driven workflow. The unused legacy implementation, database drivers
and old PDF guides have been removed. The package now uses R only and requires
no compiler. Use this README, the workflow reference, the installed vignette
and current function help for this version.

## Related packages

- [CohortContrast](https://github.com/HealthInformaticsUT/CohortContrast)
- [Cohort2Trajectory](https://github.com/HealthInformaticsUT/Cohort2Trajectory)
- [TrajectoryViz](https://github.com/HealthInformaticsUT/TrajectoryViz)

Pass Cohort2Trajectory output through `prepareTrajectories()` before analysis.
When combining package versions or using separate visualisation tools, check
the expected column names, state labels and treatment of boundary rows.

## Publication and citation

Haug M, Oja M, Pajusalu M, et al. *Markov modeling for cost-effectiveness using
federated health data network*. Journal of the American Medical Informatics
Association, 2024. [doi:10.1093/jamia/ocae044](https://doi.org/10.1093/jamia/ocae044).

When reporting an analysis, also record the package version or Git commit and
retain the model settings and study assumptions used for the run.

## License and contact

Apache License 2.0. Developed by the University of Tartu Health Informatics
research group. Contact [Markus Haug](mailto:markus.haug@ut.ee).
