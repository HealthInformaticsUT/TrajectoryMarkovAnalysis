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
if an older TMA build was already loaded. This is the 1.1.0 release candidate.

## Start with a small example

```r
source(system.file("examples", "quickstart.R", package = "TrajectoryMarkovAnalysis"))
```

The example uses synthetic histories and costs. It needs no database connection.
For your study, use the data frame returned by
`Cohort2Trajectory::createTrajectories()`:

```r
library(TrajectoryMarkovAnalysis)

histories <- prepareTrajectories(
  trajectories, cycleLength = 7, absorbingStates = "Dead"
)
transitions <- estimateTransitions(histories)
transitions$estimates

# Cost estimation can be run independently.
costs <- estimateStateCosts(histories, cost_records)
costs$estimates
```

Declare your own absorbing states. Required trajectory columns are `subject_id`,
`state_label`, `state_start_date` and `state_end_date`. Local cost records need
`subject_id`, `event_date` and `amount`. You can also read costs through an
existing CDMConnector reference with `extractCosts(cdm, histories, ...)`.
TMA leaves that connection open.

## Choose the modelling path

| Task | Public functions |
| --- | --- |
| Validate histories and estimate adjacent-cycle transitions | `prepareTrajectories()`, `estimateTransitions()` |
| Estimate analyst-defined events within a cycle | `estimateStageTransitions()` |
| Estimate first-cycle outcomes from declared starting states | `estimateEntryTransitions()` |
| Extract, adjust and allocate recorded costs | `extractCosts()`, `adjustCostPrices()`, `estimateStateCosts()` |
| Combine supplied stages and project expected outcomes | `composeTransitionStages()`, `projectCohort()` |
| Simulate patients with changing covariates | `createModelBundle()`, `simulateTrajectories()`, `advanceSimulation()` |
| Account for interval costs, life-years and QALYs | `createRewardModel()`, `summariseOutcomes()`, `compareScenarios()` |
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
