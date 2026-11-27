# TrajectoryMarkovAnalysis

TrajectoryMarkovAnalysis (TMA) turns dated patient histories into discrete-state
transition models and recorded healthcare cost estimates. Use these estimates
to project a cohort or simulate individual paths, then compare costs, life-years
and QALYs under specified scenarios.

Transition and cost estimation work independently. Clinical states, event order,
treatment effects, utilities and future mortality assumptions are study inputs.

## Install

From the cloned repository, install the current checkout in R:

```r
install.packages(".", repos = NULL, type = "source")
```

Install the packages listed under `Imports` in `DESCRIPTION` first. Install
`CDMConnector` as well when using a connected database. Restart R if an older
TMA build is already loaded.

## Start with a small example

```r
source(system.file("examples", "quickstart.R", package = "TrajectoryMarkovAnalysis"))
```

The example creates synthetic histories, fits transitions, projects a cohort
and estimates costs. It needs no database connection. For your study, start
with the data frame returned by `Cohort2Trajectory::createTrajectories()`:

```r
library(TrajectoryMarkovAnalysis)

histories <- prepareTrajectories(trajectories)
transitions <- estimateTransitions(histories)
transitions$estimates

# Cost estimation can be run independently.
costs <- estimateStateCosts(histories, cost_records)
costs$estimates
```

C2T output with a `trajectory_definition` attribute supplies the cycle length,
state labels and absorbing states. RDS preserves this definition; CSV does not.
For a plain data frame, supply the definitions explicitly:

```r
histories <- prepareTrajectories(
  rows, cycleLength = 7, states = c("Stable", "Complication", "Dead"),
  absorbingStates = "Dead"
)
```

Required trajectory columns are `subject_id`, `state_label`, `state_start_date`
and `state_end_date`. Dates include both endpoints. Local cost records need
`subject_id`, `event_date` and `amount`. An existing CDMConnector reference can
supply costs through `extractCosts(cdm, histories, ...)`; TMA leaves the
connection open. Explicit settings must agree with any attached trajectory
definition.

TMA supports one observation episode per subject. With C2T, use
`targetEpisodeHandling = "First"`. Its `"All"` mode is not yet supported;
keep episode identifiers and metadata so repeated episodes cannot be mistaken
for one continuous history.

## Inspect results

Print a model or result for a short summary. Use `transitions$estimates` for
counts and probabilities, `simulation$trajectories` for generated histories and
`comparison$outcomes` for scenario totals and per-patient differences.

`transitionMatrix()` returns fitted probabilities or the probabilities used by
a projection at a selected source cycle:

```r
transitionMatrix(transitions)
transitionMatrix(projection, cycle = 0)
```

Cycle zero is the first transition step. Select a `stage`, `period` or `stratum`
when needed. See `?transitionMatrix` for supported model types.

## Choose the modelling path

| Task | Functions |
| --- | --- |
| Prepare histories and fit adjacent-cycle transitions | `prepareTrajectories()`, `estimateTransitions()` |
| Align C2T views and fit ordered events within a cycle | `prepareStageObservations()`, `estimateStageTransitions()` |
| Estimate first-cycle outcomes from starting states | `estimateEntryTransitions()` |
| Extract, adjust and allocate recorded costs | `extractCosts()`, `adjustCostPrices()`, `estimateStateCosts()` |
| Combine supplied stages and project a cohort | `composeTransitionStages()`, `projectCohort()` |
| Simulate patients with changing covariates | `createModelBundle()`, `simulateTrajectories()`, `advanceSimulation()` |
| Account for costs, life-years and QALYs | `createRewardModel()`, `updateRewardModel()`, `summariseOutcomes()` |
| Read and compare economic outcomes | `outcomeTotals()`, `compareScenarios()` |
| Calibrate and extend mortality | `createMortalityTransitions()`, `calibrateExcessMortality()`, `extrapolateMortality()` |

For ordered models, intervention effects can target a named stage, preserving
events such as a complication followed by death within one cycle.

Choose the accounting convention before comparing scenarios:

- `createRewardModel()` and `summariseOutcomes()` use completed intervals:
  52 weekly steps represent 52 weeks. The same rewards apply to projections
  and uncosted simulations. Fitted entry models are not accepted by this path.
- `projectCosts()` and `summariseSimulation()` use full cycle labels, including
  the initial label: a full 52-step run produces 53 labels. A separate pre-entry
  baseline adds no label or recurring reward.

Unsupported transition rows stay `NA`; the package does not substitute pooled
estimates. Scenario effects and extrapolations must be supplied explicitly.

## Examples and reference

- [Task guides](https://healthinformaticsut.github.io/TrajectoryMarkovAnalysis/articles/getting-started.html)
- [Worked introduction](vignettes/trajectory-analysis.Rmd), also available as
  `vignette("trajectory-analysis", package = "TrajectoryMarkovAnalysis")`
- [Workflow guide](https://github.com/HealthInformaticsUT/TrajectoryMarkovAnalysis/blob/main/docs/workflow.md)
- [Quickstart with synthetic histories and local costs](inst/examples/quickstart.R)
- [Ordered stages, interventions and economic outcomes](inst/examples/ordered-stage-economics.R)
- [C2T histories through to a cost-effectiveness comparison](inst/examples/c2t-to-icer.R)
- [Dynamic covariates and future mortality](inst/examples/future-mortality-simulation.R)
- [Prices adjusted to a reference year](inst/examples/price-adjustment.R)

Examples are installed under
`system.file("examples", package = "TrajectoryMarkovAnalysis")`.
Function help, such as `?simulateTrajectories`, describes the full input and
output contracts. Results can contain patient-level data; review tables before
sharing them.

## Related packages

- [CohortContrast](https://github.com/HealthInformaticsUT/CohortContrast)
- [Cohort2Trajectory](https://github.com/HealthInformaticsUT/Cohort2Trajectory)
- [TrajectoryViz](https://github.com/HealthInformaticsUT/TrajectoryViz)

## Publication and citation

Haug M, Oja M, Pajusalu M, et al. *Markov modeling for cost-effectiveness using
federated health data network*. Journal of the American Medical Informatics
Association, 2024. [doi:10.1093/jamia/ocae044](https://doi.org/10.1093/jamia/ocae044).

The publication describes the earlier implementation. Report the package
version and retain the model settings and assumptions used in your analysis.

## License and contact

Apache License 2.0. Developed by the University of Tartu Health Informatics
research group. Contact [Markus Haug](mailto:markus.haug@ut.ee).
