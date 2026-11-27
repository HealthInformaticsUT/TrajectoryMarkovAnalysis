# TrajectoryMarkovAnalysis

TrajectoryMarkovAnalysis (TMA) estimates transitions and recorded healthcare
costs from dated patient histories. It uses fitted or supplied models to project
cohort outcomes, simulate individual paths and compare economic scenarios.

Use it when you need to connect recorded clinical states with explicit
assumptions about treatment, costs, quality of life or later mortality.
Transition and cost estimation can also be used on their own.

## Start with your question

| What do you need to do? | Guide |
| --- | --- |
| Run a small analysis without a database | [Get started](articles/getting-started.html) |
| Use C2T output or prepare dated histories | [Prepare histories](articles/preparing-histories.html) |
| Estimate transitions, first-cycle outcomes or ordered events | [Fit transitions](articles/estimating-transitions.html) |
| Project a cohort or simulate patients with changing covariates | [Project and simulate](articles/projecting-and-simulating.html) |
| Allocate recorded costs and compare costs, life-years and QALYs | [Costs and outcomes](articles/costs-and-economic-outcomes.html) |
| Specify treatment effects or extend follow-up | [Interventions and future mortality](articles/interventions-and-future-mortality.html) |

The [worked analysis](articles/trajectory-analysis.html) combines ordered stages,
an intervention and economic outcomes. The [example catalogue](articles/examples.html)
links runnable scripts for each feature. The [function reference](reference/index.html)
describes exact inputs and outputs.

## What the model represents

An adjacent-cycle model estimates movements between recorded state labels.
An ordered-stage model preserves supplied events within a cycle, such as a
complication followed by death. A projection returns expected cohort counts;
a simulation samples patient paths and can update their covariates between steps.

Clinical states, event order, treatment effects, utilities and extrapolation
rules remain study inputs. Unsupported probability rows stay missing. The
package does not substitute pooled estimates or infer causal treatment effects.

TMA currently supports one observation episode per subject. With C2T, use
`targetEpisodeHandling = "First"`; its new `"All"` mode requires episode-aware
analysis that TMA does not yet provide.

## Choose how time is accounted for

`summariseOutcomes()` applies shared rewards to completed intervals: 52 weekly
steps represent 52 weeks. It accepts projections and uncosted simulations with
explicit starting states.

`projectCosts()` and `summariseSimulation()` use full cycle labels, including
the initial label. A full 52-step run has 53 labels. A fitted entry model generates
the first label without adding a baseline cycle. Keep the same convention in
both scenarios of a comparison.

## Related work

[Cohort2Trajectory](https://healthinformaticsut.github.io/Cohort2Trajectory/)
creates the patient histories. TMA estimates and applies transition and cost
models from those histories. See the
[C2T-to-ICER example](https://github.com/HealthInformaticsUT/TrajectoryMarkovAnalysis/blob/main/inst/examples/c2t-to-icer.R)
for a synthetic analysis across both packages.

Haug M, Oja M, Pajusalu M, et al. *Markov modeling for cost-effectiveness using
federated health data network*. Journal of the American Medical Informatics
Association, 2024. [doi:10.1093/jamia/ocae044](https://doi.org/10.1093/jamia/ocae044).
The publication describes the earlier implementation; record the package version
and assumptions used in your analysis.
