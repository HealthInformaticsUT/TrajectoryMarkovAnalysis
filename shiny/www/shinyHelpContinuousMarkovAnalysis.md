### Continuous Markov Analysis

**Usage** 
This tab is used for continuous Markov chain analysis. In this tab we will define the continuous Markov chain analysis and retrieve an intensity matrix as well as some cost statistics from the raw data pulled from ATLAS. The input data used should be generated with **Cohort2Trajectory** package.

NB! While using discrete time data one can not generate continuous Markov model. 

**Select the cost domains included in analysis:** Select the cost domains you want to have included in cost analysis. 
**Select the states you want to exclude from analysis:** You can exclude some states from analysis. 

**Intensity matrix**
Intensity matrix for trajectories. You can forbid transistions between states when switching the value in the matrix to 0. 

**Sojourn time**
Statistics about the mean time patient stays in the respective state.

**Transition matrix**
Insert the transition time in years. For example 1 day is 0.00273785 of 1 year.
After pressing **Calculate** button the calculated transition matrix is shown.