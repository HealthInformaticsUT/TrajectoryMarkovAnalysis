### Package 'TrajectoryMarkovAnalysis':  Package for creating Markov models for patient-orientated trajectories

**Development Status: Under Development**

### Information

This shiny application aims to help users in using TrajectoryMarkovAnalysis functionalities.
R-package TrajectoryMarkovAnalysis specializes on modelling patient trajectories using Markov chains. The package supports discrete and continuous time Markov models, Kaplan-Meier plots, Markov trees and synthetic data generation. The package is based on OMOP CDM. The inputs for this package should be generateb by the Cohort2Trajectory package.

### Abstract

**Backround:**
The vast amount of electronically stored medical data offers a possibility to investigate patients' treatment trajectories. These trajectories give us a foundation to find out the best healthcare practices, evaluate the economics of treatment patterns and model the treatment paths. The aim of this work is to implement a technical solution for the aforementioned tasks.  In particular, we develop methods for converting the OMOP cohorts into linear patient level trajectories and  for learning the Markov model parameters  on such the converted data. . The project  is   as  R packages that can be distributed to any database system operating on the open source OHDSI OMOP Common Data Model. 


**Methods**

We developed two R packages **Cohort2Trajectory** (https://github.com/HealthInformaticsUT/Cohort2Trajectory) and **TrajectoryMarkovAnalysis** (https://github.com/HealthInformaticsUT/TrajectoryMarkovAnalysis). The Cohort2Trajectory package takes in the target and the state defining cohorts and outputs the sequence of states for each patient. The overlaps of the input cohorts are solved using the priorities of the states as defined by the user. The resulting trajectories can be discretized into time periods of certain length or kept continuous. The TrajectoryMarkovAnalysis analysis package uses those trajectories to learn parameters of a Markov model. Both discrete and continuous Markov models are supported, along with the tools to assess the model fit to source data. The Markov models are commonly used economic health technology assessment analyses where synthetic populations are generated based on various assumptions. Thus, the TrajectoryMarkovAnalysis package includes code for assessing the cost of different states according to OMOP CDM Cost table and also generating synthetic populations based on the model. Additionally, the package supports drawing Kaplan-Meier plots. Both packages can be used from R programmatically and also graphically via Shiny based interfaces, supporting both visual study definition and the  creation of study packages based on this code. The best practices of OHDSI R packages are used.

