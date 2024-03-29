TrajectoryMarkovAnalysis
======================

Introduction
============

R-package TrajectoryMarkovAnalysis specializes on modelling patient trajectories using Markov chains.
The package supports discrete and continuous time Markov models, Kaplan-Meier plots, Markov trees and synthetic data generation.
The package is based on OMOP CDM.
The package can be run as GUI/CLI.

Features
========

Screenshots
===========

Technology
==========
TrajectoryMarkovAnalysis is an R package, with some functions implemented in C++.

For running the package with OMOP CDM, the user must have:
1. Permissions to select on ohdsi cdm data schema.
2. Permissions to select on ohdsi cdm results schema.
3. Permissions to select, create and insert on ohdsi temp/scratch schema.


System Requirements
===================

Getting Started
===============

Package manual: [TrajectoryMarkovAnalysis.pdf](https://github.com/HealthInformaticsUT/TrajectoryMarkovAnalysis/blob/main/TrajectoryMarkovAnalysis_1.0.pdf)

Package guide: [TrajectoryMarkovAnalysis_guide.pdf](https://github.com/HealthInformaticsUT/TrajectoryMarkovAnalysis/blob/main/TrajectoryMarkovAnalysis_guide.pdf)
 
License
=======
TrajectoryMarkovAnalysis is licensed under Apache License 2.0

Development
===========
TrajectoryMarkovAnalysis is being developed in R Studio.

Publications
============
Markus Haug, Marek Oja, Maarja Pajusalu, Kerli Mooses, Sulev Reisberg, Jaak Vilo, Antonio Fernández Giménez, Thomas Falconer, Ana Danilović, Filip Maljkovic, Dalia Dawoud, Raivo Kolde, Markov modeling for cost-effectiveness using federated health data network, Journal of the American Medical Informatics Association, 2024;, ocae044, https://doi.org/10.1093/jamia/ocae044

Citation
===========
Haug, M. (2023, May). TrajectoryMarkovAnalysis, v1.0.5. GitHub. https://github.com/HealthInformaticsUT/TrajectoryMarkovAnalysis/releases/tag/v1.0.5

# Acknowledgements

1. [Cohort2Trajectory package](https://github.com/HealthInformaticsUT/Cohort2Trajectory) by Markus Haug
2. [TrajectoryViz package](https://github.com/HealthInformaticsUT/TrajectoryViz) by Maarja Pajusalu

Research group of Health-Informatics in University of Tartu https://health-informatics.cs.ut.ee/

