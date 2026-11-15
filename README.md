# Stochastic Data Processing and Simulation

Coursework from the *Stochastic Data Processing and Simulation* course, taken during my MSc in Economics at the University of Gothenburg (2020–2021). Each assignment has its code and a written report (PDF).

## Assignments

| Assignment | Topic | Methods | Files |
| --- | --- | --- | --- |
| A1 | Linear regression by hand | OLS estimates, residual standard error and confidence intervals computed from the formulas and checked against `lm` and `confint` | `A1.R`, `A1.pdf` |
| A2 | Model choice and bootstrap | Simulated prediction intervals; choosing the polynomial degree by out-of-sample prediction error (pMSE) over 50 random train/test splits; non-parametric and parametric bootstrap confidence intervals in R; Gumbel maximum likelihood with bootstrap in MATLAB | `A2.R`, `A2.m`, `A2.pdf` |
| A3 | Survival analysis | Survival function and hazard rate of a component network; expected lifetime | `A3.R`, `A3.pdf` |
| A4 | Bayesian decision theory | Monte Carlo expected utility for portfolio weights; logistic regression by maximum likelihood; decisions under asymmetric costs | `A4.R`, `A4.pdf` |
| A5 | Monte Carlo integration | Crude Monte Carlo and hit-or-miss estimators, their variance and error convergence | `A5.py`, `A5.pdf` |
| A6 | Simulation of stochastic processes | Brownian motion at different resolutions; Euler–Maruyama scheme for a geometric SDE; strong and weak error by Monte Carlo | `A6.py`, `A6 .pdf` |

A1, A3 and A5 are preparation exercises for A2, A4 and A6.

One lesson from A2: the simulated 95% prediction interval for stopping distance fell below zero at low speeds. A model's output has to be checked against what is physically possible, not only against the statistics.

## Running the code

- **R:** set the working directory to the folder that holds the data files. A4 needs the `LearnBayes` and `dplyr` packages.
- **Python:** A5 and A6 need `numpy` and `matplotlib`.
- **Data:** the datasets used in the assignments, such as `Wage.txt` and `stockvalues.txt`, are not included in this repository. `cars` is built into R.

A4 questions Q1(d) and Q2(d) were not completed.
