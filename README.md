# Bayesian-Improvements-to-Simon-2-Stage-Design

The files in this repository provide the code to reproduce the results found in the paper, "Bayesian Improvements to Simon 2-Stage Design for Efficient One-Arm Oncology Trials" by Natalia Rahman and Alexander M. Kaizer. 

# The files included are:
  - functions.R
    - This file provides the functions used to (1) simulate trial data, (2) Implement interim monitor designs, (3) Create confusion matrix-style results
  - Calculating_PPoS.R
    - This file provides code on how to calculate PPoS via simulation.
  - Scenario1_FutilityOnly.R
    - Code to reproduce the results under Scenario 1 monitoring for futility only
  - Scenario1_FutilityandEffiacy.R
    - Code to reproduce the results under Scenario 1 monitoring for futility & efficacy
  - Scenario2.R
    - Code to reproduce the results under Scenario 2

# ABSTRACT
Due to economic and ethical reasons, designs that implement interim analyses are widely used for Phase 2 single-arm trials in oncology due to their ability to terminate the trial early if the proposed treatment is likely to be ineffective, or, although less frequently used, if it is likely to be effective. A popular approach to interim monitoring is the Simon 2-Stage design, which allows interim monitoring for futility at a single interim analysis. We propose new interim analysis strategies based on Bayesian methodologies to improve the efficiency of the overall trial while also allowing stopping for efficacy in addition to futility. The proposed designs desire to reduce the expected sample size relative to the Simon 2-Stage design, while maintaining a target power and type I error rate with easy to implement criteria for interim decision making. The operating characteristics of the trial designs were evaluated via simulations in two scenarios: (1) a null response rate of 10% versus a 30% alternative response, and (2) a 30% null response versus 55% alternative response rate. Our results demonstrate that there are potential Bayesian inspired designs that maintain the power and type I error rate while reducing the expected sample size. Further, two proposed designs utilizing the predictive posterior distribution achieved greater reduction in the expected sample size, but did have some loss in efficiency for power or type I error rate. Overall, the proposed interim monitoring designs reduce the expected sample size by up to 47% more than Simon’s 2-Stage design and may be more efficient for use in one-arm clinical trials.


