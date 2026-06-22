# Chemical Process Yield Optimization

Sequential design-of-experiments and response-surface optimization study in R.

## Overview

This project optimized the yield of an eight-factor chemical process under a fixed budget of 75 experimental runs.

I used a sequential experimental strategy to identify the strongest drivers of yield, narrow the search space, model the high-yield region, and validate a recommended operating setting.

## Results

| Metric                  |           Result |
| ----------------------- | ---------------: |
| Starting process yield  |             ~50% |
| Experimental budget     |          75 runs |
| Runs used               |               58 |
| Recommended mean yield  |           90.88% |
| Validation runs         |               12 |
| 95% prediction interval | 87.51% to 94.25% |

## Approach

1. Screened eight process factors using a fractional factorial design.
2. Identified the factors with the largest effects on yield.
3. Ran targeted follow-up factorial experiments.
4. Built a face-centered central composite design in the high-yield region.
5. Fit a quadratic response-surface model in R.
6. Validated the recommended setting with 12 replicate runs.

## Key Findings

* The analysis identified a broad, stable high-yield region rather than a narrow single optimum.
* The final recommended operating setting achieved a mean yield of 90.88% across 12 confirmation runs.
* The final model and validation runs supported an expected yield range of 87.51% to 94.25% for a future run.

## Tools and Methods

* R
* Design of Experiments
* Fractional Factorial Design
* Full Factorial Experiments
* Response Surface Methodology
* Central Composite Design
* Quadratic Regression
* Prediction Intervals
* Residual Diagnostics

## Repository Contents

This repository will include:

* Organized R scripts for each experimental stage
* Cleaned project data
* Selected figures and model outputs
* Final project report

## Note

This was completed as an academic applied statistics project. The analysis demonstrates a structured workflow for experimental optimization and data-driven operating recommendations.
