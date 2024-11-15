# Vector Auto-Regression: A Tool for Diciphering VooDoo
This repository accompanies the article "Vector Auto-Regressions: A Method to the Madness of Macroeconomics." It explores the role of vector auto-regressions (VARs) in economic analysis and provides R code to replicate the results and insights shared in the article.

## Repository Overview
This repository contains the following files:

load_data.R: Script for loading and preprocessing data.
model_estimation.R: Code for estimating VAR models, including hyperparameter tuning for lag selection.
forecasting.R: Generates forecasts for 2020–2023 using data up to 2019, and compares predictions to actual values.
impulse_response.R: Computes impulse response functions (IRFs) to evaluate the elasticity of GDP, unemployment, and consumption to interest rate changes.
visualizations.R: Scripts to create plots for forecasts, IRFs, and other key outputs.
README.md: Documentation for the repository.

## About the Article
Macroeconomics often seems like "voodoo," with disagreements on how the economy works. Despite this, tools like vector auto-regressions (VARs) provide a systematic approach to understanding relationships between key macroeconomic variables such as interest rates, GDP, unemployment, and consumption.

## What is a VAR?
Auto-regression: Modeling a variable based on its own past values.
Vector: Incorporating multiple interdependent variables.
VARs can answer:

Granger causality: Does variable X help predict variable Y?
Elasticity: What happens to Y after a 1% change in X?
Regime change effects: What if policies like interest rate targeting are changed?
Key Findings
Forecast Accuracy: Rates from the previous year still impact GDP, consumption, and unemployment. Our model with four lags predicted 2020–2023 macro variables with reasonable accuracy.
Elasticity via IRFs: A 1% change in rates significantly impacts income and consumption, affirming the importance of monetary policy.
Policy Implications: Macro policy operates on a long timeline, requiring patience and foresight.

## Limitations
While VARs are robust tools, macroeconomics remains complex due to countless external influences. However, VARs reinforce key economic theories, like those proposed by David Hume over 200 years ago, and provide a valuable lens for long-term policy analysis.

## How to Use This Repository
Clone the Repository:

bash
Copy code
git clone git@github.com:better-and-bad/Vector-Autoregression.git
cd Vector-Autoregression
Run Scripts:

Start with load_data.R to prepare the dataset.
Use model_estimation.R to estimate VAR models.
Run forecasting.R to generate and visualize predictions.
Analyze elasticity with impulse_response.R.
Dependencies:

R packages: vars, forecast, ggplot2, tidyverse

## Citation
If you use this code or repository, please cite the accompanying article.
