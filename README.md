# south-central-unemployment-dynamics
Time-series analysis of unemployment dynamics across seven South-Central U.S. states using ARIMA, SARIMA, VAR, IRF, FEVD, and forecasting models.
# Unemployment Dynamics in the South-Central United States
# Unemployment Dynamics in the South-Central United States

## Overview

This project analyzes unemployment dynamics across seven South-Central U.S. states: Texas, Oklahoma, Arkansas, Kansas, Missouri, Nebraska, and New Mexico. The project combines single-state time-series modeling with cross-state regional analysis to study unemployment persistence, seasonality, labor-market shocks, interstate spillovers, and short-term forecasting performance.

## Project Type and Contribution Statement

This project was completed as a team coursework project. I was the primary contributor responsible for code implementation, empirical modeling, data analysis, result interpretation, and most of the written report. Other team members mainly contributed to presentation preparation and oral delivery.

This repository is maintained by me as a portfolio version of the project.

## Data

The project uses monthly unemployment-rate data from the Federal Reserve Economic Data database. Both non-seasonally adjusted and seasonally adjusted unemployment-rate series are analyzed.

## Methods

- ARIMA and SARIMA modeling
- Seasonal and non-seasonal differencing
- Stationarity analysis
- Residual diagnostics
- Vector Autoregression
- Impulse Response Functions
- Forecast Error Variance Decomposition
- Regional unemployment forecasting

## Key Findings

- Non-seasonally adjusted unemployment series require seasonal ARIMA treatment.
- Seasonally adjusted unemployment series can generally be modeled with simpler ARIMA specifications.
- Nebraska shows the most stable unemployment pattern in the region.
- New Mexico shows stronger volatility and seasonal sensitivity.
- Texas acts as an important transmitting state in the regional unemployment system.
- The regional forecast suggests a broadly stable short-run unemployment outlook.

## Repository Structure

```text
.
├── README.md
├── code/
├── report/
├── figures/
└── data/
```

## Skills Demonstrated

- Time-series econometrics
- ARIMA and SARIMA modeling
- VAR modeling
- Forecasting
- Labor-market analysis
- Statistical diagnostics
- Economic interpretation
- R / Python empirical workflow

## Report

The full written report is available in the `report/` folder.