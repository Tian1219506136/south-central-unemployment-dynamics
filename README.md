# Regional Unemployment Dynamics and Cross-State Spillovers — South-Central U.S.

Time-series analysis of monthly unemployment across seven South-Central states, combining
univariate forecasting with multi-state vector autoregressions to ask how a labor-market shock
in one state transmits to its neighbors.

**States:** Texas, Oklahoma, Arkansas, Kansas, Missouri, Nebraska, New Mexico
**Methods:** ARIMA / SARIMA, VAR, orthogonal impulse responses, forecast error variance decomposition
**Data:** FRED monthly unemployment rates, seasonally adjusted and non-adjusted
**Stack:** R and Python

---

## Project type and contribution

Completed as a team coursework project. I was the primary contributor, responsible for code
implementation, empirical modeling, data analysis, result interpretation, and most of the
written report. This repository is the portfolio version, maintained by me.

## Research questions

1. What model class actually forecasts these series well, and does seasonal adjustment change
   the answer?
2. When unemployment moves in one state, do neighboring states follow — and how strongly?

## Method

**Univariate.** Estimated and compared ARIMA and SARIMA specifications on seasonally adjusted
and non-adjusted series separately. Selection was based on stationarity testing and residual
diagnostics, not in-sample fit alone.

**Multivariate.** Estimated a seven-state VAR(5) and a focused Texas–Oklahoma–Arkansas VAR(6).
Used orthogonal impulse response functions and forecast error variance decomposition to trace
and quantify cross-state transmission.

**Forecast.** Produced near-term regional forecasts from the selected specification.

## Results

### Texas is the region's transmitting state

![Orthogonal impulse response, Texas to Oklahoma](figures/cross%20state/Orthogonal%20Impulse%20Response%20from%20Texas%20to%20Oklahoma.png)

Shocks originating in Texas produce measurable responses in neighboring states. Texas acts as an
important transmitting state in the regional unemployment system.

### Where forecast error actually comes from

![FEVD, three-state system](figures/cross%20state/FEVD%20for%203%20states.png)

Variance decomposition in the Texas–Oklahoma–Arkansas system shows how much of each state's
forecast error is attributable to its neighbors rather than to its own history.

### Regional forecast

![VAR(5) forecast, Texas](figures/cross%20state/All%20state%20VAR%285%29%20Forecast%3A%20Texas%20SA%20Unemployment%20Rate.png)

### State-level differences are large

- **Nebraska** shows the most stable unemployment pattern in the region.
- **New Mexico** shows stronger volatility and greater seasonal sensitivity.
- Non-seasonally-adjusted series required explicit seasonal ARIMA treatment, while seasonally
  adjusted series were well captured by simpler non-seasonal specifications. Applying one model
  family to both would have been a specification error.

Per-state diagnostics and forecasts are in `figures/`, organized by state.

## Limitations

- A reduced-form VAR identifies **conditional dynamic associations**, not causal effects. The
  impulse responses describe how these series have historically co-moved; they are not the
  causal effect of a policy or an exogenous shock.
- Orthogonal impulse responses depend on variable ordering. Results are reported under the
  stated ordering and are not invariant to it.
- The sample spans multiple structural breaks. Parameters are assumed stable within estimation
  windows, which is an approximation.
- Forecast evaluation uses a fixed out-of-sample split rather than a full rolling-origin design.

## Repository structure

```text
.
├── README.md
├── code/       # ARIMA/SARIMA and VAR estimation
├── data/       # FRED series
├── figures/    # per-state diagnostics + cross-state IRF/FEVD
└── report/     # full written report
```

## Reproducing

Place the FRED series in `data/` and run the scripts in `code/`. The full written report,
including the complete set of diagnostics, is in `report/`.
