# ===================
# ===================
# Step 0 part 1: install functions_cleaning r files
# ===================
# ===================

source("functions_cleaning.R")
library(dplyr)
install.packages("tseries")
library(tseries)

# ===================
# ===================
# Step 0 part 2: clean 7 states
# ===================
# ===================

# Kansas
kansas_sa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state SA csv/KSUR.csv",
  value_col = "KSUR",
  state_name = "Kansas",
  type_label = "SA"
)
kansas_nsa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state NSA csv/KSURN.csv",
  value_col = "KSURN",
  state_name = "Kansas",
  type_label = "NSA"
)
kansas_both <- bind_rows(kansas_sa, kansas_nsa)
head(kansas_both)
table(kansas_both$data_type)

#Arkansas
arkansas_sa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state SA csv/ARUR.csv",
  value_col = "ARUR",
  state_name = "Arkansas",
  type_label = "SA"
)
arkansas_nsa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state NSA csv/ARURN.csv",
  value_col = "ARURN",
  state_name = "Arkansas",
  type_label = "NSA"
)
arkansas_both <- bind_rows(arkansas_sa, arkansas_nsa)
head(arkansas_both)
table(arkansas_both$data_type)

#Nebraska
nebraska_sa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state SA csv/NEUR.csv",
  value_col = "NEUR",
  state_name = "Nebraska",
  type_label = "SA"
)
nebraska_nsa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state NSA csv/NEURN.csv",
  value_col = "NEURN",
  state_name = "Nebraska",
  type_label = "NSA"
)
nebraska_both <- bind_rows(nebraska_sa, nebraska_nsa)
head(nebraska_both)
table(nebraska_both$data_type)

#New Mexico
newmexico_sa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state SA csv/NMUR.csv",
  value_col = "NMUR",
  state_name = "New Mexico",
  type_label = "SA"
)
newmexico_nsa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state NSA csv/NMURN.csv",
  value_col = "NMURN",
  state_name = "New Mexico",
  type_label = "NSA"
)
newmexico_both <- bind_rows(newmexico_sa, newmexico_nsa)
head(newmexico_both)
table(newmexico_both$data_type)

#Missouri
missouri_sa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state SA csv/MOUR.csv",
  value_col = "MOUR",
  state_name = "Missouri",
  type_label = "SA"
)
missouri_nsa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state NSA csv/MOURN.csv",
  value_col = "MOURN",
  state_name = "Missouri",
  type_label = "NSA"
)
missouri_both <- bind_rows(missouri_sa, missouri_nsa)
head(missouri_both)
table(missouri_both$data_type)

#Oklahoma
oklahoma_sa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state SA csv/OKUR.csv",
  value_col = "OKUR",
  state_name = "Oklahoma",
  type_label = "SA"
)
oklahoma_nsa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state NSA csv/OKURN.csv",
  value_col = "OKURN",
  state_name = "Oklahoma",
  type_label = "NSA"
)
oklahoma_both <- bind_rows(oklahoma_sa, oklahoma_nsa)
head(oklahoma_both)
table(oklahoma_both$data_type)

#Texas
texas_sa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state SA csv/TXUR.csv",
  value_col = "TXUR",
  state_name = "Texas",
  type_label = "SA"
)
texas_nsa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state NSA csv/TXURN.csv",
  value_col = "TXURN",
  state_name = "Texas",
  type_label = "NSA"
)
texas_both <- bind_rows(texas_sa, texas_nsa)
head(texas_both)
table(texas_both$data_type)

# ===================
# ===================
# Step 0 part 3: build master dataset
# ===================
# ===================

all_states_both <- bind_rows(
  arkansas_both,
  kansas_both,
  missouri_both,
  nebraska_both,
  newmexico_both,
  oklahoma_both,
  texas_both
)
all_states_sa <- all_states_both %>%
  filter(data_type == "SA")

head(all_states_both)
head(all_states_sa)
unique(all_states_both$state)
table(all_states_both$data_type, useNA = "ifany")
all_states_both %>% count(state, data_type)
range(all_states_both$date)

# ===================
# ===================
# Step 1: filter the single state NSA csv: Oklahoma
# ===================
# ===================

oklahoma_nsa_only <- all_states_both %>%
  filter(state == "Oklahoma", data_type == "NSA")

# ===================
# ===================
# Step 2: Oklahoma NSA: STL + Unit Root Tests + Differencing Identification for SARIMA
# =================
# ===================

# 0. Load packages
library(ggplot2)
library(forecast)
# 1. Prepare Oklahoma NSA data
oklahoma_nsa_only <- all_states_both %>%
  filter(state == "Oklahoma", data_type == "NSA")

m <- 12
y_ts <- ts(oklahoma_nsa_only$value, start = c(1995, 1), frequency = m)
# 2. STL decomposition
oklahoma_nsa_stl <- stl(y_ts, s.window = "periodic")
plot(oklahoma_nsa_stl, main = "STL Decomposition of Oklahoma Unemployment Rate (NSA)")

oklahoma_stl_df <- data.frame(
  date = oklahoma_nsa_only$date,
  observed = as.numeric(y_ts),
  seasonal = as.numeric(oklahoma_nsa_stl$time.series[, "seasonal"]),
  trend = as.numeric(oklahoma_nsa_stl$time.series[, "trend"]),
  remainder = as.numeric(oklahoma_nsa_stl$time.series[, "remainder"])
)

head(oklahoma_stl_df)
# 3. Unit root test function
perform_unit_root_tests <- function(series, series_name = "Series") {
  adf_result <- adf.test(series)
  kpss_result <- kpss.test(series, null = "Level")
  
  adf_stationary <- adf_result$p.value < 0.05
  kpss_stationary <- kpss_result$p.value >= 0.05
  
  return(list(
    adf = adf_result,
    kpss = kpss_result,
    adf_stationary = adf_stationary,
    kpss_stationary = kpss_stationary
  ))
}
# 4. Test original level series
test_level <- perform_unit_root_tests(as.numeric(y_ts), "Oklahoma NSA Level Series")
# 5. Differencing strategy
# Automatic suggestions
D_auto <- nsdiffs(y_ts)                     # seasonal differencing suggestion
y_seas_temp <- if (D_auto > 0) diff(y_ts, lag = m, differences = D_auto) else y_ts
d_auto <- ndiffs(y_seas_temp)               # regular differencing suggestion after seasonal adjustment
# Manual settings to compare
# Try this first:
D_hat <- 0
d_hat <- 1
# 6. Build transformed series
# A. First difference only
oklahoma_nsa_diff1_df <- oklahoma_nsa_only %>%
  mutate(diff1 = c(NA, diff(value))) %>%
  filter(!is.na(diff1))
oklahoma_nsa_diff1 <- oklahoma_nsa_diff1_df$diff1

# B. Seasonal difference only (lag 12)
oklahoma_nsa_diff12_df <- oklahoma_nsa_only %>%
  mutate(diff12 = value - lag(value, 12)) %>%
  filter(!is.na(diff12))
oklahoma_nsa_diff12 <- oklahoma_nsa_diff12_df$diff12

# C. First + seasonal difference
oklahoma_nsa_diff1_diff12_df <- oklahoma_nsa_diff1_df %>%
  mutate(diff1_diff12 = diff1 - lag(diff1, 12)) %>%
  filter(!is.na(diff1_diff12))

oklahoma_nsa_diff1_diff12 <- oklahoma_nsa_diff1_diff12_df$diff1_diff12
# 7. Plot transformed series
ggplot(oklahoma_nsa_diff1_df, aes(x = date, y = diff1)) +
  geom_line() +
  labs(
    title = "Oklahoma NSA First Difference",
    x = "Date",
    y = "First Difference"
  )

ggplot(oklahoma_nsa_diff12_df, aes(x = date, y = diff12)) +
  geom_line() +
  labs(
    title = "Oklahoma NSA Seasonal Difference (lag 12)",
    x = "Date",
    y = "Seasonal Difference"
  )

ggplot(oklahoma_nsa_diff1_diff12_df, aes(x = date, y = diff1_diff12)) +
  geom_line() +
  labs(
    title = "Oklahoma NSA First + Seasonal Difference",
    x = "Date",
    y = "First + Seasonal Difference"
  )
# 8. Unit root tests on transformed series
test_diff1 <- perform_unit_root_tests(oklahoma_nsa_diff1, "Oklahoma NSA First-Differenced Series")
test_diff12 <- perform_unit_root_tests(oklahoma_nsa_diff12, "Oklahoma NSA Seasonal-Differenced Series (lag 12)")
test_diff1_diff12 <- perform_unit_root_tests(
  oklahoma_nsa_diff1_diff12,
  "Oklahoma NSA First + Seasonal-Differenced Series"
)
# 9. ACF / PACF comparison
op <- par(mfrow = c(1, 2))
acf(oklahoma_nsa_diff1, lag.max = 36, main = "Oklahoma NSA First Difference: ACF")
pacf(oklahoma_nsa_diff1, lag.max = 36, main = "Oklahoma NSA First Difference: PACF")
par(op)

op <- par(mfrow = c(1, 2))
acf(oklahoma_nsa_diff12, lag.max = 36, main = "Oklahoma NSA Seasonal Difference: ACF")
pacf(oklahoma_nsa_diff12, lag.max = 36, main = "Oklahoma NSA Seasonal Difference: PACF")
par(op)

op <- par(mfrow = c(1, 2))
acf(oklahoma_nsa_diff1_diff12, lag.max = 36, main = "Oklahoma NSA First + Seasonal Difference: ACF")
pacf(oklahoma_nsa_diff1_diff12, lag.max = 36, main = "Oklahoma NSA First + Seasonal Difference: PACF")
par(op)

# ===================
# ===================
# # STEP 3: APPLY DIFFERENCING TO ACHIEVE STATIONARITY
# Chosen working structure: d = 1, D = 1, m = 12
# ===================
# ===================

# 1. Automatic suggestions from forecast package
m <- 12
D_auto <- if (m > 1) nsdiffs(y_ts) else 0
y_seas_temp <- if (D_auto > 0) diff(y_ts, lag = m, differences = D_auto) else y_ts
d_auto <- ndiffs(y_seas_temp)

# 2. Manual selection for Oklahoma NSA
# seasonal difference only: not enough
# first + seasonal difference: best working choice

D_hat <- 1
d_hat <- 1

# 3. Apply seasonal differencing first
if (D_hat > 0) {
  y_unseas <- diff(y_ts, lag = m, differences = D_hat)
} else {
  y_unseas <- y_ts
}

# Test after seasonal differencing
if (D_hat > 0) {
  test_seas <- perform_unit_root_tests(as.numeric(y_unseas), "Oklahoma NSA Seasonally Differenced Series")
}

# 4. Apply regular differencing
if (d_hat > 0) {
  w_ts <- diff(y_unseas, differences = d_hat)
} else {
  w_ts <- y_unseas
}

# 5. Test the final working series
test_working <- perform_unit_root_tests(as.numeric(w_ts), "Oklahoma NSA Working Series")

# 6. Plot the differencing progression
op <- par(mfrow = c(2, 2))

plot(y_ts,
     main = "1. Original Oklahoma NSA Series",
     ylab = "Level",
     xlab = "Time",
     col = "steelblue",
     lwd = 1.5)
grid(col = "gray80")

plot(y_unseas,
     main = sprintf("2. After Seasonal Differencing (D = %d)", D_hat),
     ylab = "Seasonally Differenced",
     xlab = "Time",
     col = "darkgreen",
     lwd = 1.5)
grid(col = "gray80")

plot(w_ts,
     main = sprintf("3. Working Series (d = %d, D = %d)", d_hat, D_hat),
     ylab = "Working Series",
     xlab = "Time",
     col = "darkred",
     lwd = 1.5)
abline(h = mean(w_ts), col = "blue", lty = 2, lwd = 2)
legend("topright", "Mean", col = "blue", lty = 2, bty = "n")
grid(col = "gray80")

hist(as.numeric(w_ts),
     breaks = "FD",
     freq = FALSE,
     main = "4. Distribution of Working Series",
     xlab = "Value",
     col = "lightblue",
     border = "white")
lines(density(as.numeric(w_ts)), col = "darkred", lwd = 2)
curve(dnorm(x, mean(as.numeric(w_ts)), sd(as.numeric(w_ts))),
      add = TRUE, col = "blue", lty = 2, lwd = 2)
legend("topright",
       c("Empirical", "Normal"),
       col = c("darkred", "blue"),
       lty = c(1, 2),
       lwd = 2,
       bty = "n")

par(op)

# 7. ACF / PACF of final working series
op <- par(mfrow = c(2, 2))

Acf(w_ts, lag.max = 24,
    main = "Oklahoma NSA Working Series: ACF (Non-seasonal Focus)",
    col = "steelblue")

Pacf(w_ts, lag.max = 24,
     main = "Oklahoma NSA Working Series: PACF (Non-seasonal Focus)",
     col = "steelblue")

Acf(w_ts, lag.max = 72,
    main = "Oklahoma NSA Working Series: ACF (Seasonal Lags)",
    col = "darkgreen")

Pacf(w_ts, lag.max = 72,
     main = "Oklahoma NSA Working Series: PACF (Seasonal Lags)",
     col = "darkgreen")

par(op)

# 8. Save identification results for next step
identification_nsa <- list(
  y_ts = y_ts,
  y_unseas = y_unseas,
  w_ts = w_ts,
  d_hat = d_hat,
  D_hat = D_hat,
  m = m,
  D_auto = D_auto,
  d_auto = d_auto,
  test_level = test_level,
  test_seas = if (exists("test_seas")) test_seas else NULL,
  test_working = test_working
)

# STORE IDENTIFICATION RESULTS FOR NEXT STEP
identification <- list(
  y_ts = y_ts,
  w_ts = w_ts,
  d_hat = d_hat,
  D_hat = D_hat,
  m = m,
  test_level = test_level,
  test_working = test_working,
  candidate_models = list(
    c(1, 1, 0, 1, 1, 0, 12),
    c(0, 1, 1, 0, 1, 1, 12),
    c(1, 1, 1, 0, 1, 1, 12)
  )
)

# ===================
# ===================
# Step 4 : Oklahoma NSA SARIMA Estimation
# ===================
# ===================

# 0. Input from identification step
y_ts <- identification_nsa$y_ts
w_ts <- identification_nsa$w_ts
d_hat <- identification_nsa$d_hat
D_hat <- identification_nsa$D_hat
m <- identification_nsa$m
# 1. Helper function: AICc
AICc_ <- function(fit) {
  a <- AIC(fit)
  n <- tryCatch(stats::nobs(fit), error = function(e) sum(is.finite(residuals(fit))))
  k <- length(coef(fit)) + 1
  if (!is.finite(a) || !is.finite(n) || !is.finite(k) || (n - k - 1) <= 0) {
    return(a)
  }
  a + 2 * k * (k + 1) / (n - k - 1)
}
# 2. Fit candidate SARIMA models
fit_110_110 <- Arima(
  y_ts,
  order = c(1, d_hat, 0),
  seasonal = list(order = c(1, D_hat, 0), period = m),
  method = "ML"
)

fit_011_011 <- Arima(
  y_ts,
  order = c(0, d_hat, 1),
  seasonal = list(order = c(0, D_hat, 1), period = m),
  method = "ML"
)

fit_111_011 <- Arima(
  y_ts,
  order = c(1, d_hat, 1),
  seasonal = list(order = c(0, D_hat, 1), period = m),
  method = "ML"
)
# 3. Compare models
compare_nsa <- data.frame(
  Model = c(
    "SARIMA(1,1,0)(1,1,0)[12]",
    "SARIMA(0,1,1)(0,1,1)[12]",
    "SARIMA(1,1,1)(0,1,1)[12]"
  ),
  AIC = c(AIC(fit_110_110), AIC(fit_011_011), AIC(fit_111_011)),
  AICc = c(AICc_(fit_110_110), AICc_(fit_011_011), AICc_(fit_111_011)),
  BIC = c(BIC(fit_110_110), BIC(fit_011_011), BIC(fit_111_011))
)

compare_nsa <- compare_nsa[order(compare_nsa$AICc), ]
rownames(compare_nsa) <- NULL
print(compare_nsa)

# 4. Pick best model by AICc
best_model_name_nsa <- compare_nsa$Model[1]

best_model_nsa <- switch(
  best_model_name_nsa,
  "SARIMA(1,1,0)(1,1,0)[12]" = fit_110_110,
  "SARIMA(0,1,1)(0,1,1)[12]" = fit_011_011,
  "SARIMA(1,1,1)(0,1,1)[12]" = fit_111_011
)

# 5. Residual diagnostics
best_resid_nsa <- residuals(best_model_nsa)

op <- par(mfrow = c(1, 2))
plot(best_resid_nsa, type = "l",
     main = paste("Residuals of", best_model_name_nsa),
     xlab = "Time", ylab = "Residuals")
acf(best_resid_nsa, lag.max = 36,
    main = paste("Residual ACF of", best_model_name_nsa))
par(op)

lb24_nsa <- Box.test(best_resid_nsa, lag = 24, type = "Ljung-Box")
lb36_nsa <- Box.test(best_resid_nsa, lag = 36, type = "Ljung-Box")

print(lb24_nsa)
print(lb36_nsa)

# 6. Forecast
forecast_nsa_12 <- forecast(best_model_nsa, h = 12)

plot(forecast_nsa_12,
     main = paste("12-Month Forecast from", best_model_name_nsa),
     xlab = "Time",
     ylab = "Oklahoma Unemployment Rate (NSA)",
     cex.main = 1
)
# 7. Save results
estimation_nsa <- list(
  compare_table = compare_nsa,
  fit_110_110 = fit_110_110,
  fit_011_011 = fit_011_011,
  fit_111_011 = fit_111_011,
  best_model_name = best_model_name_nsa,
  best_model = best_model_nsa,
  lb24 = lb24_nsa,
  lb36 = lb36_nsa,
  forecast_12 = forecast_nsa_12
)

# =====================
# =====================
# Step 5: Loading Oklahoma NSA fitted model
# =====================
# =====================

if (!exists("final_fit")) {
  if (exists("estimation_nsa") && is.list(estimation_nsa) && !is.null(estimation_nsa$best_model)) {
    final_fit <- estimation_nsa$best_model
  } else {
    stop("No Oklahoma NSA fitted model found. Run the NSA estimation step first.")
  }
}

resid_vals <- residuals(final_fit)
fitted_vals <- fitted(final_fit)
m <- frequency(fitted_vals)
n <- length(resid_vals)

arma_vec <- final_fit$arma
p <- arma_vec[1]; q <- arma_vec[2]; P <- arma_vec[3]; Q <- arma_vec[4]
k_armaterms <- p + q + P + Q

# ===================
# ===================
# Step 6 : Diagnostic Checking
# ===================
# ===================

library(zoo)
# Optional: FinTS for ARCH LM test
if (!requireNamespace("FinTS", quietly = TRUE)) {
  message("Optional: install.packages('FinTS') for formal ARCH test")
}
# STEP 0: Load fitted Texas NSA model

if (!exists("final_fit")) {
  if (exists("estimation_nsa") && is.list(estimation_nsa) && !is.null(estimation_nsa$best_model)) {
    final_fit <- estimation_nsa$best_model
  } else {
    stop("No Oklahoma NSA fitted model found. Run the Oklahoma NSA estimation step first.")
  }
}
# Extract residuals and model info
resid_vals <- residuals(final_fit)
fitted_vals <- fitted(final_fit)
m <- frequency(fitted_vals)
n <- length(resid_vals)

# final_fit$arma layout: [p, q, P, Q, m, d, D]
arma_vec <- final_fit$arma
p <- arma_vec[1]
q <- arma_vec[2]
P <- arma_vec[3]
Q <- arma_vec[4]
k_armaterms <- p + q + P + Q

# SECTION A: Visual residual analysis

op <- par(no.readonly = TRUE)
on.exit(par(op), add = TRUE)

# 4-panel diagnostics
par(mfrow = c(2, 2))

# Plot 1: residuals over time
plot(resid_vals, type = "l", col = "steelblue",
     main = "1. Oklahoma NSA Residuals Over Time",
     ylab = "Residual", xlab = "Time")
abline(h = 0, col = "red", lty = 2, lwd = 2)
abline(h = c(-2, 2) * sqrt(final_fit$sigma2), col = "orange", lty = 3)
legend("topright",
       c("Mean = 0", "±2σ bands"),
       col = c("red", "orange"),
       lty = c(2, 3),
       bty = "n",
       cex = 0.8)
grid(col = "gray80")

# Plot 2: histogram + density
hist(resid_vals, breaks = "FD", freq = FALSE,
     col = "lightblue", border = "white",
     main = "2. Oklahoma NSA Residual Histogram",
     xlab = "Residual")
lines(density(resid_vals, na.rm = TRUE), col = "darkblue", lwd = 2)
curve(dnorm(x, mean = mean(resid_vals), sd = sd(resid_vals)),
      add = TRUE, col = "red", lwd = 2, lty = 2)
legend("topright",
       c("Empirical", "Normal"),
       col = c("darkblue", "red"),
       lwd = 2,
       lty = c(1, 2),
       bty = "n",
       cex = 0.8)

# Plot 3: ACF of residuals
Acf(resid_vals, lag.max = min(48, floor(n/4)),
    main = "3. ACF of Oklahoma NSA Residuals",
    col = "steelblue")

# Plot 4: PACF of residuals
Pacf(resid_vals, lag.max = min(48, floor(n/4)),
     main = "4. PACF of Oklahoma NSA Residuals",
     col = "steelblue")

par(mfrow = c(1, 2))

# Plot 5: Q-Q plot
qqnorm(resid_vals,
       main = "5. Q-Q Plot of Oklahoma NSA Residuals",
       col = "steelblue", pch = 16, cex = 0.7)
qqline(resid_vals, col = "red", lwd = 2)
legend("topleft", "Reference line", col = "red", lwd = 2, bty = "n", cex = 0.8)

# Plot 6: squared residual ACF
Acf(resid_vals^2, lag.max = min(48, floor(n/4)),
    main = expression(paste("6. ACF of ", Residuals^2, " (ARCH check)")),
    col = "darkgreen")

par(mfrow = c(1, 1))

# SECTION B: Ljung-Box test for residual autocorrelation

lb_lags <- c(12, 24, 36, 48)
lb_results <- lapply(lb_lags, function(L) {
  if (L <= k_armaterms) {
    return(data.frame(
      lag = L,
      statistic = NA,
      df = NA,
      p.value = NA,
      conclusion = "Insufficient df"
    ))
  }
  
  test <- Box.test(resid_vals, lag = L, type = "Ljung-Box", fitdf = k_armaterms)
  
  data.frame(
    lag = L,
    statistic = round(unname(test$statistic), 2),
    df = unname(test$parameter),
    p.value = round(test$p.value, 4),
    conclusion = ifelse(test$p.value > 0.05, "PASS ✓", "FAIL ✗")
  )
})

lb_tab <- do.call(rbind, lb_results)

print(lb_tab, row.names = FALSE)

if (all(lb_tab$p.value > 0.05, na.rm = TRUE)) {
} else {
}
# SECTION C: Mean-zero check

mean_test <- t.test(resid_vals, mu = 0)

# SECTION D: Normality check

jb_test <- jarque.bera.test(resid_vals)

# SECTION E: ARCH / volatility clustering check

if (requireNamespace("FinTS", quietly = TRUE)) {
  arch_test <- FinTS::ArchTest(resid_vals, lags = 12)
  print(arch_test)
} else {
}
# FINAL DIAGNOSTIC SUMMARY
# Save diagnostics results
diagnostics_nsa <- list(
  final_fit = final_fit,
  residuals = resid_vals,
  ljung_box = lb_tab,
  mean_test = mean_test,
  jb_test = jb_test
)