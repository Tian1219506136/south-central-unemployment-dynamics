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
# Step 1: filter the single state SA csv: Nebraska
# ===================
# ===================

nebraska_sa_only <- all_states_both %>%
  filter(state == "Nebraska", data_type == "SA")

# ===================
# ===================
# Step 2: Nebraska SA - Unit Root Tests + Differencing Identification for ARIMA
# ===================
# ===================

library(ggplot2)
library(forecast)


# 1. Prepare Nebraska SA data
nebraska_sa_only <- all_states_both %>%
  filter(state == "Nebraska", data_type == "SA")

m <- 12
y_ts <- ts(nebraska_sa_only$value, start = c(1995, 1), frequency = m)

# 2. Plot original Nebraska SA series
plot(y_ts,
     main = "Nebraska Unemployment Rate (SA)",
     ylab = "Unemployment Rate",
     xlab = "Time")
grid()

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
test_level <- perform_unit_root_tests(as.numeric(y_ts), "Nebraska SA Level Series")

# 5. Differencing strategy
D_auto <- 0
d_auto <- ndiffs(y_ts)

D_hat <- 0
d_hat <- 1

# 6. Build transformed series: first difference only
nebraska_sa_diff1_df <- nebraska_sa_only %>%
  mutate(diff1 = c(NA, diff(value))) %>%
  filter(!is.na(diff1))

nebraska_sa_diff1 <- nebraska_sa_diff1_df$diff1

# 7. Plot first-differenced series
ggplot(nebraska_sa_diff1_df, aes(x = date, y = diff1)) +
  geom_line() +
  labs(
    title = "Nebraska SA First Difference",
    x = "Date",
    y = "First Difference"
  )

# 8. Unit root test on differenced series
test_diff1 <- perform_unit_root_tests(
  nebraska_sa_diff1,
  "Nebraska SA First-Differenced Series"
)

# 9. ACF / PACF on first-differenced series
op <- par(mfrow = c(1, 2))
acf(nebraska_sa_diff1, lag.max = 36, main = "Nebraska SA First Difference: ACF")
pacf(nebraska_sa_diff1, lag.max = 36, main = "Nebraska SA First Difference: PACF")
par(op)

# 10. Save identification results
w_ts <- nebraska_sa_diff1

identification_sa <- list(
  y_ts = y_ts,
  w_ts = w_ts,
  d_hat = d_hat,
  D_hat = D_hat,
  m = m,
  D_auto = D_auto,
  d_auto = d_auto,
  test_level = test_level,
  test_working = test_diff1
)

# ===================
# ===================
# STEP 3: APPLY DIFFERENCING TO ACHIEVE STATIONARITY
# Nebraska SA version: chosen working structure d = 1, D = 0
# ===================
# ===================

# 1. Automatic suggestions
m <- 12
D_auto <- 0
d_auto <- ndiffs(y_ts)

# 2. Manual selection for Nebraska SA
D_hat <- 0
d_hat <- 1

# 3. No seasonal differencing for SA
y_unseas <- y_ts

# 4. Apply regular differencing
if (d_hat > 0) {
  w_ts <- diff(y_unseas, differences = d_hat)
} else {
  w_ts <- y_unseas
}

# 5. Test the final working series
test_working <- perform_unit_root_tests(as.numeric(w_ts), "Nebraska SA Working Series")

# 6. Plot the differencing progression
op <- par(mfrow = c(2, 2))

plot(y_ts,
     main = "1. Original Nebraska SA Series",
     ylab = "Level",
     xlab = "Time",
     col = "steelblue",
     lwd = 1.5)
grid(col = "gray80")

plot(w_ts,
     main = sprintf("2. Working Series (d = %d, D = %d)", d_hat, D_hat),
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
     main = "3. Distribution of Working Series",
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

acf(w_ts,
    lag.max = 36,
    main = "4. Nebraska SA Working Series: ACF")
grid(col = "gray80")

par(op)

# 7. PACF of final working series
pacf(w_ts,
     lag.max = 36,
     main = "Nebraska SA Working Series: PACF")
grid(col = "gray80")

# 8. Save identification results for next step
identification_sa <- list(
  y_ts = y_ts,
  w_ts = w_ts,
  d_hat = d_hat,
  D_hat = D_hat,
  m = m,
  D_auto = D_auto,
  d_auto = d_auto,
  test_level = test_level,
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
    c(1, 1, 0),
    c(0, 1, 1),
    c(1, 1, 1)
  )
)

# ===================
# ===================
# Step 4: Nebraska SA ARIMA Estimation
# ===================
# ===================

# 0. Input from identification step
y_ts <- identification_sa$y_ts
w_ts <- identification_sa$w_ts
d_hat <- identification_sa$d_hat
D_hat <- identification_sa$D_hat
m <- identification_sa$m

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

# 2. Fit candidate ARIMA models
fit_110 <- Arima(
  y_ts,
  order = c(1, d_hat, 0),
  method = "ML"
)

fit_011 <- Arima(
  y_ts,
  order = c(0, d_hat, 1),
  method = "ML"
)

fit_111 <- Arima(
  y_ts,
  order = c(1, d_hat, 1),
  method = "ML"
)

# 3. Compare models
compare_sa <- data.frame(
  Model = c(
    "ARIMA(1,1,0)",
    "ARIMA(0,1,1)",
    "ARIMA(1,1,1)"
  ),
  AIC = c(AIC(fit_110), AIC(fit_011), AIC(fit_111)),
  AICc = c(AICc_(fit_110), AICc_(fit_011), AICc_(fit_111)),
  BIC = c(BIC(fit_110), BIC(fit_011), BIC(fit_111))
)

compare_sa <- compare_sa[order(compare_sa$AICc), ]
rownames(compare_sa) <- NULL
print(compare_sa)

# 4. Pick best model by AICc
best_model_name_sa <- compare_sa$Model[1]

best_model_sa <- switch(
  best_model_name_sa,
  "ARIMA(1,1,0)" = fit_110,
  "ARIMA(0,1,1)" = fit_011,
  "ARIMA(1,1,1)" = fit_111
)

# 5. Residual diagnostics
best_resid_sa <- residuals(best_model_sa)

op <- par(mfrow = c(1, 2))
plot(best_resid_sa, type = "l",
     main = paste("Residuals of", best_model_name_sa),
     xlab = "Time", ylab = "Residuals")
acf(best_resid_sa, lag.max = 36,
    main = paste("Residual ACF of", best_model_name_sa))
par(op)

lb24_sa <- Box.test(best_resid_sa, lag = 24, type = "Ljung-Box")
lb36_sa <- Box.test(best_resid_sa, lag = 36, type = "Ljung-Box")

print(lb24_sa)
print(lb36_sa)

# 6. Forecast
forecast_sa_12 <- forecast(best_model_sa, h = 12)

par(mar = c(5, 6, 4, 2))
plot(
  forecast_sa_12,
  main = "Nebraska SA - 12-Month Forecast",
  xlab = "Time",
  ylab = "Nebraska Unemployment Rate (SA)",
  cex.main = 1
)

# 7. Save results
estimation_sa <- list(
  compare_table = compare_sa,
  fit_110 = fit_110,
  fit_011 = fit_011,
  fit_111 = fit_111,
  best_model_name = best_model_name_sa,
  best_model = best_model_sa,
  lb24 = lb24_sa,
  lb36 = lb36_sa,
  forecast_12 = forecast_sa_12
)

# =====================
# =====================
# Step 5: Loading Nebraska SA fitted model
# =====================
# =====================

if (!exists("final_fit")) {
  if (exists("estimation_sa") && is.list(estimation_sa) && !is.null(estimation_sa$best_model)) {
    final_fit <- estimation_sa$best_model
  } else {
    stop("No Nebraska SA fitted model found. Run the SA estimation step first.")
  }
}

resid_vals <- residuals(final_fit)
fitted_vals <- fitted(final_fit)
m <- frequency(fitted_vals)
n <- length(resid_vals)

arma_vec <- final_fit$arma
p <- arma_vec[1]
q <- arma_vec[2]
P <- arma_vec[3]
Q <- arma_vec[4]
k_armaterms <- p + q + P + Q

# ===================
# ===================
# Step 6: Nebraska SA Diagnostic Checking
# ===================
# ===================

library(forecast)
library(tseries)
library(zoo)

# Optional: FinTS for ARCH LM test
if (!requireNamespace("FinTS", quietly = TRUE)) {
  message("Optional: install.packages('FinTS') for formal ARCH test")
}

# STEP 0: Load fitted Nebraska SA model
if (!exists("final_fit")) {
  if (exists("estimation_sa") && is.list(estimation_sa) && !is.null(estimation_sa$best_model)) {
    final_fit <- estimation_sa$best_model
  } else {
    stop("No Nebraska SA fitted model found. Run the Nebraska SA estimation step first.")
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

par(mfrow = c(2, 2))

# Plot 1: residuals over time
plot(resid_vals, type = "l", col = "steelblue",
     main = "1. Nebraska SA Residuals Over Time",
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
     main = "2. Nebraska SA Residual Histogram",
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
    main = "3. ACF of Nebraska SA Residuals",
    col = "steelblue")

# Plot 4: PACF of residuals
Pacf(resid_vals, lag.max = min(48, floor(n/4)),
     main = "4. PACF of Nebraska SA Residuals",
     col = "steelblue")

par(mfrow = c(1, 2))

# Plot 5: Q-Q plot
qqnorm(resid_vals,
       main = "5. Q-Q Plot of Nebraska SA Residuals",
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

# SECTION C: Mean-zero check
mean_test <- t.test(resid_vals, mu = 0)
print(mean_test)

# SECTION D: Normality check
jb_test <- jarque.bera.test(resid_vals)
print(jb_test)

# SECTION E: ARCH / volatility clustering check
if (requireNamespace("FinTS", quietly = TRUE)) {
  arch_test <- FinTS::ArchTest(resid_vals, lags = 12)
  print(arch_test)
} else {
  arch_test <- NULL
}

# Save diagnostics results
diagnostics_sa <- list(
  final_fit = final_fit,
  residuals = resid_vals,
  ljung_box = lb_tab,
  mean_test = mean_test,
  jb_test = jb_test,
  arch_test = arch_test
)