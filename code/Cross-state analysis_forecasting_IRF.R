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

# ==============
# ==============
# Step 1: build SA wide dataset for cross-state analysis
# ==============
# ==============

library(tidyr)
all_states_sa_wide <- all_states_sa %>%
  select(date, state, value) %>%
  pivot_wider(names_from = state, values_from = value) %>%
  arrange(date)

head(all_states_sa_wide)
dim(all_states_sa_wide)

colnames(all_states_sa_wide)
range(all_states_sa_wide$date)
summary(all_states_sa_wide)

# ==============
# ==============
# Step 2: VAR
# ==============
# ==============
# ============================================================
# Cross-State VAR for 7 South Central States (SA unemployment)
# Sample: 1995-01 to 2024-12
# ============================================================

# 0) Packages
pkgs <- c("vars", "urca", "ggplot2", "scales", "zoo", "dplyr", "tidyr")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# 1) Prepare wide data
var_df <- all_states_sa_wide %>%
  rename(New_Mexico = `New Mexico`) %>%   
  arrange(date)

# check
head(var_df)
dim(var_df)
colnames(var_df)

# 2) Build multivariate monthly ts object
Y_df <- var_df %>% dplyr::select(-date)

Y <- ts(
  Y_df,
  start = c(1995, 1),
  frequency = 12
)

# 3) Optional: quick correlation matrix
cor_matrix <- cor(Y_df, use = "complete.obs")
print(round(cor_matrix, 3))

# 4) Optional: ADF summaries for each state
adf_results <- lapply(colnames(Y_df), function(v) {
  test <- ur.df(Y[, v], type = "trend", lags = 12)
  list(series = v, summary = summary(test))
})

adf_results[[which(colnames(Y_df) == "Texas")]]$summary

# 5) VAR lag selection
lagmax <- 6
det_type <- "const"
criterion <- "AIC(n)"   

sel <- VARselect(Y, lag.max = lagmax, type = det_type)
print(sel$selection)

p <- as.numeric(sel$selection[criterion])
if (is.na(p)) p <- as.numeric(sel$selection[1])

message(sprintf("Selected VAR order p = %d by %s.", p, criterion))

# 6) Estimate VAR
var_fit <- VAR(Y, p = p, type = det_type)
summary(var_fit)

# 7) Stability check
eig <- roots(var_fit)
print(eig)

if (all(Mod(eig) < 1)) {
  message("Stability OK: all roots have modulus < 1.")
} else {
  warning("Some roots have modulus >= 1. VAR may be unstable.")
}

# 8) Diagnostics
# residual serial correlation
serial_res <- serial.test(var_fit, lags.pt = 12, type = "PT.asymptotic")
print(serial_res)

# ARCH effects
arch_res <- arch.test(var_fit, lags.multi = 5)
print(arch_res)

# normality
norm_res <- normality.test(var_fit)
print(norm_res)

# 9) Forecast 12 months ahead
h <- 12
fc <- predict(var_fit, n.ahead = h, ci = 0.95)

# 10) Build forecast data frame for plotting
last_ym <- zoo::as.yearmon(tail(var_df$date, 1))
future_ym <- seq(from = last_ym + 1/12, by = 1/12, length.out = h)
future_dates <- as.Date(future_ym, frac = 1)

# Examples: only Texas & Oklahoma
fc_plot_df <- data.frame(
  date = future_dates,
  Texas_fc = fc$fcst$Texas[, "fcst"],
  Texas_lo = fc$fcst$Texas[, "lower"],
  Texas_hi = fc$fcst$Texas[, "upper"],
  Oklahoma_fc = fc$fcst$Oklahoma[, "fcst"],
  Oklahoma_lo = fc$fcst$Oklahoma[, "lower"],
  Oklahoma_hi = fc$fcst$Oklahoma[, "upper"]
)

hist_win <- 60
hist_df <- tail(var_df, hist_win)

theme_clean <- theme_minimal(base_size = 12)

# Texas forecast plot
p_tx <- ggplot() +
  geom_line(data = hist_df, aes(x = date, y = Texas)) +
  geom_ribbon(data = fc_plot_df, aes(x = date, ymin = Texas_lo, ymax = Texas_hi), alpha = 0.2) +
  geom_line(data = fc_plot_df, aes(x = date, y = Texas_fc), linetype = "dashed") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(
    title = sprintf("VAR(%d) Forecast: Texas SA Unemployment Rate", p),
    x = "Date",
    y = "Percent"
  ) +
  theme_clean

print(p_tx)

# Oklahoma forecast plot
p_ok <- ggplot() +
  geom_line(data = hist_df, aes(x = date, y = Oklahoma)) +
  geom_ribbon(data = fc_plot_df, aes(x = date, ymin = Oklahoma_lo, ymax = Oklahoma_hi), alpha = 0.2) +
  geom_line(data = fc_plot_df, aes(x = date, y = Oklahoma_fc), linetype = "dashed") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(
    title = sprintf("VAR(%d) Forecast: Oklahoma SA Unemployment Rate", p),
    x = "Date",
    y = "Percent"
  ) +
  theme_clean

print(p_ok)

# 11) Save results
cross_state_var_results <- list(
  data_wide = var_df,
  Y = Y,
  cor_matrix = cor_matrix,
  lag_selection = sel,
  selected_p = p,
  var_fit = var_fit,
  serial_test = serial_res,
  arch_test = arch_res,
  normality_test = norm_res,
  forecast = fc
)

# ===============
# ===============
# Step 3: Impulse Response Fuction 
# ===============
# ===============

# Texas shock -> Oklahoma response
irf_tx_ok <- irf(
  var_fit,
  impulse = "Texas",
  response = "Oklahoma",
  n.ahead = 12,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)
plot(irf_tx_ok)

# Texas -> Arkansas
irf_tx_ar <- irf(
  var_fit,
  impulse = "Texas",
  response = "Arkansas",
  n.ahead = 12,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)
plot(irf_tx_ar)

# Kansas -> Oklahoma
irf_ks_ok <- irf(
  var_fit,
  impulse = "Kansas",
  response = "Oklahoma",
  n.ahead = 12,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)
plot(irf_ks_ok)

# Missouri -> Kansas
irf_mo_ks <- irf(
  var_fit,
  impulse = "Missouri",
  response = "Kansas",
  n.ahead = 12,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)
plot(irf_mo_ks)


fevd_res <- fevd(var_fit, n.ahead = 12)
plot(fevd_res)

# ===============
# ===============
# Step 4: Three-State Cross-State VAR
# Texas + Oklahoma + Arkansas
# ===============
# ===============

# 1. Build 3-state dataset
var3_df <- var_df %>%
  dplyr::select(date, Texas, Oklahoma, Arkansas)

head(var3_df)
dim(var3_df)

# 2. Convert to multivariate ts
Y3_df <- var3_df %>% dplyr::select(-date)

Y3 <- ts(
  Y3_df,
  start = c(1995, 1),
  frequency = 12
)

# 3. Correlation matrix
cor_matrix_3 <- cor(Y3_df, use = "complete.obs")
print(round(cor_matrix_3, 3))

# 4. Lag selection
lagmax_3 <- 6
det_type_3 <- "const"
criterion_3 <- "AIC(n)"

sel3 <- VARselect(Y3, lag.max = lagmax_3, type = det_type_3)
print(sel3$selection)

p3 <- as.numeric(sel3$selection[criterion_3])
if (is.na(p3)) p3 <- as.numeric(sel3$selection[1])

message(sprintf("Selected 3-state VAR order p = %d by %s.", p3, criterion_3))

# 5. Estimate 3-state VAR
var3_fit <- VAR(Y3, p = p3, type = det_type_3)
summary(var3_fit)

# 6. Stability check
eig3 <- roots(var3_fit)
print(eig3)

if (all(Mod(eig3) < 1)) {
  message("3-state VAR is stable.")
} else {
  warning("3-state VAR may be unstable.")
}

# 7. Diagnostics
serial_res_3 <- serial.test(var3_fit, lags.pt = 12, type = "PT.asymptotic")
print(serial_res_3)

arch_res_3 <- arch.test(var3_fit, lags.multi = 5)
print(arch_res_3)

norm_res_3 <- normality.test(var3_fit)
print(norm_res_3)

# 8. Forecast
h3 <- 12
fc3 <- predict(var3_fit, n.ahead = h3, ci = 0.95)

last_ym_3 <- zoo::as.yearmon(tail(var3_df$date, 1))
future_ym_3 <- seq(from = last_ym_3 + 1/12, by = 1/12, length.out = h3)
future_dates_3 <- as.Date(future_ym_3, frac = 1)

fc3_plot_df <- data.frame(
  date = future_dates_3,
  Texas_fc = fc3$fcst$Texas[, "fcst"],
  Texas_lo = fc3$fcst$Texas[, "lower"],
  Texas_hi = fc3$fcst$Texas[, "upper"],
  Oklahoma_fc = fc3$fcst$Oklahoma[, "fcst"],
  Oklahoma_lo = fc3$fcst$Oklahoma[, "lower"],
  Oklahoma_hi = fc3$fcst$Oklahoma[, "upper"],
  Arkansas_fc = fc3$fcst$Arkansas[, "fcst"],
  Arkansas_lo = fc3$fcst$Arkansas[, "lower"],
  Arkansas_hi = fc3$fcst$Arkansas[, "upper"]
)

hist_win_3 <- 60
hist3_df <- tail(var3_df, hist_win_3)

# Texas forecast
p3_tx <- ggplot() +
  geom_line(data = hist3_df, aes(x = date, y = Texas)) +
  geom_ribbon(data = fc3_plot_df, aes(x = date, ymin = Texas_lo, ymax = Texas_hi), alpha = 0.2) +
  geom_line(data = fc3_plot_df, aes(x = date, y = Texas_fc), linetype = "dashed") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(title = sprintf("3-State VAR(%d) Forecast: Texas", p3),
       x = "Date", y = "Percent") +
  theme_minimal(base_size = 12)

print(p3_tx)

# Oklahoma forecast
p3_ok <- ggplot() +
  geom_line(data = hist3_df, aes(x = date, y = Oklahoma)) +
  geom_ribbon(data = fc3_plot_df, aes(x = date, ymin = Oklahoma_lo, ymax = Oklahoma_hi), alpha = 0.2) +
  geom_line(data = fc3_plot_df, aes(x = date, y = Oklahoma_fc), linetype = "dashed") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(title = sprintf("3-State VAR(%d) Forecast: Oklahoma", p3),
       x = "Date", y = "Percent") +
  theme_minimal(base_size = 12)

print(p3_ok)

# Arkansas forecast
p3_ar <- ggplot() +
  geom_line(data = hist3_df, aes(x = date, y = Arkansas)) +
  geom_ribbon(data = fc3_plot_df, aes(x = date, ymin = Arkansas_lo, ymax = Arkansas_hi), alpha = 0.2) +
  geom_line(data = fc3_plot_df, aes(x = date, y = Arkansas_fc), linetype = "dashed") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(title = sprintf("3-State VAR(%d) Forecast: Arkansas", p3),
       x = "Date", y = "Percent") +
  theme_minimal(base_size = 12)

print(p3_ar)

# 9. IRFs
irf3_tx_ok <- irf(
  var3_fit,
  impulse = "Texas",
  response = "Oklahoma",
  n.ahead = 12,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)
plot(irf3_tx_ok)

irf3_tx_ar <- irf(
  var3_fit,
  impulse = "Texas",
  response = "Arkansas",
  n.ahead = 12,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)
plot(irf3_tx_ar)

irf3_ok_tx <- irf(
  var3_fit,
  impulse = "Oklahoma",
  response = "Texas",
  n.ahead = 12,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)
plot(irf3_ok_tx)

# 10. FEVD
fevd3 <- fevd(var3_fit, n.ahead = 12)
plot(fevd3)

# 11. Save results
cross_state_var3_results <- list(
  data_wide = var3_df,
  Y = Y3,
  cor_matrix = cor_matrix_3,
  lag_selection = sel3,
  selected_p = p3,
  var_fit = var3_fit,
  serial_test = serial_res_3,
  arch_test = arch_res_3,
  normality_test = norm_res_3,
  forecast = fc3,
  irf_tx_ok = irf3_tx_ok,
  irf_tx_ar = irf3_tx_ar,
  irf_ok_tx = irf3_ok_tx,
  fevd = fevd3
)