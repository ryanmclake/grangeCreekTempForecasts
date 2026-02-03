library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ecoforecastR)  # Ensure this package is installed and loaded


o <- readr::read_csv("GRANGE_CREEK_TEMP.csv") |>
  dplyr::mutate(datetime = lubridate::ymd_hms(datetime)) |>
  dplyr::mutate(datetime = lubridate::floor_date(datetime, "hour")) |>
  rename(date = datetime)
m <- read_csv("historical_met_data.csv")
f <- read_csv("noaa_gefs_forecast.csv")

target <- left_join(o, m, by = "date")


# Define DLM function
DLM_function <- function() {
  
  y <- target$temp_c
  temp <- target$temperature_2m
  
  # Define the DLM model; simplified model since `X` isn't used
  model = list(obs="y",fixed="~ 1 + X + temp", n.iternumber = 10000)
  
  # Run the DLM
  ef.out <- ecoforecastR::fit_dlm(model = model, data = data)
  
  # Extract and process results from DLM
  params <- window(ef.out$params, start = 1000)
  summary_params <- summary(params)
  
  
  # Initial conditions for Kalman Filter
  mu0 <- as.numeric(head(y, 1))  # First observation of y
  P0 <- diag(10, length(mu0))    # Large initial covariance
  
  # Return relevant data and DLM results
  return(list(
    Y = y,
    mu0 = mu0,
    P0 = P0,
    time = target$date,
    DLM_results = ef.out,
    params = summary_params
  ))
}


# Kalman Filter functions
KalmanAnalysis <- function(mu.f, P.f, Y, R, H, I){
  obs = !is.na(Y)
  if (any(obs)) {
    K <- P.f * H / (H * P.f * H + R)
    mu.a <- mu.f + K * (Y - H * mu.f)
    P.a <- (I - K * H) * P.f
  } else {
    mu.a = mu.f
    P.a = P.f
  }
  return(list(mu.a=mu.a, P.a=P.a))
}

KalmanForecast <- function(mu.a, P.a, M, Q){
  mu.f = M * mu.a
  P.f = Q + M * P.a * M
  return(list(mu.f=mu.f, P.f=P.f))
}

# Running the Kalman Filter
KalmanFilter <- function(M, mu0, P0, Q, R, Y){
  nt = length(Y)
  mu.f = numeric(nt + 1)
  mu.a = numeric(nt)
  P.f = numeric(nt + 1)
  P.a = numeric(nt)
  
  # Initialization
  mu.f[1] = mu0
  P.f[1] = P0
  H = 1
  I = 1
  
  # Sequential updates
  for(t in 1:nt){
    KA <- KalmanAnalysis(mu.f[t], P.f[t], Y[t], R, H, I)
    mu.a[t] <- KA$mu.a
    P.a[t] <- KA$P.a
    
    KF <- KalmanForecast(mu.a[t], P.a[t], M, Q)
    mu.f[t + 1] <- KF$mu.f
    P.f[t + 1] <- KF$P.f
  }
  
  return(list(mu.f=mu.f, mu.a=mu.a, P.f=P.f, P.a=P.a))
}



# Run the DLM function to get initial parameters
results <- DLM_function()

# Define the number of states based on your data
nstates <- length(results$Y)  # This should actually be 1 since we have one state variable

# Define a simple state transition matrix M, assuming simple evolution without spatial interactions
alpha <- 0.05
M <- diag(1 - alpha, nstates)

# Define process and observation error
tau_proc <- rep(0.01, nstates)
Q <- diag(diag(tau_proc))
tau_obs <- results$DLM_results$data$OBS# var(results$Y, na.rm = TRUE)
R <- diag(tau_obs, nstates)

# Initial conditions based on historical data or estimated from DLM
mu0 <- results$mu0
P0 <- results$P0

# Prepare the observation vector Y
Y <- matrix(results$Y, ncol = 1)

KF_results <- KalmanFilter(M, mu0, P0, Q, R, Y)

# Plot the actual and predicted oxygen levels
time <- df$Time
actual <- results$Y
predicted <- matrix(KF_results$mu.a, ncol = 1)  # Ensure predicted is a column matrix

df <- data.frame(Time = time, Actual = actual, Predicted = predicted)


# Assume `KF_results` and `time` are from the previous code
time <- df$Time
mu.f <- KF_results$mu.f
mu.a <- KF_results$mu.a
P.f <- KF_results$P.f
P.a <- KF_results$P.a

# Convert time to Date format if not already
# time <- as.Date(time)

## Subset time
time2 <- time[time > as.Date("2015-01-01")]
tsel <- which(time %in% time2)

time <- as.Date(time)  # Convert time to Date if not already
time2 <- time[time > as.Date("2015-01-01")]
tsel <- which(time %in% time2)

n = length(time2) * 2
mu = p = rep(NA, n)
mu[seq(1, n, by = 2)] = mu.f[tsel]
mu[seq(2, n, by = 2)] = mu.a[tsel]
p[seq(1, n, by = 2)] = 1.96 * sqrt(P.f[tsel])
p[seq(2, n, by = 2)] = 1.96 * sqrt(P.a[tsel])
ci = cbind(mu - p, mu + p)
time3 = sort(c(time2, time2 + 1))

# Plotting
plot(time3, mu, ylim = range(ci), type = 'n', xlab = "Time", ylab = "Oxygen Level", main = "Forecast and Analysis with Confidence Intervals")
ecoforecastR::ciEnvelope(time3, ci[, 1], ci[, 2], col = "lightBlue")
lines(time3, mu, lwd = 2)
points(time[tsel], results$Y[tsel], pch = 19, cex = 0.1, col = "red")  # add actual observations
