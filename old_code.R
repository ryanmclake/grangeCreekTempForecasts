
plot(time,ci[2,],
     type='n',
     ylim=range(y,na.rm=TRUE),
     ylab="Water Temperature"
)
ecoforecastR::ciEnvelope(time,ci[1,],ci[3,],col=ecoforecastR::col.alpha("lightblue",0.75))
points(time,y,pch=1,cex=0.4, col = "red4")
observed <- 
  
  
  NT = 168
time1 = 1:NT
x = 1:length(ci[2,])

time2 = length(x):(length(x)+NT-1)

Nmc = 1000
prow = sample.int(nrow(params),Nmc,replace=TRUE)
forecast <- forecastN(IC[prow, ncol(IC)], params[prow,"betaIntercept"], params[prow,"betaX"], params[prow, "betatemp"], df_future$mean_temperature, Q = 0, n = Nmc)

par(mfrow=c(1,1))

plot(x,ci[2,],
     type='n',
     ylim=range(y,na.rm=TRUE),
     ylab="Temperature",
     # xlim = c(1500,length(ci[2,]) + NT),
     xlim = c(min(time2) - 30, max(time2)),
     main = "Forecast with IC and Parameter Uncertainty"
)
ecoforecastR::ciEnvelope(x,ci[1,],ci[3,],col=ecoforecastR::col.alpha("lightBlue",0.75))
points(x,y,pch="+",cex=0.5)

N.IP.ci = apply(forecast,2,quantile,c(0.025,0.5,0.975))
ecoforecastR::ciEnvelope(time2,N.IP.ci[1,],N.IP.ci[3,],col=col.alpha("lightBlue",1))
lines(time2,N.IP.ci[2,],lwd=0.5)
legend("bottomright", legend = c("Data", "CI", "IC Uncertainty", "Parameter Uncertainty"), lty = c(NA,1,1,1), col = c("black", "lightblue", "black", "red"), pch = c("+", NA, NA, NA), cex = 0.7)


# Format variables for return and submission
prediction <- as.vector(N.IP.ci[2,])

datetime <- seq(reference_date, by = "day", length.out = 30)
site_id <- site
forecast_df <- data.frame(site_id, datetime, prediction)



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






out <- as.matrix(results$DLM_results$predict)
ci <- apply(out,2,quantile,c(0.01,0.5,0.99))
time <- target$date
plot(time,ci[2,],
     type='n',
     ylim=range(y,na.rm=TRUE),
     ylab="Water Temperature"
)
ecoforecastR::ciEnvelope(time,ci[1,],ci[3,],col=ecoforecastR::col.alpha("lightblue",0.75))
points(time,y,pch=1,cex=0.4, col = "red4")


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

# Plot the actual and predicted temperature from the DLM
time <- target$date
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


n = length(time2) * 2
mu = p = rep(NA, n)
mu[seq(1, n, by = 2)] = mu.f[tsel]
mu[seq(2, n, by = 2)] = mu.a[tsel]
p[seq(1, n, by = 2)] = 1.96 * sqrt(P.f[tsel])
p[seq(2, n, by = 2)] = 1.96 * sqrt(P.a[tsel])
ci = cbind(mu - p, mu + p)
time3 = sort(c(time2, time2 + 1))

# Plotting
plot(time3, mu, ylim = range(ci), type = 'n', xlab = "Date", ylab = "Temperature", main = "Forecast and Analysis with Confidence Intervals")
ecoforecastR::ciEnvelope(time3, ci[, 1], ci[, 2], col = "lightBlue")
lines(time3, mu, lwd = 2)
points(time[tsel], results$Y[tsel], pch = 19, cex = 0.1, col = "red")  # add actual observations
