library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ecoforecastR)  # Ensure this package is installed and loaded
library(relevance)

o <- readr::read_csv("GRANGE_CREEK_TEMP.csv") |>
  dplyr::mutate(datetime = lubridate::ymd_hms(datetime)) |>
  dplyr::mutate(datetime = lubridate::floor_date(datetime, "hour")) |>
  rename(date = datetime) |>
  group_by(date) |>
  summarize(mean_temp = mean(temp_c),
            sd_temp = sd(temp_c))
m <- read_csv("historical_met_data.csv")
f <- read_csv("ensemble_forecast_data_feb1.csv")

target <- left_join(o, m, by = "date") |>
  filter(date < as.POSIXct("2026-02-01 00:00:00", tz = "GMT"))


# Define DLM function
DLM_function <- function() {
  
  y <- target$mean_temp
  u <- target$sd_temp
  sun <- target$shortwave_radiation
  
  data <- list(y = y,n = length(y),
               x_ic=mean(y, na.rm = T),
               tau_ic= 1/sd(y),
               a_obs=u,
               r_obs=1,          
               a_add=1,
               r_add=1,           
               sun = sun
  )
  
  # Define the DLM model; simplified model since `X` isn't used
  model = list(obs="y",fixed="~ 1 + X + sun", n.iternumber = 10000)
  
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

# Run the DLM function to get initial parameters
results <- DLM_function()

forecastN <- function(IC, betaIntercept, betaX, betasun, sun, Q = 0, n = 1){
  
  NT = 240
  N <- matrix(NA,1,NT)  ## storage
  Nprev <- IC           ## initialize
  for(t in 1:NT){
    mu = Nprev + betaX * Nprev + betasun * sun[t] + betaIntercept  ## mean
    N[,t] <- rnorm(n,mu,Q)                         ## predict next step
    Nprev <- N[,t]                                  ## update IC
  }
  return(N)
}

params <- as.matrix(results$DLM_results$params)
param.mean <- apply(params,2,mean)

rad_names <- f |>
  dplyr::select(matches('shortwave_radiation_instant'))
rad_names <- c(names(rad_names))

IC <- as.matrix(results$DLM_results$predict)
IC <- c(last(IC,30000,1))
IC = sample(IC,50)

params_forecast <- params[sample(nrow(params), 50), ] %>%
  bind_cols(., IC) |>
  rename(IC = `...6`)

object <- list()
forecasts <- list()

startTime <- Sys.time()

for(d in 1:length(params_forecast$IC)){
  
  IC = params_forecast$IC[d]
  betaIntercept = params_forecast$betaIntercept[d]
  betaX = params_forecast$betaX[d]
  betasun = params_forecast$betasun[d]
  
for(g in 1:length(rad_names)){

    sun <- f |> select(rad_names[g]) |> pull()
  
    forecast <- forecastN(IC = IC, 
                          betaIntercept = betaIntercept, 
                          betaX = betaX, 
                          betasun = betasun,
                          sun = sun,
                          Q = 0)
    
    object[[g]] <- forecast
 }
  forecasts[[d]] <- object
}



endTime <- Sys.time()

# prints recorded time
print(endTime - startTime) 

forecast_ensembles <- unlist(forecasts, recursive = FALSE)

forecast_ensembles2 <- as.data.frame(do.call("rbind", forecast_ensembles))


forecast_ensembles2 <- as.data.frame(t(forecast_ensembles2))
forecast_ensembles2 <- cbind(f$date, forecast_ensembles2)
names(forecast_ensembles2)[1] <- "date"
forecast <- forecast_ensembles2|>
  tidyr::pivot_longer(cols = starts_with("V"),
                      names_to = "variable",
                      values_to = "value",
                      values_drop_na = TRUE)


out <- as.matrix(results$DLM_results$predict)
ci <- apply(out,2,quantile,c(0.05,0.5,0.95))
calibrated <- as.data.frame(t(ci))
calibrated <- cbind(target$date, calibrated)
names(calibrated)[1] <- "date"
calibrated <- calibrated|>
  tidyr::pivot_longer(cols = `5%`:`95%`,
                      names_to = "variable",
                      values_to = "value",
                      values_drop_na = TRUE)


observed <- o |> select(date, mean_temp, sd_temp)

all_data <- bind_rows(calibrated, forecast)

write_csv(all_data, paste0(forecast$date[1],"_temp_forecast_sun.csv"))

ggplot(all_data, aes(date, value, group = variable)) +
  geom_vline(xintercept = forecast$date[1], linetype='solid', color='darkblue', linewidth=0.5)+
  geom_line()+
  geom_point(data = observed, aes(date, mean_temp), size = 0.6, color = "darkred", inherit.aes = F)+
  ylab("water temperature (C)")+
  theme_classic()+
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))

ggsave(paste0(forecast$date[1],"_forecasts_sun.png"), width = 5, height = 3.7, dpi = 300)
