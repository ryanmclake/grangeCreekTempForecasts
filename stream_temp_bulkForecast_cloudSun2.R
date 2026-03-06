library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ecoforecastR)  # Ensure this package is installed and loaded
library(relevance)
library(ragg)

dates <- seq(as.POSIXct("2026-02-10 00:00:00", tz = "GMT"), as.POSIXct("2026-02-18 00:00:00", tz = "GMT"), by="hour")

o <- readr::read_csv("GRANGE_CREEK_TEMP.csv") |>
  dplyr::mutate(datetime = lubridate::ymd_hms(datetime)) |>
  dplyr::mutate(datetime = lubridate::floor_date(datetime, "hour")) |>
  rename(date = datetime) |>
  group_by(date) |>
  summarize(mean_temp = mean(temp_c),
            sd_temp = sd(temp_c))

m <- read_csv("historical_met_data_newest3.csv")
f <- read_csv("ensemble_forecast_data_feb18.csv")

target <- left_join(o, m, by = "date") |>
  filter(date < as.POSIXct("2026-02-18 00:00:00", tz = "GMT")) %>%
  mutate(lag_temp = lag(mean_temp))

f_cloud <- f |> select(date, starts_with("cloud_cover")) |>
  tidyr::pivot_longer(cols = starts_with("cloud_cover"),
                             names_to = "variable",
                             values_to = "value",
                             values_drop_na = TRUE)

ggplot(f_cloud, aes(date, value, group = variable))+
  geom_line()+
  ylab("Forecasted Cloud Cover (%)")+
  theme_classic()+
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))

ggsave(paste0(f$date[1],"_MET_forecasts_CLOUD.png"), width = 5, height = 3.7, dpi = 300)


f_sun <- f |> select(date, starts_with("shortwave")) |>
  tidyr::pivot_longer(cols = starts_with("shortwave"),
                      names_to = "variable",
                      values_to = "value",
                      values_drop_na = TRUE)

ggplot(f_sun, aes(date, value, group = variable))+
  geom_line()+
  ylab("Forecasted Shortwave Radiation (W/m2)")+
  theme_classic()+
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))

ggsave(paste0(f$date[1],"_MET_forecasts_SUN.png"), width = 5, height = 3.7, dpi = 300)


# Define DLM function
DLM_function <- function() {
  
  y <- target$mean_temp
  u <- target$sd_temp
  cloud <- target$cloud_cover
  sun <- target$shortwave_radiation
  
  data <- list(y = y,n = length(y),
               x_ic=mean(y, na.rm = T),
               tau_ic= 1/sd(y),
               a_obs=u,
               r_obs=1,          
               a_add=1,
               r_add=1,           
               cloud = cloud,
               sun = sun
  )
  
  # Define the DLM model; simplified model since `X` isn't used
  model = list(obs="y",fixed="~ 1 + X + cloud + sun", n.iternumber = 10000)
  
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
plot(results$DLM_results$params)

forecastN <- function(IC, betaIntercept, betaX, betasun, betacloud, cloud, sun, Q, n = 1){
  
  NT = 168
  N <- matrix(NA,1,NT)  ## storage
  Nprev <- IC           ## initialize
  for(t in 1:NT){
    mu = Nprev + betaX * Nprev + betacloud * cloud[t] + betasun * sun[t] + betaIntercept  ## mean
    N[,t] <- rnorm(n,mu,Q)                         ## predict next step
    Nprev <- N[,t]                                  ## update IC
  }
  return(N)
}

params <- as.data.frame(as.matrix(results$DLM_results$params)) %>%
  slice_tail(n = 1000)

param.mean <- apply(params,2,mean)

cloud_names <- f |>
  dplyr::select(matches('cloud_cover_member'))
cloud_names <- c(names(cloud_names))

rad_names <- f |>
  dplyr::select(matches('shortwave_radiation_instant'))
rad_names <- c(names(rad_names))

IC <- as.matrix(results$DLM_results$predict)
IC <- IC[,ncol(IC)]
IC = sample(IC,50)

params_forecast <- params[sample(nrow(params), 50), ] %>%
  bind_cols(., IC) |>
  rename(IC = `...7`)

object <- list()
forecasts <- list()

startTime <- Sys.time()

for(d in 1:length(params_forecast$IC)){
  
  IC = params_forecast$IC[d]
  betaIntercept = params_forecast$betaIntercept[d]
  betaX = params_forecast$betaX[d]
  betasun = params_forecast$betasun[d]
  betacloud = params_forecast$betacloud[d]
  Q = 1/sqrt(params_forecast$tau_add[d])
  
  
  for(h in 1:length(rad_names)){
    for(g in 1:length(cloud_names)){
      
      sun <- f |> select(rad_names[h]) |> pull()
      cloud <- f |> select(cloud_names[g]) |> pull()
      
      forecast <- forecastN(IC = IC, 
                            betaIntercept = betaIntercept, 
                            betaX = betaX, 
                            betasun = betasun,
                            betacloud = betacloud,
                            cloud = cloud,
                            sun = sun, 
                            Q = Q)
      
      object[[h]] <- forecast
    }
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
calibrated2 <- calibrated|>
  tidyr::pivot_longer(cols = `5%`:`95%`,
                      names_to = "variable",
                      values_to = "value",
                      values_drop_na = TRUE)

observed <- o |> select(date, mean_temp, sd_temp)

ggplot(calibrated, aes(date, `50%`)) +
  geom_ribbon(aes(ymin = `5%`, ymax = `95%`), fill = "lightblue") +
  geom_line(aes(y = `50%`))+
  geom_point(data = observed, aes(date, mean_temp), size = 0.4, color = "darkred", inherit.aes = F)+
  ylab(" water temperature (C)")+
  theme_classic()+
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))

ggsave(paste0(forecast$date[1],"_DLM_output.png"), width = 5, height = 3.7, dpi = 300)



all_data <- bind_rows(calibrated2, forecast)

write_csv(all_data, paste0(forecast$date[1],"_temp_forecast_cloudSunModel.csv"))

forecast_viz <- all_data %>%
  filter(date >= as.POSIXct("2026-02-17 00:00:00", tz = "GMT"))

observed_viz <- observed %>%
  filter(date >= as.POSIXct("2026-02-17 00:00:00", tz = "GMT"))

date_fig <- seq(as.POSIXct("2026-02-17 00:00:00", tz = "GMT"), as.POSIXct("2026-02-24 23:00:00", tz = "GMT"), by="hour")

for(i in 1:length(date_fig)){
  forecast_viz1 <- forecast_viz %>% filter(date <= date_fig[i])
  observed_viz1 <- observed_viz %>% filter(date <= date_fig[i])

  ggplot(forecast_viz1, aes(date, value, group = variable)) +
    geom_vline(xintercept = forecast$date[1], linetype='solid', color='darkblue', linewidth=1)+
    geom_line(alpha = 0.1)+
    geom_point(data = observed_viz1, aes(date, mean_temp), size = 0.6, color = "darkred", inherit.aes = F)+
    ylab("water temperature (C)")+
    theme_classic()+
    theme(axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"))+
    coord_cartesian(xlim=c( as.POSIXct("2026-02-17 00:00:00", tz = "GMT"), as.POSIXct("2026-02-24 23:00:00", tz = "GMT")), ylim = c(-1,20))
  
  ggsave(paste0(forecast$date[1],"_forecasts_cloudSunModel_",i,".png"), width = 5, height = 3.7, dpi = 300)
}
