library(readr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ecoforecastR)  # Ensure this package is installed and loaded
library(relevance)
library(data.table)
library(ggridges)

dates <- seq(as.POSIXct("2026-02-01 00:00:00", tz = "GMT"), as.POSIXct("2026-02-18 00:00:00", tz = "GMT"), by="day")
results <- list()

for(i in 1:length(dates)){
  
o <- readr::read_csv("GRANGE_CREEK_TEMP.csv") |>
  dplyr::mutate(datetime = lubridate::ymd_hms(datetime)) |>
  dplyr::mutate(datetime = lubridate::floor_date(datetime, "hour")) |>
  rename(date = datetime) |>
  group_by(date) |>
  summarize(mean_temp = mean(temp_c),
            sd_temp = sd(temp_c))

m <- read_csv("historical_met_data_newest3.csv")

target <- left_join(o, m, by = "date") |>
  filter(date <= dates[i])

# Define DLM function
DLM_function <- function() {
  
  y <- target$mean_temp
  u <- target$sd_temp
  temp <- target$temperature_2m
  sun <- target$shortwave_radiation
  
  data <- list(y = y,n = length(y),
               x_ic=mean(y, na.rm = T),
               tau_ic= 1/sd(y),
               a_obs=u,
               r_obs=1,          
               a_add=1,
               r_add=1,           
               temp = temp,
               sun = sun
  )
  
  # Define the DLM model; simplified model since `X` isn't used
  model = list(obs="y",fixed="~ 1 + X + temp + sun", n.iternumber = 10000)
  
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
results[[i]] <- DLM_function()

}

d <- bind_rows(bind_cols(as.data.frame(results[[1]]$DLM_results$params[[1]]),matrix(max(results[[1]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[2]]$DLM_results$params[[1]]),matrix(max(results[[2]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[3]]$DLM_results$params[[1]]),matrix(max(results[[3]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[4]]$DLM_results$params[[1]]),matrix(max(results[[4]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[5]]$DLM_results$params[[1]]),matrix(max(results[[5]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[6]]$DLM_results$params[[1]]),matrix(max(results[[6]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[7]]$DLM_results$params[[1]]),matrix(max(results[[7]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[8]]$DLM_results$params[[1]]),matrix(max(results[[8]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[9]]$DLM_results$params[[1]]),matrix(max(results[[9]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[10]]$DLM_results$params[[1]]),matrix(max(results[[10]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[11]]$DLM_results$params[[1]]),matrix(max(results[[11]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[12]]$DLM_results$params[[1]]),matrix(max(results[[12]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[13]]$DLM_results$params[[1]]),matrix(max(results[[13]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[14]]$DLM_results$params[[1]]),matrix(max(results[[14]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[15]]$DLM_results$params[[1]]),matrix(max(results[[15]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[16]]$DLM_results$params[[1]]),matrix(max(results[[16]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[17]]$DLM_results$params[[1]]),matrix(max(results[[17]]$time),nrow = 10000)),
               bind_cols(as.data.frame(results[[18]]$DLM_results$params[[1]]),matrix(max(results[[18]]$time),nrow = 10000))) |>
  rename(date = `...7`)|>
  group_by(date)|>
  slice_tail(n=1000)

d$date <- as.POSIXct(d$date, origin = "1970-01-01", tz = "GMT")

ggplot(d, aes(x=betasun, y=as.factor(date))) +
  geom_density_ridges(fill = "yellow", alpha = 0.5, rel_min_height = 0.005)+
  theme_classic()

ggsave(paste0("DA_Betasun_output.png"), width = 5, height = 3.7, dpi = 300)


ggplot(d, aes(x=betatemp, y=as.factor(date))) +
  geom_density_ridges(fill = "red", alpha = 0.5, rel_min_height = 0.005)+
  theme_classic()

ggsave(paste0("DA_Betatemp_output.png"), width = 5, height = 3.7, dpi = 300)


ggplot(d, aes(x=betaX, y=as.factor(date))) +
  geom_density_ridges(fill = "blue", alpha = 0.5, rel_min_height = 0.005)+
  theme_classic()

ggsave(paste0("DA_BetaX_output.png"), width = 5, height = 3.7, dpi = 300)

ggplot(d, aes(x=betaIntercept, y=as.factor(date))) +
  geom_density_ridges(fill = "pink", alpha = 0.5, rel_min_height = 0.005)+
  theme_classic()

ggsave(paste0("DA_INT_output.png"), width = 5, height = 3.7, dpi = 300)


