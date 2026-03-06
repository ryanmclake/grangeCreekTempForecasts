set.seed(583205)

IC <- as.matrix(results$DLM_results$predict)
IC <- IC[,ncol(IC)]
IC = sample(IC,100)

params_forecast <- params[sample(nrow(params), 100), ] %>%
  bind_cols(., IC) |>
  rename(IC = `...7`)


forecastN <- function(IC, betaIntercept, betaX, betasun, betacloud, cloud, sun, Q, n = 1){
  
  
  dates <- c(f$date)
  N <- matrix(NA,100,length(dates))  ## storage
  Nprev <- IC

  for(t in 1:length(dates)){
    
    cloud <- f |> filter(date == dates[t]) %>% dplyr::select(matches('cloud_cover_member')) %>% 
    tidyr::pivot_longer(cols = starts_with("cloud"),
                        names_to = "variable",
                        values_to = "value",
                        values_drop_na = TRUE) %>%
    select(value) %>% sample_n(100, replace = T) %>% pull()
    
    sun <- f |> filter(date == dates[t]) %>% dplyr::select(matches('shortwave_radiation_instant')) %>% 
    tidyr::pivot_longer(cols = starts_with("shortwave"),
                        names_to = "variable",
                        values_to = "value",
                        values_drop_na = TRUE)  %>%
      select(value) %>% sample_n(100, replace = T) %>% pull()
    
    mu = Nprev + params_forecast$betaX * Nprev + params_forecast$betacloud * cloud + params_forecast$betasun * sun + params_forecast$betaIntercept  ## mean
    N[,t] <- rnorm(100, mean(mu), (sd(mu)+Q))                    ## predict next step
    Nprev <- rnorm(100, mean(N[,t]), sd(N[,t]))                                 ## update IC
  }
  return(N)
} 


forecast_ensembles2 <- as.data.frame(t(N))
forecast_ensembles2 <- cbind(f$date, forecast_ensembles2)
names(forecast_ensembles2)[1] <- "date"
forecast <- forecast_ensembles2|>
  tidyr::pivot_longer(cols = starts_with("V"),
                      names_to = "variable",
                      values_to = "value",
                      values_drop_na = TRUE)


ggplot(forecast, aes(date, value, group = variable)) +
  geom_vline(xintercept = f$date[1], linetype='solid', color='darkblue', linewidth=1)+
  geom_line(alpha = 0.1)+
  geom_point(data = observed_viz1, aes(date, mean_temp), size = 0.6, color = "darkred", inherit.aes = F)+
  ylab("water temperature (C)")+
  theme_classic()+
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))+
  coord_cartesian(xlim=c( as.POSIXct("2026-02-17 00:00:00", tz = "GMT"), as.POSIXct("2026-02-24 23:00:00", tz = "GMT")), ylim = c(-20,40))


var_val <- forecast %>%
  mutate(day = lubridate::ymd_hms(date)) %>%
  group_by(date) %>%
  summarise(sd(value))

ggplot(var_val, aes(date, `sd(value)`)) +
  geom_vline(xintercept = f$date[1], linetype='solid', color='darkblue', linewidth=1)+
  geom_line(alpha = 1)
