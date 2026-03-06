# Evaluate forecast output

library(hydroGOF)


forecasted <- readr::read_csv("2026-02-10_temp_forecast_tempSunModel.csv")
observations <- o

forecasted_mean <- forecasted |>
  filter(grepl("V", variable)) |>
  group_by(date)|>
    summarize(forecast_mean = mean(value))

compare <- left_join(forecasted_mean, o, by = "date") %>%
  na.omit()

RMSE <- sqrt(mean((compare$mean_temp - compare$forecast_mean)^2))
NSE <- NSE(compare$forecast_mean, compare$mean_temp)

compare_long <- compare |>
  select(date, forecast_mean, mean_temp) |>
  tidyr::pivot_longer(cols = forecast_mean:mean_temp,
                      names_to = "variable",
                      values_to = "value",
                      values_drop_na = TRUE)

ggplot(compare_long, aes(date, value, group = variable))+
  geom_line(aes(color = variable))+
  theme_classic()+
  ylab(" water temperature (C)")+
  theme_classic()+
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))+
  annotate("text", x=as.POSIXct("2026-02-12 00:00:00", tz = "GMT"), y=10, label= paste0("RMSE = 3.08 (C)"))+
  annotate("text", x=as.POSIXct("2026-02-12 00:00:00", tz = "GMT"), y=9, label= paste0("NSE = -1.24"))
  
ggsave(paste0(compare_long$date[1],"_Forecast_observe_compare.png"), width = 5, height = 3.7, dpi = 300)


