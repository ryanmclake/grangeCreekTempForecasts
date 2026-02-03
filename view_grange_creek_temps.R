# Read and view the temperature data from the HOBO logger

library(readr)
library(lubridate)
library(ggplot2)

h <- readr::read_csv("GRANGE_CREEK_TEMP.csv") |>
  dplyr::mutate(datetime = lubridate::ymd_hms(datetime)) |>
  dplyr::mutate(datetime = lubridate::floor_date(datetime, "hour"))


ggplot(h, aes(x=datetime, y=temp_c, group = datetime,)) + 
  geom_boxplot()



