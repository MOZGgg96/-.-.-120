library(tidyverse)
library(rnoaa)
library(dplyr)
station_data = ghcnd_stations()
orenburg = data.frame(id = "ORENBURG", latitude = 51.7727,  longitude = 55.0988)
orenburg_around180 = meteo_nearby_stations(lat_lon_df = orenburg, station_data = station_data,
                                       radius = 250, var = c("PRCP", "TAVG"),
                                       year_min = 2007, year_max = 2015)
orenburg_around90 = meteo_nearby_stations(lat_lon_df = orenburg, station_data = station_data,
                                      radius = 50, var = c("PRCP", "TAVG"),
                                      year_min = 2007, year_max = 2015)
orenburg_around <- as.data.frame(orenburg_around180[["ORENBURG"]]) %>% filter(id != orenburg_around90[["ORENBURG"]][["id"]])
orenburg_around_id = orenburg_around[["id"]]
all_orenburg_around_data = meteo_tidy_ghcnd(stationid = orenburg_around_id, var=c("TAVG"), date_min = "2007-01-01", date_max = "2015-12-31")
all_orenburg_around_data <- all_orenburg_around_data %>% mutate(tavg = tavg / 10)
all_orenburg_around_data <- all_orenburg_around_data %>% filter(tavg > 5)
all_orenburg_around_data <- all_orenburg_around_data %>% mutate(year = year(date), month = month(date))
all_orenburg_around_data <- all_orenburg_around_data %>% group_by(id, year, month) %>% summarise(sum_month = mean(tavg, na.rm = TRUE))
var_table <- data.frame(
  m = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  afi = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00),
  bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00),
  di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
)
q = 1600
l = 2.2
e = 25
k = 300
all_orenburg_around_data <- all_orenburg_around_data %>% 
  mutate(yj = ((var_table[["afi"]][month] + var_table[["bfi"]][month] * 1.0 * sum_month) * var_table[["di"]][month] * k)/(q * l * (100 - e)))
all_orenburg_around_data <- all_orenburg_around_data %>% group_by(id, year) %>% summarise(sum_year = mean(yj, na.rm = TRUE))
all_orenburg_around_data <- all_orenburg_around_data %>% group_by(id) %>% summarise(sum_station = mean(sum_year, na.rm = TRUE))
Y <- sum(all_orenburg_around_data$sum_station)
print(paste(Y, "тонн")) 
