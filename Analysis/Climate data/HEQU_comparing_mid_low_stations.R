library(rnoaa)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

### HEQU population coordinates ------------------------------------------------------------------


sites <- data.frame(id = c("low", "mid", "high"), 
                    station = NA, 
                    latitude = as.numeric(measurements::conv_unit(c("38 51.774", "38 57.5", "38 58.612"), from = 'deg_dec_min', to = 'dec_deg')), 
                    longitude = as.numeric(measurements::conv_unit(c("-107 09.556", "-106 59.3", "-106 58.690"), from = 'deg_dec_min', to = 'dec_deg')), 
                    distance = NA)

### Stations ------------------------------------------------------------------------------------------------------------------------------------

all_stations <- read.csv("Data/Climate data/all_stations.csv")


Stations <- meteo_nearby_stations(lat_lon_df = sites,
                                  lat_colname =  "latitude",
                                  lon_colname = "longitude",
                                  station_data = all_stations,
                                  var = "all",
                                  limit = 10
)

closest_stations <- rbind(Stations[[1]][1,], Stations[[2]][1,], Stations[[3]][1,])
closest_stations <- cbind(closest_stations, population = c(names(Stations)))

WeatherMid <- meteo_pull_monitors(closest_stations$id[3], date_max = "2012-12-31", date_min = "1997-01-01")
WeatherLow <- meteo_pull_monitors(closest_stations$id[2], date_max = "2012-12-31", date_min = "1997-01-01")

# 
# for (j in c("prcp", "tavg", "tmax", "tmin", "tobs")) {                      ## climate columns of intrest                               
#   for(i in which(is.na(WeatherMid[j]))){
#     if (is.na(WeatherMid[i,j])) {
#       WeatherMid[i,j] <- mean(WeatherMid[which(month(WeatherMid$date) == month(WeatherMid$date[i]) & 
#                                                  day(WeatherMid$date) == day(WeatherMid$date[i]) & 
#                                                 WeatherMid$id == WeatherMid$id[i]), j], na.rm = T)
#     }
#   }
# }
# 
# 
# for (j in c("prcp", "tmax", "tmin", "tobs")) {                      ## climate columns of intrest                               
#   for(i in which(is.na(WeatherLow[j]))){
#     if (is.na(WeatherLow[i,j])) {
#       WeatherLow[i,j] <- mean(WeatherLow[which(month(WeatherLow$date) == month(WeatherLow$date[i]) & 
#                                                  day(WeatherLow$date) == day(WeatherLow$date[i]) & 
#                                                  WeatherLow$id == WeatherLow$id[i]), j], na.rm = T)
#     }
#   }
# }

### Daily weather --------------------------------------------------------------------------------------------------------------------------------------
a <- merge(WeatherMid,WeatherLow, by = "date")


ggplot(a, aes(x = tobs.x, y = tobs.y))+
  geom_point()+
  facet_wrap(vars(year(date)))+
  xlab("Tobs for weatherstation Mid")+ylab("Tobs for weatherstation Low")

ggplot(a, aes(x = tmin.x, y = tmin.y))+
  geom_point()+
  facet_wrap(vars(year(date)))+
  xlab("Tmin for weatherstation Mid")+ylab("Tmin for weatherstation Low")

ggplot(a, aes(x = tmax.x, y = tmax.y))+
  geom_point()+
  facet_wrap(vars(year(date)))+
  xlab("Tmax for weatherstation Mid")+ylab("Tmax for weatherstation Low")


ggplot(a, aes(x = prcp.x, y = prcp.y))+
  geom_point()+
  facet_wrap(vars(year(date)))+
  xlab("Tobs for weatherstation Mid")+ylab("Tobs for weatherstation Low")


### Monthly data --------------------------------------------------------------------------------------------------------------------------------------------------
WeatherMid_m <- WeatherMid %>%
  group_by(id, Month = month(date), Year = year(date)) %>%
  summarise(sum_prcp = sum(prcp),
            mean_prcp = mean(prcp),
            sd_prcp = sd(prcp),
            mean_tobs = mean(tobs),
            sd_tobs = sd(tobs),
            mean_tmax = mean(tmax),
            mean_tmin = mean(tmin),
            max_tmax = max(tmax),
            min_tmin = min(tmin))

WeatherLow_m <- WeatherLow %>%
  group_by(id, Month = month(date), Year = year(date)) %>%
  summarise(sum_prcp = sum(prcp),
            mean_prcp = mean(prcp),
            sd_prcp = sd(prcp),
            mean_tobs = mean(tobs),
            sd_tobs = sd(tobs),
            mean_tmax = mean(tmax),
            mean_tmin = mean(tmin),
            max_tmax = max(tmax),
            min_tmin = min(tmin))

b <- merge(WeatherMid_m, WeatherLow_m, by = c("Month", "Year"))

ggplot(b, aes(x = mean_tobs.x, y = mean_tobs.y))+
  geom_point()+
  facet_wrap(vars(Year))+
  xlab("mean_tobs for weatherstation Mid")+ylab("mean_tobs for weatherstation Low")

ggplot(b, aes(x = mean_tmin.x, y = mean_tmin.y))+
  geom_point()+
  facet_wrap(vars(Year))+
  xlab("mean_tmin for weatherstation Mid")+ylab("mean_tmin for weatherstation Low")

ggplot(b, aes(x = mean_tmax.x, y = mean_tmax.y))+
  geom_point()+
  facet_wrap(vars(Year))+
  xlab("mean_tmax for weatherstation Mid")+ylab("mean_tmax for weatherstation Low")

ggplot(b, aes(x = min_tmin.x, y = min_tmin.y))+
  geom_point()+
  facet_wrap(vars(Year))+
  xlab("min_tmin for weatherstation Mid")+ylab("min_tmin for weatherstation Low")

ggplot(b, aes(x = max_tmax.x, y = max_tmax.y))+
  geom_point()+
  facet_wrap(vars(Year))+
  xlab("min_tmax for weatherstation Mid")+ylab("min_tmax for weatherstation Low")

ggplot(b, aes(x = mean_prcp.x, y = mean_prcp.y))+
  geom_point()+
  facet_wrap(vars(Year))+
  xlab("Mean Prcp for weatherstation Mid")+ylab("Mean Prcp for weatherstation Low")
