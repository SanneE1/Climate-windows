setwd("C:/owncloud/Documents/PhD/Biomes/Biome")

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


############################
## Find Weather Stations  ##
############################


# all_stations <- ghcnd_stations() %>%
#   filter(first_year <= 1997, last_year >= 2012)
# 
# write.csv(all_stations, "Data/Climate data/all_stations.csv")
# 
all_stations <- read.csv("Data/Climate data/all_stations.csv")


Stations <- meteo_nearby_stations(lat_lon_df = sites,
                                  lat_colname =  "latitude",
                                  lon_colname = "longitude",
                                  station_data = all_stations,
                                  var = "all",
                                  limit = 10
                                  )

closest_stations <- rbind(Stations[[1]][1,], Stations[[2]][1,], Stations[[3]][1,])


#############################
## Get weather information ##
#############################


# 
# WeatherInfo <- meteo_pull_monitors(closest_stations$id, date_max = "2012-12-31", date_min = "1997-01-01")
# WeatherInfo$population <- ifelse(WeatherInfo$id == Stations$high$id[1], "high", ifelse(WeatherInfo$id == Stations$low$id[1], "low", "mid"))
# WeatherInfo$prcp <- WeatherInfo$prcp / 10
# WeatherInfo$tavg <- WeatherInfo$tavg / 10
# WeatherInfo$tmax <- WeatherInfo$tmax / 10
# WeatherInfo$tmin <- WeatherInfo$tmin / 10
# WeatherInfo$tobs <- WeatherInfo$tobs / 10

# 
# 
# write.csv(WeatherInfo, file = "HEQU_NOAA.csv")



##########################################################################
## Add missing Clim data and Scale weather information  ---- DAILY ---- ##
##########################################################################

WeatherInfo <- read.csv("Data/Climate data/HEQU_NOAA.csv")
WeatherInfo$date <-  as.Date(WeatherInfo$date, "%m/%d/%Y")


### replace with the mean of that day from other years ---------------------------------

for (j in c(4,7,8,9)) {                                   
  for(i in which(is.na(WeatherInfo[j]))){
      if (is.na(WeatherInfo[i,j])) {
        DailyInfo[i,j] <- mean(WeatherInfo[which(month(WeatherInfo$date) == month(WeatherInfo$date[i]) & 
                                           day(WeatherInfo$date) == day(WeatherInfo$date[i]) & 
                                             WeatherInfo$population == WeatherInfo$population[i]), j], na.rm = T)
    }
  }
}



### scale Clim drivers ----------------------------------------------------------------

DailyInfo <- DailyInfo %>%                      
  group_by(id, month(date)) %>%
  mutate(prcp_scaled_M = scale(prcp),
         tmax_scaled_M = scale(tmax),
         tmin_scaled_M = scale(tmin),
         tobs_scaled_M = scale(tobs)
  )


write.csv(select(DailyInfo, "id", "date", "prcp", "tmax", "tmin", "tobs", "population", "prcp_scaled_M","tmax_scaled_M", "tmin_scaled_M", "tobs_scaled_M" ), 
          "Data/Climate data/HEQU_NOAA_supplemented.csv" )



##########################################################################
## Add missing Clim data and Scale weather information  ---- DAILY ---- ##
##########################################################################



MonthlyInfo <- WeatherInfo %>%
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


### replace with the mean of that month from other years ---------------------------------

for (j in c(4,7,8,9)) {                                   
  for(i in which(is.na(MonthlyInfo[j]))){
    if (is.na(MonthlyInfo[i,j])) {
      MonthlyInfo[i,j] <- mean(MonthlyInfo[which(MonthlyInfo$Month == MonthlyInfo$Month[i] & 
                                                 MonthlyInfo$population == MonthlyInfo$population[i]), j], na.rm = T)
    }
  }
}



### scale Clim drivers ----------------------------------------------------------------

MonthlyInfo <- MonthlyInfo %>%                      
  group_by(id, Month) %>%
  mutate(prcp_scaled_M = scale(prcp),
         tmax_scaled_M = scale(tmax),
         tmin_scaled_M = scale(tmin),
         tobs_scaled_M = scale(tobs)
  )


write.csv(select(MonthlyInfo, "id", "date", "prcp", "tmax", "tmin", "tobs", "population", "prcp_scaled_M","tmax_scaled_M", "tmin_scaled_M", "tobs_scaled_M" ), 
          "Data/Climate data/HEQU_NOAA_monthly.csv" )
