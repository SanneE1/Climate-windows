setwd("C:/owncloud/Documents/PhD/Biomes/Biome")

library(rnoaa)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

### CRFL population coordinates ------------------------------------------------------------------

sites <- data.frame(id = "Redfleet" , 
                    station = NA, 
                    latitude = 40.5, 
                    longitude = -109.375833, 
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

closest_stations <- Stations$Redfleet[1,]


#############################
## Get weather information ##
#############################



# WeatherInfo <- meteo_pull_monitors(closest_stations$id, date_max = "2011-12-31", date_min = "1996-01-01")
# WeatherInfo$population <- "Redfleet"
# WeatherInfo$prcp <- WeatherInfo$prcp / 10
# WeatherInfo$tmax <- WeatherInfo$tmax / 10
# WeatherInfo$tmin <- WeatherInfo$tmin / 10
# WeatherInfo$tobs <- WeatherInfo$tobs / 10
# 
# 
# 
# write.csv(WeatherInfo, file = "Data/Climate data/CRFL_NOAA.csv")



##########################################################################
## Add missing Clim data and Scale weather information  ---- DAILY ---- ##
##########################################################################

WeatherInfo <- read.csv("Data/Climate data/CRFL_NOAA.csv")
WeatherInfo$date <-  as.Date(WeatherInfo$date, "%m/%d/%Y")
WeatherInfo$X <- NULL

### replace with the mean of that day from other years ---------------------------------
DailyInfo <- WeatherInfo

for (j in c(4,7,8,9)) {                      ## climate columns of intrest                               
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



write.csv(select(WeatherInfo, "id", "date", "prcp", "tmax", "tmin", "tobs", "population", "prcp_scaled_M","tmax_scaled_M", "tmin_scaled_M", "tobs_scaled_M" ), 
          "Data/Climate data/CRFL_NOAA_supplemented.csv" )






############################################################################
## Add missing Clim data and Scale weather information  ---- Monthly ---- ##
############################################################################



MonthlyInfo <- DailyInfo %>%
  group_by(id, population, Month = month(date), Year = year(date)) %>%
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

for (j in c(5:12)) {                          ## climate columns of intrest                
  for(i in which(is.na(MonthlyInfo[j]))){
    if (is.na(MonthlyInfo[i,j])) {
      MonthlyInfo[i,j] <- mean(unlist(MonthlyInfo[which(MonthlyInfo$Month == MonthlyInfo$Month[i] & 
                                                          MonthlyInfo$id == MonthlyInfo$id[i]), j]), na.rm = T)
    }
  }
}



### scale Clim drivers ----------------------------------------------------------------

MonthlyInfo <- MonthlyInfo %>%                      
  group_by(id, Month) %>%
  mutate(sum_prcp_scaled = scale(sum_prcp),
         mean_prcp_scaled = scale(mean_prcp),
         sd_prcp_scaled = scale(sd_prcp),
         mean_tobs_scaled = scale(mean_tobs),
         sd_tobs_scaled = scale(sd_tobs),
         mean_tmax_scaled = scale(mean_tmax),
         mean_tmin_scaled = scale(mean_tmin),
         max_tmax_scaled = scale(max_tmax),
         min_tmin_scaled = scale(min_tmin))



write.csv(MonthlyInfo, "Data/Climate data/CRFL_NOAA_monthly.csv" )



