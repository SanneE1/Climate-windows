library(rnoaa)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

### OPIM population coordinates ------------------------------------------------------------------

sites <- data.frame(id = "Sevilleta" , 
                    station = NA, 
                    latitude = 34.334806, 
                    longitude =	-106.631444, 
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

closest_stations <- Stations[[1]][1,]

nearby_stations <- Stations[[1]][c(1:10),]
write.csv(nearby_stations, "Data/Climate data/OPIM_nearby_stations.csv")

#############################
## Get weather information ##
#############################



WeatherInfo <- meteo_pull_monitors(nearby_stations$id[1:3], date_max = "2019-12-31", date_min = "2000-01-01")
WeatherInfo$population <- "Sevilleta"
WeatherInfo$prcp <- WeatherInfo$prcp / 10
WeatherInfo$tmax <- WeatherInfo$tmax / 10
WeatherInfo$tmin <- WeatherInfo$tmin / 10
WeatherInfo$tobs <- WeatherInfo$tobs / 10
write.csv(WeatherInfo, file = "Data/Climate data/OPIM_NOAA.csv")



##########################################################################
## Add missing Clim data and Scale weather information  ---- DAILY ---- ##
##########################################################################

WeatherInfo <- read.csv("Data/Climate data/OPIM_NOAA.csv")
WeatherInfo$date <-  as.Date(WeatherInfo$date, "%Y-%m-%d")
WeatherInfo$X <- NULL

## Using Climwin's Method 1 to subsitude the last few values
  for (j in c(6:11)) {
    for (i in which(is.na(WeatherInfo[[j]]))){
      WeatherInfo[i,j] <- mean(WeatherInfo[[j]][which(WeatherInfo$id == WeatherInfo$id[i] &
                                                      WeatherInfo$date %in% 
                                                        c(WeatherInfo$date[i] - (1:2), WeatherInfo$date[i] + (1:2)))],
                               na.rm = T)
  }
  
}


############################################################################
## Add missing Clim data and Scale weather information  ---- Monthly ---- ##
############################################################################

MonthlyInfo <- WeatherInfo %>%
  group_by(id, Month = month(date), Year = year(date)) %>%
  summarise(sum_prcp = sum(prcp),
            mean_prcp = mean(prcp),
            sd_prcp = sd(prcp),
            mean_tobs = mean(tobs),
            sd_tobs = sd(tobs),
            mean_tmax = mean(tmax),
            mean_tmin = mean(tmin, na.rm = T),
            max_tmax = max(tmax),
            min_tmin = min(tmin, na.rm = T))



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



write.csv(MonthlyInfo, "Data/Climate data/OPIM_NOAA_month.csv" )
