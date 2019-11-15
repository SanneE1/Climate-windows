library(rnoaa)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(SPEI)

### CRFL population coordinates ------------------------------------------------------------------

sites <- data.frame(id = "Rocky Mountain Biol. Lab." , 
                    station = NA, 
                    latitude = 38.958646, 
                    longitude = -106.987745, 
                    distance = NA)


############################
## Find Weather Stations  ##
############################


# all_stations <- ghcnd_stations() %>%
#   filter(first_year <= 1969, last_year >= 2012)
# 
# write.csv(all_stations, "Data/Climate data/all_stations.csv")
# 
all_stations <- read.csv("Data/Climate data/all_stations.csv") %>%
  filter(first_year <= 1969, last_year >= 2012)


Stations <- meteo_nearby_stations(lat_lon_df = sites,
                                  lat_colname =  "latitude",
                                  lon_colname = "longitude",
                                  station_data = all_stations,
                                  var = "all",
                                  limit = 10
)

closest_stations <- Stations[[1]][1,]

nearby_stations <- Stations[[1]][c(1:10),]
write.csv(nearby_stations, "Data/Climate data/FRSP_nearb_stations.csv")

#############################
## Get weather information ##
#############################


# WeatherInfo <- meteo_pull_monitors(closest_stations$id, date_max = "2018-12-31", date_min = "1969-01-01")
# WeatherInfo$population <- "Rocky Mountain Biol. Lab."
# WeatherInfo$prcp <- WeatherInfo$prcp / 10
# WeatherInfo$tmax <- WeatherInfo$tmax / 10
# WeatherInfo$tmin <- WeatherInfo$tmin / 10
# WeatherInfo$tobs <- WeatherInfo$tobs / 10
# write.csv(WeatherInfo, file = "Data/Climate data/FRSP_NOAA.csv")


##########################################################################
## Add missing Clim data and Scale weather information  ---- DAILY ---- ##
##########################################################################

WeatherInfo <- read.csv("Data/Climate data/FRSP_NOAA.csv")
WeatherInfo$date <-  as.Date(WeatherInfo$date, "%Y-%m-%d")
WeatherInfo$X <- NULL


## Impute Tobs
tobs.l <- lm(tobs ~ tmin + tmax, data = WeatherInfo)

WeatherInfo$tobs_predict <- predict(tobs.l, WeatherInfo)
WeatherInfo$tobs[which(is.na(WeatherInfo$tobs))] <- WeatherInfo$tobs_predict[which(is.na(WeatherInfo$tobs))]

## Using Climwin's Method 1 to subsitude the last few values
for (j in c(5:10)) {
  for (i in which(is.na(WeatherInfo[[j]]))){
    WeatherInfo[i,j] <- mean(WeatherInfo[[j]][which(WeatherInfo$date %in% 
                                                      c(WeatherInfo$date[i] - (1:2), WeatherInfo$date[i] + (1:2)))],
                             na.rm = T)
  }
  
}

############################################################################
## Add missing Clim data and Scale weather information  ---- Monthly ---- ##
############################################################################


MonthlyInfo <- WeatherInfo %>%
  group_by(id, Month = month(date), Year = year(date)) %>%
  summarise(sum_prcp = sum(prcp, na.rm = T),
            mean_prcp = mean(prcp, na.rm = T),
            sd_prcp = sd(prcp, na.rm = T),
            mean_tobs = mean(tobs, na.rm = T),
            sd_tobs = sd(tobs, na.rm = T),
            mean_tmax = mean(tmax, na.rm = T),
            mean_tmin = mean(tmin, na.rm = T),
            max_tmax = max(tmax, na.rm = T),
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


MonthlyInfo <- MonthlyInfo[order(MonthlyInfo$Year),]


### get SPEI values ----------------------------------------------------------------

# Compute potential evapotranspiration (PET) and climatic water balance (BAL)
MonthlyInfo$PET <- thornthwaite(MonthlyInfo$mean_tobs, sites$latitude[1]) 
MonthlyInfo$BAL <- MonthlyInfo$sum_prcp - MonthlyInfo$PET

# transform in 
Timescale <- ts(MonthlyInfo[,-c(1,2)],
                end = c(2018,12),
                frequency = 12)

# calculate SPEI
SP <- spei(Timescale[,"BAL"], 12)

spei_df <- matrix(SP$fitted[1:(12*length(unique(MonthlyInfo$Year)))],
                  nrow = length(unique(MonthlyInfo$Year)), ncol = 12,
                  byrow = T) %>%
  as.data.frame %>%
  mutate(Year = c(min(MonthlyInfo$Year):max(MonthlyInfo$Year)))  %>%
  setNames(c(1:12, "Year")) %>%
  pivot_longer(-Year, names_to = "Month", values_to = "SPEI") %>%
  mutate(Month = as.numeric(Month))

All_Climate <- left_join(MonthlyInfo, spei_df)

write.csv(All_Climate, "Data/Climate data/FRSP_NOAA_month.csv" )


