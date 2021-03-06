library(rnoaa)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(SPEI)

### CRFL population coordinates ------------------------------------------------------------------

sites <- data.frame(id = "Redfleet" , 
                    station = NA, 
                    latitude = 40.59548, 
                    longitude = -109.43204, 
                    distance = NA)


############################
## Find Weather Stations  ##
############################


# all_stations <- ghcnd_stations() %>%
#   filter(first_year <= 1997, last_year >= 2012)
# 
# write.csv(all_stations, "Data/Climate data/all_stations.csv")
# 
all_stations <- read.csv("Data/Climate data/all_stations.csv") %>%
     filter(first_year <= 1987, last_year >= 2012)


# find the 10 closest stations
Stations <- meteo_nearby_stations(lat_lon_df = sites,
                                  lat_colname =  "latitude",
                                  lon_colname = "longitude",
                                  station_data = all_stations,
                                  var = "all",
                                  limit = 10
)

# 2nd closest station used, as the 1st is missing a lot of data
closest_stations <- Stations[[1]][2,]

nearby_stations <- Stations[[1]][c(1:10),]
write.csv(nearby_stations, "Data/Climate data/CRFL_nearb_stations.csv")

#############################
## Get weather information ##
#############################



WeatherInfo <- meteo_pull_monitors(closest_stations$id, date_max = "2016-12-31", date_min = "1986-01-01")
WeatherInfo$population <- "Redfleet"
WeatherInfo$prcp <- WeatherInfo$prcp / 10
WeatherInfo$tmax <- WeatherInfo$tmax / 10
WeatherInfo$tmin <- WeatherInfo$tmin / 10
WeatherInfo$tobs <- WeatherInfo$tobs / 10
WeatherInfo$tavg <- WeatherInfo$tavg / 10
write.csv(WeatherInfo, file = "Data/Climate data/CRFL_NOAA.csv")




############################################################################
## Add missing Clim data and Scale weather information  ---- Monthly ---- ##
############################################################################

WeatherInfo <- read.csv("Data/Climate data/CRFL_NOAA.csv")
WeatherInfo$date <-  as.Date(WeatherInfo$date, "%Y-%m-%d")
WeatherInfo$X <- NULL

WeatherInfo$tmin[which(WeatherInfo$date == "1993-10-06")] <- NA  ## Remove extreme value (-51.3)

## create monthly values from daily data
MonthlyInfo <- WeatherInfo %>%
  group_by(id, Month = month(date), Year = year(date)) %>%
  summarise(sum_prcp = sum(prcp),  
            mean_tmax = mean(tmax),
            mean_tmin = mean(tmin, na.rm = T), 
            max_tmax = max(tmax),
            min_tmin = min(tmin, na.rm = T),
            mean_tavg = mean(tavg))


### scale Clim drivers ----------------------------------------------------------------
# grouping by id an month scales the climate variables across months (so scales all Januaries, all Februaries etc.)
MonthlyInfo <- MonthlyInfo %>%                      
  group_by(id, Month) %>%
  mutate(sum_prcp_scaled = scale(sum_prcp),
         mean_tmax_scaled = scale(mean_tmax),
         mean_tmin_scaled = scale(mean_tmin),
         max_tmax_scaled = scale(max_tmax),
         min_tmin_scaled = scale(min_tmin),
         mean_tavg_scaled = scale(mean_tavg))


MonthlyInfo <- MonthlyInfo[order(MonthlyInfo$Year, MonthlyInfo$Month),]

## Using climwin's method 1 to substitute any missing monthly values
# doing it here prevents the sliding.R script from doing it for every parallel job!

for (j in c(9, 11:15)) {
  for (i in which(is.na(MonthlyInfo[[j]]))){
    a <- c((i-2):(i+2))
    a <- ifelse(a < 1, 12- abs(a), ifelse(a > 12, a - 12, a))
    MonthlyInfo[i,j] <- mean(MonthlyInfo[[j]][a],
                             na.rm = T)
  }
}

### get SPEI values ----------------------------------------------------------------

# Compute potential evapotranspiration (PET) and climatic water balance (BAL)
MonthlyInfo$PET <- thornthwaite(MonthlyInfo$mean_tavg, sites$latitude[1]) 
MonthlyInfo$BAL <- MonthlyInfo$sum_prcp - MonthlyInfo$PET

# transform in 
Timescale <- ts(MonthlyInfo[,-c(1,2)],
                end = c(2018,12),
                frequency = 12)

# calculate SPEI - scale set to 12 months

SP <- spei(Timescale[,"BAL"], 12)

spei_df <- matrix(SP$fitted[1:(12*length(unique(MonthlyInfo$Year)))],
                  nrow = length(unique(MonthlyInfo$Year)), ncol = 12,
                  byrow = T) %>%
  as.data.frame %>%
  mutate(Year = c(min(MonthlyInfo$Year):max(MonthlyInfo$Year)))  %>%
  setNames(c(1:12, "Year")) %>%
  pivot_longer(-Year, names_to = "Month", values_to = "SPEI") %>%  # get it in similar form to merge with other climate df
  mutate(Month = as.numeric(Month))


All_Climate <- left_join(MonthlyInfo, spei_df) %>%
  select(Month, Year, sum_prcp_scaled, mean_tavg_scaled, mean_tmin_scaled, mean_tmax_scaled,
         min_tmin_scaled, max_tmax_scaled, SPEI) %>%
  rename(sum_prcp = sum_prcp_scaled,
         mean_tavg = mean_tavg_scaled,
         mean_tmin = mean_tmin_scaled,
         mean_tmax = mean_tmax_scaled,
         min_tmin = min_tmin_scaled,
         max_tmax = max_tmax_scaled)


write.csv(All_Climate, "Data/Climate data/CRFL_NOAA_month.csv" )



