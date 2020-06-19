library(rnoaa)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(SPEI)

### CRFL population coordinates ------------------------------------------------------------------

sites <- data.frame(id = "Cumberland Pass" , 
                    station = NA, 
                    latitude = 38.693867, 
                    longitude = -106.475817, 
                    distance = NA,
                    elevation = 3772)


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

# find the 10 closest stations
Stations <- meteo_nearby_stations(lat_lon_df = sites,
                                  lat_colname =  "latitude",
                                  lon_colname = "longitude",
                                  station_data = all_stations,
                                  var = "all",
                                  limit = 10
)

# get information on closest station
Stations <- inner_join(Stations[[1]], all_stations) %>% 
  select(id, name, latitude, longitude, distance, elevation) %>% 
  unique()

# save nearby stations for location map
nearby_stations <- Stations[c(1:10),]
write.csv(nearby_stations, "Data/Climate data/FRSP_nearb_stations.csv")

#############################
## Get weather information ##
#############################

## retrieve climate data for 1st and 8th climate station

  for (k in c(1,8)) {

  closest_stations <- Stations[k,]
  
  # WeatherInfo <- meteo_pull_monitors(closest_stations$id, date_max = "2020-12-31", date_min = "1965-01-01")
  # WeatherInfo$population <- "Cumberland Pass"
  # WeatherInfo$prcp <- WeatherInfo$prcp / 10
  # WeatherInfo$tmax <- WeatherInfo$tmax / 10
  # WeatherInfo$tmin <- WeatherInfo$tmin / 10
  # WeatherInfo$tobs <- WeatherInfo$tobs / 10
  # write.csv(WeatherInfo, file = paste("Data/Climate data/FRSP_NOAA_", k, ".csv", sep = ""))

  
  ##########################################################################
  ## Add missing Clim data and Scale weather information  ---- DAILY ---- ##
  ##########################################################################
  
  WeatherInfo <- read.csv(paste("Data/Climate data/FRSP_NOAA_", k, ".csv", sep = "")) %>%
    select(id, date, prcp, tmax, tmin, tobs, population)
  WeatherInfo$date <-  as.Date(WeatherInfo$date, "%Y-%m-%d")
  WeatherInfo$X <- NULL
  
  # calculate average temperature
  WeatherInfo$tavg <- (WeatherInfo$tmin + WeatherInfo$tmax) / 2
  
  ##################################################
  ## Scale weather information  ---- Monthly ---- ##
  ##################################################
  
  # calculate monthly climate from daily data
  MonthlyInfo <- WeatherInfo %>%
    group_by(id, Month = month(date), Year = year(date)) %>%
    summarise(sum_prcp = sum(prcp, na.rm = T),
              mean_tavg = mean(tavg, na.rm = T),
              mean_tmax = mean(tmax, na.rm = T),
              mean_tmin = mean(tmin, na.rm = T),
              max_tmax = max(tmax, na.rm = T),
              min_tmin = min(tmin, na.rm = T))%>%
    mutate(mean_tmax = replace(mean_tmax, 
                               is.nan(mean_tmax), 
                               NA),
           mean_tmin = replace(mean_tmin,
                               is.nan(mean_tmin),
                               NA),
           mean_tavg = replace(mean_tavg,
                               is.nan(mean_tavg),
                               NA),
           max_tmax = replace(max_tmax, 
                              max_tmax == -Inf, 
                              NA),
           min_tmin = replace(min_tmin,
                              min_tmin == Inf,
                              NA))
  
  
  
  ### scale Clim drivers ----------------------------------------------------------------
  # scale for each month seperately 
  
  MonthlyInfo <- MonthlyInfo %>%                      
    group_by(id, Month) %>%
    mutate(sum_prcp_scaled = scale(sum_prcp),
           mean_tavg_scaled = scale(mean_tavg),
           mean_tmax_scaled = scale(mean_tmax),
           mean_tmin_scaled = scale(mean_tmin),
           max_tmax_scaled = scale(max_tmax),
           min_tmin_scaled = scale(min_tmin)) 
  
  
  MonthlyInfo <- MonthlyInfo[order(MonthlyInfo$Year, MonthlyInfo$Month),]
  
  # Use climwin's method 1 here to calculate any missing months - so it doesn't happen in each Sliding.R parallel run
  
  for (j in c(5,11:15)) {
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
    pivot_longer(-Year, names_to = "Month", values_to = "SPEI") %>%
    mutate(Month = as.numeric(Month))
  
  
  # merge SPEI dataframe with the rest of the climate data
  All_Climate <- left_join(MonthlyInfo, spei_df) %>% 
    select(Month, Year, sum_prcp_scaled, mean_tavg_scaled, mean_tmin_scaled, mean_tmax_scaled,
           min_tmin_scaled, max_tmax_scaled, SPEI) %>%
    rename(sum_prcp = sum_prcp_scaled,
           mean_tavg = mean_tavg_scaled,
           mean_tmin = mean_tmin_scaled,
           mean_tmax = mean_tmax_scaled,
           min_tmin = min_tmin_scaled,
           max_tmax = max_tmax_scaled)
  
  write.csv(All_Climate, paste("Data/Climate data/FRSP_NOAA_month_", k, ".csv", sep = ""))
  
  }

# compare/correlate climate from station 1 and station 8 
# compute missing months in station 1 using values from station 8

Clim1 <- read.csv("Data/Climate data/FRSP_NOAA_month_1.csv")
Clim8 <- read.csv("Data/Climate data/FRSP_NOAA_month_8.csv")

a <- full_join(Clim1, Clim8, by = c("Year", "Month")) 

b <- lm(sum_prcp.x ~ sum_prcp.y, a)
c <- lm(mean_tavg.x ~ mean_tavg.y, a)
d <- lm(mean_tmin.x ~ mean_tmin.y, a)
e <- lm(mean_tmax.x ~ mean_tmax.y, a)
f <- lm(min_tmin.x ~ min_tmin.y, a)
g <- lm(max_tmax.x ~ max_tmax.y, a)
h <- lm(SPEI.x ~ SPEI.y, a)

a <- a %>% mutate(sum_prcp.x = replace(sum_prcp.x, 
                                       is.na(sum_prcp.x), 
                                       predict(b, newdata = data.frame(sum_prcp.y = a$sum_prcp.y[which(is.na(a$sum_prcp.x))]))),
                  mean_tavg.x = replace(mean_tavg.x,
                                        is.na(mean_tavg.x),
                                        predict(c, newdata = data.frame(mean_tavg.y = a$mean_tavg.y[which(is.na(a$mean_tavg.x))]))),
                  mean_tmin.x = replace(mean_tmin.x,
                                        is.na(mean_tmin.x),
                                        predict(d, newdata = data.frame(mean_tmin.y = a$mean_tmin.y[which(is.na(a$mean_tmin.x))]))),
                  mean_tmax.x = replace(mean_tmax.x,
                                        is.na(mean_tmax.x),
                                        predict(e, newdata = data.frame(mean_tmax.y = a$mean_tmax.y[which(is.na(a$mean_tmax.x))]))),
                  min_tmin.x = replace(min_tmin.x,
                                        is.na(min_tmin.x),
                                        predict(f, newdata = data.frame(min_tmin.y = a$min_tmin.y[which(is.na(a$min_tmin.x))]))),
                  max_tmax.x = replace(max_tmax.x,
                                        is.na(max_tmax.x),
                                        predict(g, newdata = data.frame(max_tmax.y = a$max_tmax.y[which(is.na(a$max_tmax.x))]))),
                  SPEI.x = replace(SPEI.x,
                                   is.na(SPEI.x),
                                   predict(h, newdata = data.frame(SPEI.y = a$SPEI.y[which(is.na(a$SPEI.x))])))
)

All_Climate <- a %>%
  select(id.x, Month, Year, sum_prcp.x, mean_tavg.x, mean_tmin.x, mean_tmax.x, min_tmin.x, max_tmax.x, SPEI.x) %>%
  rename(id = id.x,
         sum_prcp = sum_prcp.x,
         mean_tavg = mean_tavg.x,
         mean_tmin = mean_tmin.x,
         mean_tmax = mean_tmax.x,
         min_tmin = min_tmin.x,
         max_tmax = max_tmax.x,
         SPEI = SPEI.x)

All_Climate <- All_Climate[order(All_Climate$Year, All_Climate$Month),]


write.csv(All_Climate, "Data/Climate data/FRSP_NOAA_month.csv")
