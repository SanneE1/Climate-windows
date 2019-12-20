library(rnoaa)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(SPEI)

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
all_stations <- read.csv("Data/Climate data/all_stations.csv") %>%
  filter(first_year <= 1996, last_year >= 2012)


Stations <- meteo_nearby_stations(lat_lon_df = sites,
                                  lat_colname =  "latitude",
                                  lon_colname = "longitude",
                                  station_data = all_stations,
                                  var = "all",
                                  limit = 10
                                  )

mid_station <- Stations[["mid"]][1,]
low_station <- Stations[["low"]][1,]



#############################
## Get weather information ##
#############################

# low <- meteo_pull_monitors(low_station$id, date_max = "2012-12-31", date_min = "1990-01-01")
# low$prcp <- low$prcp / 10
# low$tmax <- low$tmax / 10
# low$tmin <- low$tmin / 10
# low$tobs <- low$tobs / 10
# 
# mid <- meteo_pull_monitors(mid_station$id, date_max = "2012-12-31", date_min = "1990-01-01")
# mid$prcp <- mid$prcp / 10
# mid$tavg <- mid$tavg / 10
# mid$tmax <- mid$tmax / 10
# mid$tmin <- mid$tmin / 10
# mid$tobs <- mid$tobs / 10
# 
# write.csv(low, "Data/Climate data/HEQU_lowstation_original.csv")
# write.csv(mid, "Data/Climate data/HEQU_midstation_original.csv")


low <- read.csv("Data/Climate data/HEQU_lowstation_original.csv") %>%
  mutate(date = as.Date(date))

mid <- read.csv("Data/Climate data/HEQU_midstation_original.csv") %>%
  mutate(date = as.Date(date))

mid$tobs[which(mid$date > as.Date("2002-07-15") & mid$date < as.Date("2003-07-15"))] <- NA
mid$tmin[which(mid$date > as.Date("2002-07-15") & mid$date < as.Date("2003-07-15"))] <- NA
mid$tmax[which(mid$date > as.Date("2002-07-15") & mid$date < as.Date("2003-07-15"))] <- NA
mid$tavg[which(mid$date > as.Date("2002-07-15") & mid$date < as.Date("2003-07-15"))] <- NA
mid$tmin[which(mid$date == as.Date("1992-06-23"))] <- NA   ## Remove extreme values from this date (-51.3)
mid$tmax[which(mid$date == as.Date("1992-06-23"))] <- NA
mid$tavg[which(mid$date == as.Date("1992-06-23"))] <- NA


both <- merge(low, mid, by = "date", all = T)

### Tavg
tavg.l <- glm(tavg ~ tmax.x + tmin.x + tobs.x + tmax.y + tmin.y + tobs.y, data = both)

both$ptavg <- predict(tavg.l, both)
both$tavg[which(is.na(both$tavg))] <- both$ptavg[which(is.na(both$tavg))]

### Tobs
tobs.l <- glm(tobs.y ~ tobs.x, data = both)
# plot(tobs.l)

both$ptobs <- predict(tobs.l, both)
both$tobs.y[which(is.na(both$tobs.y))] <- both$ptobs[which(is.na(both$tobs.y))]

### Tmin
tmin.l <- glm(tmin.y ~ tmin.x, data = both)
# plot(tmin.l)

both$ptmin <- predict(tmin.l, both)
both$tmin.y[which(is.na(both$tmin.y))] <- both$ptmin[which(is.na(both$tmin.y))]


### Tmax
tmax.l <- glm(tmax.y ~ tmax.x, data = both)
# plot(tmax.l)

both$ptmax <- predict(tmax.l, both)
both$tmax.y[which(is.na(both$tmax.y))] <- both$ptmax[which(is.na(both$tmax.y))]


clean <- both[,c("date", "id.y", "prcp.y", "tmax.y", "tmin.y", "tobs.y", "tavg")] %>%
  rename(id = id.y,
         prcp = prcp.y,
         tmax = tmax.y,
         tmin = tmin.y,
         tobs = tobs.y,
         tavg = tavg)


## Using Climwin's Method 1 to subsitude the last few values
for (j in c("prcp", "tmax", "tmin", "tobs", "tavg")) {
  for (i in which(is.na(clean[[j]]))){
    clean[i,j] <- mean(clean[[j]][which(clean$date %in%
                                          c(clean$date[i] - (1:2), clean$date[i] + (1:2)))],
                       na.rm = T)
  }
  
}



##Monthly data ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

MonthlyInfo <- clean %>%
  group_by(Month = month(date), Year = year(date)) %>%
  summarise(sum_prcp = sum(prcp),
            mean_prcp = mean(prcp),
            sd_prcp = sd(prcp),
            mean_tobs = mean(tobs, na.rm = T),
            sd_tobs = sd(tobs, na.rm = T),
            mean_tmax = mean(tmax, na.rm = T),
            mean_tmin = mean(tmin, na.rm = T),
            max_tmax = max(tmax, na.rm = T),
            min_tmin = min(tmin, na.rm = T),
            mean_tavg = mean(tavg, na.rm = T))



### scale Clim drivers ----------------------------------------------------------------

MonthlyInfo <- MonthlyInfo %>%
  group_by(Month) %>%
  mutate(sum_prcp_scaled = scale(sum_prcp),
         mean_prcp_scaled = scale(mean_prcp),
         sd_prcp_scaled = scale(sd_prcp),
         mean_tobs_scaled = scale(mean_tobs),
         sd_tobs_scaled = scale(sd_tobs),
         mean_tmax_scaled = scale(mean_tmax),
         mean_tmin_scaled = scale(mean_tmin),
         max_tmax_scaled = scale(max_tmax),
         min_tmin_scaled = scale(min_tmin),
         mean_tavg_scaled = scale(mean_tavg))


MonthlyInfo <- MonthlyInfo[order(MonthlyInfo$Year),]


### get SPEI values ----------------------------------------------------------------

# Compute potential evapotranspiration (PET) and climatic water balance (BAL)
MonthlyInfo$PET <- thornthwaite(MonthlyInfo$mean_tavg, sites$latitude[2]) 
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

write.csv(All_Climate, "Data/Climate data/HEQU_NOAA_month_imputed.csv" )



