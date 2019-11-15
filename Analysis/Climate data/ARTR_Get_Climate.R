library(rnoaa)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(SPEI)

### CRFL population coordinates ------------------------------------------------------------------

sites <- data.frame(id = "Sheep Station" , 
                    station = NA, 
                    latitude = 44.244101, 
                    longitude = -112.200256, 
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

closest_stations <- Stations[[1]][c(1,2),]

nearby_stations <- Stations[[1]][c(1:10),]
write.csv(nearby_stations, "Data/Climate data/ARTR_nearb_stations.csv")

#############################
## Get weather information ##
#############################


# 
# Clim1 <- meteo_pull_monitors(closest_stations$id[1], date_max = "2016-12-31", date_min = "1920-01-01")
# Clim1$population <- "Sheep Station"
# Clim1$prcp <- Clim1$prcp / 10
# Clim1$tmax <- Clim1$tmax / 10
# Clim1$tmin <- Clim1$tmin / 10
# Clim1$tobs <- Clim1$tobs / 10
# Clim1$tavg <- Clim1$tavg / 10
# write.csv(Clim1, file = "Data/Climate data/ARTR_1_NOAA.csv")
# 
# Clim2 <- meteo_pull_monitors(closest_stations$id[2], date_max = "2016-12-31", date_min = "1920-01-01")
# Clim2$population <- "Sheep Station"
# Clim2$prcp <- Clim2$prcp / 10
# Clim2$tmax <- Clim2$tmax / 10
# Clim2$tmin <- Clim2$tmin / 10
# Clim2$tobs <- Clim2$tobs / 10
# Clim2$tavg <- Clim2$tavg / 10
# write.csv(Clim2, file = "Data/Climate data/ARTR_2_NOAA.csv")

Clim1 <- read.csv("Data/Climate data/ARTR_1_NOAA.csv") %>%
  mutate(date = as.Date(date))
Clim1 <- Clim1[,c("id", "date", "prcp", "snow", "snwd", "tmax", "tmin", "tobs")]

Clim2 <- read.csv("Data/Climate data/ARTR_2_NOAA.csv") %>%
  mutate(date = as.Date(date))
Clim2 <- Clim2[,c("id", "date", "prcp", "snow", "snwd", "tmax", "tmin", "tobs")]

both <- merge(Clim1, Clim2, by = "date", all = T)

### prcp
prcp.l <- glm(prcp.x ~ prcp.y, data = both)
# plot(tmax.l)

both$pprcp <- predict(prcp.l, both)
both$prcp.x[which(is.na(both$prcp.x))] <- both$pprcp[which(is.na(both$prcp.x))]

### snow
snow.l <- glm(snow.x ~ snow.y, data = both)
# plot(snow.l)

both$psnow <- predict(snow.l, both)
both$snow.x[which(is.na(both$snow.x))] <- both$psnow[which(is.na(both$snow.x))]

### snwd
snwd.l <- glm(snwd.x ~ snwd.y, data = both)
# plot(snwd.l)

both$psnwd <- predict(snwd.l, both)
both$snwd.x[which(is.na(both$snwd.x))] <- both$psnwd[which(is.na(both$snwd.x))]

### Tmax
tmax.l <- glm(tmax.x ~ tmax.y, data = both)
# plot(tmax.l)

both$ptmax <- predict(tmax.l, both)
both$tmax.x[which(is.na(both$tmax.x))] <- both$ptmax[which(is.na(both$tmax.x))]

### Tmin
tmin.l <- glm(tmin.x ~ tmin.y, data = both)
# plot(tmin.l)

both$ptmin <- predict(tmin.l, both)
both$tmin.x[which(is.na(both$tmin.x))] <- both$ptmin[which(is.na(both$tmin.x))]

### Tobs
tobs.l <- glm(tobs.x ~ tobs.y, data = both)
# plot(tmin.l)

both$ptobs <- predict(tobs.l, both)
both$tobs.x[which(is.na(both$tobs.x))] <- both$ptobs[which(is.na(both$tobs.x))]


#### Clean up dataset ----------------------------------------------------------------------------------------------
clean <- both[,c("date", "id.x", "prcp.x", "snow.x", "snwd.x", "tmax.x", "tmin.x", "tobs.x")] %>%
  rename(id = id.x,
         prcp = prcp.x,
         snow = snow.x,
         snwd = snwd.x,
         tmax = tmax.x,
         tmin = tmin.x,
         tobs = tobs.x)



## Using Climwin's Method 1 to subsitude the last few values
for (j in c("prcp", "tmax", "tmin", "tobs")) {
  for (i in which(is.na(clean[[j]]))){
    clean[i,j] <- mean(clean[[j]][which(clean$date %in%
                                          c(clean$date[i] - (1:2), clean$date[i] + (1:2)))],
                       na.rm = T)
  }
  
}



##Monthly data ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

MonthlyInfo <- clean %>%
  group_by(Month = month(date), Year = year(date)) %>%
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
  group_by(Month) %>%
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
                end = c(2016,12),
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

write.csv(All_Climate, "Data/Climate data/ARTR_NOAA_month_imputed.csv" )



