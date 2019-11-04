library(SPEI)
library(dplyr)
library(tidyr)
library(lubridate)

#############################################################
### Using the data downloaded from SEV-LTER on 24/10/2019 ###
#############################################################

### Climate data from the 3 closest stations. 50 is the closest to the population
st50 <- read.csv("Data/Climate data/OPIM_SEVLTER50.csv")
st49 <- read.csv("Data/Climate data/OPIM_SEVLTER49.csv")
st40 <- read.csv("Data/Climate data/OPIM_SEVLTER40.csv")

### Reformat climate data
st50[st50=="-999"] <- NA
st40[st40=="-999"] <- NA
st49[st49=="-999"] <- NA

st50$Date <- as.Date(st50$Date, "%Y-%m-%d")
st40$Date <- as.Date(st40$Date, "%Y-%m-%d")
st49$Date <- as.Date(st49$Date, "%Y-%m-%d")

full_dates <- data.frame(Date = seq(min(st40$Date), max(st40$Date), by = "1 day"))

st50 <- merge(full_dates, st50, by = "Date",
              all.x = T)
st40 <- merge(full_dates, st40, by = "Date",
              all.x = T)
st49 <- merge(full_dates, st49, by = "Date",
              all.x = T)

st50 <- st50[,c(1:5,7)]
st40 <- st40[,c(1:5,7)]
st49 <- st49[,c(1:5,7)]

all <- merge(st40, st49, by = "Date", all = T)
all <- merge(all, st50, by = "Date", all = T)
all$Sta <- 50
all$Min.Temp[which(all$Min.Temp == -40)] <- NA

### Impute missing values from station 50 using the next two closest stations --------------------------------------------

### Avg.Temp
tavg.l <- glm(Avg.Temp ~ Avg.Temp.x + Avg.Temp.y, data = all)

all$ptavg <- predict(tavg.l, all)
all$Avg.Temp[which(is.na(all$Avg.Temp))] <- all$ptavg[which(is.na(all$Avg.Temp))]

### min.Temp
tmin.l <- glm(Min.Temp ~ Min.Temp.x + Min.Temp.y, data = all)

all$ptmin <- predict(tmin.l, all)
all$Min.Temp[which(is.na(all$Min.Temp))] <- all$ptmin[which(is.na(all$Min.Temp))]

### min.Temp
tmax.l <- glm(Max.Temp ~ Max.Temp.x + Max.Temp.y, data = all)

all$ptmax <- predict(tmax.l, all)
all$Max.Temp[which(is.na(all$Max.Temp))] <- all$ptmax[which(is.na(all$Max.Temp))]

### Precip
prcp.l <- glm(Precip ~ Precip.x + Precip.y, data = all)

all$pprcp <- predict(prcp.l, all)
all$Precip[which(is.na(all$Precip))] <- all$pprcp[which(is.na(all$Precip))]

### 2nd round using only station 40 ------------------------------------------------------------------------------------

### Avg.Temp
tavg.l <- glm(Avg.Temp ~ Avg.Temp.x, data = all)

all$ptavg <- predict(tavg.l, all)
all$Avg.Temp[which(is.na(all$Avg.Temp))] <- all$ptavg[which(is.na(all$Avg.Temp))]

### min.Temp
tmin.l <- glm(Min.Temp ~ Min.Temp.x , data = all)

all$ptmin <- predict(tmin.l, all)
all$Min.Temp[which(is.na(all$Min.Temp))] <- all$ptmin[which(is.na(all$Min.Temp))]

### min.Temp
tmax.l <- glm(Max.Temp ~ Max.Temp.x , data = all)

all$ptmax <- predict(tmax.l, all)
all$Max.Temp[which(is.na(all$Max.Temp))] <- all$ptmax[which(is.na(all$Max.Temp))]

### Precip
prcp.l <- glm(Precip ~ Precip.x, data = all)

all$pprcp <- predict(prcp.l, all)
all$Precip[which(is.na(all$Precip))] <- all$pprcp[which(is.na(all$Precip))]

### Clean up dataframe -----------------------------------------------------------------------------------------

clean <- all[, c("Date", "Sta", "Avg.Temp", "Max.Temp", "Min.Temp", "Precip")] %>%
  rename(
    date = Date,
    id = Sta,
    tavg = Avg.Temp,
    tmax = Max.Temp,
    tmin = Min.Temp,
    prcp = Precip)

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
  group_by(id = id, Month = month(date), Year = year(date)) %>%
  summarise(sum_prcp = sum(prcp),
            mean_prcp = mean(prcp),
            sd_prcp = sd(prcp),
            mean_tmax = mean(tmax, na.rm = T),
            mean_tmin = mean(tmin, na.rm = T),
            max_tmax = max(tmax, na.rm = T),
            min_tmin = min(tmin, na.rm = T),
            mean_tavg = mean(tavg, na.rm = T),
            sd_tavg = sd(tavg, na.rm = T))



### scale Clim drivers ----------------------------------------------------------------

MonthlyInfo <- MonthlyInfo %>%                      
  group_by(Month) %>%
  mutate(sum_prcp_scaled = scale(sum_prcp),
         mean_prcp_scaled = scale(mean_prcp),
         sd_prcp_scaled = scale(sd_prcp),
         mean_tmax_scaled = scale(mean_tmax),
         mean_tmin_scaled = scale(mean_tmin),
         max_tmax_scaled = scale(max_tmax),
         min_tmin_scaled = scale(min_tmin),
         mean_tavg_scaled = scale(mean_tavg),
         sd_tavg_scaled = scale(sd_tavg))
  
MonthlyInfo <- MonthlyInfo[order(MonthlyInfo$Year),]


### get SPEI values ----------------------------------------------------------------

# Compute potential evapotranspiration (PET) and climatic water balance (BAL)
MonthlyInfo$PET <- thornthwaite(MonthlyInfo$mean_tavg, 34.334806) 
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


write.csv(All_Climate, "Data/Climate data/OPIM_SEVLTER_month_imputed.csv" )













############################################################################
### Retrieve the data from NOAA (But the station is a lot further away!) ###
############################################################################

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


# ############################
# ## Find Weather Stations  ##
# ############################
# 
# 
# all_stations <- ghcnd_stations() 
# 
# write.csv(all_stations, "Data/Climate data/all_stations.csv")
# 
# all_stations <- read.csv("Data/Climate data/all_stations.csv") %>%
#   filter(first_year <= 2000, last_year >= 2019)
# 
# 
# Stations <- meteo_nearby_stations(lat_lon_df = sites,
#                                   lat_colname =  "latitude",
#                                   lon_colname = "longitude",
#                                   station_data = all_stations,
#                                   var = "all",
#                                   limit = 10
# )
# 
# closest_stations <- Stations[[1]][1,]
# 
# nearby_stations <- Stations[[1]][c(1:10),]
# write.csv(nearby_stations, "Data/Climate data/OPIM_nearby_stations.csv")
# 
# #############################
# ## Get weather information ##
# #############################
# 
# 
# 
# WeatherInfo <- meteo_pull_monitors(nearby_stations$id[1], date_max = "2019-12-31", date_min = "2000-01-01")
# WeatherInfo$population <- "Sevilleta"
# WeatherInfo$prcp <- WeatherInfo$prcp / 10
# WeatherInfo$tmax <- WeatherInfo$tmax / 10
# WeatherInfo$tmin <- WeatherInfo$tmin / 10
# WeatherInfo$tobs <- WeatherInfo$tobs / 10
# write.csv(WeatherInfo, file = "Data/Climate data/OPIM_NOAA.csv")
# 
# 
# 
# ##########################################################################
# ## Add missing Clim data and Scale weather information  ---- DAILY ---- ##
# ##########################################################################
# 
# WeatherInfo <- read.csv("Data/Climate data/OPIM_NOAA.csv")
# WeatherInfo$date <-  as.Date(WeatherInfo$date, "%Y-%m-%d")
# WeatherInfo$X <- NULL
# 
# ## Using Climwin's Method 1 to subsitude the last few values
# for (j in c(6:11)) {
#   for (i in which(is.na(WeatherInfo[[j]]))){
#     WeatherInfo[i,j] <- mean(WeatherInfo[[j]][which(WeatherInfo$id == WeatherInfo$id[i] &
#                                                       WeatherInfo$date %in% 
#                                                       c(WeatherInfo$date[i] - (1:2), WeatherInfo$date[i] + (1:2)))],
#                              na.rm = T)
#   }
#   
# }
# 
# 
# ############################################################################
# ## Add missing Clim data and Scale weather information  ---- Monthly ---- ##
# ############################################################################
# 
# MonthlyInfo <- WeatherInfo %>%
#   group_by(id, Month = month(date), Year = year(date)) %>%
#   summarise(sum_prcp = sum(prcp),
#             mean_prcp = mean(prcp),
#             sd_prcp = sd(prcp),
#             mean_tobs = mean(tobs),
#             sd_tobs = sd(tobs),
#             mean_tmax = mean(tmax),
#             mean_tmin = mean(tmin, na.rm = T),
#             max_tmax = max(tmax),
#             min_tmin = min(tmin, na.rm = T))
# 
# 
# 
# ### scale Clim drivers ----------------------------------------------------------------
# 
# MonthlyInfo <- MonthlyInfo %>%                      
#   group_by(id, Month) %>%
#   mutate(sum_prcp_scaled = scale(sum_prcp),
#          mean_prcp_scaled = scale(mean_prcp),
#          sd_prcp_scaled = scale(sd_prcp),
#          mean_tobs_scaled = scale(mean_tobs),
#          sd_tobs_scaled = scale(sd_tobs),
#          mean_tmax_scaled = scale(mean_tmax),
#          mean_tmin_scaled = scale(mean_tmin),
#          max_tmax_scaled = scale(max_tmax),
#          min_tmin_scaled = scale(min_tmin))
# 
# 
# 
# write.csv(MonthlyInfo, "Data/Climate data/OPIM_NOAA_month.csv" )
