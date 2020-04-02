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


##Monthly data ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

MonthlyInfo <- clean %>%
  group_by(id = id, Month = month(date), Year = year(date)) %>%
  summarise(sum_prcp = sum(prcp),
            mean_tmax = mean(tmax, na.rm = T),
            mean_tmin = mean(tmin, na.rm = T),
            max_tmax = max(tmax, na.rm = T),
            min_tmin = min(tmin, na.rm = T),
            mean_tavg = mean(tavg, na.rm = T))

### scale Clim drivers ----------------------------------------------------------------

MonthlyInfo <- MonthlyInfo %>%                      
  group_by(Month) %>%
  mutate(sum_prcp_scaled = scale(sum_prcp),
         mean_tmax_scaled = scale(mean_tmax),
         mean_tmin_scaled = scale(mean_tmin),
         max_tmax_scaled = scale(max_tmax),
         min_tmin_scaled = scale(min_tmin),
         mean_tavg_scaled = scale(mean_tavg))

MonthlyInfo <- MonthlyInfo[order(MonthlyInfo$Year, MonthlyInfo$Month),]



for (j in c(4,9:15)) {
  for (i in which(is.na(MonthlyInfo[[j]]))){
    a <- c((i-2):(i+2))
    a <- ifelse(a < 1, 12- abs(a), ifelse(a > 12, a - 12, a))
    MonthlyInfo[i,j] <- mean(MonthlyInfo[[j]][a],
                             na.rm = T)
  }
  
}


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

All_Climate <- left_join(MonthlyInfo, spei_df) %>% 
  select(Month, Year, sum_prcp_scaled, mean_tavg_scaled, mean_tmin_scaled, mean_tmax_scaled,
         min_tmin_scaled, max_tmax_scaled, SPEI) %>%
  rename(sum_prcp = sum_prcp_scaled,
         mean_tavg = mean_tavg_scaled,
         mean_tmin = mean_tmin_scaled,
         mean_tmax = mean_tmax_scaled,
         min_tmin = min_tmin_scaled,
         max_tmax = max_tmax_scaled)


write.csv(All_Climate, "Data/Climate data/OPIM_SEVLTER_month_imputed.csv" )
