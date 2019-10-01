library(dplyr)
library(lubridate)

low <- read.csv("Data/Climate data/HEQU_lowstation_original.csv") %>%
  mutate(date = as.Date(date))

mid <- read.csv("Data/Climate data/HEQU_midstation_original.csv") %>%
  mutate(date = as.Date(date))
mid$tobs[which(mid$date > as.Date("2002-07-15") & mid$date < as.Date("2003-07-15"))] <- NA
mid$tmin[which(mid$date > as.Date("2002-07-15") & mid$date < as.Date("2003-07-15"))] <- NA
mid$tmax[which(mid$date > as.Date("2002-07-15") & mid$date < as.Date("2003-07-15"))] <- NA

both <- merge(low, mid, by = "date", all.x = T)

### Tobs

# ggplot(data = both, aes(x= tobs.x, y= tobs.y)) +
#   geom_point() +
#   geom_smooth(method = "glm", se = T)

tobs.l <- glm(tobs.y ~ tobs.x, data = both)
# plot(tobs.l)

both$ptobs <- predict(tobs.l, both)
both$tobs.y[which(is.na(both$tobs.y))] <- both$ptobs[which(is.na(both$tobs.y))]

### Tmin
# 
# ggplot(data = both, aes(x= tmin.x, y= tmin.y)) +
#   geom_point() +
#   geom_smooth(method = "glm", se = T)

tmin.l <- glm(tmin.y ~ tmin.x, data = both)
# plot(tmin.l)

both$ptmin <- predict(tmin.l, both)
both$tmin.y[which(is.na(both$tmin.y))] <- both$ptmin[which(is.na(both$tmin.y))]


### Tmax
# ggplot(data = both, aes(x= tmax.x, y= tmax.y)) +
#   geom_point() +
#   geom_smooth(method = "glm", se = T)

tmax.l <- glm(tmax.y ~ tmax.x, data = both)
# plot(tmax.l)

both$ptmax <- predict(tmax.l, both)
both$tmax.y[which(is.na(both$tmax.y))] <- both$ptmax[which(is.na(both$tmax.y))]

clean <- both[,c("date", "id.y", "prcp.y", "tmax.y", "tmin.y", "tobs.y")] %>%
  rename(id = id.y,
         prcp = prcp.y,
         tmax = tmax.y,
         tmin = tmin.y,
         tobs = tobs.y)


## Using Climwin's Method 1 to subsitude the last few values
for (j in c("prcp", "tmax", "tmin", "tobs")) {
  for (i in which(is.na(clean[[j]]))){
    clean[i,j] <- mean(clean[[j]][which(clean$date %in% 
                                          c(clean$date[i] - (1:2), clean$date[i] + (1:2)))],
                           na.rm = T)
  }
  
}


DailyInfo <- clean
DailyInfo$date <- format(DailyInfo$date, format = "%d/%m/%Y")

DailyInfo <- DailyInfo %>%                      
  group_by(month(date)) %>%
  mutate(prcp_scaled_M = scale(prcp),
         tmax_scaled_M = scale(tmax),
         tmin_scaled_M = scale(tmin),
         tobs_scaled_M = scale(tobs)
  )


write.csv(DailyInfo, "Data/Climate data/HEQU_NOAA_day_imputed.csv" )



##Monthly data ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

MonthlyInfo <- clean %>%
  group_by(Month = month(date), Year = year(date)) %>%
  summarise(sum_prcp = sum(prcp),
            mean_prcp = mean(prcp),
            sd_prcp = sd(prcp),
            mean_tobs = mean(tobs),
            sd_tobs = sd(tobs),
            mean_tmax = mean(tmax),
            mean_tmin = mean(tmin),
            max_tmax = max(tmax),
            min_tmin = min(tmin))


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


write.csv(MonthlyInfo, "Data/Climate data/HEQU_NOAA_month_imputed.csv" )




