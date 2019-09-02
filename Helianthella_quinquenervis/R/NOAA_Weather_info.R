library(rnoaa)
library(dplyr)
library(ggplot2)
library(leaflet)
library(lubridate)

lat <- c("38 51.774", "38 57.5", "38 58.612")
lon <- c("-107 09.556", "-106 59.3", "-106 58.690")
lat.dec = as.numeric(measurements::conv_unit(lat, from = 'deg_dec_min', to = 'dec_deg'))
lon.dec = as.numeric(measurements::conv_unit(lon, from = 'deg_dec_min', to = 'dec_deg'))

id <- c("low", "mid", "high")                                                                            ### DIFFERENT POPULATIONS? CHANGE THESE

sites <- data.frame(id = id, station = NA, latitude = lat.dec, longitude = lon.dec, distance = NA)


############################
## Find Weather Stations  ##
############################


all_stations <- ghcnd_stations() %>% 
  filter(first_year <= 1997, last_year >= 2012)
  

Stations <- meteo_nearby_stations(lat_lon_df = sites,
                                  lat_colname =  "latitude",
                                  lon_colname = "longitude",
                                  station_data = all_stations,
                                  var = "all",
                                  limit = 6
                                  )

closest_stations <- rbind(Stations[[1]][1,], Stations[[2]][1,], Stations[[3]][1,])
closest_stations <- closest_stations[!duplicated(closest_stations$id),]

nearby_Stations <- data.frame( station = c(Stations[[1]]$id, Stations[[2]]$id, Stations[[3]]$id),               ### AND HERE A FEW TIMES
                               latitude = c(Stations[[1]]$latitude, Stations[[2]]$latitude, Stations[[3]]$latitude), ### IT SHOULD THEN RUN AGAIN
                               longitude = c(Stations[[1]]$longitude, Stations[[2]]$longitude, Stations[[3]]$longitude),
                               distance = c(Stations[[1]]$distance, Stations[[2]]$distance, Stations[[3]]$distance))
nearby_Stations$id <- ifelse(nearby_Stations$station %in% closest_stations$id, "NOAA station", "NOAA options")
nearby_Stations <- nearby_Stations[!duplicated(nearby_Stations$station),]



################################
## Plot populations and sites ##
################################


locations <- rbind(sites, nearby_Stations)

pal <- colorFactor(c("navy","navy","navy", "gray50", "red"), domain = c("low", "mid", "high", "NOAA station", "NOAA options"))

leaflet(locations) %>%
  setView(lng = -107.009551, lat = 39, zoom = 10) %>%
  addTiles() %>%
  addCircleMarkers(
    color = ~pal(id),
    fillOpacity = 1,
    stroke = FALSE
  ) %>%
  addScaleBar()
  

#############################
## Get weather information ##
#############################



WeatherInfo <- meteo_pull_monitors(closest_stations$id, date_max = "2012-12-31", date_min = "1997-01-01")
WeatherInfo$population <- ifelse(WeatherInfo$id == Stations$high$id[1], "high", ifelse(WeatherInfo$id == Stations$low$id[1], "low", "mid"))
WeatherInfo$prcp <- WeatherInfo$prcp / 10
WeatherInfo$tavg <- WeatherInfo$tavg / 10
WeatherInfo$tmax <- WeatherInfo$tmax / 10
WeatherInfo$tmin <- WeatherInfo$tmin / 10
WeatherInfo$tobs <- WeatherInfo$tobs / 10

# 
# 
# write.csv(WeatherInfo, file = "NOAA_HEQU.csv")

WeatherInfo <- read.csv("NOAA_Weather_info.R")


#############################
##   Explore weather info  ##
#############################


a <- WeatherInfo %>%
  group_by(Population = population, Year = format(date, "%Y") , Month = format(date, "%b")) %>%
  summarise(tot_prcp = sum(prcp, na.rm = T), 
            sd_prcp = sd(prcp, na.rm = T), 
            mean_temp = sum(tobs, na.rm = T), 
            sd_temp = sd(tobs, na.rm = T),
            min_temp = sum(tmin, na.rm = T),
            sd_min = sd(tmin, na.rm = T),
            max_temp = sum(tmax, na.rm = T),
            sd_max = sum(tmax, na.rm = T)
            )
a$Year <- as.integer(a$Year)
a$Month <- factor(a$Month, month.abb, ordered = T)

ggplot(a, aes(x = Month, y = tot_prcp))+
  geom_boxplot() +
  scale_x_discrete(name = "Month") +
  scale_y_continuous(name = "Total Monthly precipitation (mm)")

b <- a %>%
  group_by(Population, Month) %>%
  summarise(mean_prcp = mean(tot_prcp), 
            sd_prcp = sd(tot_prcp), 
            mean_temp = mean(mean_temp),
            min_prcp = mean(min_temp), 
            max_prcp = mean(max_temp))
b$Month <- as.integer(b$Month)

ggplot(b, aes(x = Month, y = mean_prcp, colour = Population))+
  geom_line(size = 2)+
  geom_ribbon(aes(ymin=b$mean_prcp-b$sd_prcp, ymax = b$mean_prcp + b$sd_prcp), linetype = 2, alpha = 0.1)

ggplot(b, aes(x = Month, y = mean_temp, colour = Population))+
  geom_line(size = 2)+
  geom_ribbon(aes(ymin=min_prcp, ymax = max_prcp), linetype = 2, alpha = 0.1)


