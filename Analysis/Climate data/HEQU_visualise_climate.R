setwd("C:/owncloud/Documents/PhD/Biomes/Biome")

library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(lubridate)


### HEQU population coordinates ------------------------------------------------------------------


sites <- data.frame(id = c("low", "mid", "high"),
                    station = NA,
                    latitude = as.numeric(measurements::conv_unit(c("38 51.774", "38 57.5", "38 58.612"), from = 'deg_dec_min', to = 'dec_deg')),
                    longitude = as.numeric(measurements::conv_unit(c("-107 09.556", "-106 59.3", "-106 58.690"), from = 'deg_dec_min', to = 'dec_deg')),
                    distance = NA)

nearby_Stations <- read.csv("Data/Climate data/HEQU_nearb_stations.csv")
nearby_Stations$X <- NULL

### Weather stations nearby -------------------------------------------------------------------------

# all_stations <- read.csv("Data/Climate data/HEQU_all_stations.csv")
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
# closest_stations <- rbind(Stations[[1]][1,], Stations[[2]][1,], Stations[[3]][1,])
# 
# nearby_Stations <- rbind(unnest(Stations$high),
#                          unnest(Stations$mid),
#                          unnest(Stations$low)) %>%
#   rename(station = id)
# nearby_Stations$name <- NULL
# nearby_Stations <- nearby_Stations[!duplicated(nearby_Stations$station),]
# nearby_Stations$id <- ifelse(nearby_Stations$station %in% closest_stations$id, "NOAA station", "NOAA options")
# 
# write.csv(nearby_Stations, "Data/Climate data/HEQU_nearb_stations.csv")

nearby_Stations <- read.csv("Data/Climate data/HEQU_nearb_stations.csv")

WeatherInfo <- read.csv("Data/Climate data/HEQU_NOAA_supplemented.csv")
WeatherInfo$date <-  as.Date(WeatherInfo$date, "%Y-%m-%d")


################################
## Plot populations and sites ##
################################
 
locations <- rbind(sites, nearby_Stations)

pal <- colorFactor(c("#D6FA8C","#A5D721","#5D8700", "red", "gray50"), domain = c("low", "mid", "high", "NOAA station", "NOAA options"), ordered = T)

Map <- leaflet(locations) %>%
  setView(lng = sum(sites$longitude)/3, lat = sum(sites$latitude)/3, zoom = 10) %>%
  addTiles() %>%
  addCircleMarkers(
    color = ~pal(id),
    fillOpacity = 1,
    stroke = FALSE) %>%
  addLegend("bottomright",
            colors = c("#D6FA8C","#A5D721","#5D8700", "red", "gray50"),
            labels = c("Low population", "Mid population", "High population", "NOAA station", "NOAA options"),
            values = ~id, opacity = 1, title = "Locations") %>%
  addScaleBar()

setwd("Visual/")
saveWidget(Map, "HEQU_Locations.html")
setwd("C:/owncloud/Documents/PhD/Biomes/Biome")



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
  geom_ribbon(aes(ymin=b$mean_prcp-b$sd_prcp, ymax = b$mean_prcp + b$sd_prcp), linetype = 2, alpha = 0.1) +
  labs(title = "Average monthly temperature", x = "Month", y = "Mean temperature (°C)") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))
  

ggplot(b, aes(x = Month, y = mean_temp, colour = Population))+
  geom_line(size = 2)+
  geom_ribbon(aes(ymin=min_prcp, ymax = max_prcp), linetype = 2, alpha = 0.1) +
  labs(title = "Average monthly precipitation", x = "Month", y = "Mean Precipitation (mm)") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))





