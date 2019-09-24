library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(lubridate)


### HEQU population coordinates ------------------------------------------------------------------


sites <- data.frame(id = c("low", "mid", "high"),
                    station = c("Low population", "Mid population", "High population"),
                    Name = c("low", "mid", "high"),
                    latitude = as.numeric(measurements::conv_unit(c("38 51.774", "38 57.5", "38 58.612"), from = 'deg_dec_min', to = 'dec_deg')),
                    longitude = as.numeric(measurements::conv_unit(c("-107 09.556", "-106 59.3", "-106 58.690"), from = 'deg_dec_min', to = 'dec_deg')),
                    distance = NA)


### Weather stations nearby -------------------------------------------------------------------------

# all_stations <- read.csv("Data/Climate data/all_stations.csv")
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
# closest_stations <- cbind(closest_stations, population = c(names(Stations)))
# 
# nearby_Stations <- rbind(Stations[[1]], Stations[[2]], Stations[[3]])
# nearby_Stations <- nearby_Stations[!duplicated(nearby_Stations$id) ,] %>%
#   rename( station = id,
#           Name = name) %>%
#   mutate(id = ifelse(station %in% closest_stations$id, "NOAA station", "NOAA option"))

# write.csv(nearby_Stations, "Data/Climate data/HEQU_nearb_stations.csv")


nearby_Stations <- read.csv("Data/Climate data/HEQU_nearb_stations.csv")
nearby_Stations$X <- NULL
nearby_Stations$id <- as.character(nearby_Stations$id)
nearby_Stations$id[which(nearby_Stations$station == "USC00051959" )] <- "USC00051959"
nearby_Stations$id[which(nearby_Stations$station == "USS0007K11S" )] <- "USS0007K11S"
nearby_Stations$id[which(nearby_Stations$station == "USS0006L11S" )] <- "USS0006L11S"


WeatherInfo <- read.csv("Data/Climate data/HEQU_NOAA_day.csv")
WeatherInfo$date <-  as.Date(WeatherInfo$date, "%d/%m/%Y")
WeatherInfo$X <- NULL

################################
## Plot populations and sites ##
################################
#  
locations <- rbind(sites, nearby_Stations)


pal <- colorFactor(c("#be29ec","#be29ec","#be29ec", "#F8766D", "#00BA38", "#619CFF", "gray50"), domain = c("low", "mid", "high","USS0007K11S","USS0006L11S", "USC00051959", "NOAA options"), ordered = T)

Map <- leaflet(locations) %>%
setView(lng = sum(sites$longitude)/3, lat = sum(sites$latitude)/3, zoom = 10) %>%
  addTiles() %>%
  addCircleMarkers(
    color = ~pal(id),
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~htmltools::htmlEscape(station)) %>%
  addLegend("bottomright",
            colors = c("#be29ec", "#F8766D", "#00BA38", "#619CFF", "gray50"),
            labels = c("Populations", "Station USS0007K11S","Station USS0006L11S", "Station USC00051959", "Other station options"),
            values = ~id, opacity = 1, title = "Locations") %>%
  addScaleBar()


# setwd("Visual/")
# saveWidget(Map, "HEQU_Locations.html")
# setwd("C:/owncloud/Documents/PhD/Biomes/Biome")



#############################
##   Explore weather info  ##
#############################


a <- WeatherInfo %>%
  group_by(Population = population, id = id, Year = format(date, "%Y") , Month = format(date, "%b")) %>%
  summarise(tot_prcp = sum(prcp, na.rm = T), 
            sd_prcp = sd(prcp, na.rm = T), 
            mean_temp = mean(tobs, na.rm = T), 
            sd_temp = sd(tobs, na.rm = T),
            min_temp = mean(tmin, na.rm = T),
            sd_min = sd(tmin, na.rm = T),
            max_temp = mean(tmax, na.rm = T),
            sd_max = sd(tmax, na.rm = T)
  )
a$Year <- as.integer(a$Year)
a$Month <- factor(a$Month, month.abb, ordered = T)
a$id <- factor(a$id, c("USS0007K11S","USS0006L11S", "USC00051959"))
# a$Month <- match(a$Month, month.abb)

gridTemp <- ggplot(a, aes(x = Month, y = mean_temp, colour = id))+
  geom_line(size = 2)+
  geom_ribbon(aes(ymin=a$min_temp, ymax = a$max_temp), linetype = 2, alpha = 0.1) +
  labs(title = "Monthly temperature over the years", x = "Month", y = "Mean temperature (C)") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  facet_wrap(vars(Year))

ggsave("Visual/HEQU_grid_Temperature.png", gridTemp)

gridPRCP <- ggplot(a, aes(x = Month, y = tot_prcp, colour = id))+
  geom_line(size = 2)+
  labs(title = "Monthly precipitation over the years", x = "Month", y = "Mean Precipitation (mm)") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  facet_wrap(vars(Year))

ggsave("Visual/HEQU_grid_Precipitation.png", gridPRCP)

ggplot(a, aes(x = Month, y = tot_prcp))+
  geom_boxplot() +
  scale_x_discrete(name = "Month") +
  scale_y_continuous(name = "Total Monthly precipitation (mm)")

b <- a %>%
  group_by(Population, id, Month) %>%
  summarise(mean_prcp = mean(tot_prcp), 
            sd_prcp = sd(tot_prcp), 
            mean_temp = mean(mean_temp),
            min_temp = mean(min_temp), 
            max_temp = mean(max_temp))
b$Month <- as.integer(b$Month)
b$id <- factor(b$id, c("USS0007K11S","USS0006L11S", "USC00051959"))

plotTemp <- ggplot(b, aes(x = Month, y = mean_temp, colour = id))+
  geom_line(size = 2)+
  geom_ribbon(aes(ymin=b$min_temp, ymax = b$max_temp), linetype = 2, alpha = 0.1) +
  labs(title = "Average monthly temperature", x = "Month", y = "Mean temperature (C)") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

ggsave("Visual/HEQU_Plot_Temperature.png", plotTemp)

plotPRCP <- ggplot(b, aes(x = Month, y = mean_prcp, colour = id))+
  geom_line(size = 2)+
  geom_ribbon(aes(ymin=b$mean_prcp-b$sd_prcp, ymax = b$mean_prcp+b$sd_prcp), linetype = 2, alpha = 0.1) +
  labs(title = "Average monthly precipitation", x = "Month", y = "Mean Precipitation (mm)") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

ggsave("Visual/HEQU_Plot_Precipitation.png", plotPRCP)




