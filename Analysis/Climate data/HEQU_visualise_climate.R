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

Monthly <- read.csv("Data/Climate data/HEQU_NOAA_month_imputed.csv" ) 


PrcpGrid <- ggplot(Monthly, aes(x= Month, y= mean_prcp))+
  geom_line(colour = "blue")+
  geom_ribbon(aes(ymin= (mean_prcp - sd_prcp), ymax= (mean_prcp + sd_prcp)), linetype = 2, alpha = 0.1,fill="blue")+
  facet_wrap(vars(Year)) +
  scale_x_continuous(breaks = c(1:12))+
  ylab("Mean monthly Precipitation (10th mm)")

SPrcpGrid <- ggplot(Monthly, aes(x= Month, y= sum_prcp))+
  geom_line(colour = "blue")+
  facet_wrap(vars(Year)) +
  scale_x_continuous(breaks = c(1:12))+
  ylab("Total monthly Precipitation (10th mm)")

TempGrid <- ggplot(Monthly, aes(x= Month, y= mean_tobs))+
  geom_line()+
  geom_ribbon(aes(ymin= mean_tmin, ymax= mean_tmax), linetype = 2, alpha = 0.2, fill = "blue")+
  geom_ribbon(aes(ymin= min_tmin, ymax= max_tmax), linetype = 2, alpha = 0, colour = "red")+
  facet_wrap(vars(Year))+
  scale_x_continuous(breaks = c(1:12))+
  ylab("Temperature (10th degrees)")

ggsave("Visual/HEQU_grid_Precipitation.png", PrcpGrid)
ggsave("Visual/HEQU_grid_Temperature.png", TempGrid)

