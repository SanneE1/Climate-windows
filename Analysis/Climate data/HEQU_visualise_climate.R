library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(lubridate)
library(mapview)

### HEQU population coordinates ------------------------------------------------------------------


sites <- data.frame(id = c("low", "mid", "high"),
                    station = c("Low population", "Mid population", "High population"),
                    Name = c("low", "mid", "high"),
                    latitude = as.numeric(measurements::conv_unit(c("38 51.774", "38 57.5", "38 58.612"), from = 'deg_dec_min', to = 'dec_deg')),
                    longitude = as.numeric(measurements::conv_unit(c("-107 09.556", "-106 59.3", "-106 58.690"), from = 'deg_dec_min', to = 'dec_deg')),
                    distance = NA)


### Weather stations nearby -------------------------------------------------------------------------
nearby_Stations <- read.csv("Data/Climate data/HEQU_nearb_stations.csv")
nearby_Stations$X <- NULL
nearby_Stations$id <- as.character(nearby_Stations$id)


################################
## Plot populations and sites ##
################################
#  
locations <- rbind(sites, nearby_Stations)


pal <- colorFactor(c("navy","navy","navy", "gray50"), domain = c("low", "mid", "high", "NOAA options"), ordered = T)

Map <- leaflet(locations) %>%
  setView(lng = sum(sites$longitude)/3, lat = sum(sites$latitude)/3, zoom = 10) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addCircleMarkers(
    color = ~pal(id),
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~htmltools::htmlEscape(station)) %>%
  addLegend("bottomright",
            colors = c("navy", "gray50"),
            labels = c("Populations","NOAA stations"),
            values = ~id, opacity = 1, title = "Locations") %>%
  addScaleBar()

saveRDS(Map, "Visual/HEQU_Locations.rds")
mapshot(Map, file = "Visual/HEQU_Locations.png", 
        remove_controls = c("zoomControl", "layersControl", "homeButton"))



#############################
##   Explore weather info  ##
#############################

Monthly <- read.csv("Data/Climate data/HEQU_midstation_original.csv" ) %>%
  group_by(year(date), month(date)) %>%
  summarise(sum_prcp = sum(prcp),
            mean_tavg = mean(tavg, na.rm = T),
            mean_tmax = mean(tmax, na.rm = T),
            mean_tmin = mean(tmin, na.rm = T),
            tmin = min(tmin, na.rm = T),
            tmax = max(tmax, na.rm = T))
names(Monthly)[c(1,2)] <- c("Year", "Month")  

# plot precipitation (absolute values) across time
PrcpGrid <- ggplot(Monthly, aes(x= Month, y= sum_prcp))+
  geom_line(colour = "blue")+
  facet_wrap(vars(Year)) +
  scale_x_continuous(breaks = c(1:12))+
  ylab("Monthly Precipitation (mm)")

# plot temperature (absolute values) across time
TempGrid <- ggplot(Monthly, aes(x= Month, y= mean_tavg))+
  geom_line()+
  geom_ribbon(aes(ymin= mean_tmin, ymax= mean_tmax), linetype = 2, alpha = 0.2, fill = "blue")+
  geom_ribbon(aes(ymin= tmin, ymax= tmax), linetype = 2, alpha = 0, colour = "red")+
  facet_wrap(vars(Year))+
  scale_x_continuous(breaks = c(1:12))+
  ylab("Temperature (degrees)")

ggsave("Visual/HEQU_grid_Precipitation.png", PrcpGrid)
ggsave("Visual/HEQU_grid_Temperature.png", TempGrid)

