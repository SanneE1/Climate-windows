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
nearby_Stations$id[which(nearby_Stations$station == "USS0006L11S" )] <- "USS0006L11S"


################################
## Plot populations and sites ##
################################
#  
locations <- rbind(sites, nearby_Stations)


pal <- colorFactor(c("navy","navy","navy", "red", "gray50"), domain = c("low", "mid", "high","USS0006L11S", "NOAA options"), ordered = T)

Map <- leaflet(locations) %>%
  setView(lng = sum(sites$longitude)/3, lat = sum(sites$latitude)/3, zoom = 10) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addCircleMarkers(
    color = ~pal(id),
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~htmltools::htmlEscape(station)) %>%
  addLegend("bottomright",
            colors = c("navy", "red", "gray50"),
            labels = c("Populations", "Station USS0006L11S" ,"Other station options"),
            values = ~id, opacity = 1, title = "Locations") %>%
  addScaleBar()

saveRDS(Map, "Visual/HEQU_Locations.rds")
mapshot(Map, file = "Visual/HEQU_Locations.png", 
        remove_controls = c("zoomControl", "layersControl", "homeButton"))



#############################
##   Explore weather info  ##
#############################

Monthly <- read.csv("Data/Climate data/HEQU_NOAA_month_imputed.csv" ) 


PrcpGrid <- ggplot(Monthly, aes(x= Month, y= mean_prcp))+
  geom_line(colour = "blue")+
  geom_ribbon(aes(ymin= ifelse(mean_prcp - sd_prcp < 0, 0,mean_prcp - sd_prcp), ymax= (mean_prcp + sd_prcp)), linetype = 2, alpha = 0.1,fill="blue")+
  facet_wrap(vars(Year)) +
  scale_x_continuous(breaks = c(1:12))+
  ylab("Mean daily Precipitation (mm)")

TempGrid <- ggplot(Monthly, aes(x= Month, y= mean_tobs))+
  geom_line()+
  geom_ribbon(aes(ymin= mean_tmin, ymax= mean_tmax), linetype = 2, alpha = 0.2, fill = "blue")+
  geom_ribbon(aes(ymin= min_tmin, ymax= max_tmax), linetype = 2, alpha = 0, colour = "red")+
  facet_wrap(vars(Year))+
  scale_x_continuous(breaks = c(1:12))+
  ylab("Temperature (degrees)")

ggsave("Visual/HEQU_grid_Precipitation.png", PrcpGrid)
ggsave("Visual/HEQU_grid_Temperature.png", TempGrid)

