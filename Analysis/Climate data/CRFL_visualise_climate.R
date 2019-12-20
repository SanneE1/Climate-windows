library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(lubridate)
library(mapview)

### CRFL population coordinates ------------------------------------------------------------------


sites <- data.frame(id = "Redfleet" , 
                    name = "Redfleet", 
                    latitude = 40.59548, 
                    longitude = -109.43204, 
                    distance = NA)


### Weather stations nearby -------------------------------------------------------------------------

nearby_Stations <- read.csv("Data/Climate data/CRFL_nearb_stations.csv")
nearby_Stations$X <- NULL
nearby_Stations$name <- nearby_Stations$id


################################
## Plot populations and sites ##
################################

locations <- rbind(sites, nearby_Stations) %>%
  mutate(id = as.character(id),
         name = as.character(id))
locations$id[which(locations$name == "USS0009J01S")] <- "NOAA station"
locations$id[which(!(locations$name == "USS0009J01S" | locations$name == "Redfleet"))] <- "NOAA options"

pal <- colorFactor(c("navy", "red", "gray50"), domain = c("Redfleet", "NOAA station", "NOAA options"), ordered = T)

Map <- leaflet(locations) %>%
  setView(lng = -109.43204, lat = 40.59548, zoom = 10) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  addCircleMarkers(
    color = ~pal(id),
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~htmltools::htmlEscape(name)) %>%
  addLegend("bottomright",
            colors = c("navy", "red", "gray50"),
            labels = c("Redfleet", "NOAA station", "NOAA options"),
            values = ~id, opacity = 1, title = "Locations") %>%
  addScaleBar()

saveRDS(Map, "Visual/CRFL_Locations.rds")
mapshot(Map, file = "Visual/CRFL_Locations.png",
        remove_controls = c("zoomControl", "layersControl", "homeButton"))


#############################
##   Explore weather info  ##
#############################
Monthly <- read.csv("Data/Climate data/CRFL_NOAA_month.csv" ) 

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


ggsave("Visual/CRFL_grid_Precipitation.png", PrcpGrid)
ggsave("Visual/CRFL_grid_Temperature.png", TempGrid)



