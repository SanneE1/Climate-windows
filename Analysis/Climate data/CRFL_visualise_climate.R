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


### Load weather stations nearby -------------------------------------------------------------------------

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
Monthly <- read.csv("Data/Climate data/CRFL_NOAA.csv" ) %>%
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


ggsave("Visual/CRFL_grid_Precipitation.png", PrcpGrid)
ggsave("Visual/CRFL_grid_Temperature.png", TempGrid)



