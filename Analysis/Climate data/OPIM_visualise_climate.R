library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(lubridate)


### OPIM population coordinates ------------------------------------------------------------------


sites <- data.frame(id = "Sevilleta" , 
                    name = "Sevilleta", 
                    latitude = 34.334806, 
                    longitude = -106.631444, 
                    distance = NA)


### Weather stations nearby -------------------------------------------------------------------------

nearby_Stations <- read.csv("Data/Climate data/OPIM_nearby_stations.csv")
nearby_Stations$X <- NULL
nearby_Stations$name <- nearby_Stations$id


################################
## Plot populations and sites ##
################################

locations <- rbind(sites, nearby_Stations) %>%
  mutate(id = as.character(id),
         name = as.character(id))

locations$id[which(locations$name == "USC00290915")] <- "NOAA station"
locations$id[which(locations$name == "USC00295965")] <- "NOAA station"
locations$id[which(locations$name == "USC00298387")] <- "NOAA station"

locations$id[which(!(locations$name == "USC00290915" | locations$name == "Sevilleta"))] <- "NOAA options"

pal <- colorFactor(c("navy", "red", "gray50"), domain = c("Sevilleta", "NOAA station", "NOAA options"), ordered = T)

Map <- leaflet(locations) %>%
  setView(lng = -106.631444, lat = 34.334806, zoom = 9) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  addCircleMarkers(
    color = ~pal(id),
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~htmltools::htmlEscape(name)) %>%
  addLegend("bottomright",
            colors = c("navy", "red", "gray50"),
            labels = c("Sevilleta", "NOAA station", "NOAA options"),
            values = ~id, opacity = 1, title = "Locations") %>%
  addScaleBar()

saveRDS(Map, "Visual/OPIM_Locations.rds")


#############################
##   Explore weather info  ##
#############################
Monthly <- read.csv("Data/Climate data/OPIM_NOAA_month.csv" ) 

PrcpGrid <- ggplot(Monthly, aes(x= Month, y= mean_prcp, color = id))+
  geom_line(colour = "blue")+
  geom_ribbon(aes(ymin= ifelse(mean_prcp - sd_prcp < 0, 0,mean_prcp - sd_prcp), ymax= (mean_prcp + sd_prcp)), linetype = 2, alpha = 0)+
  facet_wrap(vars(Year)) +
  scale_x_continuous(breaks = c(1:12))+
  ylab("Mean daily Precipitation (mm)")

TempGrid <- ggplot(Monthly, aes(x= Month, y= mean_tobs, color = id))+
  geom_line()+
  geom_ribbon(aes(ymin= mean_tmin, ymax= mean_tmax), linetype = 2, alpha = 0)+
  geom_ribbon(aes(ymin= min_tmin, ymax= max_tmax), linetype = 2, alpha = 0, colour = "red")+
  facet_wrap(vars(Year))+
  scale_x_continuous(breaks = c(1:12))+
  ylab("Temperature (degrees)")


ggsave("Visual/OPIM_grid_Precipitation.png", PrcpGrid)
ggsave("Visual/OPIM_grid_Temperature.png", TempGrid)

