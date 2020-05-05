library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(lubridate)
library(mapview)

### FRSP population coordinates ------------------------------------------------------------------


sites <- data.frame(id = "Cumberland Pass" , 
                    name = "Cumberland Pass", 
                    latitude = 38.693867, 
                    longitude = -106.475817, 
                    distance = NA,
                    elevation = 3772)


### Weather stations nearby -------------------------------------------------------------------------

nearby_Stations <- read.csv("Data/Climate data/FRSP_nearb_stations.csv")
nearby_Stations$X <- NULL
# nearby_Stations$name <- nearby_Stations$id


################################
## Plot populations and sites ##
################################

locations <- rbind(sites, nearby_Stations) %>%
  mutate(id = as.character(id))
locations$id[which(locations$name == "TAYLOR PARK")] <- "Climate Station"
locations$id[which(!(locations$name == "Cumberland Pass" | locations$name == "TAYLOR PARK"))] <- "NOAA options"

pal <- colorFactor(c("navy", "red", "gray50"), domain = c("Cumberland Pass", "Climate Station", "NOAA options"), ordered = T)

Map <- leaflet(locations) %>%
  setView(lng = sites$longitude[1], lat = sites$latitude[1], zoom = 9) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  addCircleMarkers(
    color = ~pal(id),
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~htmltools::htmlEscape(paste(name, "<br/>", "Distance: ",round(distance, digits = 2), "km", "<br/>", "Elevation: ",  elevation, "m"))) %>%
  addLegend("bottomright",
            colors = c("navy", "red", "gray50"),
            labels = c("Cumberland Pass", "Climate Station", "NOAA options"),
            values = ~id, opacity = 1, title = "Locations") %>%
  addScaleBar()

saveRDS(Map, "Visual/FRSP_Locations.rds")
mapshot(Map, file = "Visual/FRSP_Locations_options.png",
        remove_controls = c("zoomControl", "layersControl", "homeButton"))


#############################
##   Explore weather info  ##
#############################
Monthly <- read.csv("Data/Climate data/FRSP_NOAA_month.csv" ) 

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


ggsave("Visual/FRSP_grid_Precipitation.png", PrcpGrid)
ggsave("Visual/FRSP_grid_Temperature.png", TempGrid)



