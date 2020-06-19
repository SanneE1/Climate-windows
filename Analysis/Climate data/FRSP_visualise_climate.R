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
mapshot(Map, file = "Visual/FRSP_Locations.png",
        remove_controls = c("zoomControl", "layersControl", "homeButton"))


#############################
##   plot weather info     ##
#############################
Monthly <- read.csv("Data/Climate data/FRSP_NOAA_1.csv") %>%
  group_by(year(date), month(date)) %>%
  summarise(sum_prcp = sum(prcp),
            mean_tmax = mean(tmax, na.rm = T),
            mean_tmin = mean(tmin, na.rm = T),
            tmin = min(tmin, na.rm = T),
            tmax = max(tmax, na.rm = T)) 
names(Monthly)[c(1,2)] <- c("Year", "Month")   

PrcpGrid <- ggplot(Monthly, aes(x= Month, y= sum_prcp))+
  geom_line(colour = "blue")+
  facet_wrap(vars(Year)) +
  scale_x_continuous(breaks = c(1:12))+
  ylab("Monthly Precipitation (mm)")

TempGrid <- ggplot(Monthly, aes(x= Month))+
  geom_ribbon(aes(ymin= mean_tmin, ymax= mean_tmax), linetype = 2, alpha = 0.2, fill = "blue")+
  geom_ribbon(aes(ymin= tmin, ymax= tmax), linetype = 2, alpha = 0, colour = "red")+
  facet_wrap(vars(Year))+
  scale_x_continuous(breaks = c(1:12))+
  ylab("Temperature (degrees)")


ggsave("Visual/FRSP_grid_Precipitation.png", PrcpGrid)
ggsave("Visual/FRSP_grid_Temperature.png", TempGrid)



