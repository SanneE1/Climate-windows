library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(lubridate)


### ARTR population coordinates ------------------------------------------------------------------


sites <- data.frame(id = "Sheep Station" , 
                    name = "Sheep Station",
                    latitude = 44.244101, 
                    longitude = -112.200256, 
                    distance = NA)


### Weather stations nearby -------------------------------------------------------------------------
nearby_Stations <- read.csv("Data/Climate data/ARTR_nearb_stations.csv")
nearby_Stations$X <- NULL
nearby_Stations$id <- as.character(nearby_Stations$id)
nearby_Stations$id[which(!(nearby_Stations$id == "USW00024140" | nearby_Stations$id == "USC00102707"))] <- "NOAA options"

################################
## Plot populations and sites ##
################################
#  
locations <- rbind(sites, nearby_Stations) %>%
  mutate(name = as.character(name))

locations <- locations[order(locations$name),]


pal <- colorFactor(c("#be29ec", "#F8766D", "#00BA38", "gray50"), domain = c("Sheep Station","USC00102707","USW00024140", "NOAA options"), ordered = T)

Map <- leaflet(locations) %>%
  setView(lng = sites$longitude, lat = sites$latitude, zoom = 10) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addCircleMarkers(
    color = ~pal(id),
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~htmltools::htmlEscape(name)) %>%
  addLegend("bottomright",
            colors = c("#be29ec", "#F8766D", "#00BA38", "gray50"),
            labels = c("Populations", "Nearest Station","2nd Station", "Other station options"),
            values = ~id, opacity = 1, title = "Locations") %>%
  addScaleBar()

saveRDS(Map, "Visual/ARTR_Locations.rds")


#############################
##   Explore weather info  ##
#############################

Monthly <- read.csv("Data/Climate data/ARTR_NOAA_month_imputed.csv" ) 


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

ggsave("Visual/ARTR_grid_Precipitation.png", PrcpGrid)
ggsave("Visual/ARTR_grid_Temperature.png", TempGrid)

