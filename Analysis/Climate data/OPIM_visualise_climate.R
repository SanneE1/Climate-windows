library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(lubridate)
library(geosphere)


### OPIM population coordinates ------------------------------------------------------------------


sites <- data.frame(id = c("Sevilleta", "SEVLTER50", "SEVLTER40", "SEVLTER49") , 
                    name = c("Sevilleta", "SEVLTER50", "SEVLTER40", "SEVLTER49"), 
                    latitude = c(34.334806, 34.334527, 34.358855,34.332659), 
                    longitude = c(-106.631444, -106.632057, -106.689043,-106.727045), 
                    distance = NA)

sites$distance[2] <- distVincentyEllipsoid(sites[1,c("longitude", "latitude")], sites[2, c("longitude", "latitude")])/1000
sites$distance[3] <- distVincentyEllipsoid(sites[1,c("longitude", "latitude")], sites[3, c("longitude", "latitude")])/1000
sites$distance[4] <- distVincentyEllipsoid(sites[1,c("longitude", "latitude")], sites[4, c("longitude", "latitude")])/1000

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

locations$id[which(!(locations$name == "SEVLTER50" | locations$name == "Sevilleta"))] <- "options"

pal <- colorFactor(c("red", "navy", "gray50"), domain = c("SEVLTER50", "Sevilleta", "options"), ordered = T)

Map <- leaflet(locations) %>%
  setView(lng = -106.631444, lat = 34.334806, zoom = 11) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  addCircleMarkers(
    color = ~pal(id),
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~htmltools::htmlEscape(name)) %>%
  addLegend("bottomright",
            colors = c("navy", "red", "gray50"),
            labels = c("Sevilleta", "SEVLTER50", "SEVLTER or NOAA options"),
            values = ~id, opacity = 1, title = "Locations") %>%
  addScaleBar()

saveRDS(Map, "Visual/OPIM_Locations.rds")


#############################
##   Explore weather info  ##
#############################
NOAA <- read.csv("Data/Climate data/OPIM_NOAA_month.csv" ) %>%
  rename(mean_tavg = mean_tobs,
         sd_tavg = sd_tobs,
         mean_tavg_scaled = mean_tobs_scaled,
         sd_tavg_scaled = sd_tobs_scaled) %>%
  mutate(id = as.character(id))
SEV <- read.csv("Data/Climate data/OPIM_SEVLTER_month_imputed.csv")

Monthly <- rbind(NOAA, SEV)

PrcpGrid2 <- ggplot(Monthly, aes(x= Month, y= mean_prcp, color = id))+
  geom_line(colour = "blue")+
  geom_ribbon(aes(ymin= ifelse(mean_prcp - sd_prcp < 0, 0,mean_prcp - sd_prcp), ymax= (mean_prcp + sd_prcp)), linetype = 2, alpha = 0)+
  facet_wrap(vars(Year)) +
  scale_x_continuous(breaks = c(1:12))+
  ylab("Mean daily Precipitation (mm)")

TempGrid2 <- ggplot(Monthly, aes(x= Month, y= mean_tavg, color = id))+
  geom_line()+
  geom_ribbon(aes(ymin= mean_tmin, ymax= mean_tmax), linetype = 2, alpha = 0)+
  geom_ribbon(aes(ymin= min_tmin, ymax= max_tmax), linetype = 2, alpha = 0, colour = "red")+
  facet_wrap(vars(Year))+
  scale_x_continuous(breaks = c(1:12))+
  ylab("Temperature (degrees)")


PrcpGrid <- ggplot(SEV, aes(x= Month, y= mean_prcp))+
  geom_line(colour = "blue")+
  geom_ribbon(aes(ymin= ifelse(mean_prcp - sd_prcp < 0, 0,mean_prcp - sd_prcp), ymax= (mean_prcp + sd_prcp)), linetype = 2, alpha = 0.1, fill="blue")+
  facet_wrap(vars(Year)) +
  scale_x_continuous(breaks = c(1:12))+
  ylab("Mean daily Precipitation (mm)")

TempGrid <- ggplot(SEV, aes(x= Month, y= mean_tavg))+
  geom_line()+
  geom_ribbon(aes(ymin= mean_tmin, ymax= mean_tmax), linetype = 2, alpha = 0.2, fill = "blue")+
  geom_ribbon(aes(ymin= min_tmin, ymax= max_tmax), linetype = 2, alpha = 0, colour = "red")+
  facet_wrap(vars(Year))+
  scale_x_continuous(breaks = c(1:12))+
  ylab("Temperature (degrees)")




ggsave("Visual/OPIM_grid_Precipitation.png", PrcpGrid)
ggsave("Visual/OPIM_grid_Temperature.png", TempGrid)

