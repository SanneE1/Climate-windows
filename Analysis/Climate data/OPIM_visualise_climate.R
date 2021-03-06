library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(lubridate)
library(geosphere)
library(mapview)

### OPIM population coordinates ------------------------------------------------------------------


sites <- data.frame(id = c("Sevilleta","Sevilleta","Sevilleta","Sevilleta","Sevilleta","Sevilleta","Sevilleta","Sevilleta", "SEVLTER50", "SEVLTER40", "SEVLTER49") , 
                    name = c("Plot 1","Plot 2","Plot 3","Plot 4","Plot 5","Plot 6","Plot 7","Plot 8", "SEVLTER50", "SEVLTER40", "SEVLTER49"), 
                    latitude = c(34.329,34.329,34.329,34.329,34.328,34.33,34.327,34.325, 34.334527, 34.358855,34.332659), 
                    longitude = c(-106.621,-106.62,-106.621,-106.622,-106.621,-106.619,-106.622,-106.622, -106.632057, -106.689043,-106.727045), 
                    distance = NA)

sites$distance[2] <- distVincentyEllipsoid(sites[1,c("longitude", "latitude")], sites[9, c("longitude", "latitude")])/1000
sites$distance[3] <- distVincentyEllipsoid(sites[1,c("longitude", "latitude")], sites[10, c("longitude", "latitude")])/1000
sites$distance[4] <- distVincentyEllipsoid(sites[1,c("longitude", "latitude")], sites[11, c("longitude", "latitude")])/1000

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
  setView(lng = -106.631444, lat = 34.334806, zoom = 13) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  addCircleMarkers(
    color = ~pal(id),
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~htmltools::htmlEscape(name)) %>%
  addLegend("bottomright",
            colors = c("navy", "red", "gray50"),
            labels = c("Plots", "SEVLTER50", "SEVLTER or NOAA options"),
            values = ~id, opacity = 1, title = "Locations") %>%
  addScaleBar()

saveRDS(Map, "Visual/OPIM_Locations.rds")

mapshot(Map, file = "Visual/OPIM_Locations.png",
        remove_controls = c("zoomControl", "layersControl", "homeButton"))



#############################
##   Explore weather info  ##
#############################
SEV <- read.csv("Data/Climate data/OPIM_SEVLTER50.csv") %>%
  mutate(date = as.Date(Date, "%Y-%m-%d"),
         Avg.Temp = replace(Avg.Temp, Avg.Temp == -999, NA),
         Max.Temp = replace(Max.Temp, Max.Temp == -999, NA),
         Min.Temp = replace(Min.Temp, Min.Temp == -999, NA),
         Precip = replace(Precip, Precip == -999, NA))  %>%
  group_by(year(date), month(date)) %>%
  summarise(sum_prcp = sum(Precip),
            mean_tavg = mean(Avg.Temp, na.rm = T),
            mean_tmax = mean(Max.Temp, na.rm = T),
            mean_tmin = mean(Min.Temp, na.rm = T),
            tmin = min(Max.Temp, na.rm = T),
            tmax = max(Min.Temp, na.rm = T))
names(SEV)[c(1,2)] <- c("Year", "Month")  

PrcpGrid <- ggplot(SEV, aes(x= Month, y= sum_prcp))+
  geom_line(colour = "blue") +
  facet_wrap(vars(Year)) +
  scale_x_continuous(breaks = c(1:12))+
  ylab("Mean daily Precipitation (mm)")

TempGrid <- ggplot(SEV, aes(x= Month, y= mean_tavg))+
  geom_line()+
  geom_ribbon(aes(ymin= mean_tmin, ymax= mean_tmax), linetype = 2, alpha = 0.2, fill = "blue")+
  geom_ribbon(aes(ymin= tmin, ymax= tmax), linetype = 2, alpha = 0, colour = "red")+
  facet_wrap(vars(Year))+
  scale_x_continuous(breaks = c(1:12))+
  ylab("Temperature (degrees)")




ggsave("Visual/OPIM_grid_Precipitation.png", PrcpGrid)
ggsave("Visual/OPIM_grid_Temperature.png", TempGrid)

