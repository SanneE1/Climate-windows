## Download PRISM data for HEQU populations
setwd("Data/Climate data/PRISM_check/")


library(tidyverse)
library(raster)
library(RCurl)
library(prism)


# Population coordinates
site_coord <- data.frame(id = c("low", "mid", "high"), 
                         Longitude = as.numeric(measurements::conv_unit(c("-107 09.556", "-106 59.3", "-106 58.690"), from = 'deg_dec_min', to = 'dec_deg')),
                         Latitude = as.numeric(measurements::conv_unit(c("38 51.774", "38 57.5", "38 58.612"), from = 'deg_dec_min', to = 'dec_deg'))
           )

station_coord <- data.frame(id = "NOAA station",
                           Longitude = -106.9500,
                           Latitude = 38.8900)
# PRISM set up -------------------------------------------------------------------------------

# what do we need from PRISM?
prism_df <- expand.grid( variable = c('ppt','tmean', 'tmax', 'tmin'),
                         year     = c(1985:2016),
                         # month    = c(paste0('0',1:9),paste0(10:12)),
                         stringsAsFactors = F) %>% 
  arrange(variable,year)

# set up reading path
read_dir  <- 'ftp://prism.oregonstate.edu/monthly/'


# produce file name based on index 'ii'
produce_file_name <- function(ii){
  
  if( prism_df$variable[ii] == 'ppt'){
    file_root  <- paste0(prism_df$variable[ii],'/',prism_df$year[ii],
                         '/PRISM_',prism_df$variable[ii],'_stable_4kmM3_',
                         prism_df$year[ii],'_all_bil.zip')
  }
  
  if( prism_df$variable[ii] == 'tmean'){
    file_root  <- paste0(prism_df$variable[ii],'/',prism_df$year[ii],
                         '/PRISM_',prism_df$variable[ii],'_stable_4kmM3_',
                         prism_df$year[ii],'_all_bil.zip')
  }
  
  if( prism_df$variable[ii] == 'tmin'){
    file_root  <- paste0(prism_df$variable[ii],'/',prism_df$year[ii],
                         '/PRISM_',prism_df$variable[ii],'_stable_4kmM3_',
                         prism_df$year[ii],'_all_bil.zip')
  }
  
  if( prism_df$variable[ii] == 'tmax'){
    file_root  <- paste0(prism_df$variable[ii],'/',prism_df$year[ii],
                         '/PRISM_',prism_df$variable[ii],'_stable_4kmM3_',
                         prism_df$year[ii],'_all_bil.zip')
  }
  
  return(file_root)
  
}

# get all file names 
file_names <- lapply(1:nrow(prism_df), produce_file_name) %>% unlist

# get all the file links (from file name)
file_links <- paste0(read_dir,file_names)

# produce file destinations (put it all under C:/)
file_dest  <- gsub("tmean/[0-9]{4}/|tmin/[0-9]{4}/|tmax/[0-9]{4}/|ppt/[0-9]{4}/","",file_names) %>% 
  paste0(getwd(),"/", .)


# Extract PRISM DATA ------------------------------------------------------------------------

# extract year and monthly data
extract_year_data <- function(ii, sites = site_coord){
  
  print( ii )
  
  # extac with archive 
  # devtools::install_github('jimhester/archive')
  file_path <- file_links[ii]
  
  # download
  download.file( file_path, destfile = file_dest[ii], mode = "wb")
  
  # unzip file to temp_dir
  unzip( grep( 'zip$', list.files(), value=T),
         exdir = 'temp_dir' )
  # get climate information ----------------------------------------------------------------
  
  # read raster
  raster_file <- grep( '[0-9]{6}_bil.bil$', list.files('temp_dir'), value=T)
  rast_l      <- lapply( paste0('temp_dir/',raster_file), function(x) raster(x) )
  rast_stack  <- stack( rast_l )
  
  # extract month numbers
  extract_mon <- function(x){
    regmatches(x, 
               gregexpr("[[:digit:]]{6}", 
                        x) ) %>% 
      substr( 5,6 )
  }
  
  months      <- sapply( raster_file, extract_mon) %>% setNames( NULL )
  

  # extract climatic info 
  values_clim <- raster::extract(rast_stack, sites[,-1], method = 'bilinear')
  values_df   <- values_clim %>% 
    as.data.frame() %>% 
    setNames( months ) %>% 
    mutate( variable = prism_df$variable[ii],
            year     = prism_df$year[ii],
            lat      = sites[,'Latitude'],
            lon      = sites[,'Longitude'],
            population = sites[,"id"])
  
  file.remove( paste0('temp_dir/',list.files('temp_dir/')) )
  file.remove( grep( 'zip$', list.files(), value=T) )
  
  print(ii)
  
  return(values_df)
  
}

start <- Sys.time()
climate_all_l <- lapply(1:128, function(x) 
  tryCatch(extract_year_data(x), 
           error = function(e) NULL))
Sys.time() - start


climate_all   <- climate_all_l %>% 
  bind_rows %>% 
  gather( month, value, 01:12 ) %>% 
  pivot_wider(-c(lat, lon), names_from = variable, values_from = value) %>% 
  group_by(month, population) %>%
  mutate(tmean_scaled = scale(tmean),
         ppt_scaled = scale(ppt)) %>%
  rename(Month = month,
         Year = year) 

write.csv(climate_all, file = "PRISM_HEQU_climate.csv")


### Get PRISM data for NOAA Climate station location
# climate_station <- lapply(1:128, function(x) 
#   tryCatch(extract_year_data(x, sites = station_coord), 
#            error = function(e) NULL))
# 
# climatePRISM_at_NOAA <- climate_station %>% 
#   bind_rows %>% 
#   gather( month, value, 01:12 ) %>% 
#   pivot_wider(-c(lat, lon), names_from = variable, values_from = value) %>% 
#   group_by(month, population) %>%
#   mutate(tmean_scaled = scale(tmean),
#          ppt_scaled = scale(ppt)) %>%
#   rename(Month = month,
#          Year = year) 
# 
# write.csv(climatePRISM_at_NOAA, file = "PRISM_climate_at_NOAA_station.csv")
# 


setwd("../../../")

#----------------------------------------------
# Correlate PRISM data with NOAA:


NOAA <- read.csv("Data/Climate data/HEQU_NOAA_month_imputed.csv")

PRISM <- read.csv("Data/Climate data/PRISM_check/PRISM_HEQU_climate.csv") 
  


Clim <- left_join(PRISM[,-1], NOAA[,-1])

low <- Clim %>% subset(population == "low")
mid <- Clim %>% subset(population == "mid")
high <- Clim %>% subset(population == "high")

cor(low$tmean_scaled, low$mean_tavg)
cor(mid$tmean_scaled, mid$mean_tavg)
cor(high$tmean_scaled, high$mean_tavg)

cor(low$ppt_scaled, low$sum_prcp)
cor(mid$ppt_scaled, mid$sum_prcp)
cor(high$ppt_scaled, high$sum_prcp)



### Correlate PRISM at NOAA station location with recorded NOAA data at that station

mid_noaa <- read.csv("Data/Climate data/HEQU_midstation_original.csv") %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  group_by(Month = month(date), Year = year(date)) %>%
  summarise(sum_prcp = sum(prcp),
            mean_tavg = mean(tavg, na.rm = T)) %>%
  group_by(Month) %>%
  mutate(sum_prcp_scaled = scale(sum_prcp),
         mean_tavg_scaled = scale(mean_tavg))

prism <- read.csv("Data/Climate data/PRISM_check/PRISM_climate_at_NOAA_station.csv")

Clim <- left_join(mid_noaa, prism)

cor(Clim$mean_tavg_scaled, Clim$tmean_scaled, use = "complete.obs")
cor(Clim$sum_prcp_scaled, Clim$ppt_scaled)


correlation_df <- data.frame(Tavg = c(cor(low$tmean_scaled, low$mean_tavg),
                                      cor(mid$tmean_scaled, mid$mean_tavg),
                                      cor(high$tmean_scaled, high$mean_tavg),
                                      cor(Clim$mean_tavg_scaled, Clim$tmean_scaled, use = "complete.obs")),
                             Prcp = c(cor(low$ppt_scaled, low$sum_prcp),
                                      cor(mid$ppt_scaled, mid$sum_prcp),
                                      cor(high$ppt_scaled, high$sum_prcp),
                                      cor(Clim$sum_prcp_scaled, Clim$ppt_scaled)
                                      )
)

row.names(correlation_df) <- c("low", "mid", "high", "NOAA_station")


write.csv(correlation_df, "Results/Climwin/4th draft HEQU PRISM/Climate_correlation_df.csv")


