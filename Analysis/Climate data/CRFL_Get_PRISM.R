## Download PRISM data for CRFL populations
setwd("Data/Climate data/PRISM_check/")


library(tidyverse)
library(raster)
library(RCurl)
library(prism)


# Population coordinates
site_coord <- data.frame(id = "Redfleet" , 
                         Longitude = -109.43204,
                         Latitude = 40.59548)


station_coord <- data.frame(id = "NOAA station",
                            Longitude = -109.5400,
                            Latitude = 40.72000  )
# PRISM set up -------------------------------------------------------------------------------

# what do we need from PRISM?
prism_df <- expand.grid( variable = c('ppt','tmean', 'tmin', 'tmax'),
                         year     = c(1986:2017),
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
  tryCatch(extract_year_data(x, sites = site_coord), 
           error = function(e) NULL))
Sys.time() - start


climate_all   <- climate_all_l %>% 
  bind_rows %>% 
  gather( month, value, 01:12 ) %>% 
  pivot_wider(-c(lat, lon), names_from = variable, values_from = value) %>% 
  group_by(month, population) %>%
  mutate(tmean = scale(tmean),
         tmin = scale(tmin),
         tmax = scale(tmax),
         ppt = scale(ppt)) %>%
  rename(Month = month,
         Year = year) 

write.csv(climate_all, file = "PRISM_CRFL_climate.csv")


### Get PRISM data for NOAA Climate station location

# climate_station <- lapply(1:64, function(x) 
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
# write.csv(climatePRISM_at_NOAA, file = "PRISM_climate_at_NOAA_station_CRFL.csv")


setwd("../../../")

#----------------------------------------------
# Correlate PRISM data with NOAA:


NOAA <- read.csv("Data/Climate data/CRFL_SEVLTER_month_imputed.csv")

PRISM <- read.csv("Data/Climate data/PRISM_check/PRISM_CRFL_climate.csv") 



Clim1 <- left_join(PRISM[,-1], NOAA[,-1])

cor(Clim1$tmean_scaled, Clim1$mean_tavg, use = "complete.obs")

cor(Clim1$ppt_scaled, Clim1$sum_prcp, use = "complete.obs")



### Correlate PRISM at NOAA station location with recorded NOAA data at that station

prism <- read.csv("Data/Climate data/PRISM_check/PRISM_climate_at_NOAA_station_CRFL.csv")

Clim2 <- left_join(NOAA, prism[,-1])

cor(Clim2$mean_tavg, Clim2$tmean_scaled, use = "complete.obs")
cor(Clim2$sum_prcp, Clim2$ppt_scaled, use = "complete.obs")
