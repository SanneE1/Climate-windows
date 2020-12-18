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
  download.file( file_path, destfile = file_dest[ii], mode = "wb", quiet = T)
  
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

# ### Retrieve climate at population
# start <- Sys.time()
# climate_all_l <- lapply(1:128, function(x) 
#   tryCatch(extract_year_data(x, sites = site_coord), 
#            error = function(e) NULL))
# Sys.time() - start
# 
# 
# climate_all   <- climate_all_l %>% 
#   bind_rows %>% 
#   gather( month, value, 01:12 ) %>% 
#   pivot_wider(-c(lat, lon), names_from = variable, values_from = value) %>% 
#   group_by(month, population) %>%
#   mutate(tmean = scale(tmean),
#          tmin = scale(tmin),
#          tmax = scale(tmax),
#          ppt = scale(ppt)) %>%
#   rename(Month = month,
#          Year = year) 
# 
# write.csv(climate_all, file = "PRISM_CRFL_climate.csv")
# setwd("../../../")

### get SPEI values ----------------------------------------------------------------
climate_all <- read.csv("Data/Climate data/PRISM_check/PRISM_CRFL_climate.csv")

# Compute potential evapotranspiration (PET) and climatic water balance (BAL)
climate_all$PET <- thornthwaite(climate_all$tmean, 40.59548) 
climate_all$BAL <- climate_all$ppt - climate_all$PET

# transform in 
Timescale <- ts(climate_all[,-c(1:4)],
                end = c(max(climate_all$Year), 12),
                frequency = 12)

# calculate SPEI with scale of 12 months
SP <- spei(Timescale[,"BAL"], 12)

spei_df <- matrix(SP$fitted[1:(12*length(unique(climate_all$Year)))],
                  nrow = length(unique(climate_all$Year)), ncol = 12,
                  byrow = T) %>%
  as.data.frame %>%
  mutate(Year = c(min(climate_all$Year):max(climate_all$Year)))  %>%
  setNames(c(1:12, "Year")) %>%
  pivot_longer(-Year, names_to = "Month", values_to = "SPEI") %>%
  mutate(Month = as.numeric(Month))

climate_all <- left_join(climate_all, spei_df) %>% select(-c(PET, BAL))
write.csv(climate_all, "Data/Climate data/PRISM_check/PRISM_CRFL_climate.csv")


# ### Get PRISM data for NOAA Climate station location
# 
# climate_station <- lapply(1:128, function(x)
#   tryCatch(extract_year_data(x, sites = station_coord),
#            error = function(e) NULL))
# 
# climatePRISM_at_NOAA <- climate_station %>%
#   bind_rows %>%
#   gather( month, value, 01:12 ) %>%
#   pivot_wider(-c(lat, lon), names_from = variable, values_from = value) %>%
#   group_by(month, population) %>%
#   mutate(tmean = scale(tmean),
#          tmin = scale(tmin),
#          tmax = scale(tmax),
#          ppt = scale(ppt)) %>%
#   rename(Month = month,
#          Year = year)
# 
# write.csv(climatePRISM_at_NOAA, file = "PRISM_climate_at_NOAA_station_CRFL.csv")
# setwd("../../../")


#----------------------------------------------
### Correlate PRISM at NOAA station location with recorded NOAA data at that station
library(lubridate)


NOAA <- read.csv("Data/Climate data/CRFL_NOAA_month.csv")

PRISM <- read.csv("Data/Climate data/PRISM_check/PRISM_CRFL_climate.csv") 
PRISM_at_station <- read.csv("Data/Climate data/PRISM_check/PRISM_climate_at_NOAA_station_CRFL.csv")

Clim <- left_join(PRISM[,-1], NOAA[,-1])
Clim_at_station <- left_join(PRISM_at_station[,-1], NOAA[,-1])


correlation_df <- data.frame(Tavg = c(cor(Clim$mean_tavg, Clim$tmean, use = "complete.obs"),
                                      cor(Clim_at_station$mean_tavg, Clim_at_station$tmean, use = "complete.obs")),
                             Tmin = c(cor(Clim$mean_tmin, Clim$tmin, use = "complete.obs"),
                                      cor(Clim_at_station$mean_tmin, Clim_at_station$tmin, use = "complete.obs")),
                             Tmax = c(cor(Clim$mean_tmax, Clim$tmax, use = "complete.obs"),
                                      cor(Clim_at_station$mean_tmax, Clim_at_station$tmax, use = "complete.obs")),
                             Prcp = c(cor(Clim$sum_prcp, Clim$ppt, use = "complete.obs"),
                                      cor(Clim_at_station$sum_prcp, Clim_at_station$ppt, use = "complete.obs"))
)

row.names(correlation_df) <- c("population", "NOAA_station")


write.csv(correlation_df, "Results/Climwin/4th draft PRISM/Climate_correlation_df_CRFL.csv")
