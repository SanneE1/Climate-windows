## Download PRISM data for HEQU populations


library(tidyverse)
library(raster)
library(RCurl)
library(prism)
library(pbapply)

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
  
  return(values_df)
  
}

start <- Sys.time()
climate_all_l <- pblapply(1:128, function(x) 
  tryCatch(extract_year_data(x), 
           error = function(e) NULL))
Sys.time() - start


# ## Estimate climate at populations
# setwd("Data/Climate data/PRISM_check/")
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
#          Year = year) %>%
#   as.data.frame() %>%
#   dplyr::select(population, Year, Month, ppt, tmin, tmean, tmax)
# 
# write.csv(climate_all, file = "PRISM_HEQU_climate.csv")
# setwd("../../../")

### get SPEI values ----------------------------------------------------------------
# climate_all <- read.csv("Data/Climate data/PRISM_check/PRISM_HEQU_climate.csv")
# 
# ## Low population
# low <- climate_all %>% filter(population == "low")
# 
# # Compute potential evapotranspiration (PET) and climatic water balance (BAL)
# low$PET <- thornthwaite(low$tmean, site_coord$Latitude[1]) 
# low$BAL <- low$ppt - low$PET
# 
# # transform in 
# Timescale <- ts(low[,-c(1:4)],
#                 end = c(max(low$Year),12),
#                 frequency = 12)
# 
# # calculate SPEI with scale of 12 months
# SP <- spei(Timescale[,"BAL"], 12)
# 
# spei_df <- matrix(SP$fitted[1:(12*length(unique(low$Year)))],
#                   nrow = length(unique(low$Year)), ncol = 12,
#                   byrow = T) %>%
#   as.data.frame %>%
#   mutate(Year = c(min(low$Year):max(low$Year)))  %>%
#   setNames(c(1:12, "Year")) %>%
#   pivot_longer(-Year, names_to = "Month", values_to = "SPEI") %>%
#   mutate(Month = as.numeric(Month))
# 
# low <- left_join(low, spei_df) %>% select(-c(PET, BAL))
# 
# ## mid population
# mid <- climate_all %>% filter(population == "mid")
# 
# # Compute potential evapotranspiration (PET) and climatic water balance (BAL)
# mid$PET <- thornthwaite(mid$tmean, site_coord$Latitude[1]) 
# mid$BAL <- mid$ppt - mid$PET
# 
# # transform in 
# Timescale <- ts(mid[,-c(1:4)],
#                 end = c(max(mid$Year),12),
#                 frequency = 12)
# 
# # calculate SPEI with scale of 12 months
# SP <- spei(Timescale[,"BAL"], 12)
# 
# spei_df <- matrix(SP$fitted[1:(12*length(unique(mid$Year)))],
#                   nrow = length(unique(mid$Year)), ncol = 12,
#                   byrow = T) %>%
#   as.data.frame %>%
#   mutate(Year = c(min(mid$Year):max(mid$Year)))  %>%
#   setNames(c(1:12, "Year")) %>%
#   pivot_longer(-Year, names_to = "Month", values_to = "SPEI") %>%
#   mutate(Month = as.numeric(Month))
# 
# mid <- left_join(mid, spei_df) %>% select(-c(PET, BAL))
# 
# ## high population
# high <- climate_all %>% filter(population == "high")
# 
# # Compute potential evapotranspiration (PET) and climatic water balance (BAL)
# high$PET <- thornthwaite(high$tmean, site_coord$Latitude[1]) 
# high$BAL <- high$ppt - high$PET
# 
# # transform in 
# Timescale <- ts(high[,-c(1:4)],
#                 end = c(max(high$Year),12),
#                 frequency = 12)
# 
# # calculate SPEI with scale of 12 months
# SP <- spei(Timescale[,"BAL"], 12)
# 
# spei_df <- matrix(SP$fitted[1:(12*length(unique(high$Year)))],
#                   nrow = length(unique(high$Year)), ncol = 12,
#                   byrow = T) %>%
#   as.data.frame %>%
#   mutate(Year = c(min(high$Year):max(high$Year)))  %>%
#   setNames(c(1:12, "Year")) %>%
#   pivot_longer(-Year, names_to = "Month", values_to = "SPEI") %>%
#   mutate(Month = as.numeric(Month))
# 
# high <- left_join(high, spei_df) %>% select(-c(PET, BAL))
# 
# 
# climate_all <- rbind(low,mid, high)
# write.csv(climate_all, "Data/Climate data/PRISM_check/PRISM_HEQU_climate.csv")

# 
# ## Get PRISM data for NOAA Climate station location
# setwd("Data/Climate data/PRISM_check/")
# climate_station <- pblapply(1:128, function(x)
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
# write.csv(climatePRISM_at_NOAA, file = "PRISM_climate_at_NOAA_station_HEQU.csv")
# setwd("../../../")

#----------------------------------------------
# Correlate PRISM data with NOAA:
library(lubridate)


NOAA <- read.csv("Data/Climate data/HEQU_NOAA_month_imputed.csv") %>%
  select(-X)

PRISM <- read.csv("Data/Climate data/PRISM_check/PRISM_HEQU_climate.csv") %>%
  select(-X)
  


Clim <- left_join(PRISM[,-1], NOAA[,-1])

low <- Clim %>% subset(population == "low")
mid <- Clim %>% subset(population == "mid")
high <- Clim %>% subset(population == "high")


### Correlate PRISM at NOAA station location with recorded NOAA data at that station


prism <- read.csv("Data/Climate data/PRISM_check/PRISM_climate_at_NOAA_station_HEQU.csv")%>%
  select(-X)

Clim <- left_join(NOAA, prism)


correlation_df <- data.frame(Tavg = c(cor(low$tmean, low$mean_tavg, use = "complete.obs"),
                                      cor(mid$tmean, mid$mean_tavg, use = "complete.obs"),
                                      cor(high$tmean, high$mean_tavg, use = "complete.obs"),
                                      cor(Clim$mean_tavg, Clim$tmean, use = "complete.obs")),
                             Tmin = c(cor(low$tmin, low$mean_tmin, use = "complete.obs"),
                                      cor(mid$tmin, mid$mean_tmin, use = "complete.obs"),
                                      cor(high$tmin, high$mean_tmin, use = "complete.obs"),
                                      cor(Clim$mean_tmin, Clim$tmin, use = "complete.obs")),
                             Tmax = c(cor(low$tmax, low$mean_tmax, use = "complete.obs"),
                                      cor(mid$tmax, mid$mean_tmax, use = "complete.obs"),
                                      cor(high$tmax, high$mean_tmax, use = "complete.obs"),
                                      cor(Clim$mean_tmax, Clim$tmax, use = "complete.obs")),
                             Prcp = c(cor(low$ppt, low$sum_prcp, use = "complete.obs"),
                                      cor(mid$ppt, mid$sum_prcp, use = "complete.obs"),
                                      cor(high$ppt, high$sum_prcp, use = "complete.obs"),
                                      cor(Clim$sum_prcp, Clim$ppt, use = "complete.obs")
                                      )
)

row.names(correlation_df) <- c("low", "mid", "high", "NOAA_station")


write.csv(correlation_df, "Results/Climwin/4th draft PRISM/Climate_correlation_df_HEQU.csv")


