## Download PRISM data for HEQU populations
setwd("Data/Climate data/PRISM_HEQU_check/")


library(tidyverse)
library(raster)
library(RCurl)
library(prism)


# Population coordinates
site_coord <- data.frame(id = c("low", "mid", "high"), 
                         Longitude = as.numeric(measurements::conv_unit(c("-107 09.556", "-106 59.3", "-106 58.690"), from = 'deg_dec_min', to = 'dec_deg')),
                         Latitude = as.numeric(measurements::conv_unit(c("38 51.774", "38 57.5", "38 58.612"), from = 'deg_dec_min', to = 'dec_deg'))
           )


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
file_dest  <- gsub("tmean/[0-9]{4}/|ppt/[0-9]{4}/","",file_names) %>% 
  paste0(getwd(),"/", .)


# Extract PRISM DATA ------------------------------------------------------------------------

# extract year and monthly data
extract_year_data <- function(ii){
  
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
  values_clim <- raster::extract(rast_stack, site_coord[,-1], method = 'bilinear')
  values_df   <- values_clim %>% 
    as.data.frame() %>% 
    setNames( months ) %>% 
    mutate( variable = prism_df$variable[ii],
            year     = prism_df$year[ii],
            lat      = site_coord[,'Latitude'],
            lon      = site_coord[,'Longitude'],
            population = site_coord[,"id"])
  
  file.remove( paste0('temp_dir/',list.files('temp_dir/')) )
  file.remove( grep( 'zip$', list.files(), value=T) )
  
  print(ii)
  
  return(values_df)
  
}

start <- Sys.time()
climate_all_l <- lapply(1:128, function(x) tryCatch(extract_year_data(x), error = function(e) NULL))
Sys.time() - start

climate_all   <- climate_all_l %>% 
  bind_rows %>% 
  gather( month, value, 01:12 ) %>% 
  pivot_wider(-c(X, lat, lon), names_from = variable, values_from = value) %>% 
  group_by(month, population) %>%
  mutate(tmean_scaled = scale(tmean),
         ppt_scaled = scale(ppt)) %>%
  rename(Month = month,
         Year = year) 

write.csv(climate_all, file = "PRISM_HEQU_climate.csv")


#----------------------------------------------
# Correlate PRISM data with NOAA:

NOAA <- read.csv("../HEQU_NOAA_month_imputed.csv") %>%
  select(-X)

PRISM <- read.csv("Data/Climate data/PRISM_HEQU_check/PRISM_HEQU_climate.csv") %>%
  select(-X)


Clim <- left_join(PRISM, NOAA)

low <- Clim %>% subset(population == "low")
mid <- Clim %>% subset(population == "mid")
high <- Clim %>% subset(population == "high")

cor(low$tmean_scaled, low$mean_tavg)
cor(mid$tmean_scaled, mid$mean_tavg)
cor(high$tmean_scaled, high$mean_tavg)
