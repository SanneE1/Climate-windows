## R code to run the sliding window on the Helianthella quinquenervis dataset 

setwd("c:/owncloud/Documents/PhD/Biomes/Biome/")

library(climwin)
library(dplyr)
library(lme4)

### Prepare data -------------------------------------------------------------
Clim <- read.csv("Data/Climate data/HEQU_NOAA_month.csv") 

# Clim$date <- as.Date(Clim$date)                                    ### DAILY data
# Clim$date <- format(Clim$date, format = "%d/%m/%Y")                ### DAILY data

Clim$date <- paste("15/",sprintf("%02d", Clim$Month), "/", Clim$Year, sep = "")  ### MONTHLY data         


Biol <- read.csv("Data/Biol data/HEQU_demography_data.csv") %>%
  mutate(sizeT = as.numeric(as.character(sizeT)),
         sizeT1 = as.numeric(as.character(sizeT1)),
         year = as.factor(year))

Biol$date <- paste("01/07/", Biol$year, sep = "")        ### get a date that's accepted by climwin
Biol <- Biol[which(Biol$seedling != 1),]                           ### Select Adults
Biol <- Biol[which(!is.na(Biol$survival)),]                       
Biol <- Biol[which(!is.na(Biol$sizeT)),]                           
Biol <- Biol[which(!is.na(Biol$sizeT1)),]

### Baseline window -----------------------------------------------------------------

a <- glmer.nb(sizeT1 ~ sizeT + population + (1|year), data = Biol)

saveRDS(a, "/data/gsclim/HEQU_growth_baseline.rds")
