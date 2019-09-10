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


Biol <- read.csv("Data/Biol data/HEQU_demography data_JEcol_Dryad.csv") %>%
  mutate(sizeT = as.numeric(as.character(sizeT)),
         sizeT1 = as.numeric(as.character(sizeT1)))

Biol$date <- paste("01/07/", Biol$year, sep = "")        ### get a date that's accepted by climwin
Biol <- Biol[which(Biol$seedling != 1),]                           ### Select Adults
Biol <- Biol[which(!is.na(Biol$survival)),]                       
Biol <- Biol[which(!is.na(Biol$sizeT)),]                           
Biol <- Biol[which(!is.na(Biol$sizeT1)),]

### Sliding window -----------------------------------------------------------------

g_win <- slidingwin(baseline = lmer(sizeT1 ~ sizeT + population + (1|year), data = Biol, REML = F),
                    xvar = list(Rain = Clim$mean_prcp, Temp = Clim$mean_tobs),
                    type = "absolute", 
                    range = c(12,0),                               #### change if to 365 if data = dailly or 12 if data = monthly
                    stat = c("mean", "slope"),
                    func = c("lin", "quad"),
                    refday = c(1,7),                             
                    cinterval = "month",                           #### change depending on the climate data interval
                    cdate = Clim$date, bdate = Biol$date,
                    spatial = list(as.factor(Biol$population), as.factor(Clim$population))
)


