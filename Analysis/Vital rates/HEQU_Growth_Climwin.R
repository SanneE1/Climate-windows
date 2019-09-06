setwd("c:/owncloud/Documents/PhD/Biomes/Biome/")

library(climwin)
library(dplyr)
library(lme4)


### Prepare data -------------------------------------------------------------
Clim <- read.csv("Data/Climate data/HEQU_NOAA_supplemented.csv") 
Clim$date <- as.Date(Clim$date)                                    ### get a date that's accepted by climwin
Clim$date <- format(Clim$date, format = "%d/%m/%Y")           

Biol <- read.csv("Data/Biol data/HEQU_demography data_JEcol_Dryad.csv") %>%
  mutate(sizeT = log(as.numeric(sizeT)),          
         sizeT1 = log(as.numeric(sizeT1))         
  )

Biol$date <- as.Date(paste(Biol$year, "/07/01", sep = "") )        ### get a date that's accepted by climwin
Biol$date <- format(Biol$date, format = "%d/%m/%Y")
Biol <- Biol[which(Biol$seedling != 1),]                           ### Select Adults
Biol <- Biol[which(!is.na(Biol$survival)),]                       
Biol <- Biol[which(!is.na(Biol$sizeT)),]                           
Biol <- Biol[which(!is.na(Biol$sizeT1)),]

### Sliding window -----------------------------------------------------------------

g_win <- slidingwin(baseline = lmer(sizeT1 ~ sizeT + population + (1|year), data = Biol, REML = F),
                    xvar = list(Rain = Clim$prcp, Temp = Clim$tobs),
                    type = "absolute",
                    range = c(365,0),
                    stat = c("mean", "slope"),
                    func = c("lin", "quad"),
                    refday = c(1,7),                             
                    cinterval = "day",
                    cdate = Clim$date, bdate = Biol$date,
                    spatial = list(as.factor(Biol$population), as.factor(Clim$population))
)


