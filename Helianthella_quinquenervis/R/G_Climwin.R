
library(climwin)
library(dplyr)
library(lme4)

Clim <- read.csv("NOAA_HEQU.csv") 
Clim$date <- as.Date(Clim$date)                                    ### get a date that's accepted by climwin
Clim$date <- format(Clim$date, format = "%d/%m/%Y")           

Biol <- read.csv("Data/HEQU_demography data_JEcol_Dryad.csv")
Biol$date <- as.Date(paste(Biol$year, "/07/01", sep = "") )        ### get a date that's accepted by climwin
Biol$date <- format(Biol$date, format = "%d/%m/%Y")
Biol$sizeT <- as.integer(Biol$sizeT)
Biol$sizeT1 <- as.integer(Biol$sizeT1)
Biol <- Biol[which(Biol$seedling != 1),]                           ### Select Adults
Biol <- Biol[which(!is.na(Biol$sizeT)),]                           ### remove those without size measurements for sizeT or sizeT1
Biol <- Biol[which(!is.na(Biol$sizeT1)),]





g_win <- slidingwin(baseline = lmer(log(sizeT1) ~ log(sizeT) + (1|population) + (1|year), data = Biol),
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


