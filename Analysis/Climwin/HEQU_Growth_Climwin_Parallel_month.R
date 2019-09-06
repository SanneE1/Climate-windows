setwd("c:/owncloud/Documents/PhD/Biomes/Biome/")

library(climwin)
library(dplyr)
library(lme4)
library(parallel)

### Prepare data -------------------------------------------------------------
Clim <- read.csv("Data/Climate data/HEQU_NOAA_monthly.csv") 
Clim$Date <- paste("15/",Clim$Month, "/", Clim$Year, sep = "")             ### get a date that's accepted by climwin
Clim$X <- NULL

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


### Climate signal combies ----------------------------------------------------------------

xvar <- c("Clim$Temp", "Clim$Rain")
type <- c("absolute")
stat <- c("mean")
func <- c("lin", "quad")
upper <- NA            ## specify upper limit when stat = sum, else set to NA
lower <- NA            ## specify lower limit when stat = sum, else set to NA

options <- expand.grid(xvar = xvar, type = type, stat = stat, func = func, upper = upper, lower = lower, stringsAsFactors = F)


### Creat ClimWin functions that can run parallel -----------------------------------------

source("Analysis/Climwin/Parallel_Climwin_functions.R")

### set up parallels ----------------------------------------------------------------------

# detect cores
cores     <- (detectCores() - 2)
# set up as many clusters as detected by 'detectCores()'
cluster   <- parallel::makePSOCKcluster(cores)

# attach packages that will be needed on each cluster
clusterEvalQ(cluster, list(library(climwin),  
                           library(dplyr), library(lme4)) )

# attach objects that will be needed on each cluster
clusterExport(cluster, c('Clim', 'Biol', 'options', 'xvar') )

start <- Sys.time()
GrowthPar <- parLapply(cluster, 1:length(options[,1]), ParSliding)
Sys.time() - start

stopCluster(cluster)


### Merge into one output that can be used with climwin -----------------------

GrowthMonthly <- Cleanup(GrowthPar)

save(m, file = "Results/Climwin/HEQU_Growth_Monthly")




