## Run the sliding window analyses

library(climwin)
library(dplyr)
library(lme4)
library(parallel)

source("Analysis/Climwin/Slidingwindow/Sliding_function.R")

start <- Sys.time()

### Climate signal combies - 16 combinations ----------------------------------------------------------------------------------------------------------------------------

xvar <- c("mean_prcp_scaled", "mean_tobs_scaled", "mean_tmax_scaled", "mean_tmin_scaled", "mean_tavg_scaled", "SPEI")
type <- c("absolute")
stat <- c("mean")
func <- c("lin", "quad")
upper <- NA            
lower <- NA            

options <- expand.grid(xvar = xvar, type = type, stat = stat, func = func, upper = upper, lower = lower, stringsAsFactors = F)

## add minimum and maximum observed temperature values

options <- rbind(options, c("min_tmin_scaled", "absolute", "min", "lin", 0, NA), c("min_tmin_scaled", "absolute", "min", "quad", 0, NA),
                 c("max_tmax_scaled", "absolute", "max", "lin", 0, NA), c("max_tmax_scaled", "absolute", "max", "quad", 0, NA)
)

### seperate into list to give to parLapply()
combis <- split(options, seq(nrow(options)))


## Set up parallel processing  ----------------------------------------------------------------------------------------------------------------------------
# detect cores
cores     <- (detectCores() - 2)
# set up as many clusters as detected by 'detectCores()'
cluster   <- parallel::makePSOCKcluster(cores)

# attach packages that will be needed on each cluster
clusterEvalQ(cluster, list(library(climwin),
                           library(lme4) , 
                           library(dplyr)) )


clusterExport(cluster, c("sliding_r") )

### Survival ----------------------------------------------------------------------------------------------------------------------------

Hs_c <- parLapply(cluster, 
          combis, function(x) sliding_r("HEQU", "s", 
                            "Data/Climate data/HEQU_NOAA_month_imputed.csv",
                            "Data/Biol data/HEQU_demography_data.csv", 
                            x))
Hs <- lapply(Hs_c, '[[', 1)
Hs$combos <- bind_rows(lapply(Hs_c, '[[', 2), .id="column_label")

Fs_c <- parLapply(cluster, 
               combis, function(x) sliding_r("FRSP", "s", 
                                             "Data/Climate data/FRSP_NOAA_month.csv",
                                             "Data/Biol data/FRSP_demography_data.csv", 
                                             x))
Fs <- lapply(Fs_c, '[[', 1)
Fs$combos <- bind_rows(lapply(Fs_c, '[[', 2), .id="column_label")

Os_c <- parLapply(cluster, 
                combis, function(x) sliding_r("OPIM", "s", 
                                              "Data/Climate data/OPIM_SEVLTER_month_imputed.csv",
                                              "Data/Biol data/OPIM_demography_data.csv", 
                                              x))
Os <- lapply(Os_c, '[[', 1)
Os$combos <- bind_rows(lapply(Os_c, '[[', 2), .id="column_label")

Cs_c <- parLapply(cluster, 
                combis, function(x) sliding_r("CRFL", "s", 
                                              "Data/Climate data/CRFL_NOAA_month.csv",
                                              "Data/Biol data/CRFL_demography_data.csv", 
                                              x))
Cs <- lapply(Cs_c, '[[', 1)
Cs$combos <- bind_rows(lapply(Cs_c, '[[', 2), .id="column_label")


