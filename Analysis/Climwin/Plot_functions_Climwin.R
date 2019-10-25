library(climwin)
library(dplyr)
library(lme4)

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

### Getting data --------------------------------------------------------------------------------------------


# ##HEQU
# SpeciesInput <- "Data/Biol data/HEQU_demography_data.csv"
# source_lines("Analysis/Climwin/Slidingwindow/Sliding.R", c(117:121, 140:143))
# 
# Clim <- read.csv("Data/Climate data/HEQU_NOAA_month_imputed.csv")
# Clim$date <- paste("15/",sprintf("%02d", Clim$Month), "/", Clim$Year, sep = "")          

# ## CRFL
# SpeciesInput <- "Data/Biol data/CRFL_demography_data.csv"
# source_lines("Analysis/Climwin/Slidingwindow/Sliding.R", c(126:130, 140:143))
# 
# Clim <- read.csv("Data/Climate data/CRFL_NOAA_month.csv") 
# Clim$date <- paste("15/",sprintf("%02d", Clim$Month), "/", Clim$Year, sep = "")          

## OPIM
SpeciesInput <- "Data/Biol data/OPIM_demography_data.csv"
source_lines("Analysis/Climwin/Slidingwindow/Sliding.R", c(135, 140:143))
Biol$lnsizeT <- log(Biol$sizeT)
Biol$lnsizeT1 <- log(Biol$sizeT1)

Clim <- read.csv("Data/Climate data/OPIM_SEVLTER_month_imputed.csv") 
Clim$date <- paste("15/",sprintf("%02d", Clim$Month), "/", Clim$Year, sep = "")          



### Import results
result <- readRDS("Results/Climwin/20191024/OPIM_s_month_result.rds")
random <- readRDS("Results/Climwin/20191024/OPIM_s_month_random.rds")

delta_tavg <- plotdelta(result[[2]]$Dataset, arrow = T)+ ggtitle("dAIC of mean_tavg")
delta_tmin <- plotdelta(result[[8]]$Dataset, arrow = T)+ ggtitle("dAIC of sum_tmin")
gridExtra::grid.arrange(delta_tavg, delta_tmin, nrow = 1)

plotall(datasetrand = random[[1]],
        dataset = result[[2]]$Dataset,
        bestmodel = result[[2]]$BestModel,
        bestmodeldata = result[[2]]$BestModelData,
        title = result$combos$climate[2],
        arrow = T)

crosscol <- crosswin(xvar = list(tavg = Clim$mean_tavg_scaled),
                     xvar2 = list(tmin = Clim$mean_tmin_scaled),
                     stat = "mean", stat2 = "mean",
                     cdate = Clim$date, bdate = Biol$date,
                     range = c(48,-12),
                     refday = c(1,7),
                     type = "absolute",
                     cinterval = "month")

# saveRDS(crosscol, "Results/Climwin/20190925/crosscolinearity_tmin_prcp.rds")

autocol <- autowin(reference = result[[2]],
                   xvar = list(Min_Temp = Clim$mean_tavg_scaled),
                   cdate = Clim$date, bdate = Biol$date,
                   baseline = glm(survival ~ lnsizeT + (1|Biol$year), data = Biol, family = "binomial"),
                   range = c(48,-12),
                   stat = "mean",
                   func = "quad",
                   type = "absolute",
                   refday = c(1,7),
                   cinterval = "month")
# saveRDS(autocol, "Results/Climwin/20190925/autocollinearity_tmin.rds")

gridExtra::grid.arrange(plotcor(crosscol, type = "C", arrow = T) + ggtitle("Cross colinearity between tavg_scaled and tmin_scaled"), 
                        plotcor(autocol, type = "A", arrow = T) + ggtitle("Autocolinearity for tavg_scaled"), 
                        ncol = 2)
