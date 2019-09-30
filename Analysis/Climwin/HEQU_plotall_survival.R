library(climwin)
library(dplyr)
library(lme4)

Clim <- read.csv("Data/Climate data/HEQU_NOAA_month.csv") 
Clim$date <- paste("15/",sprintf("%02d", Clim$Month), "/", Clim$Year, sep = "")          

Biol <- read.csv("Data/Biol data/HEQU_demography_data.csv") %>%
  mutate(sizeT = as.numeric(levels(sizeT))[sizeT],
         sizeT1 = as.numeric(levels(sizeT1))[sizeT1])
Biol <- Biol[which(Biol$seedling != 1),]                           
Biol <- Biol[which(!is.na(Biol$survival)),]                       
Biol <- Biol[which(!is.na(Biol$sizeT)),]                           
Biol$date <- paste(ifelse(!(is.na(Biol$day)), sprintf("%02d", Biol$day), "01") , sprintf("%02d", Biol$month), Biol$year, sep = "/")                  ### get a date that's accepted by climwin

result <- readRDS("Results/Climwin/20190925/HEQU_month_result.rds")
random <- readRDS("Results/Climwin/20190925/Random_HEQU_month_5650330_result.rds")

delta_tmin <- plotdelta(result[[2]]$Dataset, arrow = T)+ ggtitle("dAIC of mean_tmin")
delta_sprcp <- plotdelta(result[[7]]$Dataset, arrow = T)+ ggtitle("dAIC of sum_prcp")
gridExtra::grid.arrange(delta_tmin, delta_sprcp, nrow = 1)

plotall(datasetrand = random[[1]],
        dataset = result[[2]]$Dataset,
        bestmodel = result[[2]]$BestModel,
        bestmodeldata = result[[2]]$BestModelData,
        title = result$combos$climate[2],
        arrow = T)

crosscol <- crosswin(xvar = list(Min_Temp = Clim$mean_tmin_scaled),
                     xvar2 = list(Sum_PRCP = Clim$sum_prcp_scaled),
                     stat = "mean", stat2 = "mean",
                     cdate = Clim$date, bdate = Biol$date,
                     range = c(12,-12),
                     refday = c(1,7),
                     type = "absolute",
                     cinterval = "month")

# saveRDS(crosscol, "Results/Climwin/20190925/crosscolinearity_tmin_prcp.rds")

autocol <- autowin(reference = result[[2]],
                   xvar = list(Min_Temp = Clim$mean_tmin_scaled),
                   cdate = Clim$date, bdate = Biol$date,
                   baseline = glm(survival ~ sizeT + population, data = Biol, family = "binomial"),
                   range = c(12,-12),
                   stat = "mean",
                   func = "quad",
                   type = "absolute",
                   refday = c(1,7),
                   cinterval = "month")
# saveRDS(autocol, "Results/Climwin/20190925/autocollinearity_tmin.rds")

gridExtra::grid.arrange(plotcor(crosscol, type = "C", arrow = T) + ggtitle("Cross colinearity between mean_tmin and sum_prcp"), 
                        plotcor(autocol, type = "A", arrow = T) + ggtitle("Autocolinearity for mean_tmin"), 
                        ncol = 2)
