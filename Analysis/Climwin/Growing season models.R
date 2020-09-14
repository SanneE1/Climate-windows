# Looks like there's something going wrong in original climwin singlewin() function around line 270 - cast function - 
# only returns NA's even though there are no missing values in input and it does contain values in after the 
# melt in 269 Ive filtered out NA's in line 269 in the "custom" function

library(climwin)
library(dplyr)
library(lme4)
library(optparse)
library(lubridate)
library(reshape)

source("Analysis/Climwin/custom_singlewin_function.R")

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}


speciesA <- c("HEQU", "FRSP", "OPIM", "CRFL")
ClimateA <- c("data/Climate data/HEQU_NOAA_month_imputed.csv",
             "data/Climate data/FRSP_NOAA_month.csv",
             "data/Climate data/OPIM_SEVLTER_month_imputed.csv",
             "data/Climate data/CRFL_NOAA_month.csv")
SpeciesInputA <- c("data/Biol data/HEQU_demography_data.csv",
                  "data/Biol data/FRSP_demography_data.csv",
                  "data/Biol data/OPIM_demography_data.csv",
                  "data/Biol data/CRFL_demography_data.csv")
refA = c(7, 7, 5, 5)


for (j in c(1:4)) {
  
species <- speciesA[j]
Climate <- ClimateA[j]
SpeciesInput <- SpeciesInputA[j]
ref <- refA[j]

for (vitalrate in c("s", "g", "fp", "fn")) {
  
  if(species == "FRSP" & vitalrate == "fn"){
    SpeciesInput <- "data/Biol data/FRSP_Cleaned_FlowerN.csv" 
  }
  
  source_lines("Analysis/Climwin/Slidingwindow/Sliding.R", c(52:70, 75:273))
  
  # Set growing season range
  if(species == "HEQU" || species == "FRSP") {
    
    range <- c(1,-2)
    if(vitalrate == "fn") {
      range <- c(13, 10)
    }
    if(species == "HEQU" & vitalrate == "fp") {
      range <- c(13, 10)
    }
  }
  
  if(species == "OPIM" ) {
    if(vitalrate == "s"|| vitalrate == "g"){
      range <- c(5, 0)  
    } else {
      range <- c(12, 7)
    }
  }
  
  if(species == "CRFL") {
    if(vitalrate == "s"|| vitalrate == "g") {
      range <- c(1, -3)
    } else {
      range <- c(13, 9)
    }
  }
  
  # List right climate variables
  if(species == "HEQU" || species == "FRSP"){
    climate <- list(sum_prcp = Clim$sum_prcp,
                    mean_tavg = Clim$mean_tavg,
                    mean_tmin = Clim$mean_tmin,
                    mean_tmax = Clim$mean_tmax,
                    sum_snow = Clim$sum_snow,
                    mean_snwd = Clim$mean_snwd,
                    SPEI = Clim$SPEI)
  } else {
    climate <- list(sum_prcp = Clim$sum_prcp,
                    mean_tavg = Clim$mean_tavg,
                    mean_tmin = Clim$mean_tmin,
                    mean_tmax = Clim$mean_tmax,
                    SPEI = Clim$SPEI)
  }
  
  grow_mod <- lapply(climate, function(x) own.singlewin(xvar = list(climate = x),
                                                       cdate = as.factor(as.character(Clim$date)),
                                                       bdate = as.factor(as.character(Biol$date)),
                                                       baseline = model,
                                                       range = range,
                                                       type = "absolute", refday = c(1,ref),
                                                       stat = "mean", func = c("lin"),
                                                       cinterval = "month",
                                                       cmissing = FALSE))
  
  a <- sapply(grow_mod, function(x) AIC(x$BestModel)) - AIC(model)
  b <- sapply(grow_mod, function(x) rsq::rsq(x$BestModel, adj = T)$model %>% round(., digits = 4))

  c <- cbind(dAIC = a, 
             R2 = b)
  
  write.csv(c, file = paste("Results/Climwin/baseline_", species, "_", vitalrate, ".csv", sep = ""))
  
    
  }
}





