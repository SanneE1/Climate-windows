#' Function for the sliding window analysis
#'
#' @param species Species abbreviation. Options: "HEQU", "FRSP", "OPIM", "CRFL"
#' @param vitalrate Vital rate abbreviation. Options: "s", "g", "fp", "fn"
#' @param Climate File location for the file with climate variables (.csv)
#' @param SpeciesInput File location for the demographic file (.csv)
#' @param combi dataframe of 1 row & 6 variables required in the sliding window analysis (xvar, type, stat, func, upper and lower)
#' 
#' @return a rds file with the results of the climwin sliding window analysis
#' 
#' @examples 
#' sliding_r("HEQU", "s", "file/path/climate.csv", "file/path/demographic.csv", data.frame(xvar = "mean_tobs", type = "absolute", stat = "mean", func = "lin", upper = NA, lower = NA))


sliding_r <- function(species, vitalrate,
                      Climate, SpeciesInput,
                      combi) {

if(any(!(c("climwin", "dplyr", "lme4") %in% (.packages())))){
  stop("make sure that the \"climwin\", \"dplyr\" and \"lme4\" packages are loaded before running this function")
}

##----------------------------------------------------------------------------------------------------------------------------------
## Prepare Climate data 
##----------------------------------------------------------------------------------------------------------------------------------

Clim <- read.csv(Climate)                                                ### get a date that's accepted by climwin
Clim$date <- paste("15/",sprintf("%02d", Clim$Month), "/", Clim$Year, sep = "")          


##----------------------------------------------------------------------------------------------------------------------------------
## Prepare Biological data 
##----------------------------------------------------------------------------------------------------------------------------------

### Species specific data modification

if (species == "HEQU") {                                
  Biol <- read.csv(SpeciesInput) %>%
    mutate(sizeT = as.integer(levels(sizeT))[sizeT],
           sizeT1 = as.integer(levels(sizeT1))[sizeT1])
  Biol <- Biol[which(Biol$seedling != 1),]                           
  Biol <- Biol[which(Biol$year!= 2012),]
  Biol <- Biol[which(!(is.na(Biol$sizeT) | Biol$sizeT == 0)),]
  Biol$lnsizeT <- log(Biol$sizeT)
}


if (species == "CRFL"){
  Biol <- read.csv(SpeciesInput) %>%
    mutate(sizeT = as.integer(sizeT),
           sizeT1 = as.integer(sizeT1))
  
  Biol <- Biol[which(Biol$year %in% c(1997:2000,2003:2011)),]
  Biol <- Biol[which(!(is.na(Biol$sizeT) | Biol$sizeT == 0)),]
  Biol$lnsizeT <- log(Biol$sizeT)
}


if (species == "OPIM"){
  Biol <- read.csv(SpeciesInput)
  Biol <- Biol[which(!(is.na(Biol$sizeT) | Biol$sizeT == 0)),]
  Biol <- Biol[which(Biol$year != 2018),]
  Biol$lnsizeT <- log(Biol$sizeT)
}


if (species == "FRSP"){
  Biol <- read.csv(SpeciesInput)
  Biol$lnsizeT <- log(Biol$sizeT)
  Biol <- Biol[which(!is.na(Biol$sizeT)),]
  if (vitalrate == "fn"){
    Biol$year <- Biol$yearT1
  }
}

### General data modification

Biol$date <- paste(ifelse(!(is.na(Biol$day)), sprintf("%02d", Biol$day), "01") , sprintf("%02d", Biol$month), Biol$year, sep = "/")                  ### get a date that's accepted by climwin

##----------------------------------------------------------------------------------------------------------------------------------
## Use the right species specific baseline 
##----------------------------------------------------------------------------------------------------------------------------------

if (species == "HEQU") {
  
  if (vitalrate == "s") {                  
    print("Running survival vital rate")
    Biol <- Biol[which(!(is.na(Biol$survival))),]
    model <- glmer(formula = survival ~ lnsizeT + population + (1|year),
                   data = Biol, 
                   family = binomial) 
  }
  
  if (vitalrate =="g"){                         
    print("Running growth vital rate")
    Biol <- Biol[which(!is.na(Biol$sizeT1)),]
    model <- glmer(sizeT1 ~ lnsizeT + population + (1|year),
                   data = Biol,
                   family = poisson)                          
  }
  
  if (vitalrate =="fp"){
    print("Running Flower probability vital rate")
    model <- glmer(formula = pflowerT ~ lnsizeT + population + (1|year),
                   data = Biol,
                   family = binomial)
  }
  
  if (vitalrate =="fn") {                       
    print("Running Number of Flowers")
    Biol <- Biol[which(!is.na(Biol$fertilityT)),]
    Biol$fertilityT <- as.integer(Biol$fertilityT)
    model <- glmer(formula = fertilityT ~ lnsizeT + population + (1|year),
                   data = Biol,
                   family = poisson)
  }
  
  if (vitalrate =="pa") {                         
    print("Running chance to abort")
    Biol$pAbort <- Biol$abort.stalks / Biol$fertilityT
    Biol <- Biol[which(!is.na(Biol$pAbort)),]
    model <- glmer(pAbort ~ population + (1|year),
                   data = Biol,
                   family = binomial)
  }
}


if (species == "CRFL") {
  
  if(vitalrate =="s"){
    print("Running survival vital rate")
    model <- glmer(formula = survival ~ lnsizeT + Block + (1|year),
                   data = Biol, 
                   family = binomial) 
  }
  
  if(vitalrate == "g"){
    print("Running growth vital rate")
    Biol <- Biol[which(!is.na(Biol$sizeT1)),]
    Biol <- Biol[which(Biol$sizeT1 != 0),]
    model <- glmer(sizeT1 ~ lnsizeT + Block + (1|year),
                   data = Biol,
                   family = poisson) 
  }
  
  if(vitalrate == "fp"){
    print("Running flower probability vital rate")
    model <- glmer(formula = pflowerT ~ lnsizeT + Block + (1|year),
                   data = Biol,
                   family = binomial)
  }
  
  if(vitalrate == "fn"){
    print("Running Number of Flowers")
    Biol <- Biol[which(!is.na(Biol$fertilityT)),]
    Biol <- Biol[which(!is.na(Biol$sizeT)),]
    model <- glmer(formula = fertilityT ~ lnsizeT + Block + (1|year),
                   data = Biol,
                   family = poisson)
  }
}


if (species == "OPIM") {
  
  if(vitalrate =="s"){
    print("Running survival vital rate")
    Biol <- Biol[which(!(is.na(Biol$survival))),]
    model <- glmer(formula = survival ~ lnsizeT + (1|year) + (1|Plot),
                   data = Biol, 
                   family = binomial) 
  }
  
  if(vitalrate == "g"){
    print("Running growth vital rate")
    Biol <- Biol[which(!is.na(Biol$sizeT1)),]
    Biol$lnsizeT1 <- log(Biol$sizeT1)
    model <- lmer(lnsizeT1 ~ lnsizeT + (1|year) + (1|Plot),
                  data = Biol)
  }
  
  if(vitalrate == "fp"){
    print("Running flower probability vital rate")
    Biol <- Biol[which(!is.na(Biol$lnsizeT)),]
    model <- glmer(pflowerT ~ lnsizeT + (1|year) + (1|Plot),
                   data = Biol,
                   family = binomial)
  }
  
  if(vitalrate == "fn"){
    print("Running Number of Flowers")
    Biol <- Biol[which(!is.na(Biol$fertilityT)),]
    model <- glmer(fertilityT ~ lnsizeT + (1|year) + (1|Plot),
                   data = Biol,
                   family = poisson)
  }
  
  if(vitalrate == "pa") {
    Biol <- Biol[which(Biol$pflowerT == 1),]
    Biol$ABFlowerbuds_t[which(is.na(Biol$ABFlowerbuds_t))] <- 0
    Biol$pAbort <- Biol$ABFlowerbuds_t / Biol$fertilityT
    model <- glmer(pAbort ~ (1|Plot) + (1|year),
                   data = Biol,
                   family = binomial)
  }
}


if (species == "FRSP") {
  
  if(vitalrate =="s"){
    print("Running survival vital rate")
    model <- glmer(formula = survival ~ lnsizeT + (1|year),
                   data = Biol, 
                   family = binomial) 
  }
  
  if(vitalrate == "g"){
    print("Running growth vital rate")
    Biol <- Biol[which(!is.na(Biol$sizeT1)),]
    model <- glmer(sizeT1 ~ lnsizeT + (1|year),
                   data = Biol,
                   family = poisson)
  }
  
  if(vitalrate == "fp"){
    print("Running flower probability T+1 vital rate")
    Biol <- Biol[which(!is.na(Biol$pFlowerT1)),]
    model <- glmer(pFlowerT1 ~ lnsizeT + (1|year),
                   data = Biol,
                   family = binomial)
  }
  
  if(vitalrate == "fn") {
    print("Running flower numbers T+1")
    model <- glmer(nFlowersT1 ~ lnsizeT + (1|yearT1),
                   data = Biol,
                   family = poisson)
  }
}

#### Set Range ----------------------------------------------------------------------------------------------------------------------------
if(vitalrate == "s") {
  range <- c(24,-12)
}

if(vitalrate == "g") {
  if(species == "FRSP") {
    print("Range set to 5 years")
    range <- c(48, -12)
  } else {
    range <- c(24,-12)
  }
}

if(vitalrate == "fp") {
  if(species == "FRSP") {
    print("Range set to 4 years")
    range <- c(36, -12)
  } else {
    range <- c(36, 0)
  }
}

if(vitalrate == "fn") {
  if(species == "FRSP") {
    print("Range set to 4 years")
    range <- c(48, 0)
  } else {
    range <- c(36, 0)
  }
}

if(vitalrate == "pa") {
  range <- c(36, 0)
}

#### Run function ----------------------------------------------------------------------------------------------------------------------------

x <- list(Clim[[combi$xvar]]) 
names(x) <- combi$xvar

result <- slidingwin(baseline = model,
                     xvar = x,
                     type = "absolute",
                     range = range,
                     stat = combi$stat, 
                     upper = ifelse(combi$stat == "sum", combi$upper, NA),
                     lower = ifelse(combi$stat == "sum", combi$lower, NA),
                     func = combi$func,
                     refday = c(as.integer(format(min(as.Date(Biol$date, format = "%d/%m/%Y")), format = "%d")), as.integer(format(min(as.Date(Biol$date, format = "%d/%m/%Y")), format = "%m"))),                                                          
                     cinterval = "month",
                     cdate = as.character(Clim$date), bdate = as.character(Biol$date),
                     cmissing = "method1"
)



return(result)




}
