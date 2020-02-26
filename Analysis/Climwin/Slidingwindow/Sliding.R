## R code that works with the matching submission script to run the sliding window analysis on 
## individual based datasets as an array job. This code is very sensitive to correct 
## dataformats/column names. Please read the README.md file of this project to see the correct 
## file name and data formats. 

suppressPackageStartupMessages(library(climwin))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(optparse))


start <- Sys.time()
#  ----------------------------------------------------------------------------------------------------------------------------
# parsing arguments
#  ----------------------------------------------------------------------------------------------------------------------------

Parsoptions <- list (
  make_option(
    opt_str = c("-c", "--climate-data-format"),
    dest    = "climate_data_format",
    help    = "Specify the format of the climate data, either month or day",
    metavar = "month|day"),
 
   make_option(
    opt_str = c("-s", "--species-used"),
    dest    = "species_used",
    help    = "Specify the species that will be used",
    metavar = "HEQU|CRFL|OPIM|FRSP")

)
  
parser <- OptionParser(
  usage       = "Rscript %prog [options] vitalrate Climate SpeciesInput output",
  option_list = Parsoptions,
  description = "\nan Run slidingwindow analysis",
  epilogue    = ""
)

cli <- parse_args(parser, positional_arguments = 4)

#  ----------------------------------------------------------------------------------------------------------------------------
# Assign shortcuts
#  ----------------------------------------------------------------------------------------------------------------------------

cdata <- cli$options$climate_data_format    
species <- cli$options$species_used
vitalrate <- cli$args[1]
Climate   <- cli$args[2]
SpeciesInput  <- cli$args[3]
output <- cli$args[4]
taskID <- as.integer(Sys.getenv("SGE_TASK_ID"))

species

### Check 
if (!(cdata == "month"||cdata == "day")) {
  print("Climate data format needs to be either \"month\" or \"day\"")
  q(status = 1)
}

##----------------------------------------------------------------------------------------------------------------------------------
## Prepare Climate data 
##----------------------------------------------------------------------------------------------------------------------------------

Clim <- read.csv(Climate)                                                ### get a date that's accepted by climwin

if(cdata == "month") {
  Clim$date <- paste("15/",sprintf("%02d", Clim$Month), "/", Clim$Year, sep = "")          
}

if(cdata == "day") {
  Clim$date <- as.character(Clim$date)                                         
}

### Climate signal combies ----------------------------------------------------------------------------------------------------------------------------

if(cdata == "month") {                          ## 16 options
  
  xvar <- c("mean_prcp_scaled", "mean_tobs_scaled", "mean_tmax_scaled", "mean_tmin_scaled", "mean_tavg_scaled", "SPEI")
  type <- c("absolute")
  stat <- c("mean")
  func <- c("lin", "quad")
  upper <- NA            
  lower <- NA            
  
}

if(cdata == "day") {                           ## 24 options
  
  xvar <- c( "prcp_scaled_M", "tmax_scaled_M", "tmin_scaled_M", "tobs_scaled_M", "tavg_scaled_M")
  type <- c("absolute")
  stat <- c("mean", "sd")
  func <- c("lin", "quad")
  upper <- NA            ## LEAVE these as NA, when adding stat = sum, use rbind function on row 37
  lower <- NA            ## LEAVE these as NA, when adding stat = sum, use rbind function on row 37
  
}

options <- expand.grid(xvar = xvar, type = type, stat = stat, func = func, upper = upper, lower = lower, stringsAsFactors = F)

## add growing degree days using "sum"

if(cdata == "month"){
options <- rbind(options, c("min_tmin_scaled", "absolute", "min", "lin", 0, NA), c("min_tmin_scaled", "absolute", "min", "quad", 0, NA),
                          c("max_tmax_scaled", "absolute", "max", "lin", 0, NA), c("max_tmax_scaled", "absolute", "max", "quad", 0, NA)
                  )
}

print(options[taskID,])

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
  
  if (vitalrate =="g"){                         #### Change this to negative binomial
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
 
  if (vitalrate =="fn") {                        #### Change this to negative binomial
    print("Running Number of Flowers")
    Biol <- Biol[which(!is.na(Biol$fertilityT)),]
    Biol$fertilityT <- as.integer(Biol$fertilityT)
    model <- glmer(formula = fertilityT ~ lnsizeT + population + (1|year),
                   data = Biol,
                   family = poisson)
  }
  
  if (vitalrate =="pa") {                         #### Change this to Beta binomial
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

x <- list(Clim[[options$xvar[taskID]]]) 
names(x) <- options$xvar[taskID]

result <- slidingwin(baseline = model,
           xvar = x,
           type = "absolute",
           range = range,
           stat = options$stat[taskID], 
           upper = ifelse(options$stat[taskID] == "sum", options$upper[taskID], NA),
           lower = ifelse(options$stat[taskID] == "sum", options$lower[taskID], NA),
           func = options$func[taskID],
           refday = c(as.integer(format(min(as.Date(Biol$date, format = "%d/%m/%Y")), format = "%d")), as.integer(format(min(as.Date(Biol$date, format = "%d/%m/%Y")), format = "%m"))),                                                          
           cinterval = cdata,
           cdate = as.character(Clim$date), bdate = as.character(Biol$date),
           cmissing = "method1"
           )


### Save object. Will be merged with all other combinations in another script -----------------------

saveRDS(result, file = output)

Sys.time() - start
