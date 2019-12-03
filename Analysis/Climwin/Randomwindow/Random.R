suppressPackageStartupMessages(library(climwin))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(lubridate))


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
  usage       = "Rscript %prog [options] Climate SpeciesInput Results_sliding winner output",
  option_list = Parsoptions,
  description = "\nan Run randomwindow analysis",
  epilogue    = ""
)

cli <- parse_args(parser, positional_arguments = 5)

### Assign shortcuts ------------------------------------------------------------------------------------------

cdata <- cli$options$climate_data_format
species <- cli$options$species_used
Climate   <- cli$args[1]
SpeciesInput  <- cli$args[2]
Results_sliding <- cli$args[3]
w <- as.integer(cli$args[4])
output <- cli$args[5]
taskID <- as.integer(Sys.getenv("SGE_TASK_ID"))


## Get which vital rate was analysed ------------------------------------------------------------------------------
getinfo <- stringr::str_split(Results_sliding, "[[:punct:]]")
vitalrate <- getinfo[[1]][7]


cdata
species
vitalrate
Climate
SpeciesInput
Results_sliding
w

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
    print("This still needs a baseline model")
    q(status = 1)   
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
    model <- glmer(formula = fertilityT ~ lnsizeT + (1|year),
                   data = Biol,
                   family = poisson)
  }
}


if (species == "OPIM") {

  if(vitalrate =="s"){
    print("Running survival vital rate")
    Biol <- Biol[which(!(is.na(Biol$survival))),]
    model <- glmer(formula = survival ~ lnsizeT + (1|year),
                   data = Biol, 
                   family = binomial) 
  }
  
  if(vitalrate == "g"){
    print("Running growth vital rate")
    Biol <- Biol[which(!is.na(Biol$sizeT1)),]
    Biol$lnsizeT1 <- log(Biol$sizeT1)
    model <- lmer(lnsizeT1 ~ lnsizeT + (1|year),
                  data = Biol)
  }
  
  if(vitalrate == "fp"){
    print("Running flower probability vital rate")
    Biol <- Biol[which(!is.na(Biol$lnsizeT)),]
    model <- glmer(pflowerT ~ lnsizeT + (1|year),
                   data = Biol,
                   family = binomial)
  }
  
  if(vitalrate == "fn"){
    print("Running Number of Flowers")
    Biol <- Biol[which(!is.na(Biol$fertilityT)),]
    model <- glmer(fertilityT ~ lnsizeT + (1|year),
                   data = Biol,
                   family = poisson)
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
    range <- c(36, -12)
  } else {
    range <- c(36, 0)
  }
}


if(vitalrate == "fn") {
  if(species == "FRSP") {
    range <- c(48, 0)
  } else {
    range <- c(36, 0)
  }
}

if(vitalrate == "pa") {
  range <- c(36, 0)
}

##----------------------------------------------------------------------------------------------------------------------------------
## Get Sliding result 
##----------------------------------------------------------------------------------------------------------------------------------

results <- readRDS(Results_sliding)
print(results$combos[w,])

##----------------------------------------------------------------------------------------------------------------------------------
## Randomized run for selected combi
##----------------------------------------------------------------------------------------------------------------------------------

x <- list(Clim[[as.character(results$combos$climate[w])]]) 
names(x) <- results$combos$climate[w]

str(x)

random <- randwin(repeats = 1,
                  baseline =  model   ,
                  xvar = list(Clim[[as.character(results$combos$climate[w])]]),
                  type = "absolute",
                  range = range,
                  stat = c(as.character(results$combos$stat[w])),
                  func = c(as.character(results$combos$func[w])),
                  refday = c(as.integer(format(min(as.Date(Biol$date, format = "%d/%m/%Y")), format = "%d")), as.integer(format(min(as.Date(Biol$date, format = "%d/%m/%Y")), format = "%m"))),                                                          
                  cinterval = cdata,
                  cdate = Clim$date, bdate = Biol$date,
                  window = "sliding",
                  cmissing = "method1"
)



saveRDS(random, file = output)

