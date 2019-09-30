## R code that works with the matching submission script to run the sliding window analysis on 
## individual based datasets as an array job. This code is very sensitive to correct 
## dataformats/column names. Please read the README.md file of this project to see the correct 
## file name and data formats. 

suppressPackageStartupMessages(library(climwin))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(optparse))



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
    metavar = "HEQU|CRFL|OPIM|FRSP|HYGR")

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

if(cdata == "month") {                          ## 10 options
  
  xvar <- c("sum_prcp_scaled", "mean_prcp_scaled", "mean_tobs_scaled", "mean_tmax_scaled", "mean_tmin_scaled")
  type <- c("absolute")
  stat <- c("mean")
  func <- c("lin", "quad")
  upper <- NA            
  lower <- NA            
  
}

if(cdata == "day") {                           ## 16 options
  
  xvar <- c( "prcp_scaled_M", "tmax_scaled_M", "tmin_scaled_M", "tobs_scaled_M")
  type <- c("absolute")
  stat <- c("mean", "sd")
  func <- c("lin", "quad")
  upper <- NA            ## LEAVE these as NA, when adding stat = sum, use rbind function on row 37
  lower <- NA            ## LEAVE these as NA, when adding stat = sum, use rbind function on row 37
  
}

options <- expand.grid(xvar = xvar, type = type, stat = stat, func = func, upper = upper, lower = lower, stringsAsFactors = F)

## add growing degree days using "sum"

print(options[taskID,])

##----------------------------------------------------------------------------------------------------------------------------------
## Prepare Biological data 
##----------------------------------------------------------------------------------------------------------------------------------

### HEQU specific data manipulation

if (species == "HEQU") {                                
 Biol <- read.csv(SpeciesInput) %>%
  mutate(sizeT = as.integer(levels(sizeT))[sizeT],
         sizeT1 = as.integer(levels(sizeT1))[sizeT1])
Biol <- Biol[which(Biol$seedling != 1),]                           
Biol <- Biol[which(Biol$year!= 2012),]

Clim <- Clim[which(Clim$population == "mid"),]  ## only use climate data from one station
 
}


Biol$date <- paste(ifelse(!(is.na(Biol$day)), sprintf("%02d", Biol$day), "01") , sprintf("%02d", Biol$month), Biol$year, sep = "/")                  ### get a date that's accepted by climwin

##----------------------------------------------------------------------------------------------------------------------------------
## Use the right species specific baseline 
##----------------------------------------------------------------------------------------------------------------------------------

if (species == "HEQU") {

Biol <- Biol[which(!is.na(Biol$survival)),]                       
Biol <- Biol[which(!is.na(Biol$sizeT)),]  

  if (vitalrate == "s") {
    print("Running survival vital rate")
    Biol$lnsizeT <- log(Biol$sizeT)
    model <- glmer(formula = survival ~ lnsizeT + population + (1|year),
                   data = Biol, 
                   family = binomial) 
  }
  
  if (vitalrate =="g"){
    Biol <- Biol[which(!is.na(Biol$sizeT1)),]
    print("Running growth vital rate")
    model <- glmer(sizeT1 ~ sizeT + population + (1|year),
                   data = Biol,
	           family = poisson)                          
  }
  
  if (vitalrate =="fp"){
    print("Running Flower probability vital rate")
    Biol$lnsizeT <- log(Biol$sizeT)
    model <- glmer(formula = pflowerT ~ lnsizeT + population + (1|year),
                   data = Biol,
                   family = binomial)
  }
 
  if (vitalrate =="fn") {
    print("Running Number of Flowers")
    Biol <- Biol[which(!is.na(Biol$fertilityT)),]
    model <- glmer(formula = fertilityT ~ sizeT + population + (1|year),
                   data = Biol,
                   family = poisson)
  }
  
  if (vitalrate =="pa") {
    print("Running chance to abort")
    print("This still needs a baseline model")
    q(status = 1)   
  }
}

# if (species == "CRFL") {
#   model <- 
# }


#### Run function ----------------------------------------------------------------------------------------------------------------------------

x <- list(Clim[[options$xvar[taskID]]]) 
names(x) <- options$xvar[taskID]

result <- slidingwin(baseline = model,
           xvar = x,
           type = "absolute",
           range = c(ifelse(cdata == "month", 12, 365),ifelse(cdata == "month",-12,-365)),
           stat = options$stat[taskID], 
           upper = ifelse(options$stat[taskID] == "sum", options$upper[taskID], NA),
           lower = ifelse(options$stat[taskID] == "sum", options$lower[taskID], NA),
           func = options$func[taskID],
           refday = c(as.integer(format(min(as.Date(Biol$date, format = "%d/%m/%Y")), format = "%d")), as.integer(format(min(as.Date(Biol$date, format = "%d/%m/%Y")), format = "%m"))),                                                          
           cinterval = cdata,
           cdate = as.character(Clim$date), bdate = as.character(Biol$date)
           )


### Save object. Will be merged with all other combinations in another script -----------------------

saveRDS(result, file = output)