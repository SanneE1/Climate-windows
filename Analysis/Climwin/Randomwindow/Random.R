suppressPackageStartupMessages(library(climwin))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(lubridate))

#### Choose which is the 'best' combo

w <- 1    ## Keep at one. If I ever find out a way to put in the merged file, this needs to be changed to combination number we want to test

#  ----------------------------------------------------------------------------------------------------------------------------
# parsing arguments
#  ----------------------------------------------------------------------------------------------------------------------------

Parsoptions <- list (
  make_option(
    opt_str = c("-c", "--climate-data-format"),
    dest    = "climate_data_format",
    help    = "Specify the format of the climate data, either month or day",
    metavar = "month|day")
)

parser <- OptionParser(
  usage       = "Rscript %prog [options] vitalrate Climate SpeciesInput Results_sliding output",
  option_list = Parsoptions,
  description = "\nan Run randomwindow analysis",
  epilogue    = ""
)

cli <- parse_args(parser, positional_arguments = 5)

### Assign shortcuts ------------------------------------------------------------------------------------------

cdata <- cli$options$climate_data_format
species <- cli$options$species_used
vitalrate <- cli$args[1]
Climate   <- cli$args[2]
SpeciesInput  <- cli$args[3]
Results_sliding <- cli$args[4]
output <- cli$args[5]
taskID <- as.integer(Sys.getenv("SGE_TASK_ID"))

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

### HEQU specific data manipulation

if (species == "HEQU") {                                
  Biol <- read.csv(SpeciesInput) %>%
    mutate(sizeT = as.numeric(levels(sizeT))[sizeT],
           sizeT1 = as.numeric(levels(sizeT1))[sizeT1])
  Biol <- Biol[which(Biol$seedling != 1),]                           
  Biol <- Biol[which(!is.na(Biol$survival)),]                       
  Biol <- Biol[which(!is.na(Biol$sizeT)),]                           
  
}


Biol$date <- paste(ifelse(!(is.na(Biol$day)), sprintf("%02d", Biol$day), "01") , sprintf("%02d", Biol$month), Biol$year, sep = "/")                  ### get a date that's accepted by climwin

##----------------------------------------------------------------------------------------------------------------------------------
## Use the right species specific baseline 
##----------------------------------------------------------------------------------------------------------------------------------

if (species == "HEQU") {
  
  if (vitalrate == "s") {
    print("Running survival vital rate")
    model <- glmer(formula = survival ~ sizeT + population + (1|year),
                   data = Biol, 
                   family = binomial) 
  }
  
  if (vitalrate =="g"){
    print("Running growth vital rate")
    model <- readRDS("/data/gsclim/BaselineModels/HEQU_growth_baseline.rds")
    Biol <- Biol[which(!is.na(Biol$sizeT1)),]                           
    
  }
  
}

# if (species == "CRFL") {
#   model <- 
# }


##----------------------------------------------------------------------------------------------------------------------------------
## Get Sliding result 
##----------------------------------------------------------------------------------------------------------------------------------

results <- readRDS(Results_sliding)
print(results$combos)

##----------------------------------------------------------------------------------------------------------------------------------
## Randomized run for selected combi
##----------------------------------------------------------------------------------------------------------------------------------

x <- list(Clim[[as.character(results$combos$climate[w])]]) 
names(x) <- results$combos$climate[w]

random <- randwin(repeats = 1,
                  baseline =  model   ,
                  xvar = list(Clim[[as.character(results$combos$climate[w])]]),
                  type = as.character(results$combos$type[w]),
                  range = c(ifelse(cdata == "month", 12, 365),ifelse(cdata == "month",-12,-365)),
                  stat = c(as.character(results$combos$stat[w])),
                  func = c(as.character(results$combos$func[w])),
                  refday = c(as.integer(format(min(as.Date(Biol$date, format = "%d/%m/%Y")), format = "%d")), as.integer(format(min(as.Date(Biol$date, format = "%d/%m/%Y")), format = "%m"))),                                                          
                  cinterval = cdata,
                  cdate = Clim$date, bdate = Biol$date,
                  spatial = list(Biol$population, Clim$population),
                  window = "sliding"
)



saveRDS(random, file = output)

