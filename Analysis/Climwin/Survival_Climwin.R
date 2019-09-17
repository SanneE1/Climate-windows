## R code that works with the matching submission script to run the sliding window analysis on 
## individual based datasets as an array job. This code is very sensitive to correct 
## dataformats/column names. Please read the README.md file of this project to see the correct 
## file name and data formats. 

suppressPackageStartupMessages(library(climwin))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(optparse))



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
  usage       = "Rscript %prog [options] Climate SpeciesInput output",
  option_list = Parsoptions,
  description = "\nan Run slidingwindow analysis",
  epilogue    = ""
)

cli <- parse_args(parser, positional_arguments = 3)

#  ----------------------------------------------------------------------------------------------------------------------------
# assign a few shortcuts
#  ----------------------------------------------------------------------------------------------------------------------------

cdata <- cli$climate_data_format    ### change this to "month" to get it working for now
Climate   <- cli$args[1]
SpeciesInput  <- cli$args[2]
output <- cli$args[3]
taskID <- as.integer(Sys.getenv("SGE_TASK_ID"))

print("This is in the R code")

cdata
Climate
SpeciesInput
output
taskID

print("This is the end of options in R script")

### Check 
if (!(cdata == "month"||cdata == "day")) {
  print("Climate data format needs to be either \"month\" or \"day\"")
  q(status = 1)
}

### Prepare data ----------------------------------------------------------------------------------------------------------------------------
Clim <- read.csv(Climate)                                                ### get a date that's accepted by climwin

if(cdata == "month") {
  Clim$date <- paste("15/",sprintf("%02d", Clim$Month), "/", Clim$Year, sep = "")          
}

if(cdata == "day") {
  Clim$date <- as.character(Clim$date)                                         
}


Biol <- read.csv(SpeciesInput) %>%
  mutate(sizeT = as.numeric(as.character(sizeT)),
         sizeT1 = as.numeric(as.character(sizeT1)))

Biol$date <- paste(ifelse(!(is.na(Biol$day)), sprintf("%02d", Biol$day), "01") , sprintf("%02d", Biol$month), Biol$year, sep = "/")                  ### get a date that's accepted by climwin
Biol <- Biol[which(Biol$seedling != 1),]                           ### HEQU species specific
Biol <- Biol[which(!is.na(Biol$survival)),]                       
Biol <- Biol[which(!is.na(Biol$sizeT)),]                           

### Climate signal combies ----------------------------------------------------------------------------------------------------------------------------
if(cdata == "month") {

  xvar <- c("sum_prcp", "mean_prcp", "sd_prcp", "mean_tobs", "sd_tobs", "mean_tmax", "mean_tmin", "max_tmax", "min_tmin")
  type <- c("absolute")
  stat <- c("mean","slope", "sd")
  func <- c("lin", "quad")
  upper <- NA            ## LEAVE these as NA, when adding stat = sum, use rbind function on row 37
  lower <- NA            ## LEAVE these as NA, when adding stat = sum, use rbind function on row 37

}

if(cdata == "day") {
  
  xvar <- c("tobs", "prcp", "tmax", "tmin", "prcp_scaled_M", "tmax_scaled_M", "tmin_scaled_M", "tobs_scaled_M")
  type <- c("absolute")
  stat <- c("mean","slope", "sd")
  func <- c("lin", "quad")
  upper <- NA            ## LEAVE these as NA, when adding stat = sum, use rbind function on row 37
  lower <- NA            ## LEAVE these as NA, when adding stat = sum, use rbind function on row 37
  
}

options <- expand.grid(xvar = xvar, type = type, stat = stat, func = func, upper = upper, lower = lower, stringsAsFactors = F)

if(cdata == "day") {
 options <- rbind(options, c("tobs", "absolute", "sum", "lin", 5, NA))  ## example of adding a "sum" combination
}

print(options[taskID,])

#### Run function ----------------------------------------------------------------------------------------------------------------------------

x <- list(Clim[[options$xvar[taskID]]]) 
names(x) <- options$xvar[taskID]

result <- slidingwin(baseline = glmer(formula = survival ~ sizeT + population + (1|year),
                            data = Biol, 
                            family = binomial),
           xvar = x,
           type = "absolute",
           range = c(ifelse(cdata == "month", 12, 365), 0),
           stat = options$stat[taskID], 
           upper = ifelse(options$stat[taskID] == "sum", options$upper[taskID], NA),
           lower = ifelse(options$stat[taskID] == "sum", options$lower[taskID], NA),
           func = options$func[taskID],
           refday = c(day(min(as.Date(Biol$date, format = "%d/%m/%Y"))), month(min(as.Date(Biol$date, format = "%d/%m/%Y")))),                             
           cinterval = cdata,
           cdate = as.character(Clim$date), bdate = as.character(Biol$date),
           spatial = list(Biol$population, Clim$population)
           )


### Save object. Will be merged with all other combinations in another script -----------------------

saveRDS(result, file = output)
