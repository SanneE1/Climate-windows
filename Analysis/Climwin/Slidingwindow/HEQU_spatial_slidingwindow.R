## Run HEQU data with PRISM data

suppressPackageStartupMessages(library(climwin))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(optparse))

start <- Sys.time()
#  ----------------------------------------------------------------------------------------------------------------------------
# parsing arguments from commit script in EVE
#  ----------------------------------------------------------------------------------------------------------------------------


parser <- OptionParser(
  usage       = "Rscript %prog [options] vitalrate Climate SpeciesInput output",
  description = "\nan Run slidingwindow analysis",
  epilogue    = ""
)

cli <- parse_args(parser, positional_arguments = 4)


#  ----------------------------------------------------------------------------------------------------------------------------
# Assign shortcuts
#  ----------------------------------------------------------------------------------------------------------------------------

vitalrate <- cli$args[1]
Climate   <- cli$args[2]
SpeciesInput  <- cli$args[3]
output <- cli$args[4]
taskID <- as.integer(Sys.getenv("SGE_TASK_ID"))

##----------------------------------------------------------------------------------------------------------------------------------
## Prepare Climate data 
##----------------------------------------------------------------------------------------------------------------------------------

Clim <- read.csv(Climate) 
### get a date that's accepted by climwin
Clim$date <- paste("15/",sprintf("%02d", Clim$Month), "/", Clim$Year, sep = "")          

### Climate signal combinations ----------------------------------------------------------------------------------------------------------------------------


xvar <- c("tmean_scaled", "ppt_scaled")
type <- c("absolute")
stat <- c("mean")
func <- c("lin")
upper <- NA            
lower <- NA            

options <- expand.grid(xvar = xvar, type = type, stat = stat, func = func, upper = upper, lower = lower, stringsAsFactors = F)

# print out for reference in the log file
print(options[taskID,])


##----------------------------------------------------------------------------------------------------------------------------------
## Prepare Biological data 
##----------------------------------------------------------------------------------------------------------------------------------

Biol <- read.csv(SpeciesInput) %>%
  mutate(sizeT = as.integer(sizeT),
         sizeT1 = as.integer(sizeT1))
Biol <- Biol[which(Biol$seedling != 1),] # filter out seedlings - seperate stage class in previous lit.                          
Biol <- Biol[which(Biol$year!= 2012),] # filter out transition 2012-2013 due to gophers

Biol <- Biol[which(!(is.na(Biol$sizeT) | Biol$sizeT == 0)),] # filter out individuals without size in time t
Biol$lnsizeT <- log(Biol$sizeT) # log transform size at time t

Biol$date <- paste(ifelse(!(is.na(Biol$day)), sprintf("%02d", Biol$day), "01") , sprintf("%02d", Biol$month), Biol$year, sep = "/")                  ### get a date that's accepted by climwin


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
  Biol <- Biol[which(!(is.na(Biol$pflowerT))),]
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

#### Set Range ----------------------------------------------------------------------------------------------------------------------------

if(vitalrate == "s") {
  print("Range set to 3 years")
  range <- c(24,-12)
}


if(vitalrate == "g") {
    print("Range set to 3 years")
    range <- c(24,-12)
}

if(vitalrate == "fp") {
    print("Range set to 3 years")
    range <- c(36, 0)
}

if(vitalrate == "fn") {
    print("Range set to 3 years")
    range <- c(36, 0)
}


#### Run function ----------------------------------------------------------------------------------------------------------------------------

result <- slidingwin(baseline = model,
                     xvar = list(Temp = Clim$tmean_scaled,
                                 Ppt = Clim$ppt_scaled),
                     type = "absolute",
                     range = range,
                     stat = options$stat[taskID], 
                     upper = NA,
                     lower = NA,
                     func = options$func[taskID],
                     refday = c(as.integer(format(min(as.Date(Biol$date, format = "%d/%m/%Y")), format = "%d")), 
                                as.integer(format(min(as.Date(Biol$date, format = "%d/%m/%Y")), format = "%m"))),                                                          
                     cinterval = "month",
                     cdate = as.character(Clim$date), bdate = as.character(Biol$date),
                     cmissing = "method1", 
                     spatial = list(as.factor(Biol$population), as.factor(Clim$population))
)


### Save object. Will be merged with all other combinations in another script -----------------------

saveRDS(result, file = output)

Sys.time() - start

