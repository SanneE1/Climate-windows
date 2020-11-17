## run the randomization analysis of selected climate driver

suppressPackageStartupMessages(library(climwin))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(lubridate))


#  ----------------------------------------------------------------------------------------------------------------------------
# parsing arguments
#  ----------------------------------------------------------------------------------------------------------------------------

parser <- OptionParser(
  usage       = "Rscript %prog [options] Climate SpeciesInput Results_sliding winner output",
  description = "\nan Run randomwindow analysis",
  epilogue    = ""
)

cli <- parse_args(parser, positional_arguments = 5)

### Assign shortcuts ------------------------------------------------------------------------------------------

Climate   <- cli$args[1]
SpeciesInput  <- cli$args[2]
Results_sliding <- cli$args[3]
w <- as.integer(cli$args[4])
output <- cli$args[5]
taskID <- as.integer(Sys.getenv("SGE_TASK_ID"))


## Get which vital rate was analysed ------------------------------------------------------------------------------
getinfo <- stringr::str_split(Results_sliding, "[[:punct:]]")
vitalrate <- getinfo[[1]][7]


vitalrate
Climate
SpeciesInput
Results_sliding


##----------------------------------------------------------------------------------------------------------------------------------
## Prepare Climate data 
##----------------------------------------------------------------------------------------------------------------------------------

Clim <- read.csv(Climate)                                                ### get a date that's accepted by climwin
Clim$date <- paste("15/",sprintf("%02d", Clim$Month), "/", Clim$Year, sep = "")          


##----------------------------------------------------------------------------------------------------------------------------------
## Prepare Biological data 
##----------------------------------------------------------------------------------------------------------------------------------

### Species specific data modification

Biol <- read.csv(SpeciesInput) %>%
  mutate(sizeT = as.integer(sizeT),
         sizeT1 = as.integer(sizeT1))
Biol <- Biol[which(Biol$seedling != 1),] # filter out seedlings - seperate stage class in previous lit.                          
Biol <- Biol[which(Biol$year!= 2012),] # filter out transition 2012-2013 due to gophers

Biol <- Biol[which(!(is.na(Biol$sizeT) | Biol$sizeT == 0)),] # filter out individuals without size in time t
Biol$lnsizeT <- log(Biol$sizeT) # log transform size at time t

Biol$date <- paste(ifelse(!(is.na(Biol$day)), sprintf("%02d", Biol$day), "01") , sprintf("%02d", Biol$month), Biol$year, sep = "/")                  ### get a date that's accepted by climwin

##----------------------------------------------------------------------------------------------------------------------------------
## Specify species and vital rate specific baseline 
##----------------------------------------------------------------------------------------------------------------------------------

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

##----------------------------------------------------------------------------------------------------------------------------------
## Get Sliding result 
##----------------------------------------------------------------------------------------------------------------------------------

results <- readRDS(Results_sliding)
print(results$combos[w,])

##----------------------------------------------------------------------------------------------------------------------------------
## Randomized run for selected combination
##----------------------------------------------------------------------------------------------------------------------------------

x <- list(Clim[[as.character(results$combos$climate[w])]]) 
names(x) <- results$combos$climate[w]

str(x)

random <- randwin(repeats = 1,
                  baseline =  model   ,
                  xvar = list(Clim[[as.character(results$combos$climate[w])]]),
                  type = "absolute",
                  range = range,
                  stat = c("mean"),
                  func = c("lin"),
                  refday = c(as.integer(format(min(as.Date(Biol$date, format = "%d/%m/%Y")), format = "%d")), as.integer(format(min(as.Date(Biol$date, format = "%d/%m/%Y")), format = "%m"))),                                                          
                  cinterval = "month",
                  cdate = Clim$date, bdate = Biol$date,
                  window = "sliding",
                  cmissing = "method1",
                  spatial = list(as.factor(Biol$population), as.factor(Clim$population))
                  )



saveRDS(random, file = output)

