suppressPackageStartupMessages(library(climwin))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(lubridate))

#### Choose which is the 'best' combo

w <- 28             ## this should go into qsub commandline
cdata <- "month"    ## if EVE: would be ideal if this can be retrieved from the results input file

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
  usage       = "Rscript %prog [options] Climate SpeciesInput Results_sliding output",
  option_list = Parsoptions,
  description = "\nan Run slidingwindow analysis",
  epilogue    = ""
)

cli <- parse_args(parser, positional_arguments = 4)

### Assign shortcuts ------------------------------------------------------------------------------------------

Climate   <- cli$args[1]
SpeciesInput  <- cli$args[2]
Results_sliding <- cli$args[3]
output <- cli$args[4]
taskID <- as.integer(Sys.getenv("SGE_TASK_ID"))

## Get Data -----------------------------------------------------------------------------------------------------------
Clim <- read.csv(Climate) 
Clim$date <- paste("15/",sprintf("%02d", Clim$Month), "/", Clim$Year, sep = "")      


Biol <- read.csv(SpeciesInput) %>%
  mutate(sizeT = as.numeric(as.character(sizeT)),
         sizeT1 = as.numeric(as.character(sizeT1)))

Biol$date <- paste(ifelse(!(is.na(Biol$day)), sprintf("%02d", Biol$day), "01") , sprintf("%02d", Biol$month), Biol$year, sep = "/")                  ### get a date that's accepted by climwin
Biol <- Biol[which(Biol$seedling != 1),]                           ### Select Adults
Biol <- Biol[which(!is.na(Biol$survival)),]                       
Biol <- Biol[which(!is.na(Biol$sizeT)),]                           

results <- readRDS(Results_sliding)


## Randomized run for selected combi
x <- list(Clim[[results$combos$climate[w]]]) 
names(x) <- results$combos$climate[w]

random <- randwin(repeats = 1,
                  baseline = glmer(formula = survival ~ sizeT + population + (1|year),
                                   data = Biol, 
                                   family = binomial),
                  xvar = list(Clim[[as.character(results$combos$climate[w])]]),
                  type = as.character(results$combos$type[w]),
                  range = c(12, 0),
                  stat = c(as.character(results$combos$stat[w])),
                  func = c(as.character(results$combos$func[w])),
                  refday =  c(30,6),                                       ## c(day(min(as.Date(Biol$date, format = "%d/%m/%Y"))), month(min(as.Date(Biol$date, format = "%d/%m/%Y"))))
                  cinterval = cdata,
                  cdate = Clim$date, bdate = Biol$date,
                  spatial = list(Biol$population, Clim$population),
                  window = "sliding"
)




