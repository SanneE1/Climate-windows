### R code to run climwin's randomization analysis on UFZ's EVE

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

#### print them for .log files
species
vitalrate
Climate
SpeciesInput
Results_sliding
w

# Run 1 randomization --------------------------------------------------------------------------

random <- random_r(species, vitalrate, Climate, SpeciesInput, Results_sliding, w)


### Save object. Will be merged with other randomization in other script  --------------------------------------------------------------------------

saveRDS(random, file = output)

