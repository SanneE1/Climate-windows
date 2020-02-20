## R code that works with the matching submission script to run the sliding window analysis on 
## individual based datasets as an array job. This code is very sensitive to correct 
## dataformats/column names. Please read the README.md file of this project to see the correct 
## file name and data formats. 

suppressPackageStartupMessages(library(climwin))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(optparse))

source("Analysis/Climwin/Slidingwindow/Sliding_function.R")

start <- Sys.time()
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
  usage       = "Rscript %prog [options] vitalrate Climate SpeciesInput output",
  option_list = Parsoptions,
  description = "\nan Run slidingwindow analysis",
  epilogue    = ""
)

cli <- parse_args(parser, positional_arguments = 4)

#  ----------------------------------------------------------------------------------------------------------------------------
# Assign shortcuts
#  ----------------------------------------------------------------------------------------------------------------------------

species <- cli$options$species_used
vitalrate <- cli$args[1]
Climate   <- cli$args[2]
SpeciesInput  <- cli$args[3]
output <- cli$args[4]
taskID <- as.integer(Sys.getenv("SGE_TASK_ID"))

species


### Climate signal combies - 16 combinations ----------------------------------------------------------------------------------------------------------------------------

  xvar <- c("mean_prcp_scaled", "mean_tobs_scaled", "mean_tmax_scaled", "mean_tmin_scaled", "mean_tavg_scaled", "SPEI")
  type <- c("absolute")
  stat <- c("mean")
  func <- c("lin", "quad")
  upper <- NA            
  lower <- NA            

options <- expand.grid(xvar = xvar, type = type, stat = stat, func = func, upper = upper, lower = lower, stringsAsFactors = F)

## add minimum and maximum observed temperature values

options <- rbind(options, c("min_tmin_scaled", "absolute", "min", "lin", 0, NA), c("min_tmin_scaled", "absolute", "min", "quad", 0, NA),
                          c("max_tmax_scaled", "absolute", "max", "lin", 0, NA), c("max_tmax_scaled", "absolute", "max", "quad", 0, NA)
                  )

print(options[taskID,])

combi <- options[taskID,]

# Run the actual analysis --------------------------------------------------------------------------

sliding_r(species, vitalrate, Climate, SpeciesInput, combi)

### Save object. Will be merged with all other combinations in another script -----------------------

saveRDS(result, file = output)

Sys.time() - start
