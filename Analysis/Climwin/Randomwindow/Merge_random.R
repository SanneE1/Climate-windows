suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(dplyr))


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
    metavar = "HEQU|CRFL|OPIM|FRSP|HYGR")      # ,
  
  
)

parser <- OptionParser(
  usage       = "Rscript %prog [options] output_dir",
  option_list = Parsoptions,
  description = "\nan Run slidingwindow analysis",
  epilogue    = ""
)

cli <- parse_args(parser, positional_arguments = 1)

#  ----------------------------------------------------------------------------------------------------------------------------
# Assign shortcuts
#  ----------------------------------------------------------------------------------------------------------------------------

cdata <- cli$options$climate_data_format    
species <- cli$options$species_used
output_dir <- cli$args[1]

## get all RDS from one job in one list  ----------------------------------------------------------------------------------------------------------

files <- list.files(output_dir, pattern = ".rds$", full.names = T)
r <- lapply(files, readRDS)


## Create the correct result format ---------------------------------------------------------------------------------------------------------------

randoms <- bind_rows(lapply(r, '[[', 1))
combos <- bind_rows(lapply(r, '[[', 2), .id="column_label")

results <- list(randoms, combos)
##Save files in the directory created by the Climwin function -------------------------------------------------------------------------------------

saveRDS(results, file.path(output_dir, paste(species, cdata, "random_result.rds", sep = "_")))
write.csv(results$combos, file.path(output_dir, paste(species, cdata, "random_summary_combos.csv", sep = "_") ))
