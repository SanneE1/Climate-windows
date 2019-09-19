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

results <- lapply(r, '[[', 1)
results$combos <- bind_rows(lapply(r, '[[', 2), .id="column_label")


##Save files in the directory created by the Climwin function -------------------------------------------------------------------------------------
comboname <-  paste(species, cdata, "summary_combos.csv", sep = "_") 
resultname <- paste(species, cdata, "result.rds", sep = "_")

saveRDS(results, paste(output_dir,resultname))
write.csv(results$combos, paste(output_dir,comboname))


