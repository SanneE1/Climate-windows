suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(dplyr))


#  ----------------------------------------------------------------------------------------------------------------------------
# parsing arguments
#  ----------------------------------------------------------------------------------------------------------------------------

parser <- OptionParser(
  usage       = "Rscript %prog [options]",
  description = "\nan Run slidingwindow analysis",
  epilogue    = ""
)

cli <- parse_args(parser, positional_arguments = 1)

#  ----------------------------------------------------------------------------------------------------------------------------
# Assign shortcuts
#  ----------------------------------------------------------------------------------------------------------------------------

output_dir <- cli$args[1]


## get all RDS from one job in one list  ----------------------------------------------------------------------------------------------------------

files <- list.files(output_dir, pattern = ".rds$", full.names = T)
r <- lapply(files, readRDS)


## Create the correct result format ---------------------------------------------------------------------------------------------------------------

randoms <- bind_rows(lapply(r, '[[', 1))
combos <- bind_rows(lapply(r, '[[', 2), .id="column_label")

results <- list(randoms, combos)
##Save files in the directory created by the Climwin function -------------------------------------------------------------------------------------

getinfo <- stringr::str_split(files[1],"[[:punct:]]")
species <- getinfo[[1]][8]
cdata <- getinfo[[1]][9]
vitalrate <- getinfo[[1]][10]

getinfo

saveRDS(results, paste(output_dir, paste(species, vitalrate, cdata, "random.rds", sep = "_")))

