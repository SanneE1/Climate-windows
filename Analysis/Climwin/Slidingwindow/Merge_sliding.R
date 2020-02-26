
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(optparse))

#  ----------------------------------------------------------------------------------------------------------------------------
# parsing arguments
#  ----------------------------------------------------------------------------------------------------------------------------

parser <- OptionParser(
  usage       = "Rscript %prog [options] output_dir",
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

results <- lapply(r, '[[', 1)
results$combos <- bind_rows(lapply(r, '[[', 2), .id="column_label")

##Save files in the directory created by the Climwin function -------------------------------------------------------------------------------------
print("get info")
getinfo <- stringr::str_split(files[1], "[[:punct:]]")
species <- getinfo[[1]][8]
cdata <- getinfo[[1]][10]
vitalrate <- getinfo[[1]][9]
getinfo

saveRDS(results, file.path(output_dir, paste(species, vitalrate, cdata, "result.rds", sep = "_")))

