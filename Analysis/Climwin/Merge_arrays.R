
library(dplyr)

## Set directory at the output path of the first job ----------------------------------------------------------------------------------------------

output_dir <- commandArgs(trailingOnly = T)[1]

## get all RDS from one job in one list  ----------------------------------------------------------------------------------------------------------
## To make it generic, figure out how to use output destination, jobID etc.

files <- list.files(output_dir, pattern = "^Surv_Climwin.*\\.rds$", full.names = T)
r <- lapply(files, readRDS)


## Create the correct result format ---------------------------------------------------------------------------------------------------------------


results <- lapply(r, '[[', 1)
results$combos <- bind_rows(lapply(r, '[[', 2), .id="column_label")


##Save files in the directory created by the Climwin function -------------------------------------------------------------------------------------

saveRDS(results, file.path(output_dir, "result.rds"))
write.csv(results$combos, file.path(output_dir, "summary_combos.csv") )

