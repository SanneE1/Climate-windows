
library(dplyr)

output_dir <- commandArgs(trailingOnly = T)[1]
## get all RDS from one job in one list  --------------------------------------------------------------------------------------------------
## To make it generic, figure out how to use output destination, jobID etc.

files <- list.files(output_dir, pattern = "^Surv_Climwin.*\\.rds$", full.names = T)
r <- lapply(files, readRDS)


## Create the correct result format --------------------------------------------------------------------------------------------------------


results <- lapply(r, '[[', 1)
results$combos <- bind_rows(lapply(r, '[[', 2), .id="column_label")


## See if this can then also be more generic - this would then be named same as input, but withour taksID
saveRDS(results, file.path(output_dir, "result.rds"))

