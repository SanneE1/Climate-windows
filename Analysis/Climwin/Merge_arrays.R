
library(dplyr)

## get all RDS from one job in one list  --------------------------------------------------------------------------------------------------
## To make it generic, figure out how to use output destination, jobID etc.

files <- list.files("Results/Climwin/Slidingwin/", pattern = "^Surv_Climwin_HEQU_month1_5528243_.*\\.rds$", full.names = T)
r <- lapply(files, readRDS)


## Create the correct result format --------------------------------------------------------------------------------------------------------


results <- lapply(r, '[[', 1)
results$combos <- bind_rows(lapply(r, '[[', 2), .id="column_label")


## See if this can then also be more generic - this would then be named same as input, but withour taksID
saveRDS(results, "Results/Climwin/Slidingwin/Surv_Climwin_HEQU_month1_5528243.rds")

