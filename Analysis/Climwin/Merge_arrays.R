library(climwin)
library(dplyr)
library(lme4)


## get all RDS from one job in one list  --------------------------------------------------------------------------------------------------

files <- list.files("Results/Climwin/Slidingwin/", pattern = "^Surv_Climwin_HEQU_month1_5528243_.*\\.rds$", full.names = T)
r <- lapply(files, readRDS)


## Create the correct result format --------------------------------------------------------------------------------------------------------


results <- lapply(r, '[[', 1)
results$combos <- bind_rows(lapply(r, '[[', 2), .id="column_label")

saveRDS(results, "Results/Climwin/Slidingwin/Surv_Climwin_HEQU_month1_5528243.rds")

