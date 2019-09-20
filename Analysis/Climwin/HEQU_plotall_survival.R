library(climwin)

result <- readRDS("Results/Climwin/Slidingwin/20190920/Surv_Climwin_HEQU__5569371_4.rds")
random <- readRDS("Results/Climwin/Randomwin/20190920/HEQU_month_random_result.rds")


plotall(datasetrand = random[[1]],
        dataset = result[[1]]$Dataset,
        bestmodel = result[[1]]$BestModel,
        bestmodeldata = result[[1]]$BestModelData,
        title = result$combos[1,1])


