library(dplyr)
library(climwin)
library(gridExtra)
library(grid)
library(ggplot2)

### Import climwin results ----------------------------------------------------------------------------------------

### HEQU
Hs <- readRDS("Results/Climwin/20191030/HEQU_s_month_result.rds")
Hg <- readRDS("Results/Climwin/20191030/HEQU_g_month_result.rds")
Hfp <- readRDS("Results/Climwin/20191030/HEQU_fp_month_result.rds")
Hfn <- readRDS("Results/Climwin/20191030/HEQU_fn_month_result.rds")

Hsr <- readRDS("Results/Climwin/20191030/HEQU_s_month_random.rds")
Hgr <- readRDS("Results/Climwin/20191030/HEQU_g_month_random.rds")
Hfpr <- readRDS("Results/Climwin/20191030/HEQU_fp_month_random.rds")
Hfnr <- readRDS("Results/Climwin/20191030/HEQU_fn_month_random.rds")


### CRFL
Cs <- readRDS("Results/Climwin/20191030/CRFL_s_month_result.rds")
Cg <- readRDS("Results/Climwin/20191030/CRFL_g_month_result.rds")
Cfp <- readRDS("Results/Climwin/20191030/CRFL_fp_month_result.rds")
Cfn <- readRDS("Results/Climwin/20191030/CRFL_fn_month_result.rds")

Csr <- readRDS("Results/Climwin/20191030/CRFL_s_month_random.rds")
Cgr <- readRDS("Results/Climwin/20191030/CRFL_g_month_random.rds")
Cfpr4 <- readRDS("Results/Climwin/20191030/CRFL_fp_month_random4.rds")
Cfpr10 <- readRDS("Results/Climwin/20191030/CRFL_fp_month_random10.rds")
Cfnr <- readRDS("Results/Climwin/20191030/CRFL_fn_month_random.rds")

### OPIM
Os <- readRDS("Results/Climwin/20191030/OPIM_s_month_result.rds")
Og <- readRDS("Results/Climwin/20191030/OPIM_g_month_result.rds")
Ofp <- readRDS("Results/Climwin/20191030/OPIM_fp_month_result.rds")
Ofn <- readRDS("Results/Climwin/20191030/OPIM_fn_month_result.rds")

Osr <- readRDS("Results/Climwin/20191030/OPIM_s_month_random.rds")
Ogr <- readRDS("Results/Climwin/20191030/OPIM_g_month_random.rds")
Ofpr <- readRDS("Results/Climwin/20191030/OPIM_fp_month_random.rds")
Ofnr <- readRDS("Results/Climwin/20191030/OPIM_fn_month_random.rds")

### ARTR
As <- readRDS("Results/Climwin/20191030/ARTR_s_month_result.rds")
Ag <- readRDS("Results/Climwin/20191030/ARTR_g_month_result.rds")


Asr <- readRDS("Results/Climwin/20191030/ARTR_s_month_random.rds")
Agr <- readRDS("Results/Climwin/20191030/ARTR_g_month_random.rds")

### FRSP
Fs <- readRDS("Results/Climwin/20191107/FRSP_s_month_result.rds")
Fg <- readRDS("Results/Climwin/20191107/FRSP_g_month_result.rds")
Ffp <- readRDS("Results/Climwin/20191107/FRSP_fp_month_result.rds")

Fsr <- readRDS("Results/Climwin/20191107/FRSP_s_month_random.rds")
Fgr <- readRDS("Results/Climwin/20191107/FRSP_g_month_random.rds")
Ffpr <- readRDS("Results/Climwin/20191107/FRSP_fp_month_random.rds")




### HEQU plots ---------------------------------------------------------------------------------------
# plotall(datasetrand = Hsr[[1]],
#         dataset = Hs[[11]]$Dataset, 
#         bestmodel = Hs[[11]]$BestModel,
#         bestmodeldata = Hs[[11]]$BestModelData,
#         arrow = T,
#         title = Hs$combos$climate[11])
# 
# plotall(datasetrand = Hgr[[1]],
#         dataset = Hg[[2]]$Dataset, 
#         bestmodel = Hg[[2]]$BestModel,
#         bestmodeldata = Hg[[2]]$BestModelData,
#         arrow = T,
#         title = Hs$combos$climate[2])
# 
# plotall(datasetrand = Hfpr[[1]],
#         dataset = Hfp[[6]]$Dataset, 
#         bestmodel = Hfp[[6]]$BestModel,
#         bestmodeldata = Hfp[[6]]$BestModelData,
#         arrow = T,
#         title = Hfp$combos$climate[6])
# 
# plotall(datasetrand = Hfnr[[1]],
#         dataset = Hfn[[10]]$Dataset, 
#         bestmodel = Hfn[[10]]$BestModel,
#         bestmodeldata = Hfn[[10]]$BestModelData,
#         arrow = T,
#         title = Hfn$combos$climate[10])
# 
# 
# ### CRFL plots ---------------------------------------------------------------------------------------
# plotall(datasetrand = Csr[[1]],
#         dataset = Cs[[8]]$Dataset, 
#         bestmodel = Cs[[8]]$BestModel,
#         bestmodeldata = Cs[[8]]$BestModelData,
#         arrow = T,
#         title = Cs$combos$climate[8])
# 
# plotall(datasetrand = Cgr[[1]],
#         dataset = Cg[[1]]$Dataset, 
#         bestmodel = Cg[[1]]$BestModel,
#         bestmodeldata = Cg[[1]]$BestModelData,
#         arrow = T,
#         title = Cs$combos$climate[1])
# 
# plotall(datasetrand = Cfpr4[[1]],
#         dataset = Cfp[[4]]$Dataset, 
#         bestmodel = Cfp[[4]]$BestModel,
#         bestmodeldata = Cfp[[4]]$BestModelData,
#         arrow = T,
#         title = Cfp$combos$climate[4])
# 
# plotall(datasetrand = Cfpr10[[1]],
#         dataset = Cfp[[10]]$Dataset, 
#         bestmodel = Cfp[[10]]$BestModel,
#         bestmodeldata = Cfp[[10]]$BestModelData,
#         arrow = T,
#         title = Cfp$combos$climate[10])
# 
# gridExtra::grid.arrange(plotdelta(Cfp[[4]]$Dataset, arrow = T), 
#                         plotweights(Cfp[[4]]$Dataset, arrow = T),
#                         plotdelta(Cfp[[10]]$Dataset, arrow = T),
#                         plotweights(Cfp[[10]]$Dataset, arrow = T),
#                         ncol = 2)
# 
# plotall(datasetrand = Cfnr[[1]],
#         dataset = Cfn[[4]]$Dataset, 
#         bestmodel = Cfn[[4]]$BestModel,
#         bestmodeldata = Cfn[[4]]$BestModelData,
#         arrow = T,
#         title = Cfn$combos$climate[4])
# 
# ### OPIM plots ---------------------------------------------------------------------------------------
# plotall(datasetrand = Osr[[1]],
#         dataset = Os[[3]]$Dataset, 
#         bestmodel = Os[[3]]$BestModel,
#         bestmodeldata = Os[[3]]$BestModelData,
#         arrow = T,
#         title = Os$combos$climate[3])
# 
# plotall(datasetrand = Ogr[[1]],
#         dataset = Og[[10]]$Dataset, 
#         bestmodel = Og[[10]]$BestModel,
#         bestmodeldata = Og[[10]]$BestModelData,
#         arrow = T,
#         title = Os$combos$climate[10])
# 
# plotall(datasetrand = Ofpr[[1]],
#         dataset = Ofp[[3]]$Dataset, 
#         bestmodel = Ofp[[3]]$BestModel,
#         bestmodeldata = Ofp[[3]]$BestModelData,
#         arrow = T,
#         title = Ofp$combos$climate[3])
# 
# plotall(datasetrand = Ofnr[[1]],
#         dataset = Ofn[[3]]$Dataset, 
#         bestmodel = Ofn[[3]]$BestModel,
#         bestmodeldata = Ofn[[3]]$BestModelData,
#         arrow = T,
#         title = Ofn$combos$climate[3])
# 
# ### ARTR plots ---------------------------------------------------------------------------------------
# plotall(datasetrand = Asr[[1]],
#         dataset = As[[9]]$Dataset, 
#         bestmodel = As[[9]]$BestModel,
#         bestmodeldata = As[[9]]$BestModelData,
#         arrow = T,
#         title = As$combos$climate[9])
# 
# plotall(datasetrand = Agr[[1]],
#         dataset = Ag[[8]]$Dataset, 
#         bestmodel = Ag[[8]]$BestModel,
#         bestmodeldata = Ag[[8]]$BestModelData,
#         arrow = T,
#         title = As$combos$climate[8])
# 
# 
# ### FRSP --------------------------------------------------------------------------------------------------------------------
# plotall(datasetrand = Fsr[[1]],
#         dataset = Fs[[10]]$Dataset, 
#         bestmodel = Fs[[10]]$BestModel,
#         bestmodeldata = Fs[[10]]$BestModelData,
#         arrow = T,
#         title = Fs$combos$climate[11])
# 
# plotall(datasetrand = Fgr[[1]],
#         dataset = Fg[[6]]$Dataset, 
#         bestmodel = Fg[[6]]$BestModel,
#         bestmodeldata = Fg[[6]]$BestModelData,
#         arrow = T,
#         title = Fs$combos$climate[2])
# 
# plotall(datasetrand = Ffpr[[1]],
#         dataset = Ffp[[10]]$Dataset, 
#         bestmodel = Ffp[[10]]$BestModel,
#         bestmodeldata = Ffp[[10]]$BestModelData,
#         arrow = T,
#         title = Ffp$combos$climate[10])
# 
# 

#### One summary -------------------------------------------------------------------------------------
Os1 <- plotdelta(Os[[3]]$Dataset, arrow = T) + labs(title = Os$combos$climate[3])
Os2 <- plotwin(Os[[3]]$Dataset)
Os3 <- plothist(dataset = Os[[3]]$Dataset, datasetrand = Osr[[1]])

Og1 <- plotdelta(Og[[10]]$Dataset, arrow = T) + labs(title = Og$combos$climate[10])
Og2 <- plotwin(Og[[10]]$Dataset)
Og3 <- plothist(dataset = Og[[10]]$Dataset, datasetrand = Ogr[[1]])

Ofp1 <- plotdelta(Ofp[[3]]$Dataset, arrow = T) + labs(title = Ofp$combos$climate[3])
Ofp2 <- plotwin(Ofp[[3]]$Dataset)
Ofp3 <- plothist(dataset = Ofp[[3]]$Dataset, datasetrand = Ofpr[[1]])

Ofn1 <- plotdelta(Ofn[[3]]$Dataset, arrow = T) + labs(title = Ofn$combos$climate[3])
Ofn2 <- plotwin(Ofn[[3]]$Dataset)
Ofn3 <- plothist(dataset = Ofn[[3]]$Dataset, datasetrand = Ofnr[[1]])


Cs1 <- plotdelta(Cs[[8]]$Dataset, arrow = T) + labs(title = Cs$combos$climate[8])
Cs2 <- plotwin(Cs[[8]]$Dataset)
Cs3 <- plothist(dataset = Cs[[8]]$Dataset, datasetrand = Csr[[1]])

Cg1 <- plotdelta(Cg[[1]]$Dataset, arrow = T) + labs(title = Cg$combos$climate[1])
Cg2 <- plotwin(Cg[[1]]$Dataset)
Cg3 <- plothist(dataset = Cg[[1]]$Dataset, datasetrand = Cgr[[1]])

Cfp1 <- plotdelta(Cfp[[4]]$Dataset, arrow = T) + labs(title = Cfp$combos$climate[4])
Cfp2 <- plotwin(Cfp[[4]]$Dataset)
Cfp3 <- plothist(dataset = Cfp[[4]]$Dataset, datasetrand = Cfpr4[[1]])

Cfn1 <- plotdelta(Cfn[[4]]$Dataset, arrow = T) + labs(title = Cfn$combos$climate[4])
Cfn2 <- plotwin(Cfn[[4]]$Dataset)
Cfn3 <- plothist(dataset = Cfn[[4]]$Dataset, datasetrand = Cfnr[[1]])


As1 <- plotdelta(As[[9]]$Dataset, arrow = T) + labs(title = As$combos$climate[9])
As2 <- plotwin(As[[9]]$Dataset)
As3 <- plothist(dataset = As[[9]]$Dataset, datasetrand = Asr[[1]])

Ag1 <- plotdelta(Ag[[8]]$Dataset, arrow = T) + labs(title = Ag$combos$climate[8])
Ag2 <- plotwin(Ag[[8]]$Dataset)
Ag3 <- plothist(dataset = Ag[[8]]$Dataset, datasetrand = Agr[[1]])


Hs1 <- plotdelta(Hs[[11]]$Dataset, arrow = T) + labs(title = Hs$combos$climate[11])
Hs2 <- plotwin(Hs[[11]]$Dataset)
Hs3 <- plothist(dataset = Hs[[11]]$Dataset, datasetrand = Hsr[[1]])

Hg1 <- plotdelta(Hg[[2]]$Dataset, arrow = T) + labs(title = Hg$combos$climate[2])
Hg2 <- plotwin(Hg[[2]]$Dataset)
Hg3 <- plothist(dataset = Hg[[2]]$Dataset, datasetrand = Hgr[[1]])

Hfp1 <- plotdelta(Hfp[[6]]$Dataset, arrow = T) + labs(title = Hfp$combos$climate[6])
Hfp2 <- plotwin(Hfp[[6]]$Dataset)
Hfp3 <- plothist(dataset = Hfp[[6]]$Dataset, datasetrand = Hfpr[[1]])

Hfn1 <- plotdelta(Hfn[[10]]$Dataset, arrow = T) + labs(title = Hfn$combos$climate[10])
Hfn2 <- plotwin(Hfn[[10]]$Dataset)
Hfn3 <- plothist(dataset = Hfn[[10]]$Dataset, datasetrand = Hfnr[[1]])


Fs1 <- plotdelta(Fs[[10]]$Dataset, arrow = T) + labs(title = Fs$combos$climate[10])
Fs2 <- plotwin(Fs[[10]]$Dataset)
Fs3 <- plothist(dataset = Fs[[10]]$Dataset, datasetrand = Fsr[[1]])

Fg1 <- plotdelta(Fg[[6]]$Dataset, arrow = T) + labs(title = Fg$combos$climate[6])
Fg2 <- plotwin(Fg[[6]]$Dataset)
Fg3 <- plothist(dataset = Fg[[6]]$Dataset, datasetrand = Fgr[[1]])

Ffp1 <- plotdelta(Ffp[[10]]$Dataset, arrow = T) + labs(title = Ffp$combos$climate[10])
Ffp2 <- plotwin(Ffp[[10]]$Dataset)
Ffp3 <- plothist(dataset = Ffp[[10]]$Dataset, datasetrand = Ffpr[[1]])


lay <- rbind(c( 1,  1,  23,  2, 2),
             c( 3,  4,  5,  6, 7),
             c( 8,  9, 10, 11, 12),
             c( 8,  9, 10, 11, 12),
             c( 8,  9, 10, 11, 12),
             c( 8,  9, 10, 11, 12),
             c(13, 14, 15, 16, 17),
             c(13, 14, 15, 16, 17),
             c(13, 14, 15, 16, 17),
             c(13, 14, 15, 16, 17),
             c(18, 19, 20, 21, 22),
             c(18, 19, 20, 21, 22),
             c(18, 19, 20, 21, 22),
             c(18, 19, 20, 21, 22))

HEQU <- textGrob("HEQU", gp = gpar(col="blue4", fontsize = 15))
OPIM <- textGrob("OPIM", gp = gpar(col="red4", fontsize = 15))
CRFL <- textGrob("CRFL", gp = gpar(col="red4", fontsize = 15))
ARTR <- textGrob("ARTR", gp = gpar(col="red4", fontsize = 15))
FRSP <- textGrob("FRSP", gp = gpar(col="blue4", fontsize = 15))
Alpine <- textGrob("Alpine", gp = gpar(col="blue4", fontsize = 15))
Arid <- textGrob("Arid", gp = gpar(col="red4", fontsize = 15))

allsurv <- grid.arrange(Arid, Alpine,
             OPIM, CRFL, ARTR, HEQU, FRSP,
             Os1,  Cs1,  As1,  Hs1,  Fs1,
             Os2,  Cs2,  As2,  Hs2,  Fs2,
             Os3,  Cs3,  As3,  Hs3,  Fs3,
             textGrob("Survival", gp = gpar(fontface = "bold", fontsize = 15)),
             layout_matrix = lay)

saveRDS(allsurv, "Results/Quick_summary/Allsurv.RDS")

allgrowth <- grid.arrange(Arid, Alpine,
                OPIM, CRFL, ARTR, HEQU, FRSP,
                Og1,  Cg1,  Ag1,  Hg1,  Fg1,
                Og2,  Cg2,  Ag2,  Hg2,  Fg2,
                Og3,  Cg3,  Ag3,  Hg3,  Fg3,
                textGrob("Growth", gp = gpar(fontface = "bold", fontsize = 15)),
                layout_matrix = lay)

saveRDS(allgrowth, "Results/Quick_summary/Allgrowth.RDS")

layfp <- lay
layfp[c(3:14), 3] <- NA

allFp <- grid.arrange(Arid, Alpine,
             OPIM, CRFL, ARTR, HEQU, FRSP,
             Ofp1,  Cfp1,  Hfp1,  Ffp1,
             Ofp2,  Cfp2,  Hfp2,  Ffp2,
             Ofp3,  Cfp3,  Hfp3,  Ffp3,
             textGrob("Flower Propability", gp = gpar(fontface = "bold", fontsize = 15)),
             layout_matrix = layfp)

saveRDS(allFp, "Results/Quick_summary/AllFp.RDS")

layfn <- layfp
layfn[c(3:14), 5] <- NA

allFn <- grid.arrange(Arid, Alpine,
             OPIM, CRFL, ARTR, HEQU, FRSP,
             Ofp1,  Cfp1,  Hfp1,
             Ofp2,  Cfp2,  Hfp2,
             Ofp3,  Cfp3,  Hfp3,
             textGrob("Flower Numbers", gp = gpar(fontface = "bold", fontsize = 15)),
             layout_matrix = layfn)

saveRDS(allFn, "Results/Quick_summary/AllFn.RDS")

a <- list(allsurv, allgrowth, allFp, allFn)

all <- marrangeGrob(a, nrow = 1, ncol = 1)

ggsave("Results/Quick_summary/all.pdf", all, width=18, height=14)
