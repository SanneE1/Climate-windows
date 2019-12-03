library(dplyr)
library(climwin)
library(gridExtra)
library(grid)
library(ggplot2)

### Import climwin results ----------------------------------------------------------------------------------------

### HEQU
Hs <- readRDS("Results/Climwin/HEQU_s_month_result.rds")
Hg <- readRDS("Results/Climwin/HEQU_g_month_result.rds")
Hfp <- readRDS("Results/Climwin/HEQU_fp_month_result.rds")
# Hfn <- readRDS("Results/Climwin/HEQU_fn_month_result.rds")

Hsr <- readRDS("Results/Climwin/HEQU_s_month_random.rds")
Hgr <- readRDS("Results/Climwin/HEQU_g_month_random.rds")
Hfpr <- readRDS("Results/Climwin/HEQU_fp_month_random.rds")
# Hfnr <- readRDS("Results/Climwin/20191030/HEQU_fn_month_random.rds")


### CRFL
Cs <- readRDS("Results/Climwin/CRFL_s_month_result.rds")
Cg <- readRDS("Results/Climwin/CRFL_g_month_result.rds")
Cfp <- readRDS("Results/Climwin/CRFL_fp_month_result.rds")
# Cfn <- readRDS("Results/Climwin/CRFL_fn_month_result.rds")

Csr <- readRDS("Results/Climwin/CRFL_s_month_random.rds")
Cgr <- readRDS("Results/Climwin/CRFL_g_month_random.rds")
Cfpr4 <- readRDS("Results/Climwin/CRFL_fp_month_random4.rds")
# Cfpr10 <- readRDS("Results/Climwin/CRFL_fp_month_random10.rds")
# Cfnr <- readRDS("Results/Climwin/CRFL_fn_month_random.rds")

### OPIM
Os <- readRDS("Results/Climwin/OPIM_s_month_result.rds")
Og <- readRDS("Results/Climwin/OPIM_g_month_result.rds")
Ofp <- readRDS("Results/Climwin/OPIM_fp_month_result.rds")
# Ofn <- readRDS("Results/Climwin/OPIM_fn_month_result.rds")

Osr <- readRDS("Results/Climwin/OPIM_s_month_random.rds")
Ogr <- readRDS("Results/Climwin/OPIM_g_month_random.rds")
Ofpr <- readRDS("Results/Climwin/OPIM_fp_month_random.rds")
# Ofnr <- readRDS("Results/Climwin/OPIM_fn_month_random.rds")

### ARTR
# As <- readRDS("Results/Climwin/ARTR_s_month_result.rds")
# Ag <- readRDS("Results/Climwin/ARTR_g_month_result.rds")


# Asr <- readRDS("Results/Climwin/ARTR_s_month_random.rds")
# Agr <- readRDS("Results/Climwin/ARTR_g_month_random.rds")

### FRSP
Fs <- readRDS("Results/Climwin/FRSP_s_month_result.rds")
Fg <- readRDS("Results/Climwin/FRSP_g_month_result_5yrs.rds")
Ffp <- readRDS("Results/Climwin/FRSP_fp_month_result.rds")

Fsr <- readRDS("Results/Climwin/FRSP_s_month_random.rds")
Fgr <- readRDS("Results/Climwin/FRSP_g_month_random_5yrs.rds")
Ffpr <- readRDS("Results/Climwin/FRSP_fp_month_random.rds")

### Set winners ---------------------------------------------------------------------------------------------------------------------------------------------
Hsurv <- 13
Hgrowth <- 2
HpFlwr <- 16

Fsurv <- 12
Fgrowth <- 10
FpFlwr <- 14

Csurv <- 16
Cgrowth <- 1
CpFlwr <- 4

Osurv <- 3
Ogrowth <- 12
OpFlwr <- 2


### HEQU plots ---------------------------------------------------------------------------------------
a <- plotall(datasetrand = Hsr[[1]],
        dataset = Hs[[Hsurv]]$Dataset,
        bestmodel = Hs[[Hsurv]]$BestModel,
        bestmodeldata = Hs[[Hsurv]]$BestModelData,
        arrow = T,
        title = Hs$combos$climate[Hsurv])
ggsave("Results/Quick_summary/HEQU_Surv_plotall.png", a)

b <- plotall(datasetrand = Hgr[[1]],
        dataset = Hg[[Hgrowth]]$Dataset,
        bestmodel = Hg[[Hgrowth]]$BestModel,
        bestmodeldata = Hg[[Hgrowth]]$BestModelData,
        arrow = T,
        title = Hs$combos$climate[Hgrowth])
ggsave("Results/Quick_summary/HEQU_Growth_plotall.png", b)

c <- plotall(datasetrand = Hfpr[[1]],
        dataset = Hfp[[HpFlwr]]$Dataset,
        bestmodel = Hfp[[HpFlwr]]$BestModel,
        bestmodeldata = Hfp[[HpFlwr]]$BestModelData,
        arrow = T,
        title = Hfp$combos$climate[HpFlwr])
ggsave( "Results/Quick_summary/HEQU_pFl_plotall.png", c)

# ### CRFL plots ---------------------------------------------------------------------------------------
d <- plotall(datasetrand = Csr[[1]],
        dataset = Cs[[Csurv]]$Dataset,
        bestmodel = Cs[[Csurv]]$BestModel,
        bestmodeldata = Cs[[Csurv]]$BestModelData,
        arrow = T,
        title = Cs$combos$climate[Csurv])
ggsave("Results/Quick_summary/CRFL_Surv_plotall.png", d)

e <- plotall(datasetrand = Cgr[[1]],
        dataset = Cg[[Cgrowth]]$Dataset,
        bestmodel = Cg[[Cgrowth]]$BestModel,
        bestmodeldata = Cg[[Cgrowth]]$BestModelData,
        arrow = T,
        title = Cs$combos$climate[Cgrowth])
ggsave("Results/Quick_summary/CRFL_Growth_plotall.png", e)

f <- plotall(datasetrand = Cfpr4[[1]],
        dataset = Cfp[[CpFlwr]]$Dataset,
        bestmodel = Cfp[[CpFlwr]]$BestModel,
        bestmodeldata = Cfp[[CpFlwr]]$BestModelData,
        arrow = T,
        title = Cfp$combos$climate[CpFlwr])
ggsave( "Results/Quick_summary/CRFL_pFl_plotall.png", f)

# ### OPIM plots ---------------------------------------------------------------------------------------
g <- plotall(datasetrand = Osr[[1]],
        dataset = Os[[Osurv]]$Dataset,
        bestmodel = Os[[Osurv]]$BestModel,
        bestmodeldata = Os[[Osurv]]$BestModelData,
        arrow = T,
        title = Os$combos$climate[Osurv])
ggsave("Results/Quick_summary/OPIM_Surv_plotall.png", g)

h <- plotall(datasetrand = Ogr[[1]],
        dataset = Og[[Ogrowth]]$Dataset,
        bestmodel = Og[[Ogrowth]]$BestModel,
        bestmodeldata = Og[[Ogrowth]]$BestModelData,
        arrow = T,
        title = Os$combos$climate[Ogrowth])
ggsave( "Results/Quick_summary/OPIM_Growth_plotall.png", h)

i <- plotall(datasetrand = Ofpr[[1]],
        dataset = Ofp[[OpFlwr]]$Dataset,
        bestmodel = Ofp[[OpFlwr]]$BestModel,
        bestmodeldata = Ofp[[OpFlwr]]$BestModelData,
        arrow = T,
        title = Ofp$combos$climate[OpFlwr])
ggsave( "Results/Quick_summary/OPIM_pFl_plotall.png", i)


# ### FRSP --------------------------------------------------------------------------------------------------------------------
j <- plotall(datasetrand = Fsr[[1]],
        dataset = Fs[[Fsurv]]$Dataset,
        bestmodel = Fs[[Fsurv]]$BestModel,
        bestmodeldata = Fs[[Fsurv]]$BestModelData,
        arrow = T,
        title = Fs$combos$climate[Fsurv])
ggsave( "Results/Quick_summary/FRSP_Surv_plotall.png", j)

k <- plotall(datasetrand = Fgr[[1]],
        dataset = Fg[[Fgrowth]]$Dataset,
        bestmodel = Fg[[Fgrowth]]$BestModel,
        bestmodeldata = Fg[[Fgrowth]]$BestModelData,
        arrow = T,
        title = Fs$combos$climate[Fgrowth])
ggsave("Results/Quick_summary/FRSP_Growth_plotall.png", k)

l <- plotall(datasetrand = Ffpr[[1]],
        dataset = Ffp[[FpFlwr]]$Dataset,
        bestmodel = Ffp[[FpFlwr]]$BestModel,
        bestmodeldata = Ffp[[FpFlwr]]$BestModelData,
        arrow = T,
        title = Ffp$combos$climate[FpFlwr])
ggsave( "Results/Quick_summary/FRSP_pFl_plotall.png", l)



#### One summary -------------------------------------------------------------------------------------
Hs1 <- plotdelta(Hs[[Hsurv]]$Dataset, arrow = T) + labs(title = Hs$combos$climate[Hsurv])
Hs2 <- plotwin(Hs[[Hsurv]]$Dataset)
Hs3 <- plothist(dataset = Hs[[Hsurv]]$Dataset, datasetrand = Hsr[[1]])

Hg1 <- plotdelta(Hg[[Hgrowth]]$Dataset, arrow = T) + labs(title = Hg$combos$climate[Hgrowth])
Hg2 <- plotwin(Hg[[Hgrowth]]$Dataset)
Hg3 <- plothist(dataset = Hg[[Hgrowth]]$Dataset, datasetrand = Hgr[[1]])

Hfp1 <- plotdelta(Hfp[[HpFlwr]]$Dataset, arrow = T) + labs(title = Hfp$combos$climate[HpFlwr])
Hfp2 <- plotwin(Hfp[[HpFlwr]]$Dataset)
Hfp3 <- plothist(dataset = Hfp[[HpFlwr]]$Dataset, datasetrand = Hfpr[[1]])

Fs1 <- plotdelta(Fs[[Fsurv]]$Dataset, arrow = T) + labs(title = Fs$combos$climate[Fsurv])
Fs2 <- plotwin(Fs[[Fsurv]]$Dataset)
Fs3 <- plothist(dataset = Fs[[Fsurv]]$Dataset, datasetrand = Fsr[[1]])

Fg1 <- plotdelta(Fg[[Fgrowth]]$Dataset, arrow = T) + labs(title = Fg$combos$climate[Fgrowth])
Fg2 <- plotwin(Fg[[Fgrowth]]$Dataset)
Fg3 <- plothist(dataset = Fg[[Fgrowth]]$Dataset, datasetrand = Fgr[[1]])

Ffp1 <- plotdelta(Ffp[[FpFlwr]]$Dataset, arrow = T) + labs(title = Ffp$combos$climate[FpFlwr])
Ffp2 <- plotwin(Ffp[[FpFlwr]]$Dataset)
Ffp3 <- plothist(dataset = Ffp[[FpFlwr]]$Dataset, datasetrand = Ffpr[[1]])


Os1 <- plotdelta(Os[[Osurv]]$Dataset, arrow = T) + labs(title = Os$combos$climate[Osurv])
Os2 <- plotwin(Os[[Osurv]]$Dataset)
Os3 <- plothist(dataset = Os[[Osurv]]$Dataset, datasetrand = Osr[[1]])

Og1 <- plotdelta(Og[[Ogrowth]]$Dataset, arrow = T) + labs(title = Og$combos$climate[Ogrowth])
Og2 <- plotwin(Og[[Ogrowth]]$Dataset)
Og3 <- plothist(dataset = Og[[Ogrowth]]$Dataset, datasetrand = Ogr[[1]])

Ofp1 <- plotdelta(Ofp[[OpFlwr]]$Dataset, arrow = T) + labs(title = Ofp$combos$climate[OpFlwr])
Ofp2 <- plotwin(Ofp[[OpFlwr]]$Dataset)
Ofp3 <- plothist(dataset = Ofp[[OpFlwr]]$Dataset, datasetrand = Ofpr[[1]])


Cs1 <- plotdelta(Cs[[Csurv]]$Dataset, arrow = T) + labs(title = Cs$combos$climate[Csurv])
Cs2 <- plotwin(Cs[[Csurv]]$Dataset)
Cs3 <- plothist(dataset = Cs[[Csurv]]$Dataset, datasetrand = Csr[[1]])

Cg1 <- plotdelta(Cg[[Cgrowth]]$Dataset, arrow = T) + labs(title = Cg$combos$climate[Cgrowth])
Cg2 <- plotwin(Cg[[Cgrowth]]$Dataset)
Cg3 <- plothist(dataset = Cg[[Cgrowth]]$Dataset, datasetrand = Cgr[[1]])

Cfp1 <- plotdelta(Cfp[[CpFlwr]]$Dataset, arrow = T) + labs(title = Cfp$combos$climate[CpFlwr])
Cfp2 <- plotwin(Cfp[[CpFlwr]]$Dataset)
Cfp3 <- plothist(dataset = Cfp[[CpFlwr]]$Dataset, datasetrand = Cfpr4[[1]])


lay <- rbind(c( 1,  23, 23, 2),
             c( 3,  4,  6, 7),
             c( 8,  9, 11, 12),
             c( 8,  9, 11, 12),
             c( 8,  9, 11, 12),
             c( 8,  9, 11, 12),
             c(13, 14, 16, 17),
             c(13, 14, 16, 17),
             c(13, 14, 16, 17),
             c(13, 14, 16, 17),
             c(18, 19, 21, 22),
             c(18, 19, 21, 22),
             c(18, 19, 21, 22),
             c(18, 19, 21, 22))

HEQU <- textGrob("HEQU", gp = gpar(col="blue4", fontsize = 15))
OPIM <- textGrob("OPIM", gp = gpar(col="red4", fontsize = 15))
CRFL <- textGrob("CRFL", gp = gpar(col="red4", fontsize = 15))
FRSP <- textGrob("FRSP", gp = gpar(col="blue4", fontsize = 15))
Alpine <- textGrob("Alpine", gp = gpar(col="blue4", fontsize = 15))
Arid <- textGrob("Arid", gp = gpar(col="red4", fontsize = 15))


allsurv <- grid.arrange(Arid, Alpine,
                        OPIM, CRFL, HEQU, FRSP,
                        Os1,  Cs1,  Hs1,  Fs1,
                        Os2,  Cs2,  Hs2,  Fs2,
                        Os3,  Cs3,  Hs3,  Fs3,
                        textGrob("Survival", gp = gpar(fontface = "bold", fontsize = 15)),
                        layout_matrix = lay)
saveRDS(allsurv, "Results/Quick_summary/Allsurv.RDS")

allgrowth <- grid.arrange(Arid, Alpine,
                          OPIM, CRFL, HEQU, FRSP,
                          Og1,  Cg1,  Hg1,  Fg1,
                          Og2,  Cg2,  Hg2,  Fg2,
                          Og3,  Cg3,  Hg3,  Fg3,
                          textGrob("Growth", gp = gpar(fontface = "bold", fontsize = 15)),
                          layout_matrix = lay)

saveRDS(allgrowth, "Results/Quick_summary/Allgrowth.RDS")

allFp <- grid.arrange(Arid, Alpine,
                      OPIM, CRFL, HEQU, FRSP,
                      Ofp1,  Cfp1,  Hfp1,  Ffp1,
                      Ofp2,  Cfp2,  Hfp2,  Ffp2,
                      Ofp3,  Cfp3,  Hfp3,  Ffp3,
                      textGrob("Flower Propability", gp = gpar(fontface = "bold", fontsize = 15)),
                      layout_matrix = lay)

saveRDS(allFp, "Results/Quick_summary/AllFp.RDS")
