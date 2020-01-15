library(dplyr)
library(climwin)
library(gridExtra)
library(grid)
library(ggplot2)

### Import climwin results ----------------------------------------------------------------------------------------
source("Analysis/Climwin/Load_Climwin_results.R")

### HEQU plots ---------------------------------------------------------------------------------------
a <- plotall(datasetrand = Hsr[[1]],
        dataset = Hs[[Hsurv]]$Dataset,
        bestmodel = Hs[[Hsurv]]$BestModel,
        bestmodeldata = Hs[[Hsurv]]$BestModelData,
        arrow = T,
        title = Hs$combos$climate[Hsurv])
ggsave("Visual/HEQU_Surv_plotall.png", 
       a, 
       width = 33,
       height = 20, 
       units = "cm", 
       dpi = 400 )

b <- plotall(datasetrand = Hgr[[1]],
        dataset = Hg[[Hgrowth]]$Dataset,
        bestmodel = Hg[[Hgrowth]]$BestModel,
        bestmodeldata = Hg[[Hgrowth]]$BestModelData,
        arrow = T,
        title = Hs$combos$climate[Hgrowth])
ggsave("Visual/HEQU_Growth_plotall.png", b, 
       width = 33,
       height = 20, 
       units = "cm", 
       dpi = 400 )

c <- plotall(datasetrand = Hfpr[[1]],
        dataset = Hfp[[HpFlwr]]$Dataset,
        bestmodel = Hfp[[HpFlwr]]$BestModel,
        bestmodeldata = Hfp[[HpFlwr]]$BestModelData,
        arrow = T,
        title = Hfp$combos$climate[HpFlwr])
ggsave( "Visual/HEQU_pFl_plotall.png", c, 
        width = 33,
        height = 20, 
        units = "cm", 
        dpi = 400 )

m <- plotall(datasetrand = Hfnr[[1]],
             dataset = Hfn[[HnFlwr]]$Dataset,
             bestmodel = Hfn[[HnFlwr]]$BestModel,
             bestmodeldata = Hfn[[HnFlwr]]$BestModelData,
             arrow = T,
             title = Hfn$combos$climate[HnFlwr])
ggsave( "Visual/HEQU_nFl_plotall.png", m, 
        width = 33,
        height = 20, 
        units = "cm", 
        dpi = 400 )

# ### CRFL plots ---------------------------------------------------------------------------------------
d <- plotall(datasetrand = Csr[[1]],
        dataset = Cs[[Csurv]]$Dataset,
        bestmodel = Cs[[Csurv]]$BestModel,
        bestmodeldata = Cs[[Csurv]]$BestModelData,
        arrow = T,
        title = Cs$combos$climate[Csurv])
ggsave("Visual/CRFL_Surv_plotall.png", d, 
       width = 33,
       height = 20, 
       units = "cm", 
       dpi = 400 )

e <- plotall(datasetrand = Cgr[[1]],
        dataset = Cg[[Cgrowth]]$Dataset,
        bestmodel = Cg[[Cgrowth]]$BestModel,
        bestmodeldata = Cg[[Cgrowth]]$BestModelData,
        arrow = T,
        title = Cs$combos$climate[Cgrowth])
ggsave("Visual/CRFL_Growth_plotall.png", e, 
       width = 33,
       height = 20, 
       units = "cm", 
       dpi = 400 )

f <- plotall(datasetrand = Cfpr[[1]],
        dataset = Cfp[[CpFlwr]]$Dataset,
        bestmodel = Cfp[[CpFlwr]]$BestModel,
        bestmodeldata = Cfp[[CpFlwr]]$BestModelData,
        arrow = T,
        title = Cfp$combos$climate[CpFlwr])
ggsave( "Visual/CRFL_pFl_plotall.png", f, 
        width = 33,
        height = 20, 
        units = "cm", 
        dpi = 400 )

n <- plotall(datasetrand = Cfnr[[1]],
             dataset = Cfn[[CnFlwr]]$Dataset,
             bestmodel = Cfn[[CnFlwr]]$BestModel,
             bestmodeldata = Cfn[[CnFlwr]]$BestModelData,
             arrow = T,
             title = Cfn$combos$climate[CnFlwr])
ggsave( "Visual/CRFL_nFl_plotall.png", n, 
        width = 33,
        height = 20, 
        units = "cm", 
        dpi = 400 )

# ### OPIM plots ---------------------------------------------------------------------------------------
g <- plotall(datasetrand = Osr[[1]],
        dataset = Os[[Osurv]]$Dataset,
        bestmodel = Os[[Osurv]]$BestModel,
        bestmodeldata = Os[[Osurv]]$BestModelData,
        arrow = T,
        title = Os$combos$climate[Osurv])
ggsave("Visual/OPIM_Surv_plotall.png", g, 
       width = 33,
       height = 20, 
       units = "cm", 
       dpi = 400 )

h <- plotall(datasetrand = Ogr[[1]],
        dataset = Og[[Ogrowth]]$Dataset,
        bestmodel = Og[[Ogrowth]]$BestModel,
        bestmodeldata = Og[[Ogrowth]]$BestModelData,
        arrow = T,
        title = Os$combos$climate[Ogrowth])
ggsave( "Visual/OPIM_Growth_plotall.png", h, 
        width = 33,
        height = 20, 
        units = "cm", 
        dpi = 400 )

i <- plotall(datasetrand = Ofpr[[1]],
        dataset = Ofp[[OpFlwr]]$Dataset,
        bestmodel = Ofp[[OpFlwr]]$BestModel,
        bestmodeldata = Ofp[[OpFlwr]]$BestModelData,
        arrow = T,
        title = Ofp$combos$climate[OpFlwr])
ggsave( "Visual/OPIM_pFl_plotall.png", i, 
        width = 33,
        height = 20, 
        units = "cm", 
        dpi = 400 )

o <- plotall(datasetrand = Ofnr[[1]],
             dataset = Ofn[[OnFlwr]]$Dataset,
             bestmodel = Ofn[[OnFlwr]]$BestModel,
             bestmodeldata = Ofn[[OnFlwr]]$BestModelData,
             arrow = T,
             title = Ofn$combos$climate[OnFlwr])
ggsave( "Visual/OPIM_nFl_plotall.png", o, 
        width = 33,
        height = 20, 
        units = "cm", 
        dpi = 400 )

# ### FRSP --------------------------------------------------------------------------------------------------------------------
j <- plotall(datasetrand = Fsr[[1]],
        dataset = Fs[[Fsurv]]$Dataset,
        bestmodel = Fs[[Fsurv]]$BestModel,
        bestmodeldata = Fs[[Fsurv]]$BestModelData,
        arrow = T,
        title = Fs$combos$climate[Fsurv])
ggsave( "Visual/FRSP_Surv_plotall.png", j, 
        width = 33,
        height = 20, 
        units = "cm", 
        dpi = 400 )

k <- plotall(datasetrand = Fgr[[1]],
        dataset = Fg[[Fgrowth]]$Dataset,
        bestmodel = Fg[[Fgrowth]]$BestModel,
        bestmodeldata = Fg[[Fgrowth]]$BestModelData,
        arrow = T,
        title = Fs$combos$climate[Fgrowth])
ggsave("Visual/FRSP_Growth_plotall.png", k, 
       width = 33,
       height = 20, 
       units = "cm", 
       dpi = 400 )

l <- plotall(datasetrand = Ffpr[[1]],
        dataset = Ffp[[FpFlwr]]$Dataset,
        bestmodel = Ffp[[FpFlwr]]$BestModel,
        bestmodeldata = Ffp[[FpFlwr]]$BestModelData,
        arrow = T,
        title = Ffp$combos$climate[FpFlwr])
ggsave( "Visual/FRSP_pFl_plotall.png", l, 
        width = 33,
        height = 20, 
        units = "cm", 
        dpi = 400 )

p <- plotall(datasetrand = Ffnr[[1]],
             dataset = Ffn[[FnFlwr]]$Dataset,
             bestmodel = Ffn[[FnFlwr]]$BestModel,
             bestmodeldata = Ffn[[FnFlwr]]$BestModelData,
             arrow = T,
             title = Ffn$combos$climate[FnFlwr])
ggsave( "Visual/FRSP_nFl_plotall.png", p, 
        width = 33,
        height = 20, 
        units = "cm", 
        dpi = 400 )


#### One summary -------------------------------------------------------------------------------------
source("Analysis/Climwin/Custom_climwin_plot_functions.R")

Hs1 <- plotdelta_custom(Hs[[Hsurv]]$Dataset, arrow = T, legend_position = "right") 
Hs2 <- plotbetas_custom(Hs[[Hsurv]]$Dataset, arrow = T, legend_position = "bottom") 
Hs3 <- plothist_custom(dataset = Hs[[Hsurv]]$Dataset, datasetrand = Hsr[[1]])

Hg1 <- plotdelta_custom(Hg[[Hgrowth]]$Dataset, arrow = T, legend_position = "right")
Hg2 <- plotbetas_custom(Hg[[Hgrowth]]$Dataset, arrow = T, legend_position = "bottom")
Hg3 <- plothist_custom(dataset = Hg[[Hgrowth]]$Dataset, datasetrand = Hgr[[1]])

Hfp1 <- plotdelta_custom(Hfp[[HpFlwr]]$Dataset, arrow = T, legend_position = "right")
Hfp2 <- plotbetas_custom(Hfp[[HpFlwr]]$Dataset, arrow = T, legend_position = "bottom")
Hfp3 <- plothist_custom(dataset = Hfp[[HpFlwr]]$Dataset, datasetrand = Hfpr[[1]])

Hfn1 <- plotdelta_custom(Hfn[[HnFlwr]]$Dataset, arrow = T, legend_position = "right")
Hfn2 <- plotbetas_custom(Hfn[[HnFlwr]]$Dataset, arrow = T, legend_position = "bottom")
Hfn3 <- plothist_custom(dataset = Hfn[[HnFlwr]]$Dataset, datasetrand = Hfnr[[1]])


Fs1 <- plotdelta_custom(Fs[[Fsurv]]$Dataset, arrow = T, legend_position = "right")
Fs2 <- plotbetas_custom(Fs[[Fsurv]]$Dataset, arrow = T, legend_position = "bottom")
Fs3 <- plothist_custom(dataset = Fs[[Fsurv]]$Dataset, datasetrand = Fsr[[1]])

Fg1 <- plotdelta_custom(Fg[[Fgrowth]]$Dataset, arrow = T, legend_position = "right")
Fg2 <- plotbetas_custom(Fg[[Fgrowth]]$Dataset, arrow = T, legend_position = "bottom")
Fg3 <- plothist_custom(dataset = Fg[[Fgrowth]]$Dataset, datasetrand = Fgr[[1]])

Ffp1 <- plotdelta_custom(Ffp[[FpFlwr]]$Dataset, arrow = T, legend_position = "right")
Ffp2 <- plotbetas_custom(Ffp[[FpFlwr]]$Dataset, arrow = T, legend_position = "bottom")
Ffp3 <- plothist_custom(dataset = Ffp[[FpFlwr]]$Dataset, datasetrand = Ffpr[[1]])

Ffn1 <- plotdelta_custom(Ffn[[FnFlwr]]$Dataset, arrow = T, legend_position = "right")
Ffn2 <- plotbetas_custom(Ffn[[FnFlwr]]$Dataset, arrow = T, legend_position = "bottom")
Ffn3 <- plothist_custom(dataset = Ffn[[FnFlwr]]$Dataset, datasetrand = Ffnr[[1]])


Os1 <- plotdelta_custom(Os[[Osurv]]$Dataset, arrow = T, legend_position = "right")
Os2 <- plotbetas_custom(Os[[Osurv]]$Dataset, arrow = T, legend_position = "bottom")
Os3 <- plothist_custom(dataset = Os[[Osurv]]$Dataset, datasetrand = Osr[[1]])

Og1 <- plotdelta_custom(Og[[Ogrowth]]$Dataset, arrow = T, legend_position = "right")
Og2 <- plotbetas_custom(Og[[Ogrowth]]$Dataset, arrow = T, legend_position = "bottom")
Og3 <- plothist_custom(dataset = Og[[Ogrowth]]$Dataset, datasetrand = Ogr[[1]])

Ofp1 <- plotdelta_custom(Ofp[[OpFlwr]]$Dataset, arrow = T, legend_position = "right")
Ofp2 <- plotbetas_custom(Ofp[[OpFlwr]]$Dataset, arrow = T, legend_position = "bottom")
Ofp3 <- plothist_custom(dataset = Ofp[[OpFlwr]]$Dataset, datasetrand = Ofpr[[1]])

Ofn1 <- plotdelta_custom(Ofn[[OnFlwr]]$Dataset, arrow = T, legend_position = "right")
Ofn2 <- plotbetas_custom(Ofn[[OnFlwr]]$Dataset, arrow = T, legend_position = "bottom")
Ofn3 <- plothist_custom(dataset = Ofn[[OnFlwr]]$Dataset, datasetrand = Ofnr[[1]])


Cs1 <- plotdelta_custom(Cs[[Csurv]]$Dataset, arrow = T, legend_position = "right")
Cs2 <- plotbetas_custom(Cs[[Csurv]]$Dataset, arrow = T, legend_position = "bottom")
Cs3 <- plothist_custom(dataset = Cs[[Csurv]]$Dataset, datasetrand = Csr[[1]])

Cg1 <- plotdelta_custom(Cg[[Cgrowth]]$Dataset, arrow = T, legend_position = "right")
Cg2 <- plotbetas_custom(Cg[[Cgrowth]]$Dataset, arrow = T, legend_position = "bottom")
Cg3 <- plothist_custom(dataset = Cg[[Cgrowth]]$Dataset, datasetrand = Cgr[[1]])

Cfp1 <- plotdelta_custom(Cfp[[CpFlwr]]$Dataset, arrow = T, legend_position = "right")
Cfp2 <- plotbetas_custom(Cfp[[CpFlwr]]$Dataset, arrow = T, legend_position = "bottom")
Cfp3 <- plothist_custom(dataset = Cfp[[CpFlwr]]$Dataset, datasetrand = Cfpr[[1]])

Cfn1 <- plotdelta_custom(Cfn[[CnFlwr]]$Dataset, arrow = T, legend_position = "right")
Cfn2 <- plotbetas_custom(Cfn[[CnFlwr]]$Dataset, arrow = T, legend_position = "bottom")
Cfn3 <- plothist_custom(dataset = Cfn[[CnFlwr]]$Dataset, datasetrand = Cfnr[[1]])


lay <- rbind(c( 1,  2, 3, 4),
             c( 5,  6, 7,  8),
             c( 9, 10, 11, 12),
             c( 9, 10, 11, 12),
             c( 9, 10, 11, 12),
             c( 9, 10, 11, 12),
             c(13, 14, 16, 17),
             c(13, 14, 16, 17),
             c(13, 14, 16, 17),
             c(13, 14, 16, 17),
             c(18, 19, 21, 22),
             c(18, 19, 21, 22),
             c(18, 19, 21, 22),
             c(18, 19, 21, 22))

HEQU <- textGrob("H. quinquenervis", gp = gpar(fontsize = 12, fontface = "italic"), just = "left")
OPIM <- textGrob("O. imbricate", gp = gpar(fontsize = 12, fontface = "italic"), just = "left")
CRFL <- textGrob("C. flava", gp = gpar(fontsize = 12, fontface = "italic"), just = "left")
FRSP <- textGrob("F. speciose", gp = gpar( fontsize = 12, fontface = "italic"), just = "left")
Alpine <- textGrob("Alpine", gp = gpar(col="blue4", fontsize = 12))
Arid <- textGrob("Arid", gp = gpar(col="red4", fontsize = 12))

OsurvClim <- textGrob(ifelse(Os$combos$climate[Osurv] == "mean_tavg_scaled", "Average Temperature", "CHANGE CLIM DRIVER"), gp = gpar(fontsize = 10), just = "left")
CsurvClim <- textGrob(ifelse(Cs$combos$climate[Csurv] == "max_tmax_scaled", "Maximum Temperature", "CHANGE CLIM DRIVER"), gp = gpar(fontsize = 10), just = "left")
HsurvClim <- textGrob(ifelse(Hs$combos$climate[Hsurv] == "mean_tobs_scaled", "Mean Temperature at Obs.", "CHANGE CLIM DRIVER"), gp = gpar(fontsize = 10), just = "left")
FsurvClim <- textGrob(ifelse(Fs$combos$climate[Fsurv] == "mean_tmax_scaled", "Average Maximum \nTemperature", "CHANGE CLIM DRIVER"), gp = gpar(fontsize = 10), just = "left")

OgrowthClim <- textGrob(ifelse(Og$combos$climate[Ogrowth] == "mean_tmax_scaled", "Average Maximum \nTemperature", "CHANGE CLIM DRIVER"), gp = gpar(fontsize = 10), just = "right")
CgrowthClim <- textGrob(ifelse(Cg$combos$climate[Cgrowth] == "mean_prcp_scaled", "Mean precipitation", "CHANGE CLIM DRIVER"), gp = gpar(fontsize = 10), just = "right")
HgrowthClim <- textGrob(ifelse(Hg$combos$climate[Hgrowth] == "mean_tmin_scaled", "Average Minimum \nTemperature", "CHANGE CLIM DRIVER"), gp = gpar(fontsize = 10), just = "right")
FgrowthClim <- textGrob(ifelse(Fg$combos$climate[Fgrowth] == "mean_prcp_scaled", "Mean precipitation", "CHANGE CLIM DRIVER"), gp = gpar(fontsize = 10), just = "right")

OpFlwrClim <- textGrob(ifelse(Ofp$combos$climate[OpFlwr] == "mean_tmin_scaled", "Average Minimum \nTemperature", "CHANGE CLIM DRIVER"), gp = gpar(fontsize = 10), just = "right")
CpFlwrClim <- textGrob(ifelse(Cfp$combos$climate[CpFlwr] == "SPEI", "SPEI", "CHANGE CLIM DRIVER"), gp = gpar(fontsize = 10), just = "right")
HpFlwrClim <- textGrob(ifelse(Hfp$combos$climate[HpFlwr] == "max_tmax_scaled", "Maximum Temperature", "CHANGE CLIM DRIVER"), gp = gpar(fontsize = 10), just = "right")
FpFlwrClim <- textGrob(ifelse(Ffp$combos$climate[FpFlwr] == "max_tmax_scaled", "Maximum Temperature", "CHANGE CLIM DRIVER"), gp = gpar(fontsize = 10), just = "right")

OnFlwrClim <- textGrob(ifelse(Ofn$combos$climate[OnFlwr] == "mean_tavg_scaled", "Average Temperature", "CHANGE CLIM DRIVER"), gp = gpar(fontsize = 10), just = "right")
CnFlwrClim <- textGrob(ifelse(Cfn$combos$climate[CnFlwr] == "SPEI", "SPEI", "CHANGE CLIM DRIVER"), gp = gpar(fontsize = 10), just = "right")
HnFlwrClim <- textGrob(ifelse(Hfn$combos$climate[HnFlwr] == "mean_prcp_scaled", "Mean precipitation", "CHANGE CLIM DRIVER"), gp = gpar(fontsize = 10), just = "right")
FnFlwrClim <- textGrob(ifelse(Ffn$combos$climate[FnFlwr] == "mean_tmin_scaled", "Average Minimum \nTemperature", "CHANGE CLIM DRIVER"), gp = gpar(fontsize = 10), just = "right")



allsurv <- grid.arrange(OPIM, CRFL, HEQU, FRSP,
                        OsurvClim, CsurvClim, HsurvClim, FsurvClim,
                        Os1,  Cs1,  Hs1,  Fs1,
                        Os2,  Cs2,  Hs2,  Fs2,
                        Os3,  Cs3,  Hs3,  Fs3,
                        layout_matrix = lay)
saveRDS(allsurv, "Results/Quick_summary/Allsurv.RDS")

allgrowth <- grid.arrange(OPIM, CRFL, HEQU, FRSP,
                          OgrowthClim, CgrowthClim, HgrowthClim, FgrowthClim,
                          Og1,  Cg1,  Hg1,  Fg1,
                          Og2,  Cg2,  Hg2,  Fg2,
                          Og3,  Cg3,  Hg3,  Fg3,
                          layout_matrix = lay)
saveRDS(allgrowth, "Results/Quick_summary/Allgrowth.RDS")

allFp <- grid.arrange(OPIM, CRFL, HEQU, FRSP,
                      OpFlwrClim, CpFlwrClim, HpFlwrClim, FpFlwrClim,
                      Ofp1,  Cfp1,  Hfp1,  Ffp1,
                      Ofp2,  Cfp2,  Hfp2,  Ffp2,
                      Ofp3,  Cfp3,  Hfp3,  Ffp3,
                      layout_matrix = lay)

saveRDS(allFp, "Results/Quick_summary/AllFp.RDS")


allFn <- grid.arrange(OPIM, CRFL, HEQU, FRSP,
                      OnFlwrClim, CnFlwrClim, HnFlwrClim, FnFlwrClim,
                      Ofn1,  Cfn1,  Hfn1,  Ffn1,
                      Ofn2,  Cfn2,  Hfn2,  Ffn2,
                      Ofn3,  Cfn3,  Hfn3,  Ffn3,
                      layout_matrix = lay)

saveRDS(allFn, "Results/Quick_summary/AllFn.RDS")


