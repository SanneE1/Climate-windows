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


