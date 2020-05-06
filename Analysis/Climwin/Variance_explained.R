library(lme4)
library(dplyr)
library(MuMIn)

source("Analysis/Climwin/Load_Climwin_results.R")


baseHs <- update(Hs[[Hsurv]]$BestModel, . ~ lnsizeT + population + (1|year), data = Hs[[Hsurv]]$BestModelData)
climHs <- update(Hs[[Hsurv]]$BestModel, data = Hs[[Hsurv]]$BestModelData)

baseHg <- update(Hg[[Hgrowth]]$BestModel, . ~ lnsizeT + population + (1|year), data = Hg[[Hgrowth]]$BestModelData)
climHg <- update(Hg[[Hgrowth]]$BestModel, data = Hg[[Hgrowth]]$BestModelData)

baseHfp <- update(Hfp[[HpFlwr]]$BestModel, . ~ lnsizeT + population + (1|year), data = Hfp[[HpFlwr]]$BestModelData)
climHfp <- update(Hfp[[HpFlwr]]$BestModel, data = Hfp[[HpFlwr]]$BestModelData)

baseHfn <- update(Hfn[[HnFlwr]]$BestModel, . ~ lnsizeT + population + (1|year), data = Hfn[[HnFlwr]]$BestModelData)
climHfn <- update(Hfn[[HnFlwr]]$BestModel, data = Hfn[[HnFlwr]]$BestModelData)

a <- r.squaredGLMM(climHs) - r.squaredGLMM(baseHs)
b <- r.squaredGLMM(climHg) - r.squaredGLMM(baseHg)
c <- r.squaredGLMM(climHfp) - r.squaredGLMM(baseHfp)
d <- r.squaredGLMM(climHfn) - r.squaredGLMM(baseHfn)


baseFs <- update(Fs[[Fsurv]]$BestModel, . ~ lnsizeT + (1|year), data = Fs[[Fsurv]]$BestModelData)
climFs <- update(Fs[[Fsurv]]$BestModel, data = Fs[[Fsurv]]$BestModelData)

baseFg <- update(Fg[[Fgrowth]]$BestModel, . ~ lnsizeT + (1|year), data = Fg[[Fgrowth]]$BestModelData)
climFg <- update(Fg[[Fgrowth]]$BestModel, data = Fg[[Fgrowth]]$BestModelData)

baseFfp <- update(Ffp[[FpFlwr]]$BestModel, . ~ lnsizeT + (1|year), data = Ffp[[FpFlwr]]$BestModelData)
climFfp <- update(Ffp[[FpFlwr]]$BestModel, data = Ffp[[FpFlwr]]$BestModelData)

baseFfn <- update(Ffn[[FnFlwr]]$BestModel, . ~ lnsizeT + (1|yearT1), data = Ffn[[FnFlwr]]$BestModelData)
climFfn <- update(Ffn[[FnFlwr]]$BestModel, data = Ffn[[FnFlwr]]$BestModelData)

e <- r.squaredGLMM(climFs) - r.squaredGLMM(baseFs)
f <- r.squaredGLMM(climFg) - r.squaredGLMM(baseFg)
g <- r.squaredGLMM(climFfp) - r.squaredGLMM(baseFfp)
h <- r.squaredGLMM(climFfn) - r.squaredGLMM(baseFfn)


baseOs <- update(Os[[Osurv]]$BestModel, . ~ lnsizeT + (1|Plot) + (1|year), data = Os[[Osurv]]$BestModelData)
climOs <- update(Os[[Osurv]]$BestModel, data = Os[[Osurv]]$BestModelData)

baseOg <- update(Og[[Ogrowth]]$BestModel, . ~ lnsizeT + (1|Plot) + (1|year), data = Og[[Ogrowth]]$BestModelData)
climOg <- update(Og[[Ogrowth]]$BestModel, data = Og[[Ogrowth]]$BestModelData)

baseOfp <- update(Ofp[[OpFlwr]]$BestModel, . ~ lnsizeT + (1|Plot) + (1|year), data = Ofp[[OpFlwr]]$BestModelData)
climOfp <- update(Ofp[[OpFlwr]]$BestModel, data = Ofp[[OpFlwr]]$BestModelData)

baseOfn <- update(Ofn[[OnFlwr]]$BestModel, . ~ lnsizeT + (1|Plot) + (1|year), data = Ofn[[OnFlwr]]$BestModelData)
climOfn <- update(Ofn[[OnFlwr]]$BestModel, data = Ofn[[OnFlwr]]$BestModelData)

i <- r.squaredGLMM(climOs) - r.squaredGLMM(baseOs)
j <- r.squaredGLMM(climOg) - r.squaredGLMM(baseOg)
k <- r.squaredGLMM(climOfp) - r.squaredGLMM(baseOfp)
l <- r.squaredGLMM(climOfn) - r.squaredGLMM(baseOfn)


baseCs <- update(Cs[[Csurv]]$BestModel, . ~ lnsizeT + Block + (1|year), data = Cs[[Csurv]]$BestModelData)
climCs <- update(Cs[[Csurv]]$BestModel, data = Cs[[Csurv]]$BestModelData)

baseCg <- update(Cg[[Cgrowth]]$BestModel, . ~ lnsizeT + Block + (1|year), data = Cg[[Cgrowth]]$BestModelData)
climCg <- update(Cg[[Cgrowth]]$BestModel, data = Cg[[Cgrowth]]$BestModelData)

baseCfp <- update(Cfp[[CpFlwr]]$BestModel, . ~ lnsizeT + Block + (1|year), data = Cfp[[CpFlwr]]$BestModelData)
climCfp <- update(Cfp[[CpFlwr]]$BestModel, data = Cfp[[CpFlwr]]$BestModelData)

baseCfn <- update(Cfn[[CnFlwr]]$BestModel, . ~ lnsizeT + Block + (1|year), data = Cfn[[CnFlwr]]$BestModelData)
climCfn <- update(Cfn[[CnFlwr]]$BestModel, data = Cfn[[CnFlwr]]$BestModelData)

m <- r.squaredGLMM(climCs) - r.squaredGLMM(baseCs)
n <- r.squaredGLMM(climCg) - r.squaredGLMM(baseCg)
o <- r.squaredGLMM(climCfp) - r.squaredGLMM(baseCfp)
p <- r.squaredGLMM(climCfn) - r.squaredGLMM(baseCfn)


all <- list(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
summary(unlist(sapply(all, function(x) x[1,2])))



chu_files <- list.files("Data/Chu_models/")
chu <- paste("Data/Chu_models/", chu_files, sep = "")
chu <- lapply(chu, readRDS)
chu_files <- gsub("[[:blank:]]", "", chu_files)
names(chu) <- chu_files

ids <- grep("_constant", chu_files)

chuR <- vector(mode = "list", length = length(ids))

for (j in 1:length(ids)) {
  try({
    i <- ids[j]
    mod <- chu[[paste(strsplit(chu_files[[i]], "_constant.rds"), "_climate.rds", sep = "")]]
    clim <- chu[[i]]
    chuR[[j]] <- r.squaredGLMM(mod) - r.squaredGLMM(clim)
  })
}

summary(unlist(lapply(chuR, function(x) x[2])))
