# Calculate climate (auto)correlation of the selected climate driver
library(lme4)
library(climwin)
library(plyr); library(dplyr)
library(lubridate)
library(patchwork)


source("Results/Load_Climwin_results.R") # centralized script to load rds object, and selected climate drivers
source("Analysis/Climwin/Correlations/required_functions.R") # costum functions to calculate correlations

### Survival

AutoHs <- cor_wrap(species = "HEQU", 
                       vitalrate = "s", 
                       Climate = "data/Climate data/HEQU_NOAA_month_imputed.csv", 
                       SlidingObject = Hs, 
                       Winner = Hsurv)

AutoFs <- cor_wrap(species = "FRSP", 
                      vitalrate = "s", 
                      Climate = 'data/Climate data/FRSP_NOAA_month.csv', 
                      SlidingObject = Fs, 
                      Winner = Fsurv)

AutoCs <- cor_wrap(species = "CRFL", 
                       vitalrate = "s", 
                       Climate = "data/Climate data/CRFL_NOAA_month.csv", 
                       SlidingObject = Cs,
                       Winner = Csurv)

AutoOs <- cor_wrap("OPIM", 
                       "s", 
                       "data/Climate data/OPIM_SEVLTER_month_imputed.csv", 
                       Os, 
                       Osurv)


### Growth

AutoHg <- cor_wrap("HEQU", 
                       "g", 
                       "data/Climate data/HEQU_NOAA_month_imputed.csv", 
                       Hg, 
                       Hgrowth)

AutoFg <- cor_wrap("FRSP", 
                       "g", 
                       "data/Climate data/FRSP_NOAA_month.csv", 
                       Fg, 
                       Fgrowth)

AutoCg <- cor_wrap("CRFL", 
                       "g", 
                       "data/Climate data/CRFL_NOAA_month.csv", 
                       Cg,
                       Cgrowth)

AutoOg <- cor_wrap("OPIM", 
                       "g", 
                       "data/Climate data/OPIM_SEVLTER_month_imputed.csv", 
                       Og, 
                       Ogrowth)


### Flower Prob

AutoHfp <- cor_wrap("HEQU", 
                       "fp", 
                       "data/Climate data/HEQU_NOAA_month_imputed.csv", 
                       Hfp, 
                       HpFlwr)

AutoFfp <- cor_wrap("FRSP", 
                       "fp", 
                       "data/Climate data/FRSP_NOAA_month.csv", 
                       Ffp, 
                       FpFlwr)

AutoCfp <- cor_wrap("CRFL", 
                       "fp", 
                       "data/Climate data/CRFL_NOAA_month.csv", 
                       Cfp,
                       CpFlwr)

AutoOfp <- cor_wrap("OPIM", 
                       "fp", 
                       "data/Climate data/OPIM_SEVLTER_month_imputed.csv", 
                       Ofp, 
                       OpFlwr)


### Flower Numbers

AutoHfn <- cor_wrap("HEQU", 
                        "fn", 
                        "data/Climate data/HEQU_NOAA_month_imputed.csv", 
                        Hfn, 
                        HnFlwr)

AutoFfn <- cor_wrap("FRSP", 
                        "fn", 
                        "data/Climate data/FRSP_NOAA_month.csv", 
                        Ffn, 
                        FnFlwr)

AutoCfn <- cor_wrap("CRFL", 
                        "fn", 
                        "data/Climate data/CRFL_NOAA_month.csv", 
                        Cfn,
                        CnFlwr)

AutoOfn <- cor_wrap("OPIM", 
                        "fp", 
                        "data/Climate data/OPIM_SEVLTER_month_imputed.csv", 
                        Ofn, 
                        OnFlwr)


ggsave("Visual/Correlation_Hs.png", AutoHs)
ggsave("Visual/Correlation_Hg.png", AutoHg)
ggsave("Visual/Correlation_Hfp.png", AutoHfp)
ggsave("Visual/Correlation_Hfn.png", AutoHfn)

ggsave("Visual/Correlation_Fs.png", AutoFs)
ggsave("Visual/Correlation_Fg.png", AutoFg)
ggsave("Visual/Correlation_Ffp.png", AutoFfp)
ggsave("Visual/Correlation_Ffn.png", AutoFfn)

ggsave("Visual/Correlation_Os.png", AutoOs)
ggsave("Visual/Correlation_Og.png", AutoOg)
ggsave("Visual/Correlation_Ofp.png", AutoOfp)
ggsave("Visual/Correlation_Ofn.png", AutoOfn)

ggsave("Visual/Correlation_Cs.png", AutoCs)
ggsave("Visual/Correlation_Cg.png", AutoCg)
ggsave("Visual/Correlation_Cfp.png", AutoCfp)
ggsave("Visual/Correlation_Cfn.png", AutoCfn)
