library(lme4)
library(climwin)
library(plyr); library(dplyr)
library(lubridate)
library(patchwork)

source("Analysis/Climwin/Load_Climwin_results.R")
source("Analysis/Climwin/Correlations/required_functions.R")

### Survival

AutoHs <- autocor_wrap(species = "HEQU", 
                       vitalrate = "s", 
                       Climate = "data/Climate data/HEQU_NOAA_month_imputed.csv", 
                       SlidingObject = Hs, 
                       Winner = Hsurv)

AutoFs <- autocor_wrap(species = "FRSP", 
                      vitalrate = "s", 
                      Climate = 'data/Climate data/FRSP_NOAA_month.csv', 
                      SlidingObject = Fs, 
                      Winner = Fsurv)

AutoCs <- autocor_wrap(species = "CRFL", 
                       vitalrate = "s", 
                       Climate = "data/Climate data/CRFL_NOAA_month.csv", 
                       SlidingObject = Cs,
                       Winner = Csurv)

AutoOs <- autocor_wrap("OPIM", 
                       "s", 
                       "data/Climate data/OPIM_SEVLTER_month_imputed.csv", 
                       Os, 
                       Osurv)
a <- acorplot(AutoHs) + ggtitle("Autocorrelation of Tobs \nwith circled window", subtitle = "In relation to H. quinquenervis survival")
b <- acorplot(AutoFs) + ggtitle("Autocorrelation of T_mean_max \nwith circled window", subtitle = "In relation to F. speciosa survival")
c <- acorplot(AutoCs) + ggtitle("Autocorrelation of Tmax \nwith circled window", subtitle = "In relation to C. flava survival")
d <- acorplot(AutoOs) + ggtitle("Autocorrelation of Tavg \nwith circled window", subtitle = "In relation to C. imbricata survival")

AutoSurv <- (a+b) / (c+d) + plot_layout(guides = "collect")
ggsave("Visual/Autocorrelation_survival.png", AutoSurv)

### Growth

AutoHg <- autocor_wrap("HEQU", 
                       "g", 
                       "data/Climate data/HEQU_NOAA_month_imputed.csv", 
                       Hg, 
                       Hgrowth)

AutoFg <- autocor_wrap("FRSP", 
                       "g", 
                       "data/Climate data/FRSP_NOAA_month.csv", 
                       Fg, 
                       Fgrowth)

AutoCg <- autocor_wrap("CRFL", 
                       "g", 
                       "data/Climate data/CRFL_NOAA_month.csv", 
                       Cg,
                       Cgrowth)

AutoOg <- autocor_wrap("OPIM", 
                       "g", 
                       "data/Climate data/OPIM_SEVLTER_month_imputed.csv", 
                       Og, 
                       Ogrowth)

e <- acorplot(AutoHg) + ggtitle("Autocorrelation of Tmean_min \nwith circled window")
f <- acorplot(AutoFg) + ggtitle("Autocorrelation of Pmean \nwith circled window")
g <- acorplot(AutoCg) + ggtitle("Autocorrelation of Pmean \nwith circled window")
h <- acorplot(AutoOg) + ggtitle("Autocorrelation of Tmean_max \nwith circled window")

Autogrowth <- (e+f)/(g+h) + plot_layout(guides = "collect")
ggsave("Visual/Autocorrelation_growth.png", Autogrowth)


### Flower Prob

AutoHfp <- autocor_wrap("HEQU", 
                       "fp", 
                       "data/Climate data/HEQU_NOAA_month_imputed.csv", 
                       Hfp, 
                       HpFlwr)

AutoFfp <- autocor_wrap("FRSP", 
                       "fp", 
                       "data/Climate data/FRSP_NOAA_month.csv", 
                       Ffp, 
                       FpFlwr)

AutoCfp <- autocor_wrap("CRFL", 
                       "fp", 
                       "data/Climate data/CRFL_NOAA_month.csv", 
                       Cfp,
                       CpFlwr)

AutoOfp <- autocor_wrap("OPIM", 
                       "fp", 
                       "data/Climate data/OPIM_SEVLTER_month_imputed.csv", 
                       Ofp, 
                       OpFlwr)

i <- acorplot(AutoHfp) + ggtitle("Autocorrelation of Tmax \nwith circled window")
j <- acorplot(AutoFfp) + ggtitle("Autocorrelation of Tmax \nwith circled window")
k <- acorplot(AutoCfp) + ggtitle("Autocorrelation of SPEI \nwith circled window")
l <- acorplot(AutoOfp) + ggtitle("Autocorrelation of T_mean_min \nwith circled window")

AutoFp <- (i+j) / (k+l) + plot_layout(guides = "collect")
ggsave("Visual/Autocorrelation_pFlwr.png", AutoFp)

### Flower Numbers

AutoHfn <- autocor_wrap("HEQU", 
                        "fn", 
                        "data/Climate data/HEQU_NOAA_month_imputed.csv", 
                        Hfn, 
                        HnFlwr)

AutoFfn <- autocor_wrap("FRSP", 
                        "fn", 
                        "data/Climate data/FRSP_NOAA_month.csv", 
                        Ffn, 
                        FnFlwr)

AutoCfn <- autocor_wrap("CRFL", 
                        "fn", 
                        "data/Climate data/CRFL_NOAA_month.csv", 
                        Cfn,
                        CnFlwr)

AutoOfn <- autocor_wrap("OPIM", 
                        "fp", 
                        "data/Climate data/OPIM_SEVLTER_month_imputed.csv", 
                        Ofn, 
                        OnFlwr)

m <- acorplot(AutoHfn) + ggtitle("Autocorrelation of Pmean \nwith circled window")
n <- acorplot(AutoFfn) + ggtitle("Autocorrelation of Tmean_min \nwith circled window")
o <- acorplot(AutoCfn) + ggtitle("Autocorrelation of SPEI \nwith circled window")
p <- acorplot(AutoOfn) + ggtitle("Autocorrelation of Tavg \nwith circled window")

AutoFn <- (m+n) / (o+p) + plot_layout(guides = "collect")
ggsave("Visual/Autocorrelation_nFlwr.png", AutoFn)
