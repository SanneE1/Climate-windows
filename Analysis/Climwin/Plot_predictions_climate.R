library(dplyr)
library(tidyr)
library(climwin)
library(gridExtra)
library(grid)
library(ggplot2)
library(patchwork)
library(lme4)

### Import climwin results ----------------------------------------------------------------------------------------
source("Analysis/Climwin/Load_Climwin_results.R")

### Get functions ----------------------------------------------------------------------------------------
# source("../../52 Scrap code/plot_binned_prop_df.R")
logitbin_df <- function (df, resp, xvar, ..., n =100, log_trans_xvar = FALSE) {
  
  resp <- enquo(resp)
  xvar <- enquo(xvar)
  
  if (log_trans_xvar == TRUE) {
    
    if (length(filter(df, !! xvar <= 0)[,1] >= 1 )) {
      warning("Transformed data contains values of =< 0")
    }
    
    df <- df %>%
      filter(!! xvar > 0) %>%
      mutate(!! xvar := log(!! xvar))
    
  }
  
  df <- df %>%
    group_by(...) %>%
    mutate(bingroup = cut(!! xvar, breaks = n))
  
  df <- df %>%
    group_by(..., bingroup) %>%
    summarize(!! resp := mean(!! resp), !! xvar := mean(!! xvar))
  
  return(df)
}


inv_logit <- function(x) {
  return(1/(1 + exp(-x)))
}

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
### Plot vital rate responses to climate anomilies ----------------------------------------------------------------------------

###HEQU ------------------------------------------------------------------------------------------------------

size_h1 <- c(size = c(seq(from = min(Hs[[Hsurv]]$BestModelData$lnsizeT), 
                           to = max(Hs[[Hsurv]]$BestModelData$lnsizeT), 
                           length.out = 500), mean(Hs[[Hsurv]]$BestModelData$lnsizeT)))
pop_h1 <- c("mid")
clim_h1 <- c(mean(Hs[[Hsurv]]$BestModelData$climate) - sd(Hs[[Hsurv]]$BestModelData$climate), 
             mean(Hs[[Hsurv]]$BestModelData$climate),
             mean(Hs[[Hsurv]]$BestModelData$climate) + sd(Hs[[Hsurv]]$BestModelData$climate))

h1_pred <- expand.grid("lnsizeT" = size_h1, "population" = pop_h1, "climate" = clim_h1)
h1_pred$surv <- inv_logit(predict(Hs[[Hsurv]]$BestModel, newdata = h1_pred, re.form = NA))
h1_pred$Climate_anomaly <- as.factor(ifelse(h1_pred$climate == clim_h1[1], "- sd", ifelse(h1_pred$climate == clim_h1[2], "mean", "+ sd")))


H1 <- ggplot() + geom_point(data = logitbin_df(df = Hs[[Hsurv]]$BestModelData, resp = yvar, xvar = lnsizeT), aes(x = lnsizeT, y = yvar)) +
  geom_line(data = h1_pred, aes(x = lnsizeT, y = surv, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = mean(Hg[[Hgrowth]]$BestModelData$lnsizeT), size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies",
                      values = cbPalette,
                      labels = c( "- sd", "mean", "+ sd"),
                      breaks = c( "- sd", "mean", "+ sd")) +
  theme_classic() + ggtitle("H. quinquenervis mid population", subtitle = Hs$combos$climate[Hsurv]) +
  xlab("Size (log # rosettes)") +
  ylab("Survival probability")




size_h2 <- c(size = c(seq(from = min(Hg[[Hgrowth]]$BestModelData$lnsizeT), 
                          to = max(Hg[[Hgrowth]]$BestModelData$lnsizeT), 
                          length.out = 500), mean(Hg[[Hgrowth]]$BestModelData$lnsizeT)))
pop_h2 <- c("mid")
clim_h2 <- c(mean(Hg[[Hgrowth]]$BestModelData$climate) - sd(Hg[[Hgrowth]]$BestModelData$climate), 
             mean(Hg[[Hgrowth]]$BestModelData$climate),
             mean(Hg[[Hgrowth]]$BestModelData$climate) + sd(Hg[[Hgrowth]]$BestModelData$climate))

h2_pred <- expand.grid("lnsizeT" = size_h2, "population" = pop_h2, "climate" = clim_h2)
h2_pred$lnsizeT1 <- exp(predict(Hg[[Hgrowth]]$BestModel, newdata = h2_pred, re.form = NA))
h2_pred$Climate_anomaly <- as.factor(ifelse(h2_pred$climate == clim_h2[1], "- sd", ifelse(h2_pred$climate == clim_h2[2], "mean", "+ sd")))


H2 <- ggplot() +
  geom_point(data = Hg[[Hgrowth]]$BestModelData, aes(x = lnsizeT, y = yvar), alpha = 0.1) +
  geom_line(data = h2_pred, aes(x = lnsizeT, y = lnsizeT1, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = mean(Hg[[Hgrowth]]$BestModelData$lnsizeT), size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd", "mean", "+ sd"),
                      breaks = c("- sd", "mean", "+ sd")) +
  theme_classic() + ggtitle("H. quinquenervis mid population", subtitle = Hg$combos$climate[Hgrowth]) +
  xlab("Size (log # rosettes)") +
  ylab("# rosettes in t+1") +
  theme(legend.position = "none")




size_h3 <- c(size = c(seq(from = min(Hfp[[HpFlwr]]$BestModelData$lnsizeT), 
                          to = max(Hfp[[HpFlwr]]$BestModelData$lnsizeT), 
                          length.out = 500), mean(Hfp[[HpFlwr]]$BestModelData$lnsizeT)))
pop_h3 <- c("mid")
clim_h3 <- c(mean(Hfp[[HpFlwr]]$BestModelData$climate) - sd(Hfp[[HpFlwr]]$BestModelData$climate), 
             mean(Hfp[[HpFlwr]]$BestModelData$climate),
             mean(Hfp[[HpFlwr]]$BestModelData$climate) + sd(Hfp[[HpFlwr]]$BestModelData$climate))

h3_pred <- expand.grid("lnsizeT" = size_h3, "population" = pop_h3, "climate" = clim_h3)
h3_pred$fp <- inv_logit(predict(Hfp[[HpFlwr]]$BestModel, newdata = h3_pred, re.form = NA))
h3_pred$Climate_anomaly <- as.factor(ifelse(h3_pred$climate == clim_h3[1], "- sd", ifelse(h3_pred$climate == clim_h3[2], "mean", "+ sd")))

H3 <- ggplot() + geom_point(data = logitbin_df(df = Hfp[[HpFlwr]]$BestModelData, resp = yvar, xvar = lnsizeT), aes(x = lnsizeT, y = yvar)) +
  geom_line(data = h3_pred, aes(x = lnsizeT, y = fp, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = mean(Hfp[[HpFlwr]]$BestModelData$lnsizeT), size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd", "mean", "+ sd"),
                      breaks = c("- sd", "mean", "+ sd")) +
  theme_classic() + ggtitle("H. quinquenervis mid population", subtitle = Hfp$combos$climate[HpFlwr]) +
  xlab("Size (log # rosettes)") +
  ylab("Flower probability")



size_h4 <- c(size = c(seq(from = min(Hfn[[HnFlwr]]$BestModelData$lnsizeT), 
                          to = max(Hfn[[HnFlwr]]$BestModelData$lnsizeT), 
                          length.out = 500), mean(Hfn[[HnFlwr]]$BestModelData$lnsizeT)))
pop_h4 <- c("mid")
clim_h4 <- c(mean(Hfn[[HnFlwr]]$BestModelData$climate) - sd(Hfn[[HnFlwr]]$BestModelData$climate), 
             mean(Hfn[[HnFlwr]]$BestModelData$climate),
             mean(Hfn[[HnFlwr]]$BestModelData$climate) + sd(Hfn[[HnFlwr]]$BestModelData$climate))

h4_pred <- expand.grid("lnsizeT" = size_h4, "population" = pop_h4, "climate" = clim_h4)
h4_pred$flowernumber <- exp(predict(Hfn[[HnFlwr]]$BestModel, newdata = h4_pred, re.form = NA))
h4_pred$Climate_anomaly <- as.factor(ifelse(h4_pred$climate == clim_h4[1], "- sd", ifelse(h4_pred$climate == clim_h4[2], "mean", "+ sd")))

H4 <- ggplot() +
  geom_point(data = Hfn[[HnFlwr]]$BestModelData, aes(x = lnsizeT, y = yvar), alpha = 0.1) +
  geom_line(data = h4_pred, aes(x = lnsizeT, y = flowernumber, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = mean(Hfn[[HnFlwr]]$BestModelData$lnsizeT), size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("- sd","mean", "+ sd")) +
  theme_classic() + ggtitle("H. quinquenervis mid population", subtitle = Hfn$combos$climate[HnFlwr]) +
  xlab("Size (log # rosettes)") +
  ylab("Number of \nflowering stalks")



### FRSP ---------------------------------------------------------------------------------------------------------------------------------------------

size_f1 <- c(size = c(seq(from = min(Fs[[Fsurv]]$BestModelData$lnsizeT), 
                          to = max(Fs[[Fsurv]]$BestModelData$lnsizeT), 
                          length.out = 500), mean(Fs[[Fsurv]]$BestModelData$lnsizeT)))
clim_f1 <- c(mean(Fs[[Fsurv]]$BestModelData$climate) - sd(Fs[[Fsurv]]$BestModelData$climate), 
             mean(Fs[[Fsurv]]$BestModelData$climate),
             mean(Fs[[Fsurv]]$BestModelData$climate) + sd(Fs[[Fsurv]]$BestModelData$climate))

f1_pred <- expand.grid("lnsizeT" = size_f1, "climate" = clim_f1)
f1_pred$surv <- inv_logit(predict(Fs[[Fsurv]]$BestModel, newdata = f1_pred, re.form = NA))
f1_pred$Climate_anomaly <- as.factor(ifelse(f1_pred$climate == clim_f1[1], "- sd", ifelse(f1_pred$climate == clim_f1[2], "mean", "+ sd")))


F1 <- ggplot() + geom_point(data = logitbin_df(df = Fs[[Fsurv]]$BestModelData, resp = yvar, xvar = lnsizeT), aes(x = lnsizeT, y = yvar)) +
  geom_line(data = f1_pred, aes(x = lnsizeT, y = surv, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = mean(Fs[[Fsurv]]$BestModelData$lnsizeT), size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("- sd","mean", "+ sd")) +
  theme_classic() + ggtitle("F. speciosa", subtitle = Fs$combos$climate[Fsurv]) +
  xlab("Size (log # leaves in basal rosette)") +
  ylab("Survival probability")




size_f2 <- c(size = c(seq(from = min(Fg[[Fgrowth]]$BestModelData$lnsizeT), 
                          to = max(Fg[[Fgrowth]]$BestModelData$lnsizeT), 
                          length.out = 500), mean(Fg[[Fgrowth]]$BestModelData$lnsizeT)))
clim_f2 <- c(mean(Fg[[Fgrowth]]$BestModelData$climate) - sd(Fg[[Fgrowth]]$BestModelData$climate), 
             mean(Fg[[Fgrowth]]$BestModelData$climate),
             mean(Fg[[Fgrowth]]$BestModelData$climate) + sd(Fg[[Fgrowth]]$BestModelData$climate))

f2_pred <- expand.grid("lnsizeT" = size_f2, "climate" = clim_f2)
f2_pred$lnsizeT1 <- exp(predict(Fg[[Fgrowth]]$BestModel, newdata = f2_pred, re.form = NA))
f2_pred$Climate_anomaly <- as.factor(ifelse(f2_pred$climate == clim_f2[1], "- sd", ifelse(f2_pred$climate == clim_f2[2], "mean", "+ sd")))

F2 <- ggplot() +
  geom_point(data = Fg[[Fgrowth]]$BestModelData, aes(x = lnsizeT, y = yvar), alpha = 0.1) +
  geom_line(data = f2_pred, aes(x = lnsizeT, y = lnsizeT1, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = mean(Fg[[Fgrowth]]$BestModelData$lnsizeT), size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("- sd","mean", "+ sd")) +
  theme_classic() + ggtitle("F. speciosa", subtitle = Fg$combos$climate[Fgrowth]) +
  coord_cartesian(xlim = c(0, 4.5), ylim = c(0, exp(4.5))) +
  xlab("Size (log # leaves in \nbasal rosette)") +
  ylab("# leaves in basal \nrosette in T+1") +
  theme(legend.position = "none")




size_f3 <- c(size = c(seq(from = min(Ffp[[FpFlwr]]$BestModelData$lnsizeT), 
                          to = max(Ffp[[FpFlwr]]$BestModelData$lnsizeT), 
                          length.out = 500), mean(Ffp[[FpFlwr]]$BestModelData$lnsizeT)))
clim_f3 <- c(mean(Ffp[[FpFlwr]]$BestModelData$climate) - sd(Ffp[[FpFlwr]]$BestModelData$climate), 
             mean(Ffp[[FpFlwr]]$BestModelData$climate),
             mean(Ffp[[FpFlwr]]$BestModelData$climate) + sd(Ffp[[FpFlwr]]$BestModelData$climate))

f3_pred <- expand.grid("lnsizeT" = size_f3, "climate" = clim_f3)
f3_pred$fp <- inv_logit(predict(Ffp[[FpFlwr]]$BestModel, newdata = f3_pred, re.form = NA))
f3_pred$Climate_anomaly <- as.factor(ifelse(f3_pred$climate == clim_f3[1], "- sd", ifelse(f3_pred$climate == clim_f3[2], "mean", "+ sd")))

F3 <- ggplot() + geom_point(data = logitbin_df(df = Ffp[[FpFlwr]]$BestModelData, resp = yvar, xvar = lnsizeT), aes(x = lnsizeT, y = yvar)) +
  geom_line(data = f3_pred, aes(x = lnsizeT, y = fp, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = mean(Ffp[[FpFlwr]]$BestModelData$lnsizeT), size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("- sd","mean", "+ sd")) +
  theme_classic() + ggtitle("F. speciosa", subtitle = Ffp$combos$climate[FpFlwr]) +
  xlab("Size (log # leaves in basal rosette)") +
  ylab("Flower probability")




size_f4 <- c(size = c(seq(from = min(Ffn[[FnFlwr]]$BestModelData$lnsizeT), 
                          to = max(Ffn[[FnFlwr]]$BestModelData$lnsizeT), 
                          length.out = 500), mean(Ffn[[FnFlwr]]$BestModelData$lnsizeT)))
clim_f4 <- c(mean(Ffn[[FnFlwr]]$BestModelData$climate) - sd(Ffn[[FnFlwr]]$BestModelData$climate), 
             mean(Ffn[[FnFlwr]]$BestModelData$climate),
             mean(Ffn[[FnFlwr]]$BestModelData$climate) + sd(Ffn[[FnFlwr]]$BestModelData$climate))

f4_pred <- expand.grid("lnsizeT" = size_f4, "climate" = clim_f4)
f4_pred$flowernumber <- exp(predict(Ffn[[FnFlwr]]$BestModel, newdata = f4_pred, re.form = NA))
f4_pred$Climate_anomaly <- as.factor(ifelse(f4_pred$climate == clim_f4[1], "- sd", ifelse(f4_pred$climate == clim_f4[2], "mean", "+ sd")))

### Look into this one! the mean is lower than both - and + sd!! ??
F4 <- ggplot() +
  geom_point(data = Ffn[[FnFlwr]]$BestModelData, aes(x = lnsizeT, y = yvar), alpha = 0.1) +
  geom_line(data = f4_pred, aes(x = lnsizeT, y = flowernumber, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = mean(Ffn[[FnFlwr]]$BestModelData$lnsizeT), size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("- sd","mean", "+ sd")) +
  theme_classic() + ggtitle("F. speciosa", subtitle = Ffn$combos$climate[FnFlwr]) +
  xlab("Size (log # leaves in basal rosette)") +
  ylab("Number of \nflowers")




###OPIM ------------------------------------------------------------------------------------------------------

size_o1 <- c(size = c(seq(from = min(Os[[Osurv]]$BestModelData$lnsizeT), 
                          to = max(Os[[Osurv]]$BestModelData$lnsizeT), 
                          length.out = 500), mean(Os[[Osurv]]$BestModelData$lnsizeT)))
clim_o1 <- c(mean(Os[[Osurv]]$BestModelData$climate) - sd(Os[[Osurv]]$BestModelData$climate), 
             mean(Os[[Osurv]]$BestModelData$climate),
             mean(Os[[Osurv]]$BestModelData$climate) + sd(Os[[Osurv]]$BestModelData$climate))

o1_pred <- expand.grid("lnsizeT" = size_o1, "climate" = clim_o1)
o1_pred$surv <- inv_logit(predict(Os[[Osurv]]$BestModel, newdata = o1_pred, re.form = NA))
o1_pred$Climate_anomaly <- as.factor(ifelse(o1_pred$climate == clim_o1[1], "- sd", ifelse(o1_pred$climate == clim_o1[2], "mean", "+ sd")))

O1 <- ggplot() + geom_point(data = logitbin_df(df = Os[[Osurv]]$BestModelData, resp = yvar, xvar = lnsizeT), aes(x = lnsizeT, y = yvar)) +
  geom_line(data = o1_pred, aes(x = lnsizeT, y = surv, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = mean(Os[[Osurv]]$BestModelData$lnsizeT), size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("- sd","mean", "+ sd")) +
  theme_classic() + ggtitle("C. imbricata", subtitle = Os$combos$climate[Osurv]) +
  xlab("Size (log volume)") +
  ylab("Survival probability")




size_o2 <- c(size = c(seq(from = min(Og[[Ogrowth]]$BestModelData$lnsizeT), 
                          to = max(Og[[Ogrowth]]$BestModelData$lnsizeT), 
                          length.out = 500), mean(Og[[Ogrowth]]$BestModelData$lnsizeT)))
clim_o2 <- c(mean(Og[[Ogrowth]]$BestModelData$climate) - sd(Og[[Ogrowth]]$BestModelData$climate), 
             mean(Og[[Ogrowth]]$BestModelData$climate),
             mean(Og[[Ogrowth]]$BestModelData$climate) + sd(Og[[Ogrowth]]$BestModelData$climate))

o2_pred <- expand.grid("lnsizeT" = size_o2, "climate" = clim_o2)
o2_pred$lnsizeT1 <- predict(Og[[Ogrowth]]$BestModel, newdata = o2_pred, re.form = NA)
o2_pred$Climate_anomaly <- as.factor(ifelse(o2_pred$climate == clim_o2[1], "- sd", ifelse(o2_pred$climate == clim_o2[2], "mean", "+ sd")))

O2 <- ggplot() +
  geom_point(data = Og[[Ogrowth]]$BestModelData, aes(x = lnsizeT, y = yvar), alpha = 0.1) +
  geom_line(data = o2_pred, aes(x = lnsizeT, y = lnsizeT1, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = mean(Og[[Ogrowth]]$BestModelData$lnsizeT), size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("- sd","mean", "+ sd")) +
  theme_classic() + ggtitle("C. imbricata", subtitle = Og$combos$climate[Ogrowth]) +
  xlab("Size (log volume)") +
  ylab("Size (log volume) T+1")




size_o3 <- c(size = c(seq(from = min(Ofp[[OpFlwr]]$BestModelData$lnsizeT), 
                          to = max(Ofp[[OpFlwr]]$BestModelData$lnsizeT), 
                          length.out = 500), mean(Ofp[[OpFlwr]]$BestModelData$lnsizeT)))
clim_o3 <- c(mean(Ofp[[OpFlwr]]$BestModelData$climate) - sd(Ofp[[OpFlwr]]$BestModelData$climate), 
             mean(Ofp[[OpFlwr]]$BestModelData$climate),
             mean(Ofp[[OpFlwr]]$BestModelData$climate) + sd(Ofp[[OpFlwr]]$BestModelData$climate))

o3_pred <- expand.grid("lnsizeT" = size_o3, "climate" = clim_o3)
o3_pred$fp <- inv_logit(predict(Ofp[[OpFlwr]]$BestModel, newdata = o3_pred, re.form = NA))
o3_pred$Climate_anomaly <- as.factor(ifelse(o3_pred$climate == clim_o3[1], "- sd", ifelse(o3_pred$climate == clim_o3[2], "mean", "+ sd")))

O3 <- ggplot() + geom_point(data = logitbin_df(df = Ofp[[OpFlwr]]$BestModelData, resp = yvar, xvar = lnsizeT), aes(x = lnsizeT, y = yvar)) +
  geom_line(data = o3_pred, aes(x = lnsizeT, y = fp, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = mean(Ofp[[OpFlwr]]$BestModelData$lnsizeT), size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("- sd","mean", "+ sd")) +
  theme_classic() + ggtitle("C. imbricata", subtitle = Ofp$combos$climate[OpFlwr]) +
  xlab("Size (log volume)") +
  ylab("Flower probability")



size_o4 <- c(size = c(seq(from = min(Ofn[[OnFlwr]]$BestModelData$lnsizeT), 
                          to = max(Ofn[[OnFlwr]]$BestModelData$lnsizeT), 
                          length.out = 500), mean(Ofn[[OnFlwr]]$BestModelData$lnsizeT)))
clim_o4 <- c(mean(Ofn[[OnFlwr]]$BestModelData$climate) - sd(Ofn[[OnFlwr]]$BestModelData$climate), 
             mean(Ofn[[OnFlwr]]$BestModelData$climate),
             mean(Ofn[[OnFlwr]]$BestModelData$climate) + sd(Ofn[[OnFlwr]]$BestModelData$climate))

o4_pred <- expand.grid("lnsizeT" = size_o4, "climate" = clim_o4)
o4_pred$flowernumber <- exp(predict(Ofn[[OnFlwr]]$BestModel, newdata = o4_pred, re.form = NA))
o4_pred$Climate_anomaly <- as.factor(ifelse(o4_pred$climate == clim_o4[1], "- sd", ifelse(o4_pred$climate == clim_o4[2], "mean", "+ sd")))

O4 <- ggplot() +
  geom_point(data = Ofn[[OnFlwr]]$BestModelData, aes(x = lnsizeT, y = yvar), alpha = 0.1) +
  geom_line(data = o4_pred, aes(x = lnsizeT, y = flowernumber, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = mean(Ofn[[OnFlwr]]$BestModelData$lnsizeT), size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("low","mean", "high")) +
  theme_classic() + ggtitle("C. imbricata", subtitle = Ofn$combos$climate[OnFlwr]) +
  xlab("Size (log volume)") +
  ylab("Number of \nflower buds") +
  coord_cartesian(xlim = c(-5, 15), ylim = c(0, 300)) +
  theme(legend.position = "none")



###CRFL ------------------------------------------------------------------------------------------------------

size_c1 <- c(size = c(seq(from = min(Cs[[Csurv]]$BestModelData$lnsizeT), 
                          to = max(Cs[[Csurv]]$BestModelData$lnsizeT), 
                          length.out = 500), mean(Cs[[Csurv]]$BestModelData$lnsizeT)))
pop_c1 <- c("I")
clim_c1 <- c(mean(Cs[[Csurv]]$BestModelData$climate) - sd(Cs[[Csurv]]$BestModelData$climate), 
             mean(Cs[[Csurv]]$BestModelData$climate),
             mean(Cs[[Csurv]]$BestModelData$climate) + sd(Cs[[Csurv]]$BestModelData$climate))

c1_pred <- expand.grid("lnsizeT" = size_c1, "Block" = pop_c1, "climate" = clim_c1)
c1_pred$surv <- inv_logit(predict(Cs[[Csurv]]$BestModel, newdata = c1_pred, re.form = NA))
c1_pred$Climate_anomaly <- as.factor(ifelse(c1_pred$climate == clim_c1[1], "- sd", ifelse(c1_pred$climate == clim_c1[2], "mean", "+ sd")))

C1 <- ggplot() + geom_point(data = logitbin_df(df = Cs[[Csurv]]$BestModelData, resp = yvar, xvar = lnsizeT), aes(x = lnsizeT, y = yvar)) +
  geom_line(data = c1_pred, aes(x = lnsizeT, y = surv,  colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = mean(Cs[[Csurv]]$BestModelData$lnsizeT), size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("- sd","mean", "+ sd")) +
  theme_classic() + ggtitle("C. flava Block 1", subtitle = Cs$combos$climate[Csurv]) +
  xlab("Size (log # rosettes)") +
  ylab("Survival probability")




size_c2 <- c(size = c(seq(from = min(Cg[[Cgrowth]]$BestModelData$lnsizeT), 
                          to = max(Cg[[Cgrowth]]$BestModelData$lnsizeT), 
                          length.out = 500), mean(Cg[[Cgrowth]]$BestModelData$lnsizeT)))
pop_c2 <- c("I")
clim_c2 <- c(mean(Cg[[Cgrowth]]$BestModelData$climate) - sd(Cg[[Cgrowth]]$BestModelData$climate), 
             mean(Cg[[Cgrowth]]$BestModelData$climate),
             mean(Cg[[Cgrowth]]$BestModelData$climate) + sd(Cg[[Cgrowth]]$BestModelData$climate))

c2_pred <- expand.grid("lnsizeT" = size_c2, "Block" = pop_c2, "climate" = clim_c2)
c2_pred$lnsizeT1 <- exp(predict(Cg[[Cgrowth]]$BestModel, newdata = c2_pred, re.form = NA))
c2_pred$Climate_anomaly <- as.factor(ifelse(c2_pred$climate == clim_c2[1], "- sd", ifelse(c2_pred$climate == clim_c2[2], "mean", "+ sd")))


C2 <- ggplot() +
  geom_point(data = Cg[[Cgrowth]]$BestModelData, aes(x = lnsizeT, y = yvar), alpha = 0.1) +
  geom_line(data = c2_pred, aes(x = lnsizeT, y = lnsizeT1, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = mean(Cg[[Cgrowth]]$BestModelData$lnsizeT), size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("- sd","mean", "+ sd")) +
  theme_classic() + ggtitle("C. flava", subtitle = Cg$combos$climate[Cgrowth]) +
  xlab("Size (log # rosettes)") +
  ylab("# rosettes in t+1")




size_c3 <- c(size = c(seq(from = min(Cfp[[CpFlwr]]$BestModelData$lnsizeT), 
                          to = max(Cfp[[CpFlwr]]$BestModelData$lnsizeT), 
                          length.out = 500), mean(Cfp[[CpFlwr]]$BestModelData$lnsizeT)))
pop_c3 <- c("I")
clim_c3 <- c(mean(Cfp[[CpFlwr]]$BestModelData$climate) - sd(Cfp[[CpFlwr]]$BestModelData$climate), 
             mean(Cfp[[CpFlwr]]$BestModelData$climate),
             mean(Cfp[[CpFlwr]]$BestModelData$climate) + sd(Cfp[[CpFlwr]]$BestModelData$climate))

c3_pred <- expand.grid("lnsizeT" = size_c3, "Block" = pop_c3, "climate" = clim_c3)
c3_pred$fp <- inv_logit(predict(Cfp[[CpFlwr]]$BestModel, newdata = c3_pred, re.form = NA))
c3_pred$Climate_anomaly <- as.factor(ifelse(c3_pred$climate == clim_c3[1], "- sd", ifelse(c3_pred$climate == clim_c3[2], "mean", "+ sd")))

C3 <- ggplot() + geom_point(data = logitbin_df(df = Cfp[[CpFlwr]]$BestModelData, resp = yvar, xvar = lnsizeT), aes(x = lnsizeT, y = yvar)) +
  geom_line(data = c3_pred, aes(x = lnsizeT, y = fp,  colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = mean(Cfp[[CpFlwr]]$BestModelData$lnsizeT), size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("- sd","mean", "+ sd")) +
  theme_classic() + ggtitle("C. flava", subtitle = Cfp$combos$climate[CpFlwr]) +
  xlab("Size (log # rosettes)") +
  ylab("Flower probability")



size_c4 <- c(size = c(seq(from = min(Cfn[[CnFlwr]]$BestModelData$lnsizeT), 
                          to = max(Cfn[[CnFlwr]]$BestModelData$lnsizeT), 
                          length.out = 500), mean(Cfn[[CnFlwr]]$BestModelData$lnsizeT)))
pop_c4 <- c("I")
clim_c4 <- c(mean(Cfn[[CnFlwr]]$BestModelData$climate) - sd(Cfn[[CnFlwr]]$BestModelData$climate), 
             mean(Cfn[[CnFlwr]]$BestModelData$climate),
             mean(Cfn[[CnFlwr]]$BestModelData$climate) + sd(Cfn[[CnFlwr]]$BestModelData$climate))

c4_pred <- expand.grid("lnsizeT" = size_c4, "Block" = pop_c4, "climate" = clim_c4)
c4_pred$flowernumber <- exp(predict(Cfn[[CnFlwr]]$BestModel, newdata = c4_pred, re.form = NA))
c4_pred$Climate_anomaly <- as.factor(ifelse(c4_pred$climate == clim_c4[1], "- sd", ifelse(c4_pred$climate == clim_c4[2], "mean", "+ sd")))

C4 <- ggplot() +
  geom_point(data = Cfn[[CnFlwr]]$BestModelData, aes(x = lnsizeT, y = yvar), alpha = 0.1) +
  geom_line(data = c4_pred, aes(x = lnsizeT, y = flowernumber, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = mean(Cfn[[CnFlwr]]$BestModelData$lnsizeT), size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("- sd","mean", "+ sd")) +
  theme_classic() + ggtitle("C. flava", subtitle = Cfn$combos$climate[CnFlwr]) +
  xlab("Size (log # rosettes)") +
  ylab("Number of \nflowering rosettes") +
  theme(legend.position = "none")



### Arrange per vital rate ---------------------------------------------------------------------------------------------------------------

Survival <- (H1 + F1) / (O1 + C1) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")
ggsave("Visual/Best_surv_models.png", Survival, height = 7.5, width = 7.5, units = "in")

Growth <- (H2 + F2) / (O2 + C2) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")
ggsave("Visual/Best_growth_models.png", Growth, height = 7.5, width = 7.5, units = "in")

pFlwr <- (H3 + F3) / (O3 + C3) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")
ggsave("Visual/Best_pFlwr_models.png", pFlwr, height = 7.5, width = 7.5, units = "in")

nFlwr <- (H4 + F4) / (O4 + C4) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")
ggsave("Visual/Best_nFlwr_models.png", nFlwr, height = 10, width = 10, units = "in")

## Calculate effect size using mean size ---------------------------------------------------------------------------------

ESizeS <- list(
  HEQU = h1_pred[which(h1_pred$lnsizeT == mean(Hs[[Hsurv]]$BestModelData$lnsizeT) & h1_pred$Climate_anomaly != "mean"),],
  FRSP = f1_pred[which(f1_pred$lnsizeT == mean(Fs[[Fsurv]]$BestModelData$lnsizeT) & f1_pred$Climate_anomaly != "mean"),],
  OPIM = o1_pred[which(o1_pred$lnsizeT == mean(Os[[Osurv]]$BestModelData$lnsizeT) & o1_pred$Climate_anomaly != "mean"),],
  CRFL = c1_pred[which(c1_pred$lnsizeT == mean(Cs[[Csurv]]$BestModelData$lnsizeT) & c1_pred$Climate_anomaly != "mean"),]
)
ESizeS

ESizeG <- list(
  HEQU = h2_pred[which(h2_pred$lnsizeT == mean(Hg[[Hgrowth]]$BestModelData$lnsizeT) & h2_pred$Climate_anomaly != "mean"),],
  FRSP = f2_pred[which(f2_pred$lnsizeT == mean(Fg[[Fgrowth]]$BestModelData$lnsizeT) & f2_pred$Climate_anomaly != "mean"),],
  OPIM = o2_pred[which(o2_pred$lnsizeT == mean(Og[[Ogrowth]]$BestModelData$lnsizeT) & o2_pred$Climate_anomaly != "mean"),],
  CRFL = c2_pred[which(c2_pred$lnsizeT == mean(Cg[[Cgrowth]]$BestModelData$lnsizeT) & c2_pred$Climate_anomaly != "mean"),]
)
ESizeG
### Relative difference in size
lapply(ESizeG, function(x) x$lnsizeT1[2] / x$lnsizeT1[1])


ESizeFp <- list(
  HEQU = h3_pred[which(h3_pred$lnsizeT == mean(Hfp[[HpFlwr]]$BestModelData$lnsizeT) & h3_pred$Climate_anomaly != "mean"),],
  FRSP = f3_pred[which(f3_pred$lnsizeT == mean(Ffp[[FpFlwr]]$BestModelData$lnsizeT) & f3_pred$Climate_anomaly != "mean"),],
  OPIM = o3_pred[which(o3_pred$lnsizeT == mean(Ofp[[OpFlwr]]$BestModelData$lnsizeT) & o3_pred$Climate_anomaly != "mean"),],
  CRFL = c3_pred[which(c3_pred$lnsizeT == mean(Cfp[[CpFlwr]]$BestModelData$lnsizeT) & c3_pred$Climate_anomaly != "mean"),]
)
ESizeFp

ESizeFn <- list(
  HEQU = h4_pred[which(h4_pred$lnsizeT == mean(Hfn[[HnFlwr]]$BestModelData$lnsizeT) & h4_pred$Climate_anomaly != "mean"),],
  FRSP = f4_pred[which(f4_pred$lnsizeT == mean(Ffn[[FnFlwr]]$BestModelData$lnsizeT) & f4_pred$Climate_anomaly != "mean"),],
  OPIM = o4_pred[which(o4_pred$lnsizeT == mean(Ofn[[OnFlwr]]$BestModelData$lnsizeT) & o4_pred$Climate_anomaly != "mean"),],
  CRFL = c4_pred[which(c4_pred$lnsizeT == mean(Cfn[[CnFlwr]]$BestModelData$lnsizeT) & c4_pred$Climate_anomaly != "mean"),]
)
ESizeFn
### Relative difference in flower numbers
lapply(ESizeFn, function(x) x$flowernumber[2] / x$flowernumber[1])

