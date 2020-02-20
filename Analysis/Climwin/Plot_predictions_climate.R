library(dplyr)
library(tidyr)
library(climwin)
library(gridExtra)
library(grid)
library(ggplot2)
library(patchwork)

### Import climwin results ----------------------------------------------------------------------------------------
source("Analysis/Climwin/Load_Climwin_results.R")
source("../../Scrap code/plot_binned_prop_df.R")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
### Plot vital rate responses to climate anomilies ----------------------------------------------------------------------------

###HEQU ------------------------------------------------------------------------------------------------------

Hsparams <- data.frame(intercept = Hs[[Hsurv]]$BestModel@beta[1],
              size = Hs[[Hsurv]]$BestModel@beta[2],
              popl = Hs[[Hsurv]]$BestModel@beta[3],
              poph = Hs[[Hsurv]]$BestModel@beta[4],
              clim = Hs[[Hsurv]]$BestModel@beta[5],
              clim2 = Hs[[Hsurv]]$BestModel@beta[6],
              meansize = mean(Hs[[Hsurv]]$BestModelData$lnsizeT),
              meanclim = mean(Hs[[Hsurv]]$BestModelData$climate),
              sdclim = sd(Hs[[Hsurv]]$BestModelData$climate)
              )

a <- data.frame(size = c(seq(from = min(Hs[[Hsurv]]$BestModelData$lnsizeT), 
                           to = max(Hs[[Hsurv]]$BestModelData$lnsizeT), 
                           length.out = 500), Hsparams$meansize))
a$mean <- (1 / (1 + exp(-(Hsparams$intercept + a$size * Hsparams$size + Hsparams$meanclim * Hsparams$clim + Hsparams$meanclim * Hsparams$clim2))))
a$low <- (1 / (1 + exp(-(Hsparams$intercept + a$size * Hsparams$size  + ((Hsparams$meanclim - Hsparams$sdclim) * Hsparams$clim) + ((Hsparams$meanclim - Hsparams$sdclim) * Hsparams$clim2)))))
a$high <- (1 / (1 + exp(-(Hsparams$intercept + a$size * Hsparams$size + ((Hsparams$meanclim + Hsparams$sdclim) * Hsparams$clim) + ((Hsparams$meanclim + Hsparams$sdclim) * Hsparams$clim2)))))
a <- pivot_longer(a, cols = c("mean", "low", "high"), names_to = "Climate_anomaly", values_to = "surv")

H1 <- ggplot() + geom_point(data = logitbin_df(df = Hs[[Hsurv]]$BestModelData, resp = yvar, xvar = lnsizeT), aes(x = lnsizeT, y = yvar)) +
  geom_line(data = a, aes(x = size, y = surv, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = Hsparams$meansize, size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies",
                      values = cbPalette,
                      labels = c("- sd", "mean", "+ sd"),
                      breaks = c("low","mean", "high")) +
  theme_classic() + ggtitle("H. quinquenervis mid population", subtitle = Hs$combos$climate[Hsurv]) +
  xlab("Size (log # rosettes)") +
  ylab("Survival probability")




Hgparams <- data.frame(intercept = Hg[[Hgrowth]]$BestModel@beta[1],
                       size = Hg[[Hgrowth]]$BestModel@beta[2],
                       popl = Hg[[Hgrowth]]$BestModel@beta[3],
                       poph = Hg[[Hgrowth]]$BestModel@beta[4],
                       clim = Hg[[Hgrowth]]$BestModel@beta[5],
                       clim2 = Hg[[Hgrowth]]$BestModel@beta[6],
                       meansize = mean(Hg[[Hgrowth]]$BestModelData$lnsizeT),
                       meanclim = mean(Hg[[Hgrowth]]$BestModelData$climate),
                       sdclim = sd(Hg[[Hgrowth]]$BestModelData$climate))

b <- data.frame(size = c(seq(from = min(Hg[[Hgrowth]]$BestModelData$lnsizeT), 
                               to = max(Hg[[Hgrowth]]$BestModelData$lnsizeT), 
                               length.out = 500), Hgparams$meansize))

# b$mean <- exp(Hgparams$intercept + b$size * Hgparams$size  + (Hgparams$meanclim * Hgparams$clim) + (Hgparams$meanclim * Hgparams$clim2))
b$low <- exp(Hgparams$intercept + b$size * Hgparams$size  + ((Hgparams$meanclim - Hgparams$sdclim) * Hgparams$clim) + ((Hgparams$meanclim - Hgparams$sdclim) * Hgparams$clim2))
b$high <- exp(Hgparams$intercept + b$size * Hgparams$size  + ((Hgparams$meanclim + Hgparams$sdclim) * Hgparams$clim) + ((Hgparams$meanclim + Hgparams$sdclim) * Hgparams$clim2))
b <- pivot_longer(b, cols = c("low", "high"), names_to = "Climate_anomaly", values_to = "sizeT1")


H2 <- ggplot(b) +
  geom_point(data = Hg[[Hgrowth]]$BestModelData, aes(x = lnsizeT, y = yvar), alpha = 0.1) +
  geom_line(aes(x = size, y = sizeT1, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = Hgparams$meansize, size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd", "mean", "+ sd"),
                      breaks = c("low", "mean", "high")) +
  theme_classic() + ggtitle("H. quinquenervis mid population", subtitle = Hg$combos$climate[Hgrowth]) +
  xlab("Size (log # rosettes)") +
  ylab("Size (log # rosettes) T+1") +
  theme(legend.position = "none")




Hpfparams <- data.frame(intercept = Hfp[[HpFlwr]]$BestModel@beta[1],
                       size = Hfp[[HpFlwr]]$BestModel@beta[2],
                       popl = Hfp[[HpFlwr]]$BestModel@beta[3],
                       poph = Hfp[[HpFlwr]]$BestModel@beta[4],
                       clim = Hfp[[HpFlwr]]$BestModel@beta[5],
                       clim2 = Hfp[[HpFlwr]]$BestModel@beta[6],
                       meansize = mean(Hfp[[HpFlwr]]$BestModelData$lnsizeT),
                       meanclim = mean(Hfp[[HpFlwr]]$BestModelData$climate),
                       sdclim = sd(Hfp[[HpFlwr]]$BestModelData$climate))

c <- data.frame(size = c(seq(from = min(Hfp[[HpFlwr]]$BestModelData$lnsizeT), 
                               to = max(Hfp[[HpFlwr]]$BestModelData$lnsizeT), 
                               length.out = 500), Hpfparams$meansize))

c$mean <- (1 / (1 + exp(-(Hpfparams$intercept + c$size * Hpfparams$size + (Hpfparams$meanclim * Hpfparams$clim )  + ((Hpfparams$meanclim) * Hpfparams$clim2)))))
c$low <- (1 / (1 + exp(-(Hpfparams$intercept + c$size * Hpfparams$size + ((Hpfparams$meanclim - Hpfparams$sdclim) * Hpfparams$clim) + ((Hpfparams$meanclim - Hpfparams$sdclim) * Hpfparams$clim2)))))
c$high <- (1 / (1 + exp(-(Hpfparams$intercept + c$size * Hpfparams$size + ((Hpfparams$meanclim + Hpfparams$sdclim) * Hpfparams$clim) + (Hpfparams$meanclim + Hpfparams$sdclim) * Hpfparams$clim2))))
c <- pivot_longer(c, cols = c(low, mean, high), names_to = "Climate_anomaly", values_to = "fp")

H3 <- ggplot() + geom_point(data = logitbin_df(df = Hfp[[HpFlwr]]$BestModelData, resp = yvar, xvar = lnsizeT), aes(x = lnsizeT, y = yvar)) +
  geom_line(data =c, aes(x = size, y = fp, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = Hpfparams$meansize, size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd", "mean", "+ sd"),
                      breaks = c("low", "mean", "high")) +
  theme_classic() + ggtitle("H. quinquenervis mid population", subtitle = Hfp$combos$climate[HpFlwr]) +
  xlab("Size (log # rosettes)") +
  ylab("Flower probability")



Hfnparams <- data.frame(intercept = Hfn[[HnFlwr]]$BestModel@beta[1],
                       size = Hfn[[HnFlwr]]$BestModel@beta[2],
                       popl = Hfn[[HnFlwr]]$BestModel@beta[3],
                       poph = Hfn[[HnFlwr]]$BestModel@beta[4],
                       clim = Hfn[[HnFlwr]]$BestModel@beta[5],
                       clim2 = Hfn[[HnFlwr]]$BestModel@beta[6],
                       meansize = mean(Hfn[[HnFlwr]]$BestModelData$lnsizeT),
                       meanclim = mean(Hfn[[HnFlwr]]$BestModelData$climate),
                       sdclim = sd(Hfn[[HnFlwr]]$BestModelData$climate))

d <- data.frame(size = c(seq(from = min(Hfn[[HnFlwr]]$BestModelData$lnsizeT), 
                               to = max(Hfn[[HnFlwr]]$BestModelData$lnsizeT), 
                               length.out = 500), Hfnparams$meansize))
d$mean <- exp(Hfnparams$intercept + d$size * Hfnparams$size + (Hfnparams$meanclim) * Hfnparams$clim + (Hfnparams$meanclim) * Hfnparams$clim2)
d$low <- exp(Hfnparams$intercept + d$size * Hfnparams$size + (Hfnparams$meanclim - Hfnparams$sdclim) * Hfnparams$clim + (Hfnparams$meanclim - Hfnparams$sdclim) * Hfnparams$clim2)
d$high <- exp(Hfnparams$intercept + d$size * Hfnparams$size + ((Hfnparams$meanclim + Hfnparams$sdclim) * Hfnparams$clim) + ((Hfnparams$meanclim + Hfnparams$sdclim) * Hfnparams$clim2))
d <- pivot_longer(d, cols = c("low","mean", "high"), names_to = "Climate_anomaly", values_to = "flowernumber")

H4 <- ggplot(d) +
  geom_point(data = Hfn[[HnFlwr]]$BestModelData, aes(x = lnsizeT, y = yvar), alpha = 0.1) +
  geom_line(aes(x = size, y = flowernumber, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = Hfnparams$meansize, size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("low", "mean", "high")) +
  theme_classic() + ggtitle("H. quinquenervis mid population", subtitle = Hfn$combos$climate[HnFlwr]) +
  xlab("Size (log # rosettes)") +
  ylab("Number of \nflowering stalks")



### FRSP ---------------------------------------------------------------------------------------------------------------------------------------------

Fsparams <- data.frame(intercept = Fs[[Fsurv]]$BestModel@beta[1],
                       size = Fs[[Fsurv]]$BestModel@beta[2],
                       clim = Fs[[Fsurv]]$BestModel@beta[3],
                       clim2 = Fs[[Fsurv]]$BestModel@beta[4],
                       meansize = mean(Fs[[Fsurv]]$BestModelData$lnsizeT),
                       meanclim = mean(Fs[[Fsurv]]$BestModelData$climate),
                       sdclim = sd(Fs[[Fsurv]]$BestModelData$climate))

e <- data.frame(size = c(seq(from = min(Fs[[Fsurv]]$BestModelData$lnsizeT), 
                               to = max(Fs[[Fsurv]]$BestModelData$lnsizeT), 
                               length.out = 500), Fsparams$meansize))
e$mean <- (1 / (1 + exp(-(Fsparams$intercept + e$size * Fsparams$size + ((Fsparams$meanclim) * Fsparams$clim) + ((Fsparams$meanclim) * Fsparams$clim2)))))
e$low <- (1 / (1 + exp(-(Fsparams$intercept + e$size * Fsparams$size + ((Fsparams$meanclim - Fsparams$sdclim) * Fsparams$clim) + ((Fsparams$meanclim - Fsparams$sdclim) * Fsparams$clim2)))))
e$high <- (1 / (1 + exp(-(Fsparams$intercept + e$size * Fsparams$size + (Fsparams$meanclim + Fsparams$sdclim) * Fsparams$clim + (Fsparams$meanclim + Fsparams$sdclim) * Fsparams$clim2))))
e <- pivot_longer(e, cols = c("low","mean", "high"), names_to = "Climate_anomaly", values_to = "surv")

F1 <- ggplot() + geom_point(data = logitbin_df(df = Fs[[Fsurv]]$BestModelData, resp = yvar, xvar = lnsizeT), aes(x = lnsizeT, y = yvar)) +
  geom_line(data = e, aes(x = size, y = surv, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = Fsparams$meansize, size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("low","mean", "high")) +
  theme_classic() + ggtitle("F. speciosa", subtitle = Fs$combos$climate[Fsurv]) +
  xlab("Size (log # leaves in basal rosette)") +
  ylab("Survival probability")




Fgparams <- data.frame(intercept = Fg[[Fgrowth]]$BestModel@beta[1],
                       size = Fg[[Fgrowth]]$BestModel@beta[2],
                       clim = Fg[[Fgrowth]]$BestModel@beta[3],
                       clim2 = Fg[[Fgrowth]]$BestModel@beta[4],
                       meansize = mean(Fg[[Fgrowth]]$BestModelData$lnsizeT),
                       meanclim = mean(Fg[[Fgrowth]]$BestModelData$climate),
                       sdclim = sd(Fg[[Fgrowth]]$BestModelData$climate))

f <- data.frame(size = c(seq(from = min(Fg[[Fgrowth]]$BestModelData$lnsizeT), 
                               to = max(Fg[[Fgrowth]]$BestModelData$lnsizeT), 
                               length.out = 500), Fgparams$meansize))
# f$mean <- exp(Fgparams$intercept + f$size * Fgparams$size + ((Fgparams$meanclim) * Fgparams$clim) + ((Fgparams$meanclim) * Fgparams$clim2))
f$low <- exp(Fgparams$intercept + f$size * Fgparams$size + ((Fgparams$meanclim - Fgparams$sdclim) * Fgparams$clim) + ((Fgparams$meanclim - Fgparams$sdclim) * Fgparams$clim2))
f$high <- exp(Fgparams$intercept + f$size * Fgparams$size + (Fgparams$meanclim + Fgparams$sdclim) * Fgparams$clim + (Fgparams$meanclim + Fgparams$sdclim) * Fgparams$clim2)
f <- pivot_longer(f, cols = c("low", "high"), names_to = "Climate_anomaly", values_to = "sizeT1")

F2 <- ggplot(f) +
  geom_point(data = Fg[[Fgrowth]]$BestModelData, aes(x = lnsizeT, y = yvar), alpha = 0.1) +
  geom_line(aes(x = size, y = sizeT1, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = Fgparams$meansize, size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("low","mean", "high")) +
  theme_classic() + ggtitle("F. speciosa", subtitle = Fg$combos$climate[Fgrowth]) +
  coord_cartesian(xlim = c(0, 4.5), ylim = c(0, exp(4.5))) +
  xlab("Size (log # leaves in basal rosette)") +
  ylab("Size (log # leaves in basal rosette) T+1") +
  theme(legend.position = "none")




Fpfparams <- data.frame(intercept = Ffp[[FpFlwr]]$BestModel@beta[1],
                        size = Ffp[[FpFlwr]]$BestModel@beta[2],
                        clim = Ffp[[FpFlwr]]$BestModel@beta[3],
                        clim2 = Ffp[[FpFlwr]]$BestModel@beta[4],
                        meansize = mean(Ffp[[FpFlwr]]$BestModelData$lnsizeT),
                        meanclim = mean(Ffp[[FpFlwr]]$BestModelData$climate),
                        sdclim = sd(Ffp[[FpFlwr]]$BestModelData$climate))

g <- data.frame(size = c(seq(from = min(Ffp[[FpFlwr]]$BestModelData$lnsizeT), 
                               to = max(Ffp[[FpFlwr]]$BestModelData$lnsizeT), 
                               length.out = 500), Fpfparams$meansize))

g$mean <- (1 / (1 + exp(-(Fpfparams$intercept + g$size * Fpfparams$size + (Fpfparams$meanclim * Fpfparams$clim) + (Fpfparams$meanclim * Fpfparams$clim2)))))
g$low <- (1 / (1 + exp(-(Fpfparams$intercept + g$size * Fpfparams$size + ((Fpfparams$meanclim - Fpfparams$sdclim) * Fpfparams$clim) + ((Fpfparams$meanclim - Fpfparams$sdclim) * Fpfparams$clim2)))))
g$high <- (1 / (1 + exp(-(Fpfparams$intercept + g$size * Fpfparams$size + ((Fpfparams$meanclim + Fpfparams$sdclim) * Fpfparams$clim) + (Fpfparams$meanclim + Fpfparams$sdclim) * Fpfparams$clim2))))
g <- pivot_longer(g, cols = c("low","mean", "high"), names_to = "Climate_anomaly", values_to = "fp")

F3 <- ggplot() + geom_point(data = logitbin_df(df = Ffp[[FpFlwr]]$BestModelData, resp = yvar, xvar = lnsizeT), aes(x = lnsizeT, y = yvar)) +
  geom_line(data = g, aes(x = size, y = fp, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = Fpfparams$meansize, size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("low", "mean", "high")) +
  theme_classic() + ggtitle("F. speciosa", subtitle = Ffp$combos$climate[FpFlwr]) +
  xlab("Size (log # leaves in basal rosette)") +
  ylab("Flower probability")



Ffnparams <- data.frame(intercept = Ffn[[FnFlwr]]$BestModel@beta[1],
                        size = Ffn[[FnFlwr]]$BestModel@beta[2],
                        clim = Ffn[[FnFlwr]]$BestModel@beta[3],
                        clim2 = Ffn[[FnFlwr]]$BestModel@beta[4],
                        meansize = mean(Ffn[[FnFlwr]]$BestModelData$lnsizeT),
                        meanclim = mean(Ffn[[FnFlwr]]$BestModelData$climate),
                        sdclim = sd(Ffn[[FnFlwr]]$BestModelData$climate))

h <- data.frame(size = c(seq(from = min(Ffn[[FnFlwr]]$BestModelData$lnsizeT), 
                               to = max(Ffn[[FnFlwr]]$BestModelData$lnsizeT), 
                               length.out = 500), Ffnparams$meansize))

h$mean <- exp(Ffnparams$intercept + h$size * Ffnparams$size + (Ffnparams$meanclim) * Ffnparams$clim + (Ffnparams$meanclim) * Ffnparams$clim2)
h$low <- exp(Ffnparams$intercept + h$size * Ffnparams$size + (Ffnparams$meanclim - Ffnparams$sdclim) * Ffnparams$clim + (Ffnparams$meanclim - Ffnparams$sdclim) * Ffnparams$clim2)
h$high <- exp(Ffnparams$intercept + h$size * Ffnparams$size + ((Ffnparams$meanclim + Ffnparams$sdclim) * Ffnparams$clim) + ((Ffnparams$meanclim + Ffnparams$sdclim) * Ffnparams$clim2))
h <- pivot_longer(h, cols = c("low","mean", "high"), names_to = "Climate_anomaly", values_to = "flowernumber")

F4 <- ggplot(h) +
  geom_point(data = Ffn[[FnFlwr]]$BestModelData, aes(x = lnsizeT, y = yvar), alpha = 0.1) +
  geom_line(aes(x = size, y = flowernumber, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = Ffnparams$meansize, size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("low","mean", "high")) +
  theme_classic() + ggtitle("F. speciosa", subtitle = Ffn$combos$climate[FnFlwr]) +
  xlab("Size (log # leaves in basal rosette)") +
  ylab("Number of \nflowers")




###OPIM ------------------------------------------------------------------------------------------------------

Osparams <- data.frame(intercept = Os[[Osurv]]$BestModel@beta[1],
                       size = Os[[Osurv]]$BestModel@beta[2],
                       clim = Os[[Osurv]]$BestModel@beta[3],
                       clim2 = Os[[Osurv]]$BestModel@beta[4],
                       meansize = mean(Os[[Osurv]]$BestModelData$lnsizeT),
                       meanclim = mean(Os[[Osurv]]$BestModelData$climate),
                       sdclim = sd(Os[[Osurv]]$BestModelData$climate))

i <- data.frame(size = c(seq(from = min(Os[[Osurv]]$BestModelData$lnsizeT), 
                               to = max(Os[[Osurv]]$BestModelData$lnsizeT), 
                               length.out = 500), Osparams$meansize))

i$mean <- (1 / (1 + exp(-(Osparams$intercept + i$size * Osparams$size + ((Osparams$meanclim) * Osparams$clim) + ((Osparams$meanclim) * Osparams$clim2)))))
i$low <- (1 / (1 + exp(-(Osparams$intercept + i$size * Osparams$size + ((Osparams$meanclim - Osparams$sdclim) * Osparams$clim) + ((Osparams$meanclim - Osparams$sdclim) * Osparams$clim2)))))
i$high <- (1 / (1 + exp(-(Osparams$intercept + i$size * Osparams$size + (Osparams$meanclim + Osparams$sdclim) * Osparams$clim + (Osparams$meanclim + Osparams$sdclim) * Osparams$clim2))))
i <- pivot_longer(i, cols = c("low","mean", "high"), names_to = "Climate_anomaly", values_to = "surv")

O1 <- ggplot() + geom_point(data = logitbin_df(df = Os[[Osurv]]$BestModelData, resp = yvar, xvar = lnsizeT), aes(x = lnsizeT, y = yvar)) +
  geom_line(data = i, aes(x = size, y = surv, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = Osparams$meansize, size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("low","mean", "high")) +
  theme_classic() + ggtitle("C. imbricata", subtitle = Os$combos$climate[Osurv]) +
  xlab("Size (log volume)") +
  ylab("Survival probability")




Ogparams <- data.frame(intercept = Og[[Ogrowth]]$BestModel@beta[1],
                       size = Og[[Ogrowth]]$BestModel@beta[2],
                       clim = Og[[Ogrowth]]$BestModel@beta[3],
                       clim2 = Og[[Ogrowth]]$BestModel@beta[4],
                       meansize = mean(Og[[Ogrowth]]$BestModelData$lnsizeT),
                       meanclim = mean(Og[[Ogrowth]]$BestModelData$climate),
                       sdclim = sd(Og[[Ogrowth]]$BestModelData$climate))

j <- data.frame(size = c(seq(from = min(Og[[Ogrowth]]$BestModelData$lnsizeT), 
                               to = max(Og[[Ogrowth]]$BestModelData$lnsizeT), 
                               length.out = 500), Ogparams$meansize))

j$mean <- (Ogparams$intercept + j$size * Ogparams$size + ((Ogparams$meanclim) * Ogparams$clim) + ((Ogparams$meanclim) * Ogparams$clim2))
j$low <- (Ogparams$intercept + j$size * Ogparams$size + ((Ogparams$meanclim - Ogparams$sdclim) * Ogparams$clim) + ((Ogparams$meanclim - Ogparams$sdclim) * Ogparams$clim2))
j$high <- (Ogparams$intercept + j$size * Ogparams$size + (Ogparams$meanclim + Ogparams$sdclim) * Ogparams$clim + (Ogparams$meanclim + Ogparams$sdclim) * Ogparams$clim2)
j <- pivot_longer(j, cols = c("low", "mean", "high"), names_to = "Climate_anomaly", values_to = "sizeT1")

O2 <- ggplot(j) +
  geom_point(data = Og[[Ogrowth]]$BestModelData, aes(x = lnsizeT, y = yvar), alpha = 0.1) +
  geom_line(aes(x = size, y = sizeT1, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = Ogparams$meansize, size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("low","mean", "high")) +
  theme_classic() + ggtitle("C. imbricata", subtitle = Og$combos$climate[Ogrowth]) +
  xlab("Size (log volume)") +
  ylab("Size (log volume) T+1")




Opfparams <- data.frame(intercept = Ofp[[OpFlwr]]$BestModel@beta[1],
                        size = Ofp[[OpFlwr]]$BestModel@beta[2],
                        clim = Ofp[[OpFlwr]]$BestModel@beta[3],
                        clim2 = Ofp[[OpFlwr]]$BestModel@beta[4],
                        meansize = mean(Ofp[[OpFlwr]]$BestModelData$lnsizeT),
                        meanclim = mean(Ofp[[OpFlwr]]$BestModelData$climate),
                        sdclim = sd(Ofp[[OpFlwr]]$BestModelData$climate))

k <- data.frame(size = c(seq(from = min(Ofp[[OpFlwr]]$BestModelData$lnsizeT), 
                               to = max(Ofp[[OpFlwr]]$BestModelData$lnsizeT), 
                               length.out = 500), Opfparams$meansize))

k$mean <- (1 / (1 + exp(-(Opfparams$intercept + k$size * Opfparams$size + ((Opfparams$meanclim) * Opfparams$clim) + ((Opfparams$meanclim) * Opfparams$clim2)))))
k$low <- (1 / (1 + exp(-(Opfparams$intercept + k$size * Opfparams$size + ((Opfparams$meanclim - Opfparams$sdclim) * Opfparams$clim) + ((Opfparams$meanclim - Opfparams$sdclim) * Opfparams$clim2)))))
k$high <- (1 / (1 + exp(-(Opfparams$intercept + k$size * Opfparams$size  + ((Opfparams$meanclim + Opfparams$sdclim) * Opfparams$clim) + (Opfparams$meanclim + Opfparams$sdclim) * Opfparams$clim2))))
k <- pivot_longer(k, cols = c("low","mean", "high"), names_to = "Climate_anomaly", values_to = "fp")

O3 <- ggplot() + geom_point(data = logitbin_df(df = Ofp[[OpFlwr]]$BestModelData, resp = yvar, xvar = lnsizeT), aes(x = lnsizeT, y = yvar)) +
  geom_line(data = k, aes(x = size, y = fp, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = Opfparams$meansize, size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("low","mean", "high")) +
  theme_classic() + ggtitle("C. imbricata", subtitle = Ofp$combos$climate[OpFlwr]) +
  xlab("Size (log volume)") +
  ylab("Flower probability")



Ofnparams <- data.frame(intercept = Ofn[[OnFlwr]]$BestModel@beta[1],
                        size = Ofn[[OnFlwr]]$BestModel@beta[2],
                        clim = Ofn[[OnFlwr]]$BestModel@beta[3],
                        clim2 = Ofn[[OnFlwr]]$BestModel@beta[4],
                        meansize = mean(Ofn[[OnFlwr]]$BestModelData$lnsizeT),
                        meanclim = mean(Ofn[[OnFlwr]]$BestModelData$climate),
                        sdclim = sd(Ofn[[OnFlwr]]$BestModelData$climate))

l <- data.frame(size = c(seq(from = min(Ofn[[OnFlwr]]$BestModelData$lnsizeT), 
                               to = max(Ofn[[OnFlwr]]$BestModelData$lnsizeT), 
                               length.out = 500), Ofnparams$meansize))

l$mean <- exp(Ofnparams$intercept + l$size * Ofnparams$size + (Ofnparams$meanclim) * Ofnparams$clim + (Ofnparams$meanclim) * Ofnparams$clim2)
l$low <- exp(Ofnparams$intercept + l$size * Ofnparams$size + (Ofnparams$meanclim - Ofnparams$sdclim) * Ofnparams$clim + (Ofnparams$meanclim - Ofnparams$sdclim) * Ofnparams$clim2)
l$high <- exp(Ofnparams$intercept + l$size * Ofnparams$size + ((Ofnparams$meanclim + Ofnparams$sdclim) * Ofnparams$clim) + ((Ofnparams$meanclim + Ofnparams$sdclim) * Ofnparams$clim2))
l <- pivot_longer(l, cols = c("low", "mean", "high"), names_to = "Climate_anomaly", values_to = "flowernumber")

O4 <- ggplot(l) +
  geom_point(data = Ofn[[OnFlwr]]$BestModelData, aes(x = lnsizeT, y = yvar), alpha = 0.1) +
  geom_line(aes(x = size, y = flowernumber, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = Ofnparams$meansize, size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("low","mean", "high")) +
  theme_classic() + ggtitle("C. imbricata", subtitle = Ofn$combos$climate[OnFlwr]) +
  xlab("Size (log volume)") +
  ylab("Number of \nflower buds") +
  coord_cartesian(xlim = c(-5, 15), ylim = c(0, 300))



###CRFL ------------------------------------------------------------------------------------------------------

Csparams <- data.frame(intercept = Cs[[Csurv]]$BestModel@beta[1],
                       size = Cs[[Csurv]]$BestModel@beta[2],
                       B2 = Cs[[Csurv]]$BestModel@beta[3],
                       B3 = Cs[[Csurv]]$BestModel@beta[4],
                       B4 = Cs[[Csurv]]$BestModel@beta[5],
                       clim = Cs[[Csurv]]$BestModel@beta[6],
                       clim2 = Cs[[Csurv]]$BestModel@beta[7],
                       meansize = mean(Cs[[Csurv]]$BestModelData$lnsizeT),
                       meanclim = mean(Cs[[Csurv]]$BestModelData$climate),
                       sdclim = sd(Cs[[Csurv]]$BestModelData$climate))

p <- data.frame(size = c(seq(from = min(Cs[[Csurv]]$BestModelData$lnsizeT), 
                               to = max(Cs[[Csurv]]$BestModelData$lnsizeT), 
                               length.out = 500), Csparams$meansize))

p$mean <- (1 / (1 + exp(-(Csparams$intercept + p$size * Csparams$size + ((Csparams$meanclim) * Csparams$clim) + ((Csparams$meanclim) * Csparams$clim2)))))
p$low <- (1 / (1 + exp(-(Csparams$intercept + p$size * Csparams$size + ((Csparams$meanclim - Csparams$sdclim) * Csparams$clim) + ((Csparams$meanclim - Csparams$sdclim) * Csparams$clim2)))))
p$high <- (1 / (1 + exp(-(Csparams$intercept + p$size * Csparams$size + (Csparams$meanclim + Csparams$sdclim) * Csparams$clim + (Csparams$meanclim + Csparams$sdclim) * Csparams$clim2))))
p <- pivot_longer(p, cols = c("low","mean", "high"), names_to = "Climate_anomaly", values_to = "surv")

C1 <- ggplot() + geom_point(data = logitbin_df(df = Cs[[Csurv]]$BestModelData, resp = yvar, xvar = lnsizeT), aes(x = lnsizeT, y = yvar)) +
  geom_line(data = p, aes(x = size, y = surv,  colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = Csparams$meansize, size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("low","mean", "high")) +
  theme_classic() + ggtitle("C. flava Block 1", subtitle = Cs$combos$climate[Csurv]) +
  xlab("Size (log # rosettes)") +
  ylab("Survival probability")




Cgparams <- data.frame(intercept = Cg[[Cgrowth]]$BestModel@beta[1],
                       size = Cg[[Cgrowth]]$BestModel@beta[2],
                       B2 = Cg[[Cgrowth]]$BestModel@beta[3],
                       B3 = Cg[[Cgrowth]]$BestModel@beta[4],
                       B4 = Cg[[Cgrowth]]$BestModel@beta[5],
                       B5 = Cg[[Cgrowth]]$BestModel@beta[6],
                       B6 = Cg[[Cgrowth]]$BestModel@beta[7],
                       clim = Cg[[Cgrowth]]$BestModel@beta[8],
                       meansize = mean(Cg[[Cgrowth]]$BestModelData$lnsizeT),
                       meanclim = mean(Cg[[Cgrowth]]$BestModelData$climate),
                       sdclim = sd(Cg[[Cgrowth]]$BestModelData$climate))

m <- data.frame(size = c(seq(from = min(Cg[[Cgrowth]]$BestModelData$lnsizeT), 
                               to = max(Cg[[Cgrowth]]$BestModelData$lnsizeT), 
                               length.out = 500), Cgparams$meansize))

m$mean <- exp(Cgparams$intercept + m$size * Cgparams$size + ((Cgparams$meanclim) * Cgparams$clim))
m$low <- exp(Cgparams$intercept + m$size * Cgparams$size +  ((Cgparams$meanclim - Cgparams$sdclim) * Cgparams$clim))
m$high <- exp(Cgparams$intercept + m$size * Cgparams$size +  (Cgparams$meanclim + Cgparams$sdclim) * Cgparams$clim)
m <- pivot_longer(m, cols = c("low","mean", "high"), names_to = "Climate_anomaly", values_to = "sizeT1")

C2 <- ggplot(m) +
  geom_line(aes(x = size, y = sizeT1, colour = Climate_anomaly), size = 1) +
  geom_point(data = Cg[[Cgrowth]]$BestModelData, aes(x = lnsizeT, y = yvar), alpha = 0.1) +
  geom_vline(xintercept = Cgparams$meansize, size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("low","mean", "high")) +
  theme_classic() + ggtitle("C. flava", subtitle = Cg$combos$climate[Cgrowth]) +
  xlab("Size (log # rosettes)") +
  ylab("Size (log # rosettes) T+1")




Cpfparams <- data.frame(intercept = Cfp[[CpFlwr]]$BestModel@beta[1],
                        size = Cfp[[CpFlwr]]$BestModel@beta[2],
                        B2 = Cfp[[CpFlwr]]$BestModel@beta[3],
                        clim = Cfp[[CpFlwr]]$BestModel@beta[8],
                        clim2 = Cfp[[CpFlwr]]$BestModel@beta[9],
                        meansize = mean(Cfp[[CpFlwr]]$BestModelData$lnsizeT),
                        meanclim = mean(Cfp[[CpFlwr]]$BestModelData$climate),
                        sdclim = sd(Cfp[[CpFlwr]]$BestModelData$climate))

n <- data.frame(size = c(seq(from = min(Cfp[[CpFlwr]]$BestModelData$lnsizeT), 
                               to = max(Cfp[[CpFlwr]]$BestModelData$lnsizeT), 
                               length.out = 500), Cpfparams$meansize))

n$mean <- (1 / (1 + exp(-(Cpfparams$intercept + n$size * Cpfparams$size + ((Cpfparams$meanclim) * Cpfparams$clim) + ((Cpfparams$meanclim) * Cpfparams$clim2)))))
n$low <- (1 / (1 + exp(-(Cpfparams$intercept + n$size * Cpfparams$size + ((Cpfparams$meanclim - Cpfparams$sdclim) * Cpfparams$clim) + ((Cpfparams$meanclim - Cpfparams$sdclim) * Cpfparams$clim2)))))
n$high <- (1 / (1 + exp(-(Cpfparams$intercept + n$size * Cpfparams$size + (Cpfparams$meanclim + Cpfparams$sdclim) * Cpfparams$clim + (Cpfparams$meanclim + Cpfparams$sdclim) * Cpfparams$clim2))))
n <- pivot_longer(n, cols = c("low","mean", "high"), names_to = "Climate_anomaly", values_to = "fp")

C3 <- ggplot() + geom_point(data = logitbin_df(df = Cfp[[CpFlwr]]$BestModelData, resp = yvar, xvar = lnsizeT), aes(x = lnsizeT, y = yvar)) +
  geom_line(data = n, aes(x = size, y = fp,  colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = Cpfparams$meansize, size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd","mean", "+ sd"),
                      breaks = c("low","mean", "high")) +
  theme_classic() + ggtitle("C. flava", subtitle = Cfp$combos$climate[CpFlwr]) +
  xlab("Size (log # rosettes)") +
  ylab("Flower probability")



Cfnparams <- data.frame(intercept = Cfn[[CnFlwr]]$BestModel@beta[1],
                        size = Cfn[[CnFlwr]]$BestModel@beta[2],
                        clim = Cfn[[CnFlwr]]$BestModel@beta[3],
                        clim2 = Cfn[[CnFlwr]]$BestModel@beta[4],
                        meansize = mean(Cfn[[CnFlwr]]$BestModelData$lnsizeT),
                        meanclim = mean(Cfn[[CnFlwr]]$BestModelData$climate),
                        sdclim = sd(Cfn[[CnFlwr]]$BestModelData$climate))
                        
o <- data.frame(size = c(seq(from = min(Cfn[[CnFlwr]]$BestModelData$lnsizeT), 
                               to = max(Cfn[[CnFlwr]]$BestModelData$lnsizeT), 
                               length.out = 500), Cfnparams$meansize))

o$low <- exp(Cfnparams$intercept + o$size * Cfnparams$size + (Cfnparams$meanclim - Cfnparams$sdclim) * Cfnparams$clim + (Cfnparams$meanclim - Cfnparams$sdclim) * Cfnparams$clim2)
o$high <- exp(Cfnparams$intercept + o$size * Cfnparams$size + ((Cfnparams$meanclim + Cfnparams$sdclim) * Cfnparams$clim) + ((Cfnparams$meanclim + Cfnparams$sdclim) * Cfnparams$clim2))
o <- pivot_longer(o, cols = c("low", "high"), names_to = "Climate_anomaly", values_to = "flowernumber")

C4 <- ggplot(o) +
  geom_point(data = Cfn[[CnFlwr]]$BestModelData, aes(x = lnsizeT, y = yvar), alpha = 0.1) +
  geom_line(aes(x = size, y = flowernumber, colour = Climate_anomaly), size = 1) +
  geom_vline(xintercept = Cfnparams$meansize, size = 1.5, colour =  "#0072B2") +
  scale_colour_manual(name = "Climate anomalies", values = cbPalette,
                      labels = c("- sd", "+ sd"),
                      breaks = c("low", "high")) +
  theme_classic() + ggtitle("C. flava", subtitle = Cfn$combos$climate[CnFlwr]) +
  xlab("Size (log # rosettes)") +
  ylab("Number of \nflowering rosettes") +
  theme(legend.position = "none")



### Arrange per vital rate ---------------------------------------------------------------------------------------------------------------

Survival <- (H1 + F1) / (O1 + C1) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")
ggsave("Visual/Best_surv_models.png", Survival, height = 10, width = 10, units = "in")

Growth <- (H2 + F2) / (O2 + C2) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")
ggsave("Visual/Best_growth_models.png", Growth, height = 10, width = 10, units = "in")

pFlwr <- (H3 + F3) / (O3 + C3) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")
ggsave("Visual/Best_pFlwr_models.png", pFlwr, height = 10, width = 10, units = "in")

nFlwr <- (H4 + F4) / (O4 + C4) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")
ggsave("Visual/Best_nFlwr_models.png", nFlwr, height = 10, width = 10, units = "in")

## Calculate effect size using mean size ---------------------------------------------------------------------------------

ESizeS <- list(
  HEQU = a[which(a$size == Hsparams$meansize & a$Climate_anomaly != "mean"),],
  FRSP = e[which(e$size == Fsparams$meansize & e$Climate_anomaly != "mean"),],
  OPIM = i[which(i$size == Osparams$meansize & i$Climate_anomaly != "mean"),],
  CRFL = p[which(p$size == Csparams$meansize & p$Climate_anomaly != "mean"),]
)
ESizeS

ESizeG <- list(
  HEQU = b[which(b$size == Hgparams$meansize & b$Climate_anomaly != "mean"),],
  FRSP = f[which(f$size == Fgparams$meansize & f$Climate_anomaly != "mean"),],
  OPIM = j[which(j$size == Ogparams$meansize & j$Climate_anomaly != "mean"),],
  CRFL = m[which(m$size == Cgparams$meansize & m$Climate_anomaly != "mean"),]
)
ESizeG
### Relative difference in size
lapply(ESizeG, function(x) x$sizeT1[2] / x$sizeT1[1])


ESizeFp <- list(
  HEQU = c[which(c$size == Hpfparams$meansize & c$Climate_anomaly != "mean"),],
  FRSP = g[which(g$size == Fpfparams$meansize & g$Climate_anomaly != "mean"),],
  OPIM = k[which(k$size == Opfparams$meansize & k$Climate_anomaly != "mean"),],
  CRFL = n[which(n$size == Cpfparams$meansize & n$Climate_anomaly != "mean"),]
)
ESizeFp

ESizeFn <- list(
  HEQU = d[which(d$size == Hfnparams$meansize & d$Climate_anomaly != "mean"),],
  FRSP = h[which(h$size == Ffnparams$meansize & h$Climate_anomaly != "mean"),],
  OPIM = l[which(l$size == Ofnparams$meansize & l$Climate_anomaly != "mean"),],
  CRFL = o[which(o$size == Cfnparams$meansize & o$Climate_anomaly != "mean"),]
)
ESizeFn
### Relative difference in flower numbers
lapply(ESizeFn, function(x) x$flowernumber[2] / x$flowernumber[1])

