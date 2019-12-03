
library(dplyr)
library(tidyr)
library(ggplot2)
library(climwin)
library(gridExtra)
library(grid)


### Get Climwin result files ------------------------------------------------------------------------------------------
Hs <- readRDS("Results/Climwin/HEQU_s_month_result.rds")
Hg <- readRDS("Results/Climwin/HEQU_g_month_result.rds")
Hfp <- readRDS("Results/Climwin/HEQU_fp_month_result.rds")

Hsr <- readRDS("Results/Climwin/HEQU_s_month_random.rds")
Hgr <- readRDS("Results/Climwin/HEQU_g_month_random.rds")
Hfpr <- readRDS("Results/Climwin/HEQU_fp_month_random.rds")

Fs <- readRDS("Results/Climwin/FRSP_s_month_result.rds")
Fg <- readRDS("Results/Climwin/FRSP_g_month_result_5yrs.rds")
Ffp <- readRDS("Results/Climwin/FRSP_fp_month_result.rds")

Fsr <- readRDS("Results/Climwin/FRSP_s_month_random.rds")
Fgr <- readRDS("Results/Climwin/FRSP_g_month_random_5yrs.rds")
Ffpr <- readRDS("Results/Climwin/FRSP_fp_month_random.rds")

Os <- readRDS("Results/Climwin/OPIM_s_month_result.rds")
Og <- readRDS("Results/Climwin/OPIM_g_month_result.rds")
Ofp <- readRDS("Results/Climwin/OPIM_fp_month_result.rds")

Osr <- readRDS("Results/Climwin/OPIM_s_month_random.rds")
Ogr <- readRDS("Results/Climwin/OPIM_g_month_random.rds")
Ofpr <- readRDS("Results/Climwin/OPIM_fp_month_random.rds")

Cs <- readRDS("Results/Climwin/CRFL_s_month_result.rds")
Cg <- readRDS("Results/Climwin/CRFL_g_month_result.rds")
Cfp <- readRDS("Results/Climwin/CRFL_fp_month_result.rds")

Csr <- readRDS("Results/Climwin/CRFL_s_month_random.rds")
Cgr <- readRDS("Results/Climwin/CRFL_g_month_random.rds")
Cfpr <- readRDS("Results/Climwin/CRFL_fp_month_random4.rds")

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


### Functions to get 95 CI for the windows -------------------------------------------------------------------------------------------------------


getwindow <- function(dataset, species, vitalrate, cw = 0.95) {
  
  dataset <- dataset[order(-dataset$ModWeight), ]
  dataset$cw <- as.numeric(cumsum(dataset$ModWeight) <= 
                               cw)
  datasetcw <- subset(dataset, cw == 1)
  if (nrow(datasetcw) == 0) {
    datasetcw <- dataset[1, ]
  }
  keep = c("WindowClose", "WindowOpen")
  datasetcw <- datasetcw[keep]
  datasetcw$species <- species
  datasetcw$vitalrate <- vitalrate

  return(datasetcw)
  
}

a <- getwindow(Hs[[Hsurv]]$Dataset, "HEQU", "s")
b <- getwindow(Hg[[Hgrowth]]$Dataset, "HEQU", "g")
c <- getwindow(Hfp[[HpFlwr]]$Dataset, "HEQU", "fp")
a$climate <- Hs$combos$climate[Hsurv]
b$climate <- Hg$combos$climate[Hgrowth]
c$climate <- Hfp$combos$climate[HpFlwr]

d <- getwindow(Fs[[Fsurv]]$Dataset, "FRSP", "s")
e <- getwindow(Fg[[Fgrowth]]$Dataset, "FRSP", "g")
f <- getwindow(Ffp[[FpFlwr]]$Dataset, "FRSP", "fp")
d$climate <- Fs$combos$climate[Fsurv]
e$climate <- Fg$combos$climate[Fgrowth]
f$climate <- Ffp$combos$climate[FpFlwr]

g <- getwindow(Os[[Osurv]]$Dataset, "OPIM", "s")
h <- getwindow(Og[[Ogrowth]]$Dataset, "OPIM", "g")
i <- getwindow(Ofp[[OpFlwr]]$Dataset, "OPIM", "fp")
g$climate <- Os$combos$climate[Osurv]
h$climate <- Og$combos$climate[Ogrowth]
i$climate <- Ofp$combos$climate[OpFlwr]

j <- getwindow(Cs[[Csurv]]$Dataset, "CRFL", "s")
k <- getwindow(Cg[[Cgrowth]]$Dataset, "CRFL", "g")
l <- getwindow(Cfp[[CpFlwr]]$Dataset, "CRFL", "fp")
j$climate <- Cs$combos$climate[Csurv]
k$climate <- Cg$combos$climate[Cgrowth]
l$climate <- Cfp$combos$climate[CpFlwr]

df <- bind_rows(a,b,c,d,e,f,g,h,i,j,k,l)
df$species <- factor(df$species, levels = c("HEQU", "FRSP", "OPIM", "CRFL"))
df$climate <- factor(df$climate)



### Get best windows ---------------------------------------------------------------------------------------------------------------------------------------------------------------

df1 <- bind_rows(c(Hs[[Hsurv]]$Dataset[1,c("WindowClose", "WindowOpen"),1], driver = Hs$combos$climate[Hsurv]),
             c(Hg[[Hgrowth]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Hg$combos$climate[Hgrowth]),
             c(Hfp[[HpFlwr]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Hfp$combos$climate[HpFlwr]),
             c(Fs[[Fsurv]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Fs$combos$climate[Fsurv]),
             c(Fg[[Fgrowth]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Fg$combos$climate[Fgrowth]),
             c(Ffp[[FpFlwr]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Ffp$combos$climate[FpFlwr]),
             c(Os[[Osurv]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Os$combos$climate[Osurv]),
             c(Og[[Ogrowth]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Og$combos$climate[Ogrowth]),
             c(Ofp[[OpFlwr]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Ofp$combos$climate[OpFlwr]),
             c(Cs[[Csurv]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Cs$combos$climate[Csurv]),
             c(Cg[[Cgrowth]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Cg$combos$climate[Cgrowth]),
             c(Cfp[[CpFlwr]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Cfp$combos$climate[CpFlwr]))
df1$species <- rep(c("HEQU", "FRSP", "OPIM", "CRFL"), each = 3)
df1$vitalrate <- rep(c("s", "g", "fp"), times = 4)

df1$WindowClose <- ifelse(df1$WindowClose != -12, df1$WindowClose - 1, df1$WindowClose)            ### The month mentioned is the whole month, so not until the start


### Plot best windows ----------------------------------------------------------------------------------------------------------------------------------------------------------------

rects <- data.frame(xstart = seq(0.5,3.5,1), 
                    xend = seq(1.5,4.5,1),
                    col = c("HEQU", "FRSP", "OPIM", "CRFL"))
grow <- data.frame(xstart = rep(rects$xstart, each = 6),
                   xend = rep(rects$xend, each = 6),
                   col = rep(rects$col, each = 6),
                   ymin = c(-12,-2, 10, 22, 34, 46,
                            -12,-2, 10, 22, 34, 46,
                            NA, -5, 7, 19, 31, 43,
                            -12, -1, 11, 23, 35, 47),
                   ymax = c(-11, 1, 13, 25, 37, 48,
                            -11, 1, 13, 25, 37, 48,
                            0, 12, 24, 36, 48, NA,
                            NA, -10, 2, 14, 26, 38)
                   )

s <- df[which(df$vitalrate == "s"),]
s1 <- df1[which(df1$vitalrate == "s"),]

g <- df[which(df$vitalrate == "g"),]
g1 <- df1[which(df1$vitalrate == "g"),]

f <- df[which(df$vitalrate == "fp"),] 
f1 <- df1[which(df1$vitalrate == "fp"),]

f$WindowClose[which(f$species != "FRSP")] <- f$WindowClose[which(f$species != "FRSP")] - 12
f$WindowOpen[which(f$species != "FRSP")] <- f$WindowOpen[which(f$species != "FRSP")] - 12
f1$WindowClose[which(f1$species != "FRSP")] <- f1$WindowClose[which(f1$species != "FRSP")] - 12
f1$WindowOpen[which(f1$species != "FRSP")] <- f1$WindowOpen[which(f1$species != "FRSP")] - 12

Surv <- ggplot() +
  geom_linerange(data= s, aes(ymin = s$WindowClose, ymax = s$WindowOpen, x = s$species), size = 0.8, alpha = 0.6, position = position_dodge2(width = 0.4)) +
  geom_rect(data=grow, aes(ymin=ymin, 
                           ymax= ymax, 
                           xmin=xstart,
                           xmax=xend), 
            alpha =0.3) +
  geom_rect(data=rects, aes(ymin=-12, ymax=24, xmin=xstart,
                            xmax=xend, fill=col), alpha =0.3) +
  geom_linerange(data= s1, aes(ymin = s1$WindowClose, ymax = s1$WindowOpen, x = s1$species), size = 2, colour = c("red", "black", "red", "red")) +
  geom_text(aes(label = c(Hs$combos$climate[Hsurv], Fs$combos$climate[Fsurv], Os$combos$climate[Osurv], Cs$combos$climate[Csurv]), x = c(1:4), y = 45, fontface = 'bold')) +
  coord_flip() +
  scale_y_continuous(breaks = c(-12, 0, 12, 24, 36, 48),
                     labels = c("T+1", "T", "T-1", "T-2", "T-3", "T-4"),
                     name = "Year",
                     limits = c(-12,48)) +
  scale_x_discrete(name = "Survival") +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "grey50"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_blank(),
        legend.position = "none")


Growth <-   ggplot() +
  geom_linerange(data= g, aes(ymin = g$WindowClose, ymax = g$WindowOpen, x = g$species), size = 0.8, alpha = 0.6, position = position_dodge2(width = 0.4)) +
  geom_rect(data=grow, aes(ymin=ymin, 
                           ymax= ymax, 
                           xmin=xstart,
                           xmax=xend), 
            alpha =0.3) +
  geom_rect(data=rects, aes(ymin=-12, ymax=c(24, 48, 24, 24), xmin=xstart,
                            xmax=xend, fill=col), alpha =0.3) +
  geom_linerange(data= g1, aes(ymin = g1$WindowClose, ymax = g1$WindowOpen, x = g1$species), size = 2, colour = c("red", "red", "black", "red")) +
  geom_text(aes(label = c(Hg$combos$climate[Hgrowth], Fg$combos$climate[Fgrowth], Og$combos$climate[Ogrowth], Cg$combos$climate[Cgrowth]), x = c(1:4), y = 45, fontface = 'bold')) +
  coord_flip() +
  scale_y_continuous(breaks = c(-12, 0, 12, 24, 36, 48),
                     labels = c("T+1", "T", "T-1", "T-2", "T-3", "T-4"),
                     name = "Year",
                     limits = c(-12,48)) +
  scale_x_discrete(name = "Growth") +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "grey50"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_blank(),
        legend.position = "none")




Fec <-   ggplot() +
  geom_linerange(data= f, aes(ymin = f$WindowClose, ymax = f$WindowOpen, x = f$species), size = 0.8, alpha = 0.6, position = position_dodge2(width = 0.4)) +
  geom_rect(data=grow, aes(ymin=ymin, 
                           ymax= ymax, 
                           xmin=xstart,
                           xmax=xend), 
            alpha =0.3) +
  geom_rect(data=rects, aes(ymin= -12, ymax=c(24,36,24,24), xmin=xstart,
                            xmax=xend, fill=col), alpha =0.3) +
  geom_linerange(data= f1, aes(ymin = f1$WindowClose, ymax = f1$WindowOpen, x = f1$species), size = 2, colour = "red") +
  geom_text(aes(label = c(Hfp$combos$climate[HpFlwr], Ffp$combos$climate[FpFlwr], Ofp$combos$climate[OpFlwr], Cfp$combos$climate[CpFlwr]), x = c(1:4), y = 45, fontface = 'bold')) +
  coord_flip() +
  scale_y_continuous(breaks = c(-12, 0, 12, 24, 36, 48),
                     labels = c("0", "1", "2", "3", "4", "5"),
                     name = "Year before census of response variable",
                     limits = c(-12,48)) +
  scale_x_discrete(name = "Flower probability") +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_blank(),
        legend.position = "none")


WindowsA <- grid.arrange(Surv, Growth, Fec, ncol = 1)
ggsave("Visual/Best_Windows_w_other95_A.png", WindowsA, height = 10, units = "in")

# source("../../Scrap code/grid_arrange_shared_legend.R")
# 
# WindowsB <- grid_arrange_shared_legend(Surv, Growth, Fec, ncol = 1, nrow = 3, position = "right")
# grid.draw(Windows)
# 
# ggsave("Visual/Best_Windows_w_other95_B.png", WindowsB)
