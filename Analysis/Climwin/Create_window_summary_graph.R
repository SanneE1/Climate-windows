
library(dplyr)
library(tidyr)
library(ggplot2)
library(climwin)
library(gridExtra)
library(grid)
library(patchwork)

source("../../Scrap code/get_legend.R")
source("../../Scrap code/create_dashed.R")

### Get Climwin result files ------------------------------------------------------------------------------------------
### HEQU
Hs <- readRDS("Results/Climwin/HEQU_s_month_result.rds")
Hg <- readRDS("Results/Climwin/HEQU_g_month_result.rds")
Hfp <- readRDS("Results/Climwin/HEQU_fp_month_result.rds")
Hfn <- readRDS("Results/Climwin/HEQU_fn_month_result.rds")

Hsr <- readRDS("Results/Climwin/HEQU_s_month_random.rds")
Hgr <- readRDS("Results/Climwin/HEQU_g_month_random.rds")
Hfpr <- readRDS("Results/Climwin/HEQU_fp_month_random.rds")
Hfnr <- readRDS("Results/Climwin/HEQU_fn_month_random.rds")


### CRFL
Cs <- readRDS("Results/Climwin/CRFL_s_month_result.rds")
Cg <- readRDS("Results/Climwin/CRFL_g_month_result.rds")
Cfp <- readRDS("Results/Climwin/CRFL_fp_month_result.rds")
Cfn <- readRDS("Results/Climwin/CRFL_fn_month_result.rds")

Csr <- readRDS("Results/Climwin/CRFL_s_month_random.rds")
Cgr <- readRDS("Results/Climwin/CRFL_g_month_random.rds")
Cfpr <- readRDS("Results/Climwin/CRFL_fp_month_random4.rds")
Cfnr <- readRDS("Results/Climwin/CRFL_fn_month_random.rds")

### OPIM
Os <- readRDS("Results/Climwin/OPIM_s_month_result.rds")
Og <- readRDS("Results/Climwin/OPIM_g_month_result.rds")
Ofp <- readRDS("Results/Climwin/OPIM_fp_month_result.rds")
Ofn <- readRDS("Results/Climwin/OPIM_fn_month_result.rds")

Osr <- readRDS("Results/Climwin/OPIM_s_month_random.rds")
Ogr <- readRDS("Results/Climwin/OPIM_g_month_random.rds")
Ofpr <- readRDS("Results/Climwin/OPIM_fp_month_random.rds")
Ofnr <- readRDS("Results/Climwin/OPIM_fn_month_random.rds")

### FRSP
Fs <- readRDS("Results/Climwin/FRSP_s_month_result.rds")
Fg <- readRDS("Results/Climwin/FRSP_g_month_result_5yrs.rds")
Ffp <- readRDS("Results/Climwin/FRSP_fp_month_result.rds")
Ffn <- readRDS("Results/Climwin/FRSP_fn_month_result.rds")

Fsr <- readRDS("Results/Climwin/FRSP_s_month_random.rds")
Fgr <- readRDS("Results/Climwin/FRSP_g_month_random_5yrs.rds")
Ffpr <- readRDS("Results/Climwin/FRSP_fp_month_random.rds")
Ffnr <- readRDS("Results/Climwin/FRSP_fn_month_random.rds")

### Set winners ---------------------------------------------------------------------------------------------------------------------------------------------
Hsurv <- 13
Hgrowth <- 2
HpFlwr <- 16
HnFlwr <-12 

Fsurv <- 12
Fgrowth <- 10
FpFlwr <- 14
FnFlwr <- 2

Csurv <- 16
Cgrowth <- 1
CpFlwr <- 4
CnFlwr <- 4

Osurv <- 3
Ogrowth <- 12
OpFlwr <- 2
OnFlwr <- 3


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

m <- getwindow(Hfn[[HnFlwr]]$Dataset, "HEQU", "fn")
n <- getwindow(Ffn[[FnFlwr]]$Dataset, "FRSP", "fn")
o <- getwindow(Ofn[[OnFlwr]]$Dataset, "OPIM", "fn")
p <- getwindow(Cfn[[CnFlwr]]$Dataset, "CRFL", "fn")
m$climate <- Hfn$combos$climate[HnFlwr]
n$climate <- Ffn$combos$climate[FnFlwr]
o$climate <- Ofn$combos$climate[OnFlwr]
p$climate <- Cfn$combos$climate[CnFlwr]



df <- bind_rows(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
df$species <- factor(df$species, levels = c("HEQU", "FRSP", "OPIM", "CRFL"))
df$climate <- factor(df$climate)

df$WindowClose <- df$WindowClose - 1

### Get best windows ---------------------------------------------------------------------------------------------------------------------------------------------------------------

df1 <- bind_rows(c(Hs[[Hsurv]]$Dataset[1,c("WindowClose", "WindowOpen"),1], driver = Hs$combos$climate[Hsurv]),
             c(Hg[[Hgrowth]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Hg$combos$climate[Hgrowth]),
             c(Hfp[[HpFlwr]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Hfp$combos$climate[HpFlwr]),
             c(Hfn[[HnFlwr]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Hfn$combos$climate[HnFlwr]),
             c(Fs[[Fsurv]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Fs$combos$climate[Fsurv]),
             c(Fg[[Fgrowth]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Fg$combos$climate[Fgrowth]),
             c(Ffp[[FpFlwr]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Ffp$combos$climate[FpFlwr]),
             c(Ffn[[FnFlwr]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Ffn$combos$climate[FnFlwr]),
             c(Os[[Osurv]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Os$combos$climate[Osurv]),
             c(Og[[Ogrowth]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Og$combos$climate[Ogrowth]),
             c(Ofp[[OpFlwr]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Ofp$combos$climate[OpFlwr]),
             c(Ofn[[OnFlwr]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Ofn$combos$climate[OnFlwr]),
             c(Cs[[Csurv]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Cs$combos$climate[Csurv]),
             c(Cg[[Cgrowth]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Cg$combos$climate[Cgrowth]),
             c(Cfp[[CpFlwr]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Cfp$combos$climate[CpFlwr]),
             c(Cfn[[CnFlwr]]$Dataset[1,c("WindowClose", "WindowOpen")], driver = Cfn$combos$climate[CnFlwr]))

df1$species <- rep(c("HEQU", "FRSP", "OPIM", "CRFL"), each = 4)
df1$vitalrate <- rep(c("s", "g", "fp", "fn"), times = 4)
df1$species <- factor(df1$species, levels = c("HEQU", "FRSP", "OPIM", "CRFL"))

df1$WindowClose <- df1$WindowClose - 1           ### The month mentioned is the whole month, so not until the start

####################################################################### 
# WINDOWS CLOSE ONE MONTH LATER, AS THE WHOLE MONTH IS INCLUDED       #
# YEAR LINES -> ARE IN THE MIDDLE OF THE CENSUS MONTH!!!              #
#######################################################################

### get dfs needed for shading or range and growing season ----------------------------------------------------------------------------------------------------------------------------------------------------------------

rects <- data.frame(xstart = seq(0.5,3.5,1), 
                    xend = seq(1.5,4.5,1),
                    col = c("HEQU", "FRSP", "OPIM", "CRFL"))
grow <- data.frame(xstart = rep(rects$xstart, each = 6),
                   xend = rep(rects$xend, each = 6),
                   col = rep(rects$col, each = 6),
                   ymin = c(-13,-2, 10, 22, 34, 46,
                            -13,-2, 10, 22, 34, 46,
                            -13, -5, 7, 19, 31, 43,
                            -13, -1, 11, 23, 35, 47),
                   ymax = c(-11, 1, 13, 25, 37, 49,
                            -11, 1, 13, 25, 37, 49,
                            -12, 0, 12, 24, 36, 48, 
                            -10, 2, 14, 26, 38, 49)
                   )

### split up df into different vital rates ----------------------------------------------------------------------------------------------------------------------------------------------------------------


s <- df[which(df$vitalrate == "s"),]
s1 <- df1[which(df1$vitalrate == "s"),]

g <- df[which(df$vitalrate == "g"),]
g1 <- df1[which(df1$vitalrate == "g"),]

f <- df[which(df$vitalrate == "fp"),] 
f1 <- df1[which(df1$vitalrate == "fp"),]

n <- df[which(df$vitalrate == "fn"),] 
n1 <- df1[which(df1$vitalrate == "fn"),]

f$WindowClose[which(f$species != "FRSP")] <- f$WindowClose[which(f$species != "FRSP")] - 12
f$WindowOpen[which(f$species != "FRSP")] <- f$WindowOpen[which(f$species != "FRSP")] - 12
f1$WindowClose[which(f1$species != "FRSP")] <- f1$WindowClose[which(f1$species != "FRSP")] - 12
f1$WindowOpen[which(f1$species != "FRSP")] <- f1$WindowOpen[which(f1$species != "FRSP")] - 12

n$WindowClose <- n$WindowClose - 12
n$WindowOpen <- n$WindowOpen - 12
n1$WindowClose <- n1$WindowClose - 12
n1$WindowOpen <- n1$WindowOpen - 12


### Plot best windows ----------------------------------------------------------------------------------------------------------------------------------------------------------------

Surv <- ggplot() +
  geom_rect(data=grow, aes(ymin=ymin, 
                           ymax= ymax, 
                           xmin=xstart,
                           xmax=xend,
                           size = 'Growing seasons'), 
            alpha =0.3,
            fill = NA,
            show.legend = TRUE) +
  geom_rect(data=rects, aes(ymin=-13, ymax=24, xmin=xstart,
                            xmax=xend, fill= col), 
            alpha =0.3) +
  geom_linerange(data= s1, aes(ymin = s1$WindowClose, ymax = s1$WindowOpen, x = s1$species,  colour = c("red", "grey50", "red", "red")), size = 2,  
                 show.legend = NA) +
  geom_text(aes(label = c("T[obs]", "T[mean_max]", "T[avg]", "T[max]"), x = c(1:4), y = 41, fontface = 'bold'), colour = c('#009E73', '#F0E442', '#D55E00', '#CC79A7'),
            size = 4,
            parse = TRUE, 
            show.legend = FALSE) +
  coord_flip() +
  scale_colour_manual(name = "Window", values = c('red' = '#0072B2', 'grey50' = '#56B4E9'), labels = c( 'Non-significant', 'Significant')) +
  scale_fill_manual(name = "Range for", values = c('HEQU' = '#009E73','FRSP' = '#F0E442', 'OPIM' = '#D55E00', 'CRFL' = '#CC79A7') , 
                    labels = c( 'H. quinquenervis','F. speciosa', 'O. imbricata', 'C. flava'),
                    breaks = c('HEQU', 'FRSP', 'OPIM', 'CRFL')) +
  scale_size_manual(name = "", values = c(4), guide = guide_legend(override.aes = list(fill = c("grey50")))) +
  scale_y_continuous(breaks = c(-12.5, -6.5, -0.5, 5.5, 11.5, 17.5, 23.5, 29.5, 35.5, 41.5, 47.5),
                     labels = c("", "T", "", "T-1", "", "T-2", "", "T-3", "", "T-4", ""),
                     name = "Year before census of response variable",
                     limits = c(-13, 49)) +
  scale_x_discrete(name = "\nSurvival",
                   labels = c('HEQU' = 'H. \nquinquenervis','FRSP' = 'F. speciosa', 'OPIM' = 'O. imbricata', 'CRFL' = 'C. flava')) +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 11, face = "italic"),
        panel.grid.major.x = element_line(colour = c("grey50", rep(c(NA, "grey50"), 5))),
        panel.grid.minor.x=element_blank(),
        legend.position = "right",
        legend.key = element_blank(),
        legend.background = element_blank()
        ) 


Growth <-   ggplot() +
  geom_rect(data=grow, aes(ymin=ymin, 
                           ymax= ymax, 
                           xmin=xstart,
                           xmax=xend,
                           size = 'Growing seasons'), 
            alpha =0.3,
            fill = 'grey50',
            show.legend = TRUE) +
  geom_rect(data=rects, aes(ymin=-13, ymax=c(24, 48, 24, 24), xmin=xstart,
                            xmax=xend, fill=col), 
            alpha =0.3) +
  geom_linerange(data= g1, aes(ymin = g1$WindowClose, ymax = g1$WindowOpen, x = g1$species, colour = c("red", "red", "grey50", "red")), size = 2,
                 show.legend = NA) +
  geom_text(aes(label = c("T[mean_min]", "Prcp", "T[mean_max]", "Prcp"), x = c(1:4), y = 41, fontface = 'bold'), colour = c('#009E73', '#F0E442', '#D55E00', '#CC79A7'),
            size = 4,
            parse = TRUE, 
            show.legend = FALSE) +
  coord_flip() +
  scale_colour_manual(name = "Window", values = c('red' = '#0072B2', 'grey50' = '#56B4E9'), labels = c( 'Non-significant', 'Significant')) +
  scale_fill_manual(name = "Range for", values = c('HEQU' = '#009E73','FRSP' = '#F0E442', 'OPIM' = '#D55E00', 'CRFL' = '#CC79A7') , 
                    labels = c( 'H. quinquenervis','F. speciosa', 'O. imbricata', 'C. flava'),
                    breaks = c('HEQU', 'FRSP', 'OPIM', 'CRFL')) +
  scale_size_manual(name = "", values = c(4), guide = guide_legend(override.aes = list(fill = c("grey50")))) +
  scale_y_continuous(breaks = c(-12.5, -6.5, -0.5, 5.5, 11.5, 17.5, 23.5, 29.5, 35.5, 41.5, 47.5),
                     labels = c("", "T", "", "T-1", "", "T-2", "", "T-3", "", "T-4", ""),
                     name = "Year before census of response variable",
                     limits = c(-13, 49)) +
  scale_x_discrete(name = "\nGrowth",
                   labels = c('HEQU' = 'H. \nquinquenervis','FRSP' = 'F. speciosa', 'OPIM' = 'O. imbricata', 'CRFL' = 'C. flava')) +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 11, face = "italic"),
        panel.grid.major.x = element_line(colour = c("grey50", rep(c(NA, "grey50"), 5))),
        panel.grid.minor.x=element_blank()
  )




FlwrProb <-   ggplot() +
  geom_rect(data=grow, aes(ymin=ymin, 
                           ymax= ymax, 
                           xmin=xstart,
                           xmax=xend,
                           size = 'Growing seasons'), 
            alpha =0.3,
            fill = 'grey50',
            show.legend = TRUE) +
  geom_rect(data=rects, aes(ymin= -13, ymax=c(24,36,24,24), xmin=xstart,
                            xmax=xend, fill=col), 
            alpha =0.3) +
  geom_linerange(data= f1, aes(ymin = f1$WindowClose, ymax = f1$WindowOpen, x = f1$species, colour = "red"), size = 2,
                 show.legend = NA) +
  geom_text(aes(label = c("T[max]", "T[max]", "T[mean_min]", "SPEI"), x = c(1:4), y = 41, fontface = 'bold'), colour = c('#009E73', '#F0E442', '#D55E00', '#CC79A7'),
            size = 4,
            parse = TRUE, 
            show.legend = FALSE) +
  coord_flip() +
  scale_colour_manual(name = "Window", values = c('red' = '#0072B2', 'grey50' = '#56B4E9'), labels = c( 'Non-significant', 'Significant')) +
  scale_fill_manual(name = "Range for", values = c('HEQU' = '#009E73','FRSP' = '#F0E442', 'OPIM' = '#D55E00', 'CRFL' = '#CC79A7') , 
                    labels = c( 'H. quinquenervis','F. speciosa', 'O. imbricata', 'C. flava'),
                    breaks = c('HEQU', 'FRSP', 'OPIM', 'CRFL')) +
  scale_size_manual(name = "", values = c(4), guide = guide_legend(override.aes = list(fill = c("grey50")))) +
  scale_y_continuous(breaks = c(-12.5, -6.5, -0.5, 5.5, 11.5, 17.5, 23.5, 29.5, 35.5, 41.5, 47.5),
                     labels = c("", "T", "", "T-1", "", "T-2", "", "T-3", "", "T-4", ""),
                     name = "Year before census of response variable",
                     limits = c(-13, 49)) +
  scale_x_discrete(name = "Flower \nprobability",
                   labels = c('HEQU' = 'H. \nquinquenervis','FRSP' = 'F. speciosa', 'OPIM' = 'O. imbricata', 'CRFL' = 'C. flava')) +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 11, face = "italic"),
        panel.grid.major.x = element_line(colour = c("grey50", rep(c(NA, "grey50"), 5))),
        panel.grid.minor.x=element_blank()
  )


FlwrNum <-   ggplot() +
  geom_rect(data=grow, aes(ymin=ymin, 
                           ymax= ymax, 
                           xmin=xstart,
                           xmax=xend,
                           size = 'Growing seasons'), 
            alpha =0.3,
            fill = 'grey50',
            show.legend = TRUE) +
  geom_rect(data=rects, aes(ymin= -13, ymax=c(24,36,24,24), xmin=xstart,
                            xmax=xend, fill=col), 
            alpha =0.3) +
  geom_linerange(data= n1, aes(ymin = n1$WindowClose, ymax = n1$WindowOpen, x = n1$species, colour = "red"), size = 2,
                 show.legend = FALSE) +
  geom_text(aes(label = c("Prcp", "T[mean_min]", "T[avg]", "SPEI"), x = c(1:4), y = 41, fontface = 'bold'), colour = c('#009E73', '#F0E442', '#D55E00', '#CC79A7'),
            size = 4,
            parse = TRUE, 
            show.legend = FALSE) +
  coord_flip() +
  scale_colour_manual(name = "Window", values = c('red' = '#0072B2', 'grey50' = '#56B4E9'), labels = c( 'Non-significant', 'Significant')) +
  scale_fill_manual(name = "Range for", values = c('HEQU' = '#009E73','FRSP' = '#F0E442', 'OPIM' = '#D55E00', 'CRFL' = '#CC79A7') , 
                    labels = c( 'H. quinquenervis','F. speciosa', 'O. imbricata', 'C. flava'),
                    breaks = c('HEQU', 'FRSP', 'OPIM', 'CRFL')) +
  scale_size_manual(name = "", values = c(4), guide = guide_legend(override.aes = list(fill = c("grey50")))) +
  scale_y_continuous(breaks = c(-12.5, -6.5, -0.5, 5.5, 11.5, 17.5, 23.5, 29.5, 35.5, 41.5, 47.5),
                     labels = c("", "T", "", "T-1", "", "T-2", "", "T-3", "", "T-4", ""),
                     name = "Year before census of response variable",
                     limits = c(-13, 49)) +
  scale_x_discrete(name = "Inflorescence  \nnumbers",
                   labels = c('HEQU' = 'H. \nquinquenervis','FRSP' = 'F. speciosa', 'OPIM' = 'O. imbricata', 'CRFL' = 'C. flava')) +
  theme(panel.background = element_blank(),
        # axis.title.x = element_blank(),
        # axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 11, face = "italic"),
        panel.grid.major.x = element_line(colour = c("grey50", rep(c(NA, "grey50"), 5))),
        panel.grid.minor.x=element_blank()
  )


### Grow period hacked to dashes -------------------------------------------------------------------------------------------------

legend <- get_legend(Surv)
legend$grobs[[3]]$grobs[[4]] <- patternGrob(
  y = legend$grobs[[3]]$grobs[[4]]$y,
  x = legend$grobs[[3]]$grobs[[4]]$x,
  width = legend$grobs[[3]]$grobs[[4]]$width,
  height = legend$grobs[[3]]$grobs[[4]]$height,
  pattern = 1, granularity = unit(2.5, "mm"),
  gp = gpar(fill = NA))
Surv <- Surv + theme(legend.position = "none")

Surv <- dashing(Surv)
Growth <- dashing(Growth)
FlwrProb <- dashing(FlwrProb)
FlwrNum <- dashing(FlwrNum)

# WindowsB <- grid.arrange(Surv, Growth, FlwrProb, FlwrNum, ncol = 1)
# ggsave("Visual/Best_Windows_w_other95_B.png", WindowsB, width = 15, height = 10, units = "in")

WindowsC <- arrangeGrob(Surv, Growth, FlwrProb, FlwrNum, legend = legend, ncol = 1, heights = c(3,3,3,3.3))

WindowsC

ggsave("Visual/Best_Windows_w_other95_C.png", WindowsC, width = 14, height = 7, units = "in", dpi = 450)
