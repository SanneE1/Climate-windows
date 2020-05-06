
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(ggpattern)

source("../../52 Scrap code/get_legend.R")
source("../../52 Scrap code/create_dashed.R")

### Get Climwin result files ------------------------------------------------------------------------------------------
source("Analysis/Climwin/Load_Climwin_results.R")

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
grow <- data.frame(xstart = rep(rects$xstart, each = 4),
                   xend = rep(rects$xend, each = 4),
                   col = rep(rects$col, each = 4),
                   ymin = c(-13,-2, 10, 22,
                            -13,-2, 10, 22,
                            -13, -5, 7, 19,
                            -13, -3, 9, 21),
                   ymax = c(-11, 1, 13, 25,
                            -11, 1, 13, 25,
                            -12, 0, 12, 24,
                            -11, 1, 13, 25)
                   )
grow <- rbind(grow,
              data.frame(xstart = rep(1.5, 3),
                         xend = rep(2.5, 3),
                         col = rep("FRSP", 3),
                         ymin = c(34,46,58),
                         ymax = c(37,49,61))
              )


### split up df into different vital rates ----------------------------------------------------------------------------------------------------------------------------------------------------------------


s1 <- df1[which(df1$vitalrate == "s"),]

g1 <- df1[which(df1$vitalrate == "g"),]

f1 <- df1[which(df1$vitalrate == "fp"),]

n1 <- df1[which(df1$vitalrate == "fn"),]

f1$WindowClose[which(f1$species != "FRSP")] <- f1$WindowClose[which(f1$species != "FRSP")] - 12
f1$WindowOpen[which(f1$species != "FRSP")] <- f1$WindowOpen[which(f1$species != "FRSP")] - 12


n1$WindowClose <- n1$WindowClose - 12
n1$WindowOpen <- n1$WindowOpen - 12


### Plot best windows ----------------------------------------------------------------------------------------------------------------------------------------------------------------

Surv <- ggplot() +
  geom_rect_pattern(data=grow, aes(ymin=ymin, 
                           ymax= ymax, 
                           xmin=xstart,
                           xmax=xend,
                           size = 'Growing seasons'), 
                    fill= NA,
                    colour = NA,
                    pattern = "stripe",
                    pattern_spacing = 0.02,
                    pattern_density = 0.1,
                    pattern_fill = "grey20",
                    pattern_colour = "grey20",
                    pattern_size = 0.01,
                    pattern_angle = 45,
            show.legend = FALSE) +
  geom_rect(data=rects, aes(ymin=-13, ymax=c(24,60,24,24), xmin=xstart,
                            xmax=xend, fill= col), 
            alpha =0.3) +
  geom_linerange(data= s1, aes(ymin = s1$WindowClose, ymax = s1$WindowOpen, x = s1$species,  colour = c("red", "grey50", "red", "red")), size = 2,  
                 show.legend = NA) +
  geom_text(aes(label = c("T[mean_min]", "T[mean_min]", "T[avg]", "T[mean_max]"), x = c(1:4), y = 41, fontface = 'bold'), colour = c('#009E73', '#F0E442', '#D55E00', '#CC79A7'),
            size = 4,
            parse = TRUE, 
            show.legend = FALSE) +
  coord_flip() +
  scale_colour_manual(name = "Randomization", values = c('red' = '#0072B2', 'grey50' = '#56B4E9'), labels = c( 'Non-significant', 'Significant')) +
  scale_fill_manual(name = "Time range \nconsidered for", values = c('HEQU' = '#009E73','FRSP' = '#F0E442', 'OPIM' = '#D55E00', 'CRFL' = '#CC79A7') , 
                    labels = c( 'H. quinquenervis','F. speciosa', 'C. imbricata', 'C. flava'),
                    breaks = c('HEQU', 'FRSP', 'OPIM', 'CRFL')) +
  scale_size_manual(name = "", values = c(4), guide = guide_legend(override.aes = list(fill = c("grey20")))) +
  scale_y_continuous(breaks = c(-12.5, -6.5, -0.5, 5.5, 11.5, 17.5, 23.5, 29.5, 35.5, 41.5, 47.5, 53.5, 59.5),
                     labels = c("", "T", "", "T-1", "", "T-2", "", "T-3", "", "T-4", "", "T-5", ""),
                     name = "Year before census of response variable",
                     limits = c(-13, 61)) +
  scale_x_discrete(name = "\nSurvival",
                   labels = c('HEQU' = 'H. \nquinquenervis','FRSP' = 'F. speciosa', 'OPIM' = 'C. imbricata', 'CRFL' = 'C. flava')) +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 11, face = "italic"),
        panel.grid.major.x = element_line(colour = c("grey50", rep(c(NA, "grey50"), 6))),
        panel.grid.minor.x=element_blank(),
        legend.position = "right",
        legend.key = element_blank(),
        legend.background = element_blank()
        )


Growth <-   ggplot() +
  geom_rect_pattern(data=grow, aes(ymin=ymin, 
                                   ymax= ymax, 
                                   xmin=xstart,
                                   xmax=xend,
                                   size = 'Growing seasons'), 
                    fill= NA,
                    colour = NA,
                    pattern = "stripe",
                    pattern_spacing = 0.02,
                    pattern_density = 0.1,
                    pattern_fill = "grey20",
                    pattern_colour = "grey20",
                    pattern_size = 0.01,
                    pattern_angle = 45,
                    show.legend = TRUE) + 
  geom_rect(data=rects, aes(ymin=-13, ymax=c(24,60,24,24), xmin=xstart,
                            xmax=xend, fill=col), 
            alpha =0.3,
            show.legend = FALSE) +
  geom_linerange(data= g1, aes(ymin = g1$WindowClose, ymax = g1$WindowOpen, x = g1$species, colour = c("red", "red", "grey50", "red")), size = 2,
                 show.legend = FALSE) +
  geom_text(aes(label = c("T[mean_min]", "P", "T[avg]", "P[sum]"), x = c(1:4), y = 41, fontface = 'bold'), colour = c('#009E73', '#F0E442', '#D55E00', '#CC79A7'),
            size = 4,
            parse = TRUE, 
            show.legend = FALSE) +
  coord_flip() +
  scale_colour_manual(name = "Window", values = c('red' = '#0072B2', 'grey50' = '#56B4E9'), labels = c( 'Non-significant', 'Significant')) +
  scale_fill_manual(name = "Range for", values = c('HEQU' = '#009E73','FRSP' = '#F0E442', 'OPIM' = '#D55E00', 'CRFL' = '#CC79A7') , 
                    labels = c( 'H. quinquenervis','F. speciosa', 'C. imbricata', 'C. flava'),
                    breaks = c('HEQU', 'FRSP', 'OPIM', 'CRFL')) +
  scale_size_manual(name = "", values = c(4), guide = guide_legend(override.aes = list(fill = c("grey20")))) +
  scale_y_continuous(breaks = c(-12.5, -6.5, -0.5, 5.5, 11.5, 17.5, 23.5, 29.5, 35.5, 41.5, 47.5, 53.5, 59.5),
                     labels = c("", "T", "", "T-1", "", "T-2", "", "T-3", "", "T-4", "", "T-5", ""),
                     name = "Year before census of response variable",
                     limits = c(-13, 61)) +
  scale_x_discrete(name = "\nGrowth",
                   labels = c('HEQU' = 'H. \nquinquenervis','FRSP' = 'F. speciosa', 'OPIM' = 'C. imbricata', 'CRFL' = 'C. flava')) +
  guides(fill = guide_legend(order = 1),
         colour = guide_legend(order = 2),
         size = guide_legend(order = 3)) +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 11, face = "italic"),
        panel.grid.major.x = element_line(colour = c("grey50", rep(c(NA, "grey50"), 6))),
        panel.grid.minor.x=element_blank(),
        legend.position = "right",
        legend.key = element_blank(),
        legend.background = element_blank()
  ) + guides(fill = FALSE,
             colour = FALSE,
             size = guide_legend(override.aes = list(pattern_spacing = 0.01)))




FlwrProb <-   ggplot() +
  geom_rect_pattern(data=grow[-c(18,19),], aes(ymin=ymin, 
                                   ymax= ymax, 
                                   xmin=xstart,
                                   xmax=xend,
                                   size = 'Growing seasons'), 
                    fill= NA,
                    colour = NA,
                    pattern = "stripe",
                    pattern_spacing = 0.02,
                    pattern_density = 0.1,
                    pattern_fill = "grey20",
                    pattern_colour = "grey20",
                    pattern_size = 0.01,
                    pattern_angle = 45,
                    show.legend = TRUE) +
  geom_rect(data=rects, aes(ymin= -13, ymax=c(24,36,24,24), xmin=xstart,
                            xmax=xend, fill=col), 
            alpha =0.3) +
  geom_linerange(data= f1, aes(ymin = f1$WindowClose, ymax = f1$WindowOpen, x = f1$species, colour = "red"), size = 2,
                 show.legend = NA) +
  geom_text(aes(label = c("SPEI", "T[mean_max]", "T[mean_min]", "SPEI"), x = c(1:4), y = 41, fontface = 'bold'), colour = c('#009E73', '#F0E442', '#D55E00', '#CC79A7'),
            size = 4,
            parse = TRUE, 
            show.legend = FALSE) +
  coord_flip() +
  scale_colour_manual(name = "Window", values = c('red' = '#0072B2', 'grey50' = '#56B4E9'), labels = c( 'Non-significant', 'Significant')) +
  scale_fill_manual(name = "Range for", values = c('HEQU' = '#009E73','FRSP' = '#F0E442', 'OPIM' = '#D55E00', 'CRFL' = '#CC79A7') , 
                    labels = c( 'H. quinquenervis','F. speciosa', 'C. imbricata', 'C. flava'),
                    breaks = c('HEQU', 'FRSP', 'OPIM', 'CRFL')) +
  scale_size_manual(name = "", values = c(4), guide = guide_legend(override.aes = list(fill = c("grey20")))) +
  scale_y_continuous(breaks = c(-12.5, -6.5, -0.5, 5.5, 11.5, 17.5, 23.5, 29.5, 35.5, 41.5, 47.5, 53.5, 59.5),
                     labels = c("", "T", "", "T-1", "", "T-2", "", "T-3", "", "T-4", "", "T-5", ""),
                     name = "Year before census of response variable",
                     limits = c(-13, 61)) +
  scale_x_discrete(name = "Flower \nprobability",
                   labels = c('HEQU' = 'H. \nquinquenervis','FRSP' = 'F. speciosa', 'OPIM' = 'C. imbricata', 'CRFL' = 'C. flava')) +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 11, face = "italic"),
        panel.grid.major.x = element_line(colour = c("grey50", rep(c(NA, "grey50"), 6))),
        panel.grid.minor.x=element_blank()
  )


FlwrNum <-   ggplot() +
  geom_rect_pattern(data=grow[-c(18,19),], aes(ymin=ymin, 
                                   ymax= ymax, 
                                   xmin=xstart,
                                   xmax=xend,
                                   size = 'Growing seasons'), 
                    fill= NA,
                    colour = NA,
                    pattern = "stripe",
                    pattern_spacing = 0.02,
                    pattern_density = 0.1,
                    pattern_fill = "grey20",
                    pattern_colour = "grey20",
                    pattern_size = 0.01,
                    pattern_angle = 45,
                    show.legend = TRUE) +
  geom_rect(data=rects, aes(ymin= -13, ymax=c(24,36,24,24), xmin=xstart,
                            xmax=xend, fill=col), 
            alpha =0.3) +
  geom_linerange(data= n1, aes(ymin = n1$WindowClose, ymax = n1$WindowOpen, x = n1$species, colour = "red"), size = 2,
                 show.legend = FALSE) +
  geom_text(aes(label = c("P", "T[avg]", "T[avg]", "SPEI"), x = c(1:4), y = 41, fontface = 'bold'), colour = c('#009E73', '#F0E442', '#D55E00', '#CC79A7'),
            size = 4,
            parse = TRUE, 
            show.legend = FALSE) +
  coord_flip() +
  scale_colour_manual(name = "Window", values = c('red' = '#0072B2', 'grey50' = '#56B4E9'), labels = c( 'Non-significant', 'Significant')) +
  scale_fill_manual(name = "Range for", values = c('HEQU' = '#009E73','FRSP' = '#F0E442', 'OPIM' = '#D55E00', 'CRFL' = '#CC79A7') , 
                    labels = c( 'H. quinquenervis','F. speciosa', 'C. imbricata', 'C. flava'),
                    breaks = c('HEQU', 'FRSP', 'OPIM', 'CRFL')) +
  scale_size_manual(name = "", values = c(4), guide = guide_legend(override.aes = list(fill = c("grey20")))) +
  scale_y_continuous(breaks = c(-12.5, -6.5, -0.5, 5.5, 11.5, 17.5, 23.5, 29.5, 35.5, 41.5, 47.5, 53.5, 59.5),
                     labels = c("", "t", "", "t-1", "", "t-2", "", "t-3", "", "t-4", "", "t-5", ""),
                     name = "Year before census of response variable",
                     limits = c(-13, 61)) +
  scale_x_discrete(name = "Inflorescence  \nnumbers",
                   labels = c('HEQU' = 'H. \nquinquenervis','FRSP' = 'F. speciosa', 'OPIM' = 'C. imbricata', 'CRFL' = 'C. flava')) +
  theme(panel.background = element_blank(),
        # axis.title.x = element_blank(),
        # axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 11, face = "italic"),
        panel.grid.major.x = element_line(colour = c("grey50", rep(c(NA, "grey50"), 6))),
        panel.grid.minor.x=element_blank()
  )

## Plot all 4 together -------------------------------------------------------------------------------------------------------

WindowsC <- (Surv / Growth / FlwrProb / FlwrNum) + plot_layout(guides = "collect")
ggsave("Visual/Best_Windows.png", WindowsC, width = 14, height = 7, units = "in", dpi = 450)

