library(dplyr)
library(lme4)
library(ggplot2)

## Get and prepare data -----------------------------------------------------------------------------------------------------------------------------------------

df <- read.csv("Data/Biol data/FRSP_demography_data.csv")
df$X<- NULL
df <- df[which(df$year != 1972) , ]

###################
###  Survival  ####
###################

SurvPlot <- ggplot(df, aes(log(sizeT), survival)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family="binomial"), se=F)+
  facet_wrap(vars(year), ncol = 7) +
  xlab("log(#rosettes) at time T")

ggsave("Visual/FRSP_Plot_Survival.png", plot = SurvPlot, width = 12, height = 6)
saveRDS(SurvPlot, "Results/FRSP_summary/Survival_facetplot.rds")

#################
###  Growth  ####
#################

GrowthPlot <- ggplot(df, aes(log(sizeT), log(sizeT1))) +
  geom_point() +
  stat_smooth(method = "glm", se=F)+
  facet_wrap(vars(year), ncol = 7)+
  xlab("log(#rosettes) at time T") +
  ylab("log(#rosettes) at time T+1")

ggsave("Visual/FRSP_Plot_Growth.png", plot = GrowthPlot, width = 12, height = 6)
saveRDS(GrowthPlot, "Results/FRSP_summary/Growth_facetplot.rds")


###################
###  Fecundity  ###
###################

PFlower <- ggplot(df, aes(log(sizeT), pFlowerT1)) +
  geom_point()+
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se=F)+
  facet_wrap(vars(year)) +
  xlab("log(#rosettes) at time T") +
  ylab("Probability of Flowering at time T+1")

ggsave("Visual/FRSP_Plot_pFlower.png", plot = PFlower, width = 12, height = 6)
saveRDS(PFlower, "Results/FRSP_summary/pFlower_facetplot.rds")
