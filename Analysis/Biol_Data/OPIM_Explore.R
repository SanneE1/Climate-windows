library(dplyr)
library(lme4)
library(ggplot2)

## Get and prepare data -----------------------------------------------------------------------------------------------------------------------------------------

df <- read.csv("Data/Biol data/OPIM_demography_data.csv")
df$X<- NULL

###################
###  Survival  ####
###################

SurvPlot <- ggplot(df, aes(log(sizeT), survival)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family="binomial"), se=F)+
  facet_wrap(vars(year), ncol = 7) +
  xlab("log(volume(cm3)) at time T")

ggsave("Visual/OPIM_Plot_Survival.png", plot = SurvPlot, width = 12, height = 6)
saveRDS(SurvPlot, "Results/OPIM_summary/Survival_facetplot.rds")

#################
###  Growth  ####
#################

GrowthPlot <- ggplot(df, aes(log(sizeT), log(sizeT1))) +
  geom_point() +
  stat_smooth(method = "glm", se=F)+
  facet_wrap(vars(year), ncol = 7)+
  xlab("log(volume(cm3)) at time T") +
  ylab("log(volume(cm3)) at time T+1")

ggsave("Visual/OPIM_Plot_Growth.png", plot = GrowthPlot, width = 12, height = 6)
saveRDS(GrowthPlot, "Results/OPIM_summary/Growth_facetplot.rds")


###################
###  Fecundity  ###
###################

PFlower <- ggplot(df, aes(log(sizeT), pflowerT)) +
  geom_point()+
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se=F)+
  facet_wrap(vars(year)) +
  xlab("log(volume(cm3)) at time T") +
  ylab("Probability of Flowering")

ggsave("Visual/OPIM_Plot_pFlower.png", plot = PFlower, width = 12, height = 6)
saveRDS(PFlower, "Results/OPIM_summary/pFlower_facetplot.rds")

Fertility <- ggplot(df[which(df$pflowerT == 1),], aes(log(sizeT), fertilityT))+
  geom_point()+
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=F)+
  facet_wrap(vars(year))+
  xlab("log(volume(cm3)) at time T") +
  ylab("Numbers of flowers produced if flowering")

ggsave("Visual/OPIM_Plot_nFlower.png", plot = Fertility, width = 12, height = 6)
saveRDS(Fertility, "Results/OPIM_summary/nFlower_facetplot.rds")

