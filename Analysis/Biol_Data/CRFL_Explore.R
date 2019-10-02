
library(dplyr)
library(lme4)
library(ggplot2)
library(ggiraphExtra)

## Get and prepare data -----------------------------------------------------------------------------------------------------------------------------------------

df <- read.csv("Data/Biol data/CRFL_demography_data.csv") %>%
  mutate(sizeT = as.integer(sizeT),
         sizeT1 = as.integer(sizeT1))

df <- df[which(df$Treatment == "C"),]
df <- df[which(df$year %in% c(1997:2000,2003:2011)),]

###################
###  Survival  ####
###################

SurvPlot <- ggplot(df, aes(log(sizeT), survival)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family="binomial"), se=F)+
  facet_wrap(vars(year), ncol = 7) +
  xlab("log(# of rosettes) at time T")

ggsave("Visual/CRFL_Plot_Survival.png", plot = SurvPlot, width = 12, height = 6)
# saveRDS(SurvPlot, "Results/CRFL_summary/Survival_facetplot.rds")

#################
###  Growth  ####
#################

GrowthPlot <- ggplot(df, aes(log(sizeT), log(sizeT1))) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family="poisson"), se=F)+
  facet_wrap(vars(year), ncol = 7)+
  xlab("log(# of rosettes) at time T") +
  ylab("log(# of rosettes) at time T+1")

ggsave("Visual/CRFL_Plot_Growth.png", plot = GrowthPlot, width = 12, height = 6)
# saveRDS(GrowthPlot, "Results/CRFL_summary/Growth_facetplot.rds")


###################
###  Fecundity  ###
###################

PFlower <- ggplot(df, aes(log(sizeT), pflowerT)) +
  geom_point()+
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se=F)+
  facet_wrap(vars(year)) +
  xlab("log(# of rosettes) at time T") +
  ylab("Probability of Flowering")

ggsave("Visual/HEQU_Plot_pFlower.png", plot = PFlower, width = 12, height = 6)
# saveRDS(PFlower, "Results/HEQU_summary/pFlower_facetplot.rds")

Fertility <- ggplot(df[which(df$pflowerT == 1),], aes(log(sizeT), fertilityT))+
  geom_point()+
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=F)+
  facet_wrap(vars(year))+
  xlab("log(# of rosettes) at time T") +
  ylab("Numbers of flowers produced if flowering")

ggsave("Visual/HEQU_Plot_nFlower.png", plot = Fertility, width = 12, height = 6)
# saveRDS(Fertility, "Results/HEQU_summary/nFlower_facetplot.rds")

