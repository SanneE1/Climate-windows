
library(dplyr)
library(lme4)
library(ggplot2)
library(ggiraphExtra)

## Get and prepare data -----------------------------------------------------------------------------------------------------------------------------------------

df <- read.csv("Data/Biol data/CRFL_demography_data.csv") %>%
  mutate(sizeT = log(sizeT),
         sizeT1 = log(sizeT1))

df <- df[which(df$Treatment == "C"),]
df <- df[which(df$year %in% c(1997:2001,2003:2011)),]

###################
###  Survival  ####
###################

SurvPlot <- ggplot(df, aes(sizeT, survival)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family="binomial"), se=F)+
  facet_wrap(vars(year), ncol = 7) +
  xlab("log(# of rosettes) at time T")

# ggsave("Visual/CRFL_Plot_Survival.png", plot = SurvPlot, width = 12, height = 6)
saveRDS(SurvPlot, "Results/CRFL_summary/Survival_facetplot.rds")

#################
###  Growth  ####
#################

GrowthPlot <- ggplot(df, aes(sizeT, sizeT1)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family="poisson"), se=F)+
  facet_wrap(vars(year), ncol = 7)+
  xlab("log(# of rosettes) at time T")

# ggsave("Visual/CRFL_Plot_Growth.png", plot = GrowthPlot, width = 12, height = 6)
saveRDS(GrowthPlot, "Results/CRFL_summary/Growth_facetplot.rds")


###################
###  Fecundity  ###
###################


