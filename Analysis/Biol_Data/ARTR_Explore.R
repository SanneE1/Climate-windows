library(dplyr)
library(ggplot2)
library(lme4)

Biol <- read.csv("Data/Biol data/ARTR_demography_data.csv")
Biol$X <- NULL
Biol <- Biol[which(Biol$seedling != 1),]  ### Remove seedlings

###################
###  Survival  ####
###################

SurvPlot <- ggplot(Biol, aes(lnsizeT, survival)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family="binomial"), se=F)+
  facet_wrap(~ year) +
  labs(x = "log(area) at time T") 

ggsave("Visual/ARTR_Plot_Survival.png", plot = SurvPlot, width = 12, height = 6)
saveRDS(SurvPlot, "Results/ARTR_summary/Survival_facetplot.rds")

surv <- glmer(survival ~ lnsizeT + (1|year) + (1|quad), data = Biol, family = binomial)

summary(surv)


#################
###  Growth  ####
#################

GrowthPlot <- ggplot(Biol, aes(lnsizeT, lnsizeT1)) +
  geom_point() +
  stat_smooth(method = "glm", se=F)+
  facet_wrap(~ year)+
  labs(x = "log(area) at time T", y = "log(area) at time T")

ggsave("Visual/ARTR_Plot_Growth.png", plot = GrowthPlot, width = 12, height = 6)
saveRDS(GrowthPlot, "Results/ARTR_summary/Growth_facetplot.rds")


growth <- lmer(lnsizeT1 ~ lnsizeT + (1|year) + (1|quad), data = Biol)
summary(growth)
