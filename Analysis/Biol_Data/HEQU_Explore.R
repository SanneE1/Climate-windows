
library(dplyr)
library(lme4)
library(ggplot2)
library(ggiraphExtra)

## Get and prepare data -----------------------------------------------------------------------------------------------------------------------------------------

df <- read.csv("Data/Biol data/HEQU_demography_data.csv") %>%
  mutate(sizeT = as.integer(levels(sizeT))[sizeT],
         sizeT1 = as.integer(levels(sizeT1))[sizeT1],
         population_f = factor(population, levels = c("high", "mid","low"))
         )

adult <- df[which(df$seedling != 1),]
adult$abort.stalks[which(adult$pflowerT==1)] <- ifelse(is.na(adult$abort.stalks[which(adult$pflowerT==1)]), 0, adult$abort.stalks[which(adult$pflowerT==1)])
adult$pAbort <- adult$abort.stalks / adult$fertilityT
adult$success <- as.integer(adult$fertilityT - adult$abort.stalks)
adult <- subset(adult, adult$year!= 2012)

sdl <- df[which(df$seedling ==1),]
sdl$survival[which(is.na(sdl$survival))] <- 0


## Survival plot -----------------------------------------------------------------------------------------------------------------------------------------

SurvPlot <- ggplot(adult, aes(sizeT, survival)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family="binomial"), se=F)+
  facet_grid( vars(population_f), vars(year)) +
  xlab("log(# of leaf clumps) at time T")

# ggsave("Visual/HEQU_Plot_Survival.png", plot = SurvPlot, width = 12, height = 6)
saveRDS(SurvPlot, "Results/HEQU_summary/Survival_facetplot.rds")


#################
###  Growth  ####
#################

GrowthPlot <- ggplot(adult, aes(sizeT, sizeT1)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family="poisson"), se=F)+
  facet_grid( vars(population_f), vars(year))+
  xlab("log(# of leaf clumps) at time T")

# ggsave("Visual/HEQU_Plot_Growth.png", plot = GrowthPlot, width = 12, height = 6)
saveRDS(GrowthPlot, "Results/HEQU_summary/Growth_facetplot.rds")

###################
###  Fecundity  ###
###################


PFlower <- ggplot(adult, aes(sizeT, pflowerT)) +
  geom_point()+
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se=F)+
  facet_grid(vars(population_f), vars(year)) +
  xlab("log(# of leaf clumps) at time T")

# ggsave("Visual/HEQU_Plot_pFlower.png", plot = PFlower, width = 12, height = 6)
saveRDS(PFlower, "Results/HEQU_summary/pFlower_facetplot.rds")


NFlower <- ggplot(adult[which(adult$pflowerT==1),], aes(sizeT, fertilityT)) +
  geom_point()+
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=F)+
  facet_grid(vars(population_f), vars(year)) +
  xlab("log(# of leaf clumps) at time T")

# ggsave("Visual/HEQU_Plot_nFlower.png", plot = NFlower, width = 12, height = 6)
saveRDS(NFlower, "Results/HEQU_summary/Nflower_facetplot.rds")


pAbort <- ggplot(adult[which(adult$pflowerT==1),], aes(sizeT, pAbort)) +
  geom_point()+
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=F)+
  facet_grid(vars(population_f), vars(year))+
  xlab("log(# of leaf clumps) at time T")

# ggsave("Visual/HEQU_Plot_pAbort.png", plot = pAbort, width = 12, height = 6)
saveRDS(pAbort, "Results/HEQU_summary/pAbort_facetplot.rds")


### Stalk components
heads.per.stalk = 2.17                                   ## flowerheads per stalk  ##hard-coded by Amy Iler
seeds.per.head = 64.1                                    ## seeds per flowerhead   ##hard-coded by Amy Iler

Pe <- df %>%
  group_by(population, year) %>%
  summarise(Nsdl = sum(seedling, na.rm = T),
            Nstalks = sum(fertilityT - abort.stalks, na.rm = T),
            Nseeds = Nstalks * (heads.per.stalk * seeds.per.head),
            Pe = Nsdl/Nseeds) %>% 
  filter(Nstalks != 0)                                  ## There are two years with no stalks at T but with seedlings at T1


### Germination propability
PeSummary <- c(mean = mean(Pe$Pe),sd = sd(Pe$Pe))

Nseedlings <- ggplot(sdl, aes(year))+
  geom_bar( stat = "count")+
  scale_x_continuous("Year", breaks = c(1998:2012), labels = c(as.character(1998:2012))) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  facet_grid(vars(population_f))

# ggsave("Visual/HEQU_Plot_Nseedlings.png", plot = Nseeldings, width = 12, height = 6)
saveRDS(Nseedlings, "Results/HEQU_summary/Nseedling_facetplot.rds")


min.mean.sd.max <- function(x) {
  r <- c(min(x, na.rm=T),mean(x, na.rm = T)-sd(x, na.rm = T), mean(x, na.rm = T), mean(x, na.rm = T) + sd(x, na.rm = T), max(x, na.rm = T))
  names(r) <- c("ymin","lower", "middle", "upper", "ymax")
  r
}

SDLsurv <- ggplot(aes(x= year, y=survival, group=year), data = sdl) +
  stat_summary(fun.data = min.mean.sd.max, geom = "boxplot")+
  scale_x_continuous("Year", breaks = c(1998:2012), labels = c(as.character(1998:2012)))+
  coord_cartesian( ylim = c(0, 1), xlim = c(1998, 2011))+
  facet_grid(vars(population_f))+
  xlab("Year at time T") + ylab("Seedling survival probability to time T+1")

# ggsave("Visual/HEQU_Plot_sdlsurvival.png", plot = SurvPlot, width = 12, height = 6)  
saveRDS(SDLsurv, "Results/HEQU_summary/SDLsurv_facetplot.rds")


### size distribution into continuous stage
FdSummary <- c(mean = mean(sdl$sizeT1, na.rm = T), sd = sd(sdl$sizeT1, na.rm = T))

Fd <- ggplot(sdl, aes(x=sizeT1)) +
  geom_histogram(bins = 60) +
  xlab("Recruitment size [log(# of leaf clumps)] of sdl -> continuous stage") +
  facet_grid(vars(population_f))

# ggsave("Visual/HEQU_Plot_Fd.png", plot = Fd, width = 12, height = 6)
saveRDS(Fd, "Results/HEQU_summary/Fd_facetplot.rds")







