setwd("C:/owncloud/Documents/PhD/Biomes/Biome/")

library(dplyr)
library(lme4)
library(ggplot2)
library(ggiraphExtra)

df <- read.csv("Data/Biol data/HEQU_demography data_JEcol_Dryad.csv") %>%
  mutate(sizeT = log(as.integer(sizeT)),          #### ln transform here. From Appendix S1 it sounds like they transform within each vital rate
         sizeT1 = log(as.integer(sizeT1)),         #### In the end it's the same, but is there a reason to do it one or the other way for forms sake??
         population_f = factor(population, levels = c("high", "mid","low"))
         )

sdl <- df[which(df$seedling ==1),]
adult <- df[which(df$seedling != 1),]
adult$abort.stalks[which(adult$pflowerT==1)] <- ifelse(is.na(adult$abort.stalks[which(adult$pflowerT==1)]), 0, adult$abort.stalks[which(adult$pflowerT==1)])
adult$pAbort <- adult$abort.stalks / adult$fertilityT
adult$success <- as.integer(adult$fertilityT - adult$abort.stalks)


#################
### Survival ####
#################

plot(adult$sizeT, adult$survival,
     main = "Adult Survival",
     xlab = "Size at T",
     ylab = "Survival",
     pch = 19,
     frame = F)
s_a <- glm(formula = survival ~ sizeT + population,
             data = adult, family = binomial)

ggplot(adult, aes(sizeT, survival)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family="binomial"), se=F)+
  facet_grid( vars(population_f), vars(year))



#################
###  Growth  ####
#################

plot(adult$sizeT, adult$sizeT1,
          main = "Adult Growth",
          xlab = "Size at T",
          ylab = "Size at T+1",
          pch = 19,
          frame = F)


g_a <- lmer(formula = sizeT1 ~ sizeT + (1|year) + population, data = adult)  ### relate growth sd to size as well? see plot ^ 
gsd_a <- sd(resid(g_a))




###################
###  Fecundity  ###
###################

plot(adult$sizeT, adult$pflowerT, 
           main = "Flower Probability",
           xlab = "Size at T",
           ylab = "Flower Probability at T",
           pch = 19,
           frame = F)

plot(adult$sizeT[which(adult$pflowerT==1)], adult$fertilityT[which(adult$pflowerT==1)],
     main = "N of Stalks",
     xlab = "Size at T",
     ylab = "Number of Stalks (if Fp = 1)", 
     pch = 19,
     frame = F)

plot(adult$sizeT[which(adult$pflowerT==1)], adult$pAbort[which(adult$pflowerT==1)],
     main = "Chance of Stalk abortion",
     xlab = "Size at T",
     ylab = "Survival chance of Stalks",
     pch = 19,
     frame = F)



Pe <- df %>%
  group_by(population, year) %>%
  summarise(Nsdl = sum(seedling, na.rm = T),
            Nstalks = sum(fertilityT - abort.stalks, na.rm = T),
            Nseeds = Nstalks * (heads.per.stalk * seeds.per.head),
            Pe = Nsdl/Nseeds) %>% 
  filter(Nstalks != 0)                                  ## There are two years with no stalks at T but with seedlings at T1
chisq.test(a$Pe, a$population)

hist(sdl$sizeT1,
     breaks = 24)


### Flowering propability
Fp <- glmer(pflowerT ~ sizeT + (1|year) +population, data = adult , family = binomial)

### Total number of Stalks produced
Nstalk <- lmer(fertilityT ~ sizeT + (1|year) + population, data = adult[which(adult$pflowerT==1),])

### Chance for Viable Stalks
FAp <- glmer(cbind(success, abort.stalks) ~ sizeT + (1|year) + population, data = adult[which(adult$pflowerT==1),], family = binomial)


### Stalk components
heads.per.stalk<-2.17                                   ## flowerheads per stalk  ##hard-coded by Amy Iler
seeds.per.head<-64.1                                    ## seeds per flowerhead   ##hard-coded by Amy Iler

### Germination propability
Pe <- c(mean(Pe$Pe), sd(Pe$Pe))

### seedling survival
s_s <- glmer(formula = survival ~ (1|year), data = sdl, family = binomial)  

### Stay seedling?
## is that what the seedling == 1, survival ==1, sizeT1 = NA means?
a <- sdl[which(sdl$survival == 1),]
a <- sdl[which(is.na(a$sizeT1)),]

### size distribution into continuous stage
Fd <- c(mean(sdl$sizeT1, na.rm = T), sd(sdl$sizeT1, na.rm = T))



