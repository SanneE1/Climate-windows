library(lme4)
library(climwin)

#### HEQU growth  (FRSP Fp below)
Biol <- read.csv("Data/Biol data/HEQU_demography_data.csv")%>%
  mutate(sizeT = as.integer(levels(sizeT))[sizeT],
         sizeT1 = as.integer(levels(sizeT1))[sizeT1])
Biol <- Biol[which(Biol$seedling != 1),]                           
Biol <- Biol[which(Biol$year!= 2012),]
Biol <- Biol[which(!(is.na(Biol$sizeT) | Biol$sizeT == 0)),]
Biol$lnsizeT <- log(Biol$sizeT)
Biol$date <- paste(ifelse(!(is.na(Biol$day)), sprintf("%02d", Biol$day), "01") , sprintf("%02d", Biol$month), Biol$year, sep = "/")                  ### get a date that's accepted by climwin
Biol <- Biol[which(!is.na(Biol$sizeT1)),]

Clim <- read.csv("Data/Climate data/HEQU_NOAA_month_imputed.csv")
Clim$date <- paste("15/",sprintf("%02d", Clim$Month), "/", Clim$Year, sep = "")          

Hg <- readRDS("Results/Climwin/HEQU_g_month_result.rds")
Hgrowth = 2

# auto <- autowin(reference = Hg[[Hgrowth]],
#                 xvar = list(Tmin = Clim$mean_tmin_scaled),
#                 cdate = Clim$date, bdate = Biol$date,
#                 baseline = glmer(sizeT1 ~ lnsizeT + population + (1|year),
#                                  data = Biol,
#                                  family = poisson),
#                 range = c(24, -12),
#                 stat = "mean", func = "quad",
#                 type = "absolute",
#                 refday = c(1,7),
#                 cmissing = "method1", cinterval = "month")


### All months
# C <- Clim[,c("Month", "Year", "mean_tmin_scaled")]
# c <- C %>%
#   mutate(Year = Year - 1)
# Tmin <- full_join(C,c, by = c("Month", "Year"))
# ggplot(Tmin, aes(x = mean_tmin_scaled.x, y = mean_tmin_scaled.y)) +
#   geom_point() +
#   facet_wrap(vars(Month))


### Correlation best window (relative 9-12 ==  July - Oct)  
C <- Clim[,c("Month", "Year", "mean_tmin_scaled")]
C <- subset(C, C$Month %in% c(7:10)) %>%
  group_by(Year) %>%
  summarise(mean_tmin = mean(mean_tmin_scaled))
c <- C %>%
  mutate(Year = Year - 1)
Tmin <- full_join(C,c, by = c("Year"))

a <- cor.test(Tmin$mean_tmin.x, Tmin$mean_tmin.y)

ggplot(Tmin, aes(x = mean_tmin.x, y = mean_tmin.y)) +
  geom_point() +
  geom_smooth(method = lm)+
  geom_text(aes(x = -1.2,y = 1.5, label = paste("Correlation Estimate = ", a$estimate, sep = "")), hjust = "inward") +
  geom_text(aes(x = -1.2,y = 1.25, label = paste("P-value ", a$p.value, sep = "")), hjust = "inward") +
  xlab("Mean minimum temperatures (scaled) at time T") +
  ylab("Mean minimum temperatures (scaled) at time T + 1")

ggsave("Visual/HEQU_g_CorrTest.png")




### FRSP Fp ------------------------------------------------------------------------------------------------

Biol <- read.csv("Data/Biol data/FRSP_demography_data.csv")
Biol$lnsizeT <- log(Biol$sizeT)
Biol <- Biol[which(!is.na(Biol$sizeT)),]
Biol$date <- paste(ifelse(!(is.na(Biol$day)), sprintf("%02d", Biol$day), "01") , sprintf("%02d", Biol$month), Biol$year, sep = "/")                  ### get a date that's accepted by climwin
Biol <- Biol[which(!is.na(Biol$pFlowerT1)),]

Clim <- read.csv("Data/Climate data/FRSP_NOAA_month.csv")
Clim$date <- paste("15/",sprintf("%02d", Clim$Month), "/", Clim$Year, sep = "")          

Ff <- readRDS("Results/Climwin/FRSP_fp_month_result.rds")
Fflwr = 14


C <- Clim[,c("Month", "Year", "max_tmax_scaled")]
C <- subset(C, C$Month %in% c(5:11)) %>%
  group_by(Year) %>%
  summarise(max_tmax = max(max_tmax_scaled))
c <- C %>%
  mutate(Year = Year - 2)
Tmax <- full_join(C,c, by = c("Year"))

a <- cor.test(Tmax$max_tmax.x, Tmax$max_tmax.y)

ggplot(Tmax, aes(x = max_tmax.x, y = max_tmax.y)) +
  geom_point() +
  geom_smooth(method = lm) +
  geom_text(aes(x = 2.5,y = 3, label = paste("Correlation Estimate = ", a$estimate, sep = "")), hjust = "inward") +
  geom_text(aes(x = 2.5,y = 2.75, label = paste("P-value ", a$p.value, sep = "")), hjust = "inward") +
  xlab("Mean minimum temperatures (scaled) at time T") +
  ylab("Mean minimum temperatures (scaled) at time T + 2")
ggsave("Visual/FRSP_Fp_CorrTest.png")
