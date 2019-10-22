library(dplyr)


### Read in data, rename columns
Biol <- read.csv("Data/Biol data/Original data/OPIM_original_data/cholla_demography_20042018.csv") %>%
  rename(year = Year_t,
         survival = Survival_t1,
         fertilityT = TotFlowerbuds_t,
         fertilityT1 = TotFlowerbuds_t1) %>%
  mutate(sizeT = ifelse(is.na(Height_t) | is.na(Width_t) | is.na(Perp_t)  , NA, pi * ((Width_t + Perp_t)/4)^2 * Height_t * (1/3)),               ### Calculate sizeT and sizeT1 (as cone)
         sizeT1 = ifelse(is.na(Height_t1) | is.na(Width_t1) | is.na(Perp_t1)  , NA, pi * ((Width_t1 + Perp_t1)/4)^2 * Height_t1 * (1/3)),
         plantID = paste(Plot, TagID, sep = "."))

Biol <- Biol[which(Biol$Transplant!= 1),]

### Add some missing columns -------------------------------------------------------------------------------------------------------------------
Biol$month <- 5
Biol$day <- 20
Biol$pflowerT <- ifelse(Biol$fertilityT > 0 | Biol$Goodbuds_t > 0 | Biol$ABFlowerbuds_t > 0 , 1, 0)
Biol$pflowerT[which(is.na(Biol$pflowerT))] <- 0
Biol$pflowerT1 <- ifelse(Biol$fertilityT > 0 | Biol$Goodbuds_t > 0 | Biol$ABFlowerbuds_t > 0, 1, 0)
Biol$pflowerT1[which(is.na(Biol$pflowerT1))] <- 0

for (i in c(which(is.na(Biol$fertilityT)))) {
  Biol$fertilityT[i] <- sum(Biol$Goodbuds_t[i], Biol$ABFlowerbuds_t[i], na.rm = T)
}

for (i in c(which(is.na(Biol$fertilityT1)))) {
  Biol$fertilityT1[i] <- sum(Biol$Goodbuds_t1[i], Biol$ABFlowerbuds_t1[i], na.rm = T)
}

Biol$year[which(is.na(Biol$year))] <- Biol$Year_t1[which(is.na(Biol$year))] - 1
Biol <- Biol[,c("plantID", "Plot", "year", "month", "day", "sizeT", "pflowerT", "fertilityT", "survival", "sizeT1", "pflowerT1", "fertilityT1", "Goodbuds_t", "Goodbuds_t1", "Recruit", "Newplant", "Antcount_t", "Antcount_t1")]

write.csv(Biol, "Data/Biol data/OPIM_demography_data.csv")



