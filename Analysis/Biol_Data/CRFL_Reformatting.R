library(dplyr)

### Read the original dataand rename columns -------------------------------------------------------------------------------------------------------------------
Biol <- read.csv("Data/Biol data/Original data/CRFL_original_data/Population_dynamics_data.csv") %>%
  rename(plantID = ID,
         sizeT = Size,
         year = Year,
         ageT = Age,
         fertilityT = Fert)

### Get only the Control Treatment -------------------------------------------------------------------------------------------------------------------
Biol <- subset(Biol, Treatment == "C")

### Add some missing columns -------------------------------------------------------------------------------------------------------------------
Biol$month <- 5
Biol$day <- 20

Biol1 <- Biol %>%
  mutate(year = year - 1) %>%
  rename(sizeT1 = sizeT) %>%
  select(year, plantID, sizeT1)

Biol <- left_join(Biol, Biol1)

### remove extra rows -------------------------------------------------------------------------------------------------------------------
Biol <- subset(Biol, !(is.na(Biol$sizeT) & is.na(Biol$sizeT1)))


### Add other missing columns -------------------------------------------------------------------------------------------------------------------
Biol$survival <- ifelse(Biol$sizeT > 0 & Biol$sizeT1 > 0, 1, ifelse(Biol$sizeT > 0, 0, NA))
Biol$pflowerT <- ifelse(Biol$fertilityT > 0, 1, 0)
Biol$fertilityT <- replace(Biol$fertilityT, Biol$fertilityT == 0, NA)

Biol <- Biol[,c("plantID", "year", "month", "day", "ageT", "sizeT", "pflowerT", "fertilityT", "survival", "sizeT1", "Treatment", "Block", "Plot", "Quadrat", "X", "Y", "Shrub", "Compass", "Distance")]

write.csv(Biol, "Data/Biol data/CRFL_demography_data.csv")
