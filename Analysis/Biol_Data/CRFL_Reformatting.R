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
# Biol$sizeT1 <- 0
# Biol$fertilityT1 <- 0
for (i in 1:length(Biol$plantID)) {
  Biol$sizeT1[i] <- Biol$sizeT[which(Biol$plantID == Biol$plantID[i] & Biol$year == Biol$year[i] + 1)]
  Biol$fertilityT1[i] <- Biol$fertilityT[which(Biol$plantID == Biol$plantID[i] & Biol$year == Biol$year[i] + 1)]
}

### remove extra rows -------------------------------------------------------------------------------------------------------------------
Biol <- subset(Biol, !(is.na(Biol$sizeT) & is.na(Biol$sizeT1)))


### Add other missing columns -------------------------------------------------------------------------------------------------------------------

Biol$survival <- ifelse(!(is.na(Biol$sizeT)) & is.na(Biol$sizeT1), 0,
                        ifelse(!(is.na(Biol$sizeT)) & !(is.na(Biol$sizeT1)), 1, 
                               NA))
Biol$pflowerT <- ifelse(!(is.na(Biol$fertilityT)) & !(Biol$fertilityT == 0), 1, 0)
Biol$pflowerT1 <- ifelse(!(is.na(Biol$fertilityT1)) & !(Biol$fertilityT1 == 0), 1, 0)

Biol <- Biol[,c("plantID", "year", "month", "day", "ageT", "sizeT", "pflowerT", "fertilityT", "survival", "sizeT1", "pflowerT1", "fertilityT1", "Treatment", "Block", "Plot", "Quadrat", "X", "Y", "Shrub", "Compass", "Distance")]

write.csv(Biol, "Data/Biol data/CRFL_demography_data.csv")
