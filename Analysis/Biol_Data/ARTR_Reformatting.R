library(dplyr)


#### Preparing historical data ---------------------------------------------------------------------------------------------------------------------------------------------------

surv1 <- read.csv("Data/Biol data/Original data/ARTR_original_data/removals_data&code/data/idaho/speciesData/ARTR/survD.csv") %>%
  mutate(lnsizeT = log(area),
         plantID = paste(quad, trackID, sep = "_"),
         year = year + 1900) %>%
  rename(areaT = area,
         survival = survives)

surv1 <- surv1[, c("Group", "quad", "year", "plantID", "areaT", "lnsizeT", "survival", "seedling", "Grazing")]

growth1 <- read.csv("Data/Biol data/Original data/ARTR_original_data/removals_data&code/data/idaho/speciesData/ARTR/growDnoNA.csv") %>%
  mutate(lnsizeT1 = log(area.t1),
         lnsizeT = log(area.t0),
         plantID = paste(quad, trackID, sep = "_"),
         year = year + 1900) %>%
  rename(areaT = area.t0,
         areaT1 = area.t1)

growth1 <- growth1[, c("Group", "quad", "year", "plantID", "areaT", "lnsizeT", "areaT1", "lnsizeT1")]

BiolHis <- merge(surv1, growth1, by = c("Group", "quad", "year", "plantID", "areaT", "lnsizeT"), all = T)


### Preparing recent data ---------------------------------------------------------------------------------------------------------------------------------------------------

surv2 <- read.csv("Data/Biol data/Original data/ARTR_original_data/removals_data&code/data/idaho_modern/speciesData/ARTR/survD.csv") %>%
  mutate(lnsizeT = log(area),
         plantID = paste(quad, trackID, sep = "_")) %>%
  rename(areaT = area,
         survival = survives)

surv2 <- surv2[which(surv2$Treatment == "Control"),]    ### only use control plants

surv2 <- surv2[, c("Group", "quad", "year", "plantID", "areaT", "lnsizeT", "survival", "seedling", "Grazing")]

growth2 <- read.csv("Data/Biol data/Original data/ARTR_original_data/removals_data&code/data/idaho_modern/speciesData/ARTR/growDnoNA.csv") %>%
  mutate(lnsizeT1 = log(area.t1),
         lnsizeT = log(area.t0),
         plantID = paste(quad, trackID, sep = "_")) %>%
  rename(areaT = area.t0,
         areaT1 = area.t1)

growth2 <- growth2[which(growth2$Treatment == "Control"),]    ### only use control plants

growth2 <- growth2[, c("Group", "quad", "year", "plantID", "areaT", "lnsizeT", "areaT1", "lnsizeT1")]

BiolRecent <- merge(surv2, growth2, by = c("Group", "quad", "year", "plantID", "areaT", "lnsizeT"), all = T)


### Merge both files and save ---------------------------------------------------------------------------------------------------------------------------------------------------
Biol <- rbind(BiolHis, BiolRecent)
Biol <- Biol[which(Biol$Grazing != "yes"),]  ### only use ungrazed records (either "UG" or "no")
Biol$month <- 6
Biol$day <- "NA"
  


write.csv(Biol[,c("Group", "quad", "year", "month", "day", "plantID", 
                  "areaT", "lnsizeT", "areaT1", "lnsizeT1",
                  "survival", "seedling")],
          "Data/Biol data/ARTR_demography_data.csv")
