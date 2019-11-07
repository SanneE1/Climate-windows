library(dplyr)
library(readxl)
library(tidyr)

# I checked -> There is no PlantID in the "CPASS" sheets that is also not in the "Flowered" or "Died" sheet, 
# but these sheets do have extra IDs

### Sheet Flowered
df9 <- read_xls("Data/Biol data/Original data/FRSP_original_data/FRSO_original.xls", 
                sheet = "Flowered", 
                skip = 2)


df9 <- df9 %>%
  mutate(TAG = gsub("\\s", "", df9$TAG)) %>%
  filter(!is.na(TAG)) %>%
  mutate_all(as.character)

### Sheet Died
df10 <- read_xls("Data/Biol data/Original data/FRSP_original_data/FRSO_original.xls", 
                sheet = "Died", 
                skip = 1)


df10 <- df10 %>%
  rename(TAG = "...1") %>%
  mutate(TAG = gsub("\\s", "", df10$...1)) %>%
  filter(!is.na(TAG)) %>%
  mutate_all(as.character)


### Merge sheets and expand --------------------------------------------------------------------------------------------------------------

Sheet <- bind_rows(df9, df10) %>%
  select(-contains("...")) %>%
  select(-"MAP") %>%
  rename(plantID = TAG) %>%
  mutate(plantID = case_when(duplicated(plantID) ~ paste(plantID, "dub", sep = "_"),
                         !(duplicated(plantID)) ~ plantID)) %>%
  pivot_longer(-plantID, names_to = "year", values_to = "sizeT") 


### Get Recruit data
Sheet$recruit <- "NA"
Sheet$recruit[grepl("(?i)rec" , Sheet$sizeT )] <- 1
Sheet$recruit[grepl("(?i)r" , Sheet$sizeT )] <- 1
                            
Sheet$sizeT[grepl("(?i)rec" , Sheet$sizeT )] <- NA
Sheet$sizeT[grepl("(?i)r" , Sheet$sizeT )] <- NA

Sheet$recruit[grepl("(?i)no rec" , Sheet$sizeT )] <- 0
Sheet$sizeT[grepl("(?i)no rec" , Sheet$sizeT )] <- NA

## Set Flowering
Sheet$pFlower <- 0
Sheet$pFlower[grepl("(?i)FL" , Sheet$sizeT )] <- 1
Sheet$pFlower[grepl("(?i)Flowering" , Sheet$sizeT )] <- 1

Sheet$sizeT[grepl("(?i)FL" , Sheet$sizeT )] <- NA
Sheet$sizeT[grepl("(?i)Flowering" , Sheet$sizeT )] <- NA


### Survival
Sheet$survival <- "NA"
Sheet$survival[grepl("(?i)dead", Sheet$sizeT )] <- 0
Sheet$survival[grepl("(?i)d", Sheet$sizeT )] <- 0
Sheet$survival[grepl("(?i)gone" , Sheet$sizeT )] <- 0
Sheet$survival[grepl("(?i)G", Sheet$sizeT )] <- 0
Sheet$survival[grepl("(?i)GTP", Sheet$sizeT )] <- 0

Sheet$sizeT[grepl("(?i)dead", Sheet$sizeT )] <- NA
Sheet$sizeT[grepl("(?i)d", Sheet$sizeT )] <- NA
Sheet$sizeT[grepl("(?i)gone" , Sheet$sizeT )] <- NA
Sheet$sizeT[grepl("(?i)G", Sheet$sizeT )] <- NA
Sheet$sizeT[grepl("(?i)GTP", Sheet$sizeT )] <- NA

### Clean up Size column
Sheet$sizeT[which(Sheet$sizeT == "?")] <- NA

Sheet$sizeT <- gsub("\\(.*\\)", "", Sheet$sizeT)
Sheet$sizeT <- gsub("\\[.*\\]", "", Sheet$sizeT)
Sheet$sizeT <- gsub("H", "", Sheet$sizeT)
Sheet$sizeT <- gsub("S", "", Sheet$sizeT)
Sheet$sizeT <- gsub("\\*", "", Sheet$sizeT)
Sheet$sizeT <- gsub("^\\s", "", Sheet$sizeT)
Sheet$sizeT <- gsub("\\s$", "", Sheet$sizeT)

Sheet$sizeT[which(Sheet$sizeT == "tiny")] <- 1 
Sheet$sizeT <- gsub("(?i)tiny ", "", Sheet$sizeT)
Sheet$sizeT <- gsub(" (?i)tiny", "", Sheet$sizeT)
Sheet$sizeT[which(Sheet$sizeT == "check map")] <- NA
Sheet$sizeT[which(Sheet$sizeT == "CECK MAP")] <- NA
Sheet$sizeT <- gsub("^\\s", "", Sheet$sizeT)

Sheet$sizeT[which(Sheet$sizeT == "NF")] <- NA



#Check non-numeric values  ----- 141 entries remainig. Discard them all for now
# Sheet %>%
#   filter(!grepl("^([0-9]+)$", Sheet$sizeT),
#          !is.na(Sheet$sizeT),
#          Sheet$sizeT != "FL") %>%
#   View()

Biol0 <- Sheet[which(grepl("^([0-9]+)$", Sheet$sizeT) | is.na(Sheet$sizeT)),] %>%
  mutate(sizeT = as.numeric(sizeT),
         year = as.integer(year),
         recruit = as.integer(recruit),
         survival = as.integer(survival)) %>%
  rename(recruitT = recruit)


### Match sizeT1 to sizeT ----------------------------------------------------------------------------------------------------------------

Biolm1 <- select(Biol0,-c("survival")) %>%
  mutate(year = year - 1) %>%
  rename(sizeT1 = sizeT,
         recruitT1 = recruitT,
         pFlowerT1 = pFlower)

Biol <- full_join(Biol0, Biolm1)
Biol <- Biol[c("plantID", "year", "sizeT", "recruitT", "sizeT1", "survival", "pFlowerT1")]
Biol$survival <- ifelse(!is.na(Biol$sizeT) & !is.na(Biol$sizeT1), 1, ifelse(!is.na(Biol$sizeT) & is.na(Biol$sizeT1), 0, NA))
Biol$pFlowerT1 <- ifelse(is.na(Biol$survival) & Biol$pFlowerT1 == 0, NA, ifelse(Biol$survival == 1, 0, Biol$pFlowerT1))

Biol <- Biol[which(!(is.na(Biol$sizeT) & is.na(Biol$sizeT1))),]
Biol$month <- 7
Biol$day <- 15


write.csv(Biol, "Data/Biol data/FRSP_demography_data.csv")
