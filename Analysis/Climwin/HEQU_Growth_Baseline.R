library(dplyr)
library(lme4)

### Prepare data -------------------------------------------------------------

Biol <- read.csv("/data/gsclim/HEQU_demography_data.csv") %>%
  mutate(sizeT = as.numeric(as.character(sizeT)),
         sizeT1 = as.numeric(as.character(sizeT1)),
         year = as.factor(year))

Biol$date <- paste("01/07/", Biol$year, sep = "")        ### get a date that's accepted by climwin
Biol <- Biol[which(Biol$seedling != 1),]                           ### Select Adults
Biol <- Biol[which(!is.na(Biol$survival)),]                       
Biol <- Biol[which(!is.na(Biol$sizeT)),]                           
Biol <- Biol[which(!is.na(Biol$sizeT1)),]

### Baseline window -----------------------------------------------------------------

a <- glmer.nb(sizeT1 ~ sizeT + population + (1|year), data = Biol)

saveRDS(a, "/data/gsclim/HEQU_growth_baseline.rds")
