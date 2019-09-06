setwd("C:/owncloud/Documents/PhD/Biomes/")

library(dplyr)
library(ggplot2)


data <- readxl::read_excel("Data/Biomes from sApropos.xlsx",sheet = "Sheet1", skip = 1)
data <- data[!is.na(data$Biome),]

Tropical <- c("TMB", "TDB", "TSC", "TGV", "MAN")
Temperate <-  c("FGS", "TBM", "TGS", "TCF", "MED")
Alpine <- c("MON")
Tundra <- c("TUN", "BOR")
Boreal <- c("BOR")
Arid <- c("DES")

data$region <- 1
data$region[which(data$Biome %in% Tropical)] <- "Tropical & Subtropical"
data$region[which(data$Biome %in% Temperate)] <- "Temperate"
data$region[which(data$Biome %in% Alpine)] <- "Alpine"
data$region[which(data$Biome %in% Tundra)] <- "Tundra and Boreal Forest"
#data$region[which(data$Biome %in% Boreal)] <- "Boreal Forest"
data$region[which(data$Biome %in% Arid)] <- "Arid"
data$region <- factor(data$region, levels = c("Arid", "Tundra and Boreal Forest", "Temperate", "Tropical & Subtropical"))

df <- data %>%
  group_by(region) %>%
  summarise(Populations = length(unique(c(plant_species, Population))))
             

byBiome <- ggplot(data = df, aes(x=region, y=Populations, fill = region)) +
  geom_bar(stat = "identity") +
  labs(fill = "Biomes", title = "A") +
  scale_fill_brewer(palette = "Greens" ) +
  theme_minimal() +
  theme(legend.position = "none") 
  
  

df1 <- data %>%
  group_by(region, TType) %>%
  summarise(Populations = length(unique(c(plant_species, Population))))%>%
  na.omit(TType)

df2 <- data %>%
  group_by(region, Ptype) %>%
  summarise(Populations = length(unique(c(plant_species, Population))))%>%
  na.omit(Ptype)

byTType <- ggplot(data = df1, aes(x = TType , y= Populations, fill = region )) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_discrete(name = "Type of Temperature window", limits= c("Seasonal", "Annual", "Both")) +
  labs(fill = "Biomes", title = "B")+
  scale_fill_brewer(palette = "OrRd" ) +
  theme_minimal()

byPType <- ggplot(data = df2, aes(x = Ptype , y= Populations, fill = region )) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_discrete(name = "Type of Percipitation window", limits= c("Seasonal", "Annual", "Both")) +
  labs(fill = "Biomes", title = "C") +
  scale_fill_brewer(palette = "Blues" ) +
  theme_minimal()

ATemp <- ggplot(data = df1, aes(x = region , y= Populations, fill = TType )) +
  geom_bar(stat = "identity") +
  labs(fill = "Biomes", title = "B")+
  scale_fill_brewer(palette = "OrRd" ) +
  theme_minimal()+
  theme(legend.position = c(0.9,0.9))

APrcp <- ggplot(data = df2, aes(x = region , y= Populations, fill = Ptype )) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Biomes") +
  labs(fill = "Biomes", title = "C") +
  scale_fill_brewer(palette = "Blues" ) +
  theme_minimal() +
  theme(legend.position = c(0.9,0.9))
  


gridExtra::grid.arrange(byBiome, byTType, byPType, nrow = 3, ncol = 1)
gridExtra::grid.arrange(byBiome, ATemp, APrcp, nrow = 3, ncol = 1)

df3 <- df
df3$Ptype <- "None" %>%

