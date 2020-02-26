## Create plots for an overview of populations in the plant revies (Compagnoni et al., in preperation)
##

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(patchwork)

data <- readxl::read_excel("Data/Lit_review/Biomes from sApropos.xlsx",sheet = "Sheet1", skip = 1)
data <- data[!is.na(data$Biome),]


### Divide into Regions --------------------------------------------------------------------------------------

Tropical <- c("TMB", "TDB", "TSC", "TGV", "MAN")
Temperate <-  c("FGS", "TBM", "TGS", "TCF", "MED")
Alpine <- c("MON")
Tundra <- c("TUN", "BOR")
Arid <- c("DES")

data$region <- 1
data$region[which(data$Biome %in% Tropical)] <- "Tropical & Subtropical"
data$region[which(data$Biome %in% Temperate)] <- "Temperate"
data$region[which(data$Biome %in% Alpine)] <- "Alpine"
data$region[which(data$Biome %in% Tundra)] <- "Tundra & Boreal Forest"
data$region[which(data$Biome %in% Arid)] <- "Arid"

data$region <- factor(data$region, levels = c("Arid", "Tundra & Boreal Forest", "Temperate", "Tropical & Subtropical", "Alpine"))


km2 <- data.frame( Biome = c("TMB", "TDB", "TSC", "TBM", "TCF", "BOR", 
                             "TGV", "TGS", "FGS", "MON", "TUN", "MED", 
                             "DES", "MAN"),
                   area = c(19775456.55, 3009534.172, 709291.3819, 12831028.28, 4086174.833, 15126778.94,
                            20177549.1, 10101575.31, 1091569.309, 5187550.471, 11655052.69, 3220386.04,
                            27885678.92, 346432.0369)
)

km2$region <- 1
km2$region[which(km2$Biome %in% Tropical)] <- "Tropical & Subtropical"
km2$region[which(km2$Biome %in% Temperate)] <- "Temperate"
km2$region[which(km2$Biome %in% Alpine)] <- "Alpine"
km2$region[which(km2$Biome %in% Tundra)] <- "Tundra & Boreal Forest"
km2$region[which(km2$Biome %in% Arid)] <- "Arid"
km2$region[which(km2$Biome %in% Alpine)] <- "Alpine"

km2$region <- factor(km2$region, levels = c("Arid", "Tundra & Boreal Forest", "Temperate", "Tropical & Subtropical", "Alpine"))

km2 <- km2 %>%
  group_by(region) %>%
  summarise( area = sum(area))

## Prepare dataframes ---------------------------------------------------------------------------------------------

df <- data %>%
  group_by(region) %>%
  summarise(Populations = length(unique(c(plant_species, Population))))
             
df <- left_join(df, km2)
totkm2 <- sum(df$area)
df$prop <- df$Populations * (df$area/totkm2)


df1 <- data %>%
  group_by(region, TType) %>%
  summarise(Populations = length(unique(c(plant_species, Population)))) %>%
  na.omit(TType)

df1$total2 <- 1
df1$total2[which(df1$region=="Arid")] = sum(df1$Populations[which(df1$region == "Arid")])
df1$total2[which(df1$region=="Tundra & Boreal Forest")] = sum(df1$Populations[which(df1$region == "Tundra & Boreal Forest")])
df1$total2[which(df1$region=="Temperate")] = sum(df1$Populations[which(df1$region == "Temperate")])
df1$total2[which(df1$region=="Tropical & Subtropical")] = sum(df1$Populations[which(df1$region == "Tropical & Subtropical")])
df1$relative2 <- df1$Populations/df1$total2
df1$TType <- factor(df1$TType, levels = c("Both", "Seasonal", "Annual"))

df2 <- data %>%
  group_by(region, Ptype) %>%
  summarise(Populations = length(unique(c(plant_species, Population)))) %>%
  na.omit(Ptype)

df2$total2 <- 1
df2$total2[which(df2$region=="Arid")] = sum(df2$Populations[which(df2$region == "Arid")])
df2$total2[which(df2$region=="Tundra & Boreal Forest")] = sum(df2$Populations[which(df2$region == "Tundra & Boreal Forest")])
df2$total2[which(df2$region=="Temperate")] = sum(df2$Populations[which(df2$region == "Temperate")])
df2$total2[which(df2$region=="Tropical & Subtropical")] = sum(df2$Populations[which(df2$region == "Tropical & Subtropical")])
df2$relative2 <- df2$Populations/df2$total2

df2$Ptype <- factor(df2$Ptype, levels = c("Both", "Seasonal", "Annual"))


### Absolute population numbers ------------------------------------------------------------------------------------

byBiome <- ggplot(data = df, aes(x=region, y=Populations)) +
  geom_bar(stat = "identity", fill = "#009E73") +
  labs(fill = "Biomes") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"))


### Relative bargraphs ----------------------------------------------------------------------------------------------------

CTemp <- ggplot(data = df1, aes(x = region , y= relative2, fill = TType, label = Populations )) +
  geom_bar(stat = "identity") +
  geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  scale_x_discrete(name = element_blank(), labels = c("Arid", "Tundra & \nBoreal Forest", "Temperate", "Tropical & \n Subtropical")) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), name = element_blank())+
  labs(fill = "Temperature \ndriver", title = "Populations considering temperature")+
  scale_fill_brewer(palette = "OrRd" ) +
  theme_classic()+
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"))

CPrcp <- ggplot(data = df2, aes(x = region , y= relative2, fill = Ptype, label = Populations )) +
  geom_bar(stat = "identity") +
  geom_text(size = 4, position = position_stack(vjust = 0.5))+
  scale_x_discrete(name = element_blank(), labels = c("Arid", "Tundra & \nBoreal Forest", "Temperate", "Tropical & \n Subtropical")) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), name = element_blank())+
  labs(fill = "Precipitation \ndriver", title = "Populations considering precipitation") +
  scale_fill_brewer(palette = "Blues" ) +
  theme_classic() +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"))


### Arrange all options --------------------------------------------------------------------------------

All <- byBiome / (CTemp / CPrcp)  + 
  plot_layout(heights = c(1,2)) + 
  plot_annotation(tag_levels = c('A'))


### Save plots ----------------------------------------------------------------------------------------

ggsave("Visual/Biomeplot.png", All, width = 8, height = 9)

