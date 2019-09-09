## Create plots for an overview of populations in the plant revies (Compagnoni et al., in preperation)
##

setwd("C:/owncloud/Documents/PhD/Biomes/Biome")

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)

data <- readxl::read_excel("Data/Biomes from sApropos.xlsx",sheet = "Sheet1", skip = 1)
data <- data[!is.na(data$Biome),]


### Divide into Regions --------------------------------------------------------------------------------------

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


## Prepare dataframes ---------------------------------------------------------------------------------------------

df <- data %>%
  group_by(region) %>%
  summarise(Populations = length(unique(c(plant_species, Population))))
             

df1 <- data %>%
  group_by(region, TType) %>%
  summarise(Populations = length(unique(c(plant_species, Population))))%>%
  na.omit(TType)
df1$total <- 1
df1$total[which(df1$region=="Arid")] = df$Populations[which(df$region == "Arid")]
df1$total[which(df1$region=="Tundra and Boreal Forest")] = df$Populations[which(df$region == "Tundra and Boreal Forest")]
df1$total[which(df1$region=="Temperate")] = df$Populations[which(df$region == "Temperate")]
df1$total[which(df1$region=="Tropical & Subtropical")] = df$Populations[which(df$region == "Tropical & Subtropical")]
df1$relative <- df1$Populations/df1$total
df1$total2 <- 1
df1$total2[which(df1$region=="Arid")] = sum(df1$Populations[which(df1$region == "Arid")])
df1$total2[which(df1$region=="Tundra and Boreal Forest")] = sum(df1$Populations[which(df1$region == "Tundra and Boreal Forest")])
df1$total2[which(df1$region=="Temperate")] = sum(df1$Populations[which(df1$region == "Temperate")])
df1$total2[which(df1$region=="Tropical & Subtropical")] = sum(df1$Populations[which(df1$region == "Tropical & Subtropical")])
df1$relative2 <- df1$Populations/df1$total2


df2 <- data %>%
  group_by(region, Ptype) %>%
  summarise(Populations = length(unique(c(plant_species, Population))))%>%
  na.omit(Ptype)
df2$total <- 1
df2$total[which(df2$region=="Arid")] = df$Populations[which(df$region == "Arid")]
df2$total[which(df2$region=="Tundra and Boreal Forest")] = df$Populations[which(df$region == "Tundra and Boreal Forest")]
df2$total[which(df2$region=="Temperate")] = df$Populations[which(df$region == "Temperate")]
df2$total[which(df2$region=="Tropical & Subtropical")] = df$Populations[which(df$region == "Tropical & Subtropical")]
df2$relative <- df2$Populations/df2$total
df2$total2 <- 1
df2$total2[which(df2$region=="Arid")] = sum(df2$Populations[which(df2$region == "Arid")])
df2$total2[which(df2$region=="Tundra and Boreal Forest")] = sum(df2$Populations[which(df2$region == "Tundra and Boreal Forest")])
df2$total2[which(df2$region=="Temperate")] = sum(df2$Populations[which(df2$region == "Temperate")])
df2$total2[which(df2$region=="Tropical & Subtropical")] = sum(df2$Populations[which(df2$region == "Tropical & Subtropical")])
df2$relative2 <- df2$Populations/df2$total2


### Original option ------------------------------------------------------------------------------------

byBiome <- ggplot(data = df, aes(x=region, y=Populations, fill = region)) +
  geom_bar(stat = "identity") +
  labs(fill = "Biomes", title = "A") +
  scale_fill_brewer(palette = "Greens" ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank()) 


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


### Option 1 --------------------------------------------------------------------------------------------

ATemp <- ggplot(data = df1, aes(x = region , y= Populations, fill = TType )) +
  geom_bar(stat = "identity") +
  labs(fill = "Temperature type", title = "B")+
  scale_fill_brewer(palette = "OrRd" ) +
  theme_minimal()+
  theme(legend.position = c(0.9,0.9),
        axis.title.x = element_blank())

APrcp <- ggplot(data = df2, aes(x = region , y= Populations, fill = Ptype )) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Biomes") +
  labs(fill = "Precipitation type", title = "C") +
  scale_fill_brewer(palette = "Blues" ) +
  theme_minimal() +
  theme(legend.position = c(0.9,0.9))
  

### Option 2 ----------------------------------------------------------------------------------------------------

BTemp <- ggplot(data = df1, aes(x = region , y= relative, fill = TType )) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), name = element_blank())+
  labs(fill = "Temperature driver", title = "B")+
  scale_fill_brewer(palette = "OrRd" ) +
  theme_minimal()+
  theme(axis.title.x = element_blank())

BPrcp <- ggplot(data = df2, aes(x = region , y= relative, fill = Ptype )) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = element_blank()) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), name = element_blank())+
  labs(fill = "Precipitation driver", title = "C") +
  scale_fill_brewer(palette = "Blues" ) +
  theme_minimal() +
  theme()


### Option 3 ----------------------------------------------------------------------------------------------------

CTemp <- ggplot(data = df1, aes(x = region , y= relative2, fill = TType )) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), name = element_blank())+
  labs(fill = "Temperature driver", title = "B")+
  scale_fill_brewer(palette = "OrRd" ) +
  theme_minimal()+
  theme(axis.title.x = element_blank())

CPrcp <- ggplot(data = df2, aes(x = region , y= relative2, fill = Ptype )) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = element_blank()) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), name = element_blank())+
  labs(fill = "Precipitation driver", title = "C") +
  scale_fill_brewer(palette = "Blues" ) +
  theme_minimal() +
  theme()



### Needed for nice grid.arrange -----------------------------------------------------------------------

blankPlot <- grid.rect(gp=gpar(col="white"))
  

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


BT <- get_legend(BTemp)
BP <- get_legend(BPrcp)

CT <- get_legend(CTemp)
CP <- get_legend(CPrcp)


### Arrange all options --------------------------------------------------------------------------------

OptionOriginal <- gridExtra::grid.arrange(byBiome, byTType, byPType, nrow = 3, ncol = 1)
Option1 <- grid.arrange(byBiome, ATemp, APrcp, nrow = 3, ncol = 1)
Option2 <- grid.arrange(byBiome + theme(axis.title = element_blank()), 
                        BTemp + theme(legend.position = "none"), 
                        BT,
                        BPrcp + theme(legend.position = "none"),
                        BP,
                        blankPlot,
                        textGrob("Populations", gp=gpar(fontsize =15), rot = 90), 
                        textGrob("% of populations", gp=gpar(fontsize =15), rot = 90),
                        layout_matrix = cbind(c(7,8,8),c(1,2,4), c(6,3,5)),
                        widths = c(0.15, 3, 0.7),
                        bottom = textGrob("Biomes"))
Option3 <- grid.arrange(byBiome + theme(axis.title = element_blank()), 
                        CTemp + theme(legend.position = "none"), 
                        CT,
                        CPrcp + theme(legend.position = "none"),
                        CP,
                        blankPlot,
                        textGrob("Populations", gp=gpar(fontsize =15), rot = 90), 
                        textGrob("% of populations", gp=gpar(fontsize =15), rot = 90),
                        layout_matrix = cbind(c(7,8,8),c(1,2,4), c(6,3,5)),
                        widths = c(0.15, 3, 0.7),
                        bottom = textGrob("Biomes"))


### Save plots ----------------------------------------------------------------------------------------

ggsave("Visual/Biomeplot.png", OptionOriginal, width = 7, height = 7)
ggsave("Visual/Biomeplot_1.png", Option1, width = 7, height = 7)
ggsave("Visual/Biomeplot_2.png", Option2, width = 8, height = 9)
ggsave("Visual/Biomeplot_3.png", Option3, width = 8, height = 9)

