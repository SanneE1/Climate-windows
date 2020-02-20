library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(patchwork)


### Get data --------------------------------------------------------------------------------------------------------

df <- readxl::read_excel("Data/Lit_review/Biomes window open close.xlsx", skip = 1) %>%
  filter(Biome == "DES")
df$Studies <- as.factor(df$Studies)
df$TP <- as.factor(df$`T/P`)
df <- df[which(df$Studies != "Maschinski et al. 1997"),]     ## Remove studies with missing information
df <- df[which(df$Studies != "Raghu et al. 2013"),]

df1 <- readxl::read_excel("Data/Lit_review/Biomes window open close.xlsx", skip = 1) %>%
  filter(Biome == "TCF")
df1$Studies <- as.factor(df1$Studies)
df1$TP <- as.factor(df1$`T/P`)
df1 <- df1[which(df1$Studies != "Stevens&Latimer 2015"),]     ## Remove studies with missing information
df1 <- df1[which(df1$Studies != "Olsen et al. 2016"),]
df1 <- df1[which(df1$Studies != "Conlisk et al. 2017"),]

### Create graphs --------------------------------------------------------------------------------------------------------

Arid <- ggplot(df, aes(ymin = df$Closes, ymax = df$Open, x = df$Studies, colour = df$TP, group = df$TP)) +
  geom_point(aes(y=df$Open, x = df$Studies), shape = 4, position = position_dodge(width = 0.4), size = 1, stroke = 2)+
  geom_point(aes(y=df$Closes, x = df$Studies), shape = 3, position = position_dodge(width = 0.4), size = 1, stroke = 2)+
  geom_linerange(aes(ymin = df$Closes, ymax = df$Open, x = df$Studies), size = 1.25 ,position = position_dodge(width = 0.4)) +
  coord_flip()+
  ggtitle("Arid") +
  scale_y_continuous(breaks = c(0,6,12,18,24), 
                     name = "Months relative to observation at month 0") +
  scale_colour_manual(name = "Driver", 
                      labels = c('Precipitation', 'Temperature'),
                      values = c('P' = '#0072B2', 'T' = '#D55E00'),
                      breaks = c('P', 'T')) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = -0.3))


TCF <- ggplot(df1, aes(ymin = df1$Closes, ymax = df1$Open, x = df1$Studies, colour = df1$TP, group = df1$TP)) +
  geom_point(aes(y=df1$Open, x = df1$Studies), shape = 4, position = position_dodge(width = 0.4), size = 1, stroke = 2)+
  geom_point(aes(y=df1$Closes, x = df1$Studies), shape = 3, position = position_dodge(width = 0.4), size = 1, stroke = 2)+
  geom_linerange(aes(ymin = df1$Closes, ymax = df1$Open, x = df1$Studies), size = 1.25 ,position = position_dodge(width = 0.4)) +
  coord_flip()+
  ggtitle("TCF") +
  scale_y_continuous(breaks = c(0,6,12,18,24), 
                     name = "Months relative to observation at month 0") +
  scale_colour_manual(name = "Driver", 
                      labels = c('Precipitation', 'Temperature'),
                      values = c('P' = '#0072B2', 'T' = '#D55E00'),
                      breaks = c('P', 'T')) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = -0.3))




both <- (Arid / TCF) + plot_layout(guides = "collect", heights = c(4,2))

# ggsave("Visual/Time windows Arid.png", Arid)
# ggsave("Visual/Time windows TCF.png", TCF)
ggsave("Visual/Time windows Arid&TCF.png", both, width = 10, height = 5)

