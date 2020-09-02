library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(patchwork)
library(tidyr)

### Get data --------------------------------------------------------------------------------------------------------

df <- readxl::read_excel("Data/Lit_review/Biomes window open close.xlsx", skip = 1)
df$Studies <- as.factor(df$Studies)
df$TP <- as.factor(df$`T/P`)
df$Closes <- df$Closes - 0.5
df$Open <- df$Open + 0.5

### McNemar test --------------------------------------------------------------------------------------------------------

McN <- df %>%
  group_by(Biome, Studies, TP) %>%
  summarise(N = n()) %>%
  ungroup() %>%
  complete(N, fill = list(N = 0)) %>%
  mutate(N = replace(N, N > 0, 1)) %>%
  pivot_wider(names_from = TP, values_from = N, values_fill = list(N = 0))

y <- as.list(unique(McN$Biome))

McTest <- function(x) {
  a <- McN %>% filter(Biome == x)
  mcnemar.test(table(factor(a$T, levels = c(0,1)), factor(a$P, levels = c(0,1))))
  
}

x <- lapply(y, McTest)
names(x) <- unique(McN$Biome)
x

### Create graphs --------------------------------------------------------------------------------------------------------
df1 <- df[which(df$Biome %in% c("BOR", "TUN", "DES", "MED")),]
df2 <- df[which(df$Biome %in% c("TBM", "TCF", "TGS","TMB", "TSG")),]



Plot1 <- ggplot(df1, aes(ymin = df1$Closes, ymax = df1$Open, x = df1$Studies, colour = df1$TP, group = df1$TP)) +
  geom_point(aes(y=df1$Open, x = df1$Studies), shape = 4, position = position_dodge(width = 0.4), size = 1, stroke = 2)+
  geom_point(aes(y=df1$Closes, x = df1$Studies), shape = 3, position = position_dodge(width = 0.4), size = 1, stroke = 2)+
  geom_linerange(aes(ymin = df1$Closes, ymax = df1$Open, x = df1$Studies), size = 1.25 ,position = position_dodge(width = 0.4)) +
  coord_flip()+
  scale_y_continuous(breaks = c(0,6,12,18,24), 
                     name = "Time windows of climate driver") +
  scale_colour_manual(name = "Climate Variable", 
                      labels = c('Precipitation', 'Temperature'),
                      values = c('P' = '#0072B2', 'T' = '#D55E00'),
                      breaks = c('P', 'T')) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = -1.5),
        text = element_text(size = 20)) +
  facet_grid(rows = vars(Biome), scales = "free_y", space = "free")
  
Plot2 <- ggplot(df2, aes(ymin = df2$Closes, ymax = df2$Open, x = df2$Studies, colour = df2$TP, group = df2$TP)) +
  geom_point(aes(y=df2$Open, x = df2$Studies), shape = 4, position = position_dodge(width = 0.4), size = 1, stroke = 2)+
  geom_point(aes(y=df2$Closes, x = df2$Studies), shape = 3, position = position_dodge(width = 0.4), size = 1, stroke = 2)+
  geom_linerange(aes(ymin = df2$Closes, ymax = df2$Open, x = df2$Studies), size = 1.25 ,position = position_dodge(width = 0.4)) +
  coord_flip()+
  scale_y_continuous(breaks = c(0,6,12,18,24), 
                     name = "Time windows of climate driver") +
  scale_colour_manual(name = "Climate Variable", 
                      labels = c('Precipitation', 'Temperature'),
                      values = c('P' = '#0072B2', 'T' = '#D55E00'),
                      breaks = c('P', 'T')) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = -1.5),
        text = element_text(size = 20))+
  facet_grid(rows = vars(Biome), scales = "free_y", space = "free")

dims <- get_dim(Plot1)
Plot2 <- set_dim(Plot2, dims)

ggsave("Visual/Time windows1.png", Plot1, width = 10, height = 9)
ggsave("Visual/Time windows2.png", Plot2, width = 10, height = 9)


arid <- df[which(df$Biome == "DES"),]

Arid <- ggplot(arid, aes(ymin = arid$Closes, ymax = arid$Open, x = arid$Studies, colour = arid$TP, group = arid$TP)) +
  geom_point(aes(y=arid$Open, x = arid$Studies), shape = 4, position = position_dodge(width = 0.4), size = 1, stroke = 2)+
  geom_point(aes(y=arid$Closes, x = arid$Studies), shape = 3, position = position_dodge(width = 0.4), size = 1, stroke = 2)+
  geom_linerange(aes(ymin = arid$Closes, ymax = arid$Open, x = arid$Studies), size = 1.25 ,position = position_dodge(width = 0.4)) +
  coord_flip()+
  scale_y_continuous(breaks = c(0,6,12,18,24), 
                     name = "Time windows of climate driver") +
  scale_colour_manual(name = "Climate Variable", 
                      labels = c('Precipitation', 'Temperature'),
                      values = c('P' = '#0072B2', 'T' = '#D55E00'),
                      breaks = c('P', 'T')) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = -1.5),
        text = element_text(size = 20))

ggsave("Visual/Time windows Arid.png", Arid)


