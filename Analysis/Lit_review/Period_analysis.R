## Create plots for an overview of populations in the plant revies (Compagnoni et al., in preperation)
##

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpattern)
library(patchwork)
library(RVAideMemoire)
library(scales)
library(ggpattern)


### Read data -----------------------------------------------------------------------------------------------------------------
data <- readxl::read_excel("Data/Lit_review/Biomes from sApropos.xlsx",sheet = "Sheet1", skip = 1)
data <- data[!is.na(data$Biome),]

### create dataframes -----------------------------------------------------------------------------------------------------------------
temp <- data %>%
  group_by(Authors) %>%
  summarise(Annual = sum(Annual...9),
            Growing = sum(Growing...11),
            Dormant = sum(Dormant...12)) %>%
  mutate(Annual = replace(Annual, Annual > 0, 1),
         Growing = replace(Growing, Growing > 0, 1),
         Dormant = replace(Dormant, Dormant > 0, 1),
         Climate_Variable = "Temperature")

prcp <- data %>%
  group_by(Authors) %>%
  summarise(Annual = sum(Annual...18),
            Growing = sum(Growing...20),
            Dormant = sum(Dormant...21)) %>%
  mutate(Annual = replace(Annual, Annual > 0, 1),
         Growing = replace(Growing, Growing > 0, 1),
         Dormant = replace(Dormant, Dormant > 0, 1),
         Climate_Variable = "Precipitation")


### Cochran's Q test -----------------------------------------------------------------------------------------------------------------

df <- rbind(temp, prcp) %>%
  pivot_longer(cols = c(Annual, Growing, Dormant), names_to = "Period", values_to = "Used") %>%
  mutate(Period = as.factor(Period),
         Authors = as.factor(Authors),
         Used = replace(Used, is.na(Used), 0))

df_temp <- df[which(df$Climate_Variable == "Temperature"),]
df_prcp <- df[which(df$Climate_Variable == "Precipitation"),] 

temp_mcnemar <- df_temp %>% pivot_wider(names_from = Period, values_from = Used)
prcp_mcnemar <- df_prcp %>% pivot_wider(names_from = Period, values_from = Used)

cochran.qtest(Used ~ Period | Authors, data = df_temp)
mcnemar.test(table(temp_mcnemar[,c("Annual", "Dormant")]))
mcnemar.test(table(temp_mcnemar[,c("Annual", "Growing")]))
mcnemar.test(table(temp_mcnemar[,c("Growing", "Dormant")]))

cochran.qtest(Used ~ Period | Authors, data = df_prcp)
mcnemar.test(table(prcp_mcnemar[,c("Annual", "Dormant")]))
mcnemar.test(table(prcp_mcnemar[,c("Annual", "Growing")]))
mcnemar.test(table(prcp_mcnemar[,c("Growing", "Dormant")]))

### Divide in all possible categories for plot -----------------------------------------------------------------------------------------------------------------

temp$category <- ifelse(temp$Annual == 1 & temp$Growing == 0 & temp$Dormant == 0, "Annual",
                        ifelse(temp$Annual == 0 & temp$Growing == 1 & temp$Dormant == 0, "Growing season",
                               ifelse(temp$Annual == 0 & temp$Growing == 0 & temp$Dormant == 1, "Dormant season",
                                      ifelse(temp$Annual == 1 & temp$Growing == 1 & temp$Dormant == 0, "Annual & Growing",
                                             ifelse(temp$Annual == 1 & temp$Growing == 0 & temp$Dormant == 1, "Annual & Dormant",
                                                    ifelse(temp$Annual == 0 & temp$Growing == 1 & temp$Dormant == 1, "Growing & Dormant",
                                                           ifelse(temp$Annual == 1 & temp$Growing == 1 & temp$Dormant == 1, "All", NA)))))))
prcp$category <- ifelse(prcp$Annual == 1 & prcp$Growing == 0 & prcp$Dormant == 0, "Annual",
                               ifelse(prcp$Annual == 0 & prcp$Growing == 1 & prcp$Dormant == 0, "Growing season",
                                      ifelse(prcp$Annual == 0 & prcp$Growing == 0 & prcp$Dormant == 1, "Dormant season",
                                             ifelse(prcp$Annual == 1 & prcp$Growing == 1 & prcp$Dormant == 0, "Annual & Growing",
                                                    ifelse(prcp$Annual == 1 & prcp$Growing == 0 & prcp$Dormant == 1, "Annual & Dormant",
                                                           ifelse(prcp$Annual == 0 & prcp$Growing == 1 & prcp$Dormant == 1, "Growing & Dormant",
                                                                  ifelse(prcp$Annual == 1 & prcp$Growing == 1 & prcp$Dormant == 1, "All", NA)))))))

a <- temp %>%
  group_by(category, Climate_Variable) %>%
  summarise(number = n())
b <- prcp %>%
  group_by(category, Climate_Variable) %>%
  summarise(number = n())
c <- rbind(a,b)
c$category <- factor(c$category, levels = c("All", "Dormant season", "Growing & Dormant", "Annual & Dormant", 
                                            "Growing season", "Annual & Growing", "Annual"))
c$Climate_Variable <- factor(c$Climate_Variable, levels = c("Temperature", "Precipitation"))

c <- c %>% subset(!is.na(category)) %>%
  group_by(Climate_Variable) %>%
  mutate(prop_number = prop.table(number))

c$dorm <- NA
c$dorm[which(c$category %in% c("All", "Dormant season", "Growing & Dormant", "Annual & Dormant"))] <- "yes" 
c$dorm <- as.factor(c$dorm)

### Create Plot -----------------------------------------------------------------------------------------------------------------
d <- ggplot() +
  geom_bar(data = c, stat = "identity", aes(x = Climate_Variable, y = prop_number, fill = category),
                   position = position_stack(reverse = TRUE)) +
  geom_bar_pattern(data = c %>% drop_na(dorm),
                   stat = "identity", aes(x = Climate_Variable, y = prop_number, 
                                          fill = category, pattern_fill = dorm, pattern_colour = dorm),
                   pattern = "stripe",
                   position = position_stack(reverse = TRUE),
                   show.legend = FALSE) +
  geom_text(aes(x = 1, y = 1, label = c("N =  36")), vjust = -0.75, size = 5) +
  geom_text(aes(x = 2, y = 1, label = c("N =  58")), vjust = -0.75, size = 5) +
  ylim(c(0,1)) +
  theme_classic() +
  ylab("Proportion of Studies") +
  xlab("Climate Variable") +
  ggtitle("The type of time periods \nconsidered by studies", subtitle = "") +
  scale_fill_manual(breaks = c("Annual", "Annual & Growing", "Growing season", "Annual & Dormant", "Growing & Dormant", "Dormant season", "All"),
                    labels = c("Annual", "Annual & Growing", "Growing season", "Annual & Dormant", "Growing & Dormant", "Dormant season", "All"),
                    values = c(RColorBrewer::brewer.pal("Paired", n = 7), "Grey50"),
                    name = "Periods considered") +
  scale_pattern_color_manual(breaks = c("yes"),
                             labels = c("Dormatn season explicitely considered"),
                             values = c("black"), na.translate = FALSE) +
  scale_pattern_fill_manual(values = c("black"), na.translate = FALSE) +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(clip = "off")

ggsave("Visual/Seasons_plot.png", d, width = 7)


