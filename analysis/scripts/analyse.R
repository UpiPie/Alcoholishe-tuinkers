library("ggplot2")
library("dplyr")
library("tidyverse")
setwd("C:/Users/Fabian/Alcoholishe-tuinkers/raw_data")

df <- read.csv("2026-05-growth-tuinkers-alcohol.csv")
df <- rename(df, concentratie = concentratie_ethanol, lengte = lengte_cm)

df$concentratie <- factor(df$concentratie, levels = c("0.0%", "0.1%", "0.5%", "1%", "2%", "5%", "10%"), ordered = TRUE)
df$dag <- factor(df$dag)


df_day1 <- filter(df, dag %in% c(1,4,7,10))
df_day2 <- filter(df, dag %in% c(1,2,3,4,7,10))
df_day10 <- filter(df, dag == 10)

df_day10$concentratie <- factor(df_day10$concentratie)


data_samenvatting <- df_day2 %>%
  count(dag, concentratie, ontkiemd, name = "aantal")

data_samenvatting$ontkiemd <- factor(data_samenvatting$ontkiemd,
                                     levels = c("ja", "nee"),
                                     ordered = TRUE)

df_mean <- df %>%
  group_by(dag, concentratie) %>%
  summarise(mean_lengte = mean(lengte))

df_mean_petrischaal <- df %>%
  group_by(dag, petrischaal, concentratie) %>%
  summarise(mean_lengte = mean(lengte))

df_day10mean <- filter(df_mean_petrischaal, dag == 10)

# Anova test concnetratie verschillen
anova_resultaten_lengtes <- aov(lengte ~ concentratie, data = df_day10)
summary(anova_resultaten_lengtes)

TukeyHSD(anova_resultaten_lengtes)

# Anova test petrischalen verschillen
anova_model_petri <- aov(mean_lengte ~ petrischaal, data = df_day10mean)
summary(anova_model_petri)


# Chi test ontkieming dag 10
chi_ontkiemd <- chisq.test(table(df_day10$concentratie, df_day10$ontkiemd))
chi_ontkiemd



# Plots 
ggplot(df_day1, aes(x = concentratie, y = lengte)) +
  geom_boxplot() + 
  labs(x = "Concentratie Ethanol Oplossing", y = "Lengte (in cm)") +
  facet_wrap(~ dag)

ggplot(data_samenvatting, aes(x = concentratie, y = aantal, fill = ontkiemd)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("ja"= "darkgreen", "nee"="red"))+
  labs(x = "Concentratie Ethanol Oplossing", y = "Aantal zaadjes", fill = "Ontkiemd") +
  facet_wrap(~ dag)

ggplot(df_mean, aes(x = dag, y = mean_lengte, group = concentratie, color = factor(concentratie))) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6)) +
  scale_color_manual(values = c("0.0%" = "red", "0.1%" = "orange", "0.5%" = "yellow", "1%" = "green", "2%" = "blue", "5%" = "purple", "10%" = "violet")) +
  labs(x = "Dag", y = "Gemiddelde Lengte", color = "Concentratie")

ggplot(df_day10mean, aes(x = concentratie, y = mean_lengte, group = petrischaal, fill = factor(petrischaal))) +
  geom_col(position = "dodge") +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6)) +
  scale_fill_manual(values = c("1" = "orange", "2" = "purple", "3" = "green")) +
  labs(x = "Concentratie (Ethanol oplossing)", y = "Gemiddelde Lengte (in cm)", fill = "Petrischaal")




