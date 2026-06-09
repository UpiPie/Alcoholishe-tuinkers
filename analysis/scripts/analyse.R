library("ggplot2")
library(dplyr)
library("tidyverse")
setwd("/homes/fkinds/Alcoholishe-tuinkers/raw_data")

df <- read.csv("tuinkers-rawww.csv")
df$concentratie <- factor(df$concentratie, levels = c("0.0%", "0.1%", "0.5%", "1%", "2%", "5%", "10%"), ordered = TRUE)
df$dag <- factor(df$dag)


df_day1 <- df[df$dag == 1 | df$dag == 4 | df$dag == 7 | df$dag == 10, ]
df_day2 <- df[df$dag == 1 | df$dag == 2 | df$dag == 3 | df$dag == 4 | df$dag == 7 | df$dag == 10, ]
df_day10 <- df[df$dag == 10, ]


ggplot(df_day1, aes(x = concentratie, y = lengte)) +
  geom_boxplot() + 
  facet_wrap(~ dag)

model <- aov(lengte ~ concentratie, data = df_day10)
summary(model)

shapiro.test(residuals(model))
TukeyHSD(model)



df_day10$concentratie <- factor(df_day10$concentratie)
df_day10$ontkiemd <- factor(df_day10$ontkiemd,
                            levels = c("nee", "ja"))

model <- glm(ontkiemd ~ concentratie,
             family = binomial,
             data = df_day10)

summary(model)


data_samenvatting <- df_day2 %>%
  group_by(dag, concentratie, ontkiemd) %>%
  summarise(aantal = n(), .groups = "drop")

data_samenvatting$ontkiemd <- factor(data_samenvatting$ontkiemd,
                                     levels = c("ja", "nee"),
                                     ordered = TRUE)

ggplot(data_samenvatting, aes(x = concentratie, y = aantal, fill = ontkiemd)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("ja"= "darkgreen", "nee"="darkred"))+
  labs(
    x = "Alcoholconcentratie",
    y = "Aantal zaadjes",
    fill = "Ontkiemd"
  ) +
  theme_minimal()+
  facet_wrap(~ dag)


ggplot(df, aes(x = dag, y = lengte)) +
  geom_line()

df_mean <- df %>%
  group_by(dag, concentratie) %>%
  summarise(mean_lengte = mean(lengte, na.rm = TRUE))


ggplot(df_mean, aes(x = dag, y = mean_lengte, group = concentratie, color = factor(concentratie))) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6)) +
  scale_color_manual(values = c("0.0%" = "red", "0.1%" = "orange", "0.5%" = "yellow", "1%" = "green", "2%" = "blue", "5%" = "purple", "10%" = "violet")) +
  labs(x = "Dag", y = "Gemiddelde Lengte", color = "Concentratie")
