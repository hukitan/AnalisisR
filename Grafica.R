#####
source("summarySE.R")
library(tidyverse)
#### Grafica por tarea ------------------------
summarySE(wm_RC, measurevar = "value", groupvars = c("Tarea", "Dificultad")) %>%
  ggplot(aes(x = factor(Tarea), y = value, fill = Tarea)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
    width = .4,
    size = .9
  ) +
  xlab("Tarea") +
  ylab("% de respuestas correctas") +
  scale_fill_hue(
    name = "Tarea", # Legend label, use darker colors
    breaks = c("Mt", "Mp"),
    labels = c("Mantenimiento", "Manipulación")
  ) +
  ggtitle("Porcentaje de respuestas correctas\n por tareas") +
  ylim(c(0, 100)) +
  theme_minimal() +
ggsave("graf/tarea.png")

##### Grafica por tarea x turno -------------
summarySE(wm_RC, measurevar = "value", groupvars = c("Tarea", "Dificultad", "Turno")) %>%
  ggplot(aes(x = factor(Tarea), y = value, fill = Turno)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
    width = .4,
    size = .9,
    position = position_dodge(.9)
  ) +
  xlab("Tarea") +
  ylab("% de respuestas correctas") +
  scale_fill_hue(
    name = "Turno", # Legend label, use darker colors
    breaks = c("1", "2", "3"),
    labels = c("Mañana", "Tarde", "Noche")
  ) +
  ggtitle("Porcentaje de respuestas correctas\n por turno y tarea") +
  ylim(c(0, 100)) +
  theme_minimal() +
ggsave("graf/tarxtur.png")
##### Grafica por turno -------------
summarySE(wm_RC, measurevar = "value", groupvars = c("Dificultad", "Turno")) %>%
  ggplot(aes(x = factor(Turno), y = value, fill = Turno)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
    width = .4,
    size = .9,
    position = position_dodge(.9)
  ) +
  xlab("Turno") +
  ylab("% de respuestas correctas") +
  scale_fill_hue(
    name = "Turno", # Legend label, use darker colors
    breaks = c("1", "2", "3"),
    labels = c("Mañana", "Tarde", "Noche")
  ) +
  ggtitle("Porcentaje de respuestas correctas\n por turno") +
  ylim(c(0, 100)) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank()) +
  ggsave("graf/turno.png")
  