#####
source("summarySE.R")
library(tidyverse)
## Graficas respuestas correctas ------------------------------------------------
### Grafica por tarea ------------------------
summarySE(wm_RC, measurevar = "value", groupvars = c("Tarea", "Dificultad")) %>%
  ggplot(aes(x = factor(Tarea), y = value, fill = Tarea)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
    width = .4,
    size = .9
  ) +
  xlab("Tarea") +
  ylab("Porcentaje ± sd") +
  scale_fill_hue(
    name = "Tarea", # Legend label, use darker colors
    breaks = c("Mt", "Mp"),
    labels = c("Mantenimiento", "Manipulación")
  ) +
  ggtitle("Porcentaje de respuestas correctas\n por tareas") +
  ylim(c(0, 100)) +
  theme_minimal() +
  ggsave("graf/tarea.png", height = 3, width = 5)

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
  ylab("Porcentaje ± sd") +
  scale_fill_hue(
    name = "Turno", # Legend label, use darker colors
    breaks = c("1", "2", "3"),
    labels = c("Mañana", "Tarde", "Noche")
  ) +
  ggtitle("Porcentaje de respuestas correctas\n por turno y tarea") +
  ylim(c(0, 100)) +
  theme_minimal() +
  ggsave("graf/tarxtur.png", height = 3, width = 5)
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
  ylab("Porcentaje ± sd") +
  scale_fill_hue(
    name = "Turno", # Legend label, use darker colors
    breaks = c("1", "2", "3"),
    labels = c("Mañana", "Tarde", "Noche")
  ) +
  ggtitle("Porcentaje de respuestas correctas\n por turno") +
  ylim(c(0, 100)) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ggsave("graf/turno.png", height = 3, width = 5)

### Tiempo de reaccion ------------

summarySE(wm_RT, measurevar = "value", groupvars = c("Tarea", "Dificultad")) %>%
  ggplot(aes(x = factor(Tarea), y = value, fill = Tarea)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
    width = .4,
    size = .9
  ) +
  xlab("Tarea") +
  ylab("ms±sd") +
  scale_fill_hue(
    name = "Tarea", # Legend label, use darker colors
    breaks = c("Mt", "Mp"),
    labels = c("Mantenimiento", "Manipulación")
  ) +
  ggtitle("Tiempo de reacción\n por tareas") +
  ylim(c(0, 1000)) +
  theme_minimal() +
  ggsave("graf/tareaRT.png", height = 3, width = 5)

##### Grafica RT por tarea x turno -------------
summarySE(wm_RT, measurevar = "value", groupvars = c("Tarea", "Dificultad", "Turno")) %>%
  ggplot(aes(x = factor(Tarea), y = value, fill = Turno)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
    width = .4,
    size = .9,
    position = position_dodge(.9)
  ) +
  xlab("Tarea") +
  ylab("ms±sd") +
  scale_fill_hue(
    name = "Turno", # Legend label, use darker colors
    breaks = c("1", "2", "3"),
    labels = c("Mañana", "Tarde", "Noche")
  ) +
  ggtitle("Tiempo de reacción\n por turno y tarea") +
  ylim(c(0, 1000)) +
  theme_minimal() +
  ggsave("graf/tarxturRT.png", height = 3, width = 5)
##### Grafica RC por turno -------------
summarySE(wm_RT, measurevar = "value", groupvars = c("Dificultad", "Turno")) %>%
  ggplot(aes(x = factor(Turno), y = value, fill = Turno)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
    width = .4,
    size = .9,
    position = position_dodge(.9)
  ) +
  xlab("Turno") +
  ylab("ms±sd") +
  scale_fill_hue(
    name = "Turno", # Legend label, use darker colors
    breaks = c("1", "2", "3"),
    labels = c("Mañana", "Tarde", "Noche")
  ) +
  ggtitle("Tiempo de reacción\n por turno") +
  ylim(c(0, 1000)) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ggsave("graf/turnoRT.png", height = 3, width = 5)

###### Graficas  d prima-----------

summarySE(wm_Dprim, measurevar = "value", groupvars = c("Tarea", "Dificultad")) %>%
  ggplot(aes(x = factor(Tarea), y = value, fill = Tarea)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
    width = .4,
    size = .9
  ) +
  xlab("Tarea") +
  ylab("d'±sd") +
  scale_fill_hue(
    name = "Tarea", # Legend label, use darker colors
    breaks = c("Mt", "Mp"),
    labels = c("Mantenimiento", "Manipulación")
  ) +
  ggtitle("Indice d'\n por tareas") +
  theme_minimal() +
  ggsave("graf/tareaDprim.png", height = 3, width = 5)

##### Grafica RT por tarea x turno -------------
summarySE(wm_Dprim, measurevar = "value", groupvars = c("Tarea", "Dificultad", "Turno")) %>%
  ggplot(aes(x = factor(Tarea), y = value, fill = Turno)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
    width = .4,
    size = .9,
    position = position_dodge(.9)
  ) +
  xlab("Tarea") +
  ylab("d'±sd") +
  scale_fill_hue(
    name = "Turno", # Legend label, use darker colors
    breaks = c("1", "2", "3"),
    labels = c("Mañana", "Tarde", "Noche")
  ) +
  ggtitle("Indice d´\n por turno y tarea") +
  theme_minimal() +
  ggsave("graf/tarxturDprim.png", height = 3, width = 5)
##### Grafica RC por turno -------------
summarySE(wm_Dprim, measurevar = "value", groupvars = c("Dificultad", "Turno")) %>%
  ggplot(aes(x = factor(Turno), y = value, fill = Turno)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
    width = .4,
    size = .9,
    position = position_dodge(.9)
  ) +
  xlab("Turno") +
  ylab("d'±sd") +
  scale_fill_hue(
    name = "Turno", # Legend label, use darker colors
    breaks = c("1", "2", "3"),
    labels = c("Mañana", "Tarde", "Noche")
  ) +
  ggtitle("Indice d´\n por turno") +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ggsave("graf/turnoDprim.png", height = 3, width = 5)
