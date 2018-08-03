library(readxl)
library(ez)
library(tidyverse)
# Importar y Ordenar datos ---------------------------------------------------
datos_wm_wide <- read_excel("WMdatos.xlsx", sheet = "Resultados")
# Es muy importante que los nombres de columna tengan un patron de
  # 2 caracteres por caracteristica acumulada
datos_wm <- gather(
  datos_wm_wide, dificultad_tarea, value,
  -c(Turno, Sujeto, SEXO, ShipleyPE, Prefsub, CT, Ctgrupo)
) %>%
  separate(dificultad_tarea, c("Tarea", "dificultad_modalidad"), sep = 2) %>%
  separate(dificultad_modalidad, c("Dificultad", "Modalidad"), sep = 2) %>%
  arrange(Sujeto, Tarea, Dificultad) %>%
  as.tibble()

datos_wm$Tarea <- factor(datos_wm$Tarea, ordered = F)
datos_wm$Turno <- factor(datos_wm$Turno,
   #levels = c("Mañana","Tarde","Noche"),
  ordered = T
)
datos_wm$Dificultad <- factor(datos_wm$Dificultad,
  levels = c("LL", "HL"),
  ordered = T
)


# Anova por respuestas correctas ------------------------------------------
wm_RC <- filter(datos_wm, Modalidad == "RC" & Dificultad == "HL")
# wm_RC <- filter(datos_wm, Modalidad == "RC")
aovrc <- ezANOVA(
  data = wm_RC, dv = value, wid = Sujeto, between = Turno,
  within = .(Tarea), return_aov = T
)
RC.post <- pairwise.t.test(wm_RC$value, interaction(
  wm_RC$Turno, wm_RC$Tarea,
  wm_RC$Dificultad
),
paired = F,
p.adjust.method = "bonferroni"
)
#otra manera de hacer el ANOVA

aovrc2 <- summary(aov(data = wm_RC, formula = value ~ Turno * Tarea + Error(Sujeto)))

# Anova por RT ---------------------------------------------
wm_RT <- filter(datos_wm, Modalidad == "RT" & Dificultad == "HL")

aovrt <- ezANOVA(
  data = wm_RT, dv = value, wid = Sujeto, between = Turno,
  within = .(Tarea), return_aov = T
)


# ANOVA por d' -----------------------------------------
wm_Dprim <- filter(datos_wm, Modalidad == "dp"  & Dificultad == "HL")
aovdprim <- ezANOVA(
  data = wm_Dprim, dv = value, wid = Sujeto,
  between = Turno, within = .(Tarea, Dificultad)
)



