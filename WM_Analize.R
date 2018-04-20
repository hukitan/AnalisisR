library(readxl)
library(tidyverse)
library(ez)
# Importar y Ordenar datos ---------------------------------------------------
datos_wm_wide <- read_excel("WMdatos.xlsx", sheet = "Resultados")
datos_wm <- gather(
  datos_wm_wide, dificultad_tarea, value,
  -c(Turno, Sujeto, ShipleyPE, Prefsub, CT, Ctgrupo)
) %>%
  separate(dificultad_tarea, c("Tarea", "dificultad_modalidad"), sep = 2) %>%
  separate(dificultad_modalidad, c("Dificultad", "Modalidad"), sep = 2) %>%
  arrange(Sujeto, Tarea, Dificultad)

datos_wm$Turno <- factor(datos_wm$Turno)
datos_wm$Dificultad <- factor(datos_wm$Dificultad,
  levels = c("LL", "HL", NA),
  ordered = T
)


# Anova por respuestas correctas ------------------------------------------
wm_RC <- filter(datos_wm, Modalidad == "RC" & Dificultad=="HL")
aovrc <- ezANOVA(
  data = wm_RC, dv = value, wid = Sujeto, between = Turno,
  within = .(Tarea), return_aov = T
)
pairwise.t.test(wm_RC$value, interaction(
  wm_RC$Tarea, wm_RC$Turno,
  wm_RC$Dificultad
),
paired = F,
p.adjust.method = "bonferroni"
)


# Anova por RT ---------------------------------------------
wm_RT <- filter(datos_wm, Modalidad == "RT")

aovrt <- ezANOVA(
  data = wm_RT, dv = value, wid = Sujeto, between = Turno,
  within = .(Tarea, Dificultad), return_aov = T
)


# ANOVA por d' -----------------------------------------
wm_Dprim <- filter(datos_wm, Modalidad == "dp")
aovdprim <- ezANOVA(
  data = wm_Dprim, dv = value, wid = Sujeto,
  between = Turno, within = .(Tarea, Dificultad)
)


# para algo servira tener las descipciones ----------------
wm_RC_desc <- psych::describeBy(datos_wm_wide, group = "Turno")
