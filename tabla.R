# Se lee la base de datos de VDAM
library(readxl)
datos_vdam_wide <- read_excel("BaseVDAM.xlsx", sheet = "BaseDatos")
datos_vdam_wide <- datos_vdam_wide[-10,] #Elias elimina el sujeto 1010
rec <- function(x) {
  round(as.numeric(x), 2)
} # funcion para simplificar a dos digitos los resultados

sgmm <- "±" #Se reqiere para ejecutar directamente el script, sino colocar antes del source


# se declaran las filas de la tabla
titulo <- c(" ", "Mañana", "Tarde", "Noche", "Estadistica", "p")
N_tur <- c(
  "Sexo(Hombres/Mujeres)",
  paste(sum(datos_vdam_wide$SEXO == 1 & datos_vdam_wide$Grupo == 8), "/", sum(datos_vdam_wide$SEXO == 2 & datos_vdam_wide$Grupo == 8)),
  paste(sum(datos_vdam_wide$SEXO == 1 & datos_vdam_wide$Grupo == 12), "/", sum(datos_vdam_wide$SEXO == 2 & datos_vdam_wide$Grupo == 12)),
  paste(sum(datos_vdam_wide$SEXO == 1 & datos_vdam_wide$Grupo == 19), "/", sum(datos_vdam_wide$SEXO == 2 & datos_vdam_wide$Grupo == 19)),
  rec(chisq.test(datos_vdam_wide$SEXO)[1]),
  rec(chisq.test(datos_vdam_wide$SEXO)[3])
)

aov1 <- summary(aov(data = datos_vdam_wide,formula = Edad~Grupo))
Ed <- c(
  "Edad",
  paste(rec(mean(datos_vdam_wide$Edad[datos_vdam_wide$Grupo == 8])),sgmm,rec(sd(datos_vdam_wide$Edad[datos_vdam_wide$Grupo == 8]))),
  paste(rec(mean(datos_vdam_wide$Edad[datos_vdam_wide$Grupo == 12])),sgmm,rec(sd(datos_vdam_wide$Edad[datos_vdam_wide$Grupo == 12]))),
  paste(rec(mean(datos_vdam_wide$Edad[datos_vdam_wide$Grupo == 19])),sgmm,rec(sd(datos_vdam_wide$Edad[datos_vdam_wide$Grupo == 19]))),
  paste("F=(",aov1[[1]][["Df"]][1],",",aov1[[1]][["Df"]][2],")=",rec(aov1[[1]][["F value"]][1]),sep = ""),
  rec(aov1[[1]][["Pr(>F)"]][1])
  )




# se genera la tabla y se le asignan nombres
tabla <- data.table::data.table(rbind(N_tur,Ed))
names(tabla) <- titulo

View(tabla)