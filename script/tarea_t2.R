# =============================================================================
# Laboratorio Semana 5 — Un análisis completo con dplyr
# Fundamentos de Programación para Análisis Económico · UdeC-EAN
#
# Autor: [Matias Pino Parra]
# Fecha: [22/09/2026]
#
# Objetivo: responder UNA pregunta económica de principio a fin usando los
#           cinco verbos, group_by() y el pipe. Es el ensayo directo de la
#           T2 (calificada, 13 %): el mismo formato, otra pregunta.
#
# Regla IA: ChatGPT es CONSULTOR, no escritor. Debes poder explicar cada línea.
# =============================================================================

library(dplyr)

# Se ejecuta desde la RAÍZ del proyecto
casen    <- read.csv("data/raw/casen_reducido.csv")
ingresos <- read.csv("data/raw/casen_ingresos.csv")   # mismas 60 personas,
# con el ingreso desagregado


# -----------------------------------------------------------------------------
# PASO 1 — Tu pregunta
# -----------------------------------------------------------------------------
# MI PREGUNTA: ¿Cuál es la brecha de ingreso en promedio entre hombres y mujeres 
# con baja, media y alta experiencia laboral de cualquiera de las regiones estudiadas?


# -----------------------------------------------------------------------------
# PASO 2 — Auditar antes de analizar
# -----------------------------------------------------------------------------
str(casen)
dim(casen)
summary(casen$ingreso)      
sum(is.na(casen$ingreso))      

# ANOTACIONES:
# FILAS: 60   COLUMNAS: 6   FALTANTES EN INGRESO: 5


# -----------------------------------------------------------------------------
# PASO 2b — Elegir columnas cuando son muchas
# -----------------------------------------------------------------------------
names(ingresos)

names(select(ingresos, starts_with("ing")))       # por cómo EMPIEZA el nombre
names(select(ingresos, where(is.numeric)))        # por el TIPO de contenido (numérico)

# ¿Por qué el segundo devuelve MÁS columnas que el primero? 
# Porque el segundo seleccionador (where(is.numeric)) incluye todas las variables 
# que contienen números (como edad, educ y horas), mientras que starts_with("ing") 
# solo selecciona las que empiezan con esa palabra.


# -----------------------------------------------------------------------------
# PASO 3 — Preparar las variables (mutate)
# -----------------------------------------------------------------------------
casen <- casen |>
  mutate(
    experiencia = pmax(edad - educ - 6, 0)
  )

# Se crea la variable con case_when() exigiendo 3 niveles 
casen <- casen |>
  mutate(
    nivel_experiencia = case_when(
      experiencia <  10 ~ "Baja experiencia",
      experiencia >= 10 & experiencia <= 20 ~ "Media experiencia",
      TRUE       ~ "Alta experiencia"
    )
  )

# Verifica que quedaron bien creadas y que NINGUNA quedó en NA.
summary(casen$experiencia)
table(casen$nivel_experiencia, useNA = "ifany")


# -----------------------------------------------------------------------------
# PASO 4 — Responder con una cadena de verbos (y agregaciones)
# -----------------------------------------------------------------------------
# (a) Primera agregación: Un grupo y uso de los 5 verbos
resultado_1 <- casen |>
  select(genero, nivel_experiencia, ingreso, sector) |> # Verbo select
  filter(sector == "Comercio" | sector == "Servicios" | sector == "Industria") |> # Verbo filter con |
  group_by(nivel_experiencia) |>
  summarise(
    n   = n(),               
    ingreso_promedio = mean(ingreso, na.rm = TRUE) # Uso de na.rm = TRUE explícito
  ) |>
  arrange(desc(ingreso_promedio))

print(resultado_1)

# (b) Segunda agregación: Dos grupos cruzados
resultado_2 <- casen |>
  filter(!is.na(ingreso)) |>
  group_by(nivel_experiencia, genero) |>
  summarise(
    n            = n(),
    ingreso_prom = mean(ingreso, na.rm = TRUE),
    .groups      = "drop"
  )
print(resultado_2)

# -----------------------------------------------------------------------------
# PASO 5 — La brecha de cada persona (S5S2) (group_by + mutate)
# -----------------------------------------------------------------------------
# Comparar a cada PERSONA con el promedio de su grupo:
brechas_individuales <- casen |>
  filter(!is.na(ingreso)) |>
  group_by(nivel_experiencia) |>
  mutate(
    ingreso_promedio = mean(ingreso, na.rm = TRUE),
    desviacion      = ingreso - ingreso_promedio
  ) |>
  ungroup() |>
  select(region, genero, edad, ingreso, ingreso_promedio, desviacion)


# -----------------------------------------------------------------------------
# PASO 6 — Mirar el resultado con desconfianza (Tratar los NA)
# -----------------------------------------------------------------------------
# TRATAMIENTO DE NA: Al calcular las medias con `na.rm = TRUE`, perdemos los 
# 5 casos donde el ingreso viene como faltante (NA). Esto es aceptable perderlo
# porque no podemos inventar el salario de esas personas, y dejarlos como NA 
# arruinaría el promedio matemático devolviendo NA para todo el grupo.

mean(casen$ingreso)                # Retorna NA
mean(casen$ingreso, na.rm = TRUE)  # Retorna $661.164
#se excluyeron los 5 registros que habian con valor NA


# -----------------------------------------------------------------------------
# PASO 7 — Interpretar 
# -----------------------------------------------------------------------------
resultado_final <- casen |>
  filter(!is.na(ingreso)) |>
  group_by(nivel_experiencia) |>
  summarise(
    M = mean(ingreso[genero == "M"], na.rm = TRUE),
    F = mean(ingreso[genero == "F"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    brecha_absoluta = M - F
  ) |>
  arrange(desc(brecha_absoluta))

print(resultado_final)

# INTERPRETACIÓN:
# En los datos observados, las mujeres categorizadas en "Baja Experiencia" 
#tienen una mayor brecha salarial dando como resultado que los hombres ganan en promedio
# 136086 pesos mas que las mujeres, en la logica esto puede tener sentido, muchas hombres
# con baja experiencia se dedican a hacer trabajos forzosos que si bien son mejor
# remunerados tambien suponen un desgaste fisico muy alto, como los son por ejemplo
# los obreros en la construccion haciendo la mano de obra pesada. 

# LIMITACIÓN: La muestra de la CASEN analizada está muy reducida (solo 60 observaciones)
# y cuenta con 5 ingresos faltantes que tuvimos que descartar. Por ende, los
# promedios pueden ser sensibles a datos atípicos y no representan causalidad generalizada.


# -----------------------------------------------------------------------------
# PASO 8 — Guardar el dataset limpio
# -----------------------------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE)
write.csv(casen, "data/processed/casen_s5_derivadas.csv", row.names = FALSE)

file.exists("data/processed/casen_s5_derivadas.csv")