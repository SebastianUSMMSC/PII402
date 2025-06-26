
setwd("D:/Educación/Magister/Semestre_1/PII402 - Procesos Estocasticos/Trabajo")

# Cargar librerías
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(tibble)

# Leer el archivo Excel
ruta <- "BaseDatos.xlsx"
df <- read_excel(ruta)

# Asegurar nombres válidos
names(df)[1] <- "Fecha"
df <- df %>%
  mutate(Fecha = as.Date(Fecha)) %>%
  arrange(Fecha) %>%
  column_to_rownames("Fecha")

# Convertir todas las columnas a numéricas
df[] <- lapply(df, function(x) as.numeric(as.character(x)))

# Calcular retornos logarítmicos
retornos_log <- diff(log(as.matrix(df)))
retornos_log <- na.omit(retornos_log)

# Separar IPSA y activos
ipsa <- retornos_log[, "IPSA"]
activos <- retornos_log[, colnames(retornos_log) != "IPSA"]

# Calcular retorno logarítmico acumulado anual promedio del IPSA (años completos)
fechas <- as.Date(rownames(retornos_log))
años <- format(fechas, "%Y")
retorno_anual_ipsa <- tapply(ipsa, años, sum)

# Filtrar solo años completos (2022, 2023, 2024)
retorno_anual_ipsa <- retorno_anual_ipsa[names(retorno_anual_ipsa) %in% c("2022", "2023", "2024")]
promedio_anual_ipsa <- mean(retorno_anual_ipsa)
cat("Promedio del retorno logarítmico acumulado anual del IPSA (años completos):", promedio_anual_ipsa, "\n")

# Calcular desplazamiento: promedio del valor absoluto y desviación estándar
promedios <- apply(abs(activos), 2, mean)
desviaciones <- apply(abs(activos), 2, sd)

desplazamiento <- data.frame(
  Activo = names(promedios),
  Promedio = promedios,
  DesviacionEstandar = desviaciones
)

# Ver resultados clave
cat("Promedio del retorno logarítmico anual del IPSA:", promedio_anual_ipsa, "\n")
head(desplazamiento)



################################################################################

#Clasificación

# 1. Clasificación por volatilidad
# Obtener percentiles 20 y 80 de la desviación estándar del desplazamiento
p20 <- quantile(desplazamiento$DesviacionEstandar, 0.20, na.rm = TRUE)
p80 <- quantile(desplazamiento$DesviacionEstandar, 0.80, na.rm = TRUE)

# Asignar categoría de volatilidad y el parámetro c
desplazamiento <- desplazamiento %>%
  mutate(
    Volatilidad = case_when(
      DesviacionEstandar < p20 ~ "Baja",
      DesviacionEstandar > p80 ~ "Alta",
      TRUE ~ "Media"
    ),
    c_param = case_when(
      Volatilidad == "Baja" ~ 0.15,
      Volatilidad == "Media" ~ 0.30,
      Volatilidad == "Alta" ~ 0.50
    )
  )


# Establecer semilla para reproducibilidad
# ============================
set.seed(123)

# 2. Simulación del tick aleatorio (δ)
# Definir función para generar tick aleatorio controlado
simular_tick <- function(mu_abs, sigma_abs, c_val, epsilon = 0.0001) {
  ancho <- min(c_val * sigma_abs, mu_abs - epsilon)
  delta_min <- mu_abs - ancho
  delta_max <- mu_abs + ancho
  runif(1, delta_min, delta_max)
}

# Generar columna con tick aleatorio para cada activo
desplazamiento <- desplazamiento %>%
  rowwise() %>%
  mutate(
    Tick = simular_tick(Promedio, DesviacionEstandar, c_param)
  ) %>%
  ungroup()


# 3. Visualización preliminar
# Mostrar primeros activos con parámetros y tick generado
head(desplazamiento[, c("Activo", "Volatilidad", "c_param", "Promedio", "DesviacionEstandar", "Tick")])


###############################################################################

#Generacion de Malla

# 1. Definir umbrales de absorción
TP <- promedio_anual_ipsa     # umbral de éxito
SL <- -TP / 2                 # umbral de pérdida

# 2. Función para construir la malla adaptativa de un activo
generar_malla_activo <- function(mu_abs, sigma_abs, c_val, tick_seed, TP, SL, epsilon = 0.0001) {
  set.seed(tick_seed)  # Para reproducibilidad específica por activo
  
  # Función de tick aleatorio
  tick <- function() {
    ancho <- min(c_val * sigma_abs, mu_abs - epsilon)
    delta_min <- mu_abs - ancho
    delta_max <- mu_abs + ancho
    runif(1, delta_min, delta_max)
  }
  
  # Malla hacia abajo (desde 0 hasta SL)
  estado <- 0
  abajo <- c()
  repeat {
    paso <- tick()
    nuevo <- estado - paso
    if (nuevo <= SL) {
      abajo <- c(abajo, SL)
      break
    } else {
      abajo <- c(abajo, nuevo)
      estado <- nuevo
    }
  }
  
  # Malla hacia arriba (desde 0 hasta TP)
  estado <- 0
  arriba <- c()
  repeat {
    paso <- tick()
    nuevo <- estado + paso
    if (nuevo >= TP) {
      arriba <- c(arriba, TP)
      break
    } else {
      arriba <- c(arriba, nuevo)
      estado <- nuevo
    }
  }
  
  # Combinar malla: [SL, ..., 0, ..., TP]
  malla <- c(rev(abajo[-length(abajo)]), 0, arriba)
  return(malla)
}


###############################################################################

# Construcción de malla por activo

# Crear lista de mallas para todos los activos
mallas_por_activo <- list()

# Iterar sobre cada fila del dataframe de desplazamientos
for (i in 1:nrow(desplazamiento)) {
  activo_nombre <- desplazamiento$Activo[i]
  mu_abs <- desplazamiento$Promedio[i]
  sigma_abs <- desplazamiento$DesviacionEstandar[i]
  c_val <- desplazamiento$c_param[i]
  tick_seed <- i  # Semilla distinta para cada activo
  
  malla <- generar_malla_activo(
    mu_abs = mu_abs,
    sigma_abs = sigma_abs,
    c_val = c_val,
    tick_seed = tick_seed,
    TP = TP,
    SL = SL
  )
  
  mallas_por_activo[[activo_nombre]] <- malla
}


# Ejemplo de inspección

# Mostrar malla de un activo específico
print(mallas_por_activo[["AGUAS-A"]])
length(mallas_por_activo[["AGUAS-A"]])  # número de estados

print(mallas_por_activo[["LTM"]])
length(mallas_por_activo[["LTM"]])  # número de estados





