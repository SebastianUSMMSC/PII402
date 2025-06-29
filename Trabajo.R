
setwd("D:/Educación/Magister/Semestre_1/PII402 - Procesos Estocasticos/Trabajo")

# Cargar librerías
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(tibble)
library(ggplot2)
library(writexl)
library(openxlsx)

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
# Filtrar activos solo para los años 2022, 2023 y 2024
fechas_activos <- as.Date(rownames(activos))
activos <- activos[year(fechas_activos) %in% c(2022, 2023, 2024), ]

# Calcular retorno logarítmico acumulado anual promedio del IPSA (años completos)
fechas <- as.Date(rownames(retornos_log))
años <- format(fechas, "%Y")
retorno_anual_ipsa <- tapply(ipsa, años, sum)

# Filtrar solo años completos (2022, 2023, 2024)
retorno_anual_ipsa <- retorno_anual_ipsa[names(retorno_anual_ipsa) %in% c("2022", "2023", "2024")]
promedio_anual_ipsa <- mean(retorno_anual_ipsa)

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

#%desplazamiento %>% 
#  select(Activo, Volatilidad, DesviacionEstandar,Promedio) %>% 
#  print(n = Inf)


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

#Generar columna con tick aleatorio para cada activo
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
# print(mallas_por_activo[["AGUAS-A"]])
# length(mallas_por_activo[["AGUAS-A"]])  # número de estados
# 
# print(mallas_por_activo[["LTM"]])
# length(mallas_por_activo[["LTM"]])  # número de estados


###############################################################################

# 1. Calcular retornos acumulados por activo
retornos_acumulados <- apply(activos, 2, cumsum)

# 2. Umbral y configuración
epsilon <- 0.0001
umbral_conteo <- 3



# Gráfico de retornos acumulados con línea de benchmark
graficar_retornos_acumulados <- function(activo_nombre) {
  serie <- retornos_acumulados[, activo_nombre]
  fechas <- as.Date(rownames(activos))
  data <- data.frame(Fecha = fechas, Retorno = serie)
  
  ggplot(data, aes(x = Fecha, y = Retorno)) +
    geom_line(color = "steelblue", size = 1) +
    geom_hline(yintercept = promedio_anual_ipsa, color = "red", linetype = "dashed", size = 1) +
    labs(title = paste("Retorno acumulado de", activo_nombre),
         y = "Retorno acumulado logarítmico", x = "Fecha") +
    theme_minimal()
}

# Ejemplo: graficar
graficar_retornos_acumulados("AGUAS-A")


#############################################################################


# PARÁMETROS
TP <- promedio_anual_ipsa        # Umbral superior
SL <- -TP / 2                    # Umbral inferior
umbral_mantencion <- 0.0001     # Umbral para considerar "sin movimiento"

# 1. FUNCIÓN PARA IDENTIFICAR FECHAS DE EPISODIOS SEGÚN TP y SL
identificar_episodios <- function(ret_acum, TP, SL) {
  fechas <- as.Date(names(ret_acum))  # <- CORREGIDO
  valores <- as.numeric(ret_acum)
  episodios <- c(fechas[1])
  ref <- valores[1]
  for (i in 2:length(valores)) {
    if (valores[i] >= ref + TP || valores[i] <= ref + SL) {
      episodios <- c(episodios, fechas[i])
      ref <- valores[i]
    }
  }
  return(episodios)
}


calcular_probabilidades_magnitud <- function(retornos, fechas_episodios) {
  fechas <- as.Date(names(retornos))
  lista_prob_episodios <- list()
  
  for (i in 1:(length(fechas_episodios) - 1)) {
    inicio <- fechas_episodios[i]
    fin <- fechas_episodios[i + 1]
    
    tramo <- retornos[fechas > inicio & fechas <= fin]
    if (length(tramo) == 0) next
    
    suma_sube <- sum(tramo[tramo > 0])
    suma_baja <- sum(abs(tramo[tramo < 0]))
    total <- suma_sube + suma_baja
    
    if (total > 0) {
      lista_prob_episodios[[length(lista_prob_episodios) + 1]] <- c(
        sube = suma_sube / total,
        baja = suma_baja / total
      )
    }
    
  }
  
  if (length(lista_prob_episodios) > 0) {
    matriz <- do.call(rbind, lista_prob_episodios)
    promedios <- colMeans(matriz, na.rm = TRUE)
    return(promedios)
  } else {
    return(c(sube = NA, baja = NA))
  }
}


# 3. ESTIMAR PROBABILIDADES PARA TODOS LOS ACTIVOS
probabilidades_finales <- data.frame()

for (activo in colnames(activos)) {
  
  ret_acum <- retornos_acumulados[, activo]
  ret_log <- activos[, activo]
  
  fechas_episodios <- identificar_episodios(ret_acum, TP, SL)
  
  probs <- calcular_probabilidades_magnitud(ret_log, fechas_episodios)
  
  probabilidades_finales <- rbind(probabilidades_finales,
                                  data.frame(Activo = activo,
                                             ProbSube = probs["sube"],
                                             ProbBaja = probs["baja"]))
}

# === 4. VER RESULTADOS ===
print(probabilidades_finales)

#############################################################################

#Grafico
# Graficar retornos acumulados de AGUAS-A con líneas verticales en los episodios

# 1. Serie de retornos acumulados del activo
activo_nombre <- "SMU"
serie <- retornos_acumulados[, activo_nombre]
fechas <- as.Date(rownames(retornos_acumulados))
data <- data.frame(Fecha = fechas, Retorno = serie)

# 2. Identificar fechas de episodios
fechas_episodios <- identificar_episodios(serie, TP, SL)

# 3. Crear gráfico con ggplot2


grafico <- ggplot(data, aes(x = Fecha, y = Retorno)) +
  geom_line(color = "steelblue", size = 1) +
  geom_vline(xintercept = as.numeric(fechas_episodios), color = "red", linetype = "dashed") +
  labs(
    title = paste("Retorno acumulado de", activo_nombre, "con episodios"),
    y = "Retorno acumulado logarítmico",
    x = "Fecha"
  ) +
  theme_minimal()
grafico

###############################################################################

# Crear lista para almacenar matrices de transición por activo
matrices_transicion <- list()

# Iterar sobre cada activo
for (i in 1:nrow(probabilidades_finales)) {
  activo <- probabilidades_finales$Activo[i]
  p_sube <- probabilidades_finales$ProbSube[i]
  p_baja <- probabilidades_finales$ProbBaja[i]
  
  malla <- mallas_por_activo[[activo]]
  n_estados <- length(malla)
  
  # Inicializar matriz de transición
  P <- matrix(0, nrow = n_estados, ncol = n_estados)
  
  # Estados absorbentes
  P[1, 1] <- 1
  P[n_estados, n_estados] <- 1
  
  # Llenar estados transitorios con estructura tridiagonal
  for (j in 2:(n_estados - 1)) {
    P[j, j - 1] <- p_baja
    P[j, j + 1] <- p_sube
  }
  
  matrices_transicion[[activo]] <- P
}

# Extraer matriz de transición y nombres de estados
ejemplo <- matrices_transicion[["LTM"]]
estados <- mallas_por_activo[["LTM"]]

# Convertir los nombres de estado a texto (opcionalmente con formato)
nombres_estados <- sprintf("%.4f", estados)  # redondear a 4 decimales

# Asignar nombres a filas y columnas
rownames(ejemplo) <- nombres_estados
colnames(ejemplo) <- nombres_estados
ejemplo

##############################################################################

#Simulacion

markov <- function(init,mat,n,labels) { 
  if (missing(labels)) labels <- 1:length(init)
  simlist <- numeric(n+1)
  states <- 1:length(init)
  simlist[1] <- sample(states,1,prob=init)
  for (i in 2:(n+1)) 
  { simlist[i] <- sample(states,1,prob=mat[simlist[i-1],]) }
  labels[simlist]
}


init <- c(0, 0, 1, 0, 0, 0, 0, 0, 0)

simlist <- markov(init,ejemplo,25,nombres_estados)
simlist

sim <- replicate(10000,markov(init,ejemplo,25,nombres_estados)[26])
table(sim)/10000















################################################################################

# Crear lista para guardar resultados
resultados_cadena <- list()

for (activo in names(matrices_transicion)) {
  P <- as.matrix(matrices_transicion[[activo]])
  malla <- as.numeric(mallas_por_activo[[activo]])
  n <- length(malla)
  
  # Validar tamaño mínimo
  if (n < 3) {
    warning(paste("Malla muy pequeña para activo", activo))
    next
  }
  
  estado_cero <- which(malla == 0)
  
  if (length(estado_cero) != 1) {
    warning(paste("Estado cero mal definido en", activo))
    next
  }
  
  estados_transitorios <- setdiff(1:n, c(1, n))
  estados_absorbentes <- c(1, n)
  
  Q <- P[estados_transitorios, estados_transitorios, drop = FALSE]
  R <- P[estados_transitorios, estados_absorbentes, drop = FALSE]
  
  I <- diag(nrow(Q))
  F <- solve(I - Q)
  t <- F %*% rep(1, nrow(F))
  B <- F %*% R
  
  idx_cero_rel <- which(estados_transitorios == estado_cero)
  
  resultados_cadena[[activo]] <- list(
    P = P,
    Q = Q,
    R = R,
    F = F,
    t = t,
    B = B,
    estado_cero = estado_cero,
    estado_cero_F = estado_cero - 1,
    tiempo_esperado_desde_estado_0 = t[idx_cero_rel],
    prob_exito_desde_estado_0 = B[idx_cero_rel, 2],
    prob_ruina_desde_estado_0 = B[idx_cero_rel, 1]
  )
}

resultados_cadena[["AGUAS-A"]]

# Extraer y mostrar prob_exito_desde_estado_0 para todos los activos
probs_exito <- sapply(resultados_cadena, function(x) x$prob_exito_desde_estado_0)
probs_dias <- sapply(resultados_cadena, function(x) x$tiempo_esperado_desde_estado_0)
factor <- sapply(resultados_cadena, function(x) ((x$tiempo_esperado_desde_estado_0 - 0.5)*(1- (x$tiempo_esperado_desde_estado_0/252))))

T_total <- 252 # Constante T (número total de días en el año financiero)
lambda <- 0.1 # Parámetro de ajuste lambda, este parametro se cambiara en Python

# Cálculo del factor multiplicativo para ajustar desviación estándar
factor <- sapply(resultados_cadena, function(x) {
  p <- x$prob_exito_desde_estado_0   # Probabilidad de éxito
  t <- x$tiempo_esperado_desde_estado_0  # Tiempo esperado hasta absorción
  ajuste <- (p - 0.5) * (1 - t / T_total)
  return (ajuste)
})

# Tabla
probs_df <- data.frame(
  ProbabilidadExito = round(probs_exito, 4),
  DiasEsperados = round(probs_dias, 4),
  Ajuste = round(factor, 4)
)

# Mostrar el resultado
print(probs_df)




# Crear libro de Excel
wb <- createWorkbook()

# Agregar retornos logarítmicos completos
addWorksheet(wb, "Retornos_Log")
writeData(wb, sheet = "Retornos_Log", x = data.frame(Fecha = as.Date(rownames(retornos_log)), retornos_log))

# Agregar retornos logarítmicos de los activos (sin IPSA)
addWorksheet(wb, "Activos")
writeData(wb, sheet = "Activos", x = data.frame(Fecha = as.Date(rownames(activos)), activos))

# Agregar tabla de probabilidades de éxito y ajustes
addWorksheet(wb, "Probabilidades")
writeData(wb, sheet = "Probabilidades", x = data.frame(Activo = rownames(probs_df), probs_df), rowNames = FALSE)

# Guardar archivo Excel
saveWorkbook(wb, "Datos_Portafolio_Markowitz.xlsx", overwrite = TRUE)





