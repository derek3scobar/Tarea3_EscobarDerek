# Ejercicio 1
#Recibe:
# -df: data frame de transacciones con columna fecha
# -ventana: entero con la cantidad de dias previos de la ventana movil
# -k.alerta: numero real con el multiplicador de desviacion estandar para el umbral
#
#Devuelve:
# -Data frame:
#  -fecha: fecha de cada dia evaluado
#  -n.tx: numero de transacciones registradas en el dia
#  -media: promedio movil de transacciones de los ventana dias previos
#  -desv: desviacion estandar muestral de los ventana dias previos
#  -alerta: valor logico que indica si n.tx supera media + k.alerta * desv
alerta.movil <- function(df, ventana, k.alerta){
  #Contamos las repeticiones por fecha
  df.diario <- df |>
    dplyr::count(fecha, name = "n.tx") |>
    dplyr::arrange(fecha)
  
  n.total <- nrow(df.diario)
  n.vec   <- df.diario$n.tx
  
  #Creación de vectores para inyectar la media, desviación estandar y aleta de cada día t
  media.vec  <- rep(NA_real_, n.total)
  desv.vec   <- rep(NA_real_, n.total)
  alerta.vec <- rep(FALSE, n.total)
  
  #Recorrido de cálculos y detección de alerta a partir de ventada + 1 hasta el final (n.total)
  for (t in seq(from = ventana + 1, to = n.total)) {
    bloque.previo <- n.vec[(t - ventana):(t - 1)]
    
    med <- mean(bloque.previo)
    desv.est <- sd(bloque.previo)
    
    media.vec[t]  <- med
    desv.vec[t]   <- desv.est
    alerta.vec[t] <- n.vec[t] > (med + k.alerta * desv.est)
  }
  
  #Pegamos los vectores al df.
  df.diario$media  <- media.vec
  df.diario$desv   <- desv.vec
  df.diario$alerta <- alerta.vec
  
  return(df.diario)
}

# Ejercicio 2
#
# Función 1
#Recibe:
# -x: vector numerico de montos positivos
#
#Devuelve:
# -MAD: numero real con la desviacion absoluta media respecto a la Ley de Benford
benford.mad <- function(x){
  # Limpiamos valores faltantes o no positivos
  x <- x[!is.na(x) & x > 0]
  # Extraemos el primer digito (1 a 9) de cada monto
  d <- x %/% 10^floor(log10(x))
  # Calculamos proporcion observada del digito d
  conteos <- tabulate(d, nbins = 9)
  pd <- conteos / sum(conteos)
  # Calculamos proporcion teorica de Benford para los digitos 1 al 9
  Bd <- log10(1 + 1 / (1:9))
  # Calculamos MAD
  MAD <- sum(abs(pd - Bd)) / 9
  return(MAD)
}

# Función 2
#Recibe:
# -df: data frame de transacciones con columnas canal, cod_canton y monto_crc
# -canal: cadena de texto con el canal a filtrar
# -cantones: data frame catalogo con cod_canton y canton
#
#Devuelve:
# -Data frame:
#  -cod_canton: codigo identificador del canton
#  -canton: nombre del canton
#  -n: cantidad de transacciones del canton en el canal
#  -mad: desviacion absoluta media de los montos del canton
#  -clase: clasificacion de conformidad segun umbrales de Nigrini
benford.canton <- function(df, canal, cantones){
  # Filtramos las transacciones del canal indicado
  df.canal <- df[df$canal == canal, ]
  
  # Calculamos n y MAD por canton usando benford.mad
  res.canton <- df.canal |>
    dplyr::group_by(cod_canton) |>
    dplyr::summarise(
      n = dplyr::n(),
      mad = benford.mad(monto_crc),
      .groups = "drop"
    )
  
  # Unimos con el catalogo oficial de cantones
  res.canton <- res.canton |>
    dplyr::right_join(cantones[, c("cod_canton", "canton")], by = "cod_canton")
  
  # Rellenamos n con 0 si hay cantones sin datos y clasificamos segun Nigrini
  res.canton <- res.canton |>
    dplyr::mutate(
      n = dplyr::coalesce(n, 0L),
      clase = dplyr::case_when(
        mad <= 0.006 ~ "conformidad cercana",
        mad <= 0.012 ~ "conformidad aceptable",
        mad <= 0.015 ~ "conformidad marginal",
        mad >  0.015 ~ "no conformidad",
        TRUE ~ NA_character_
      )
    )
  
  # Ordenamos columnas y filas de mayor a menor MAD
  res.canton <- res.canton |>
    dplyr::select(cod_canton, canton, n, mad, clase) |>
    dplyr::arrange(dplyr::desc(mad))
  
  res.canton <- as.data.frame(res.canton)
  return(res.canton)
}

# Ejercicio 3
#
#Recibe:
# -df: data frame de transacciones con columnas fecha, canal y monto_crc
# -tasa.anual: numero real con la tasa efectiva anual de interes
# -canal: cadena de texto con el canal a filtrar
#
#Devuelve:
# -Lista con:
#  -tabla: data frame con columnas mes (1 a 12), monto (Sm) y factor ((1 + i_m)^(12 - m))
#  -tasa.mensual: numero real con la tasa efectiva mensual equivalente
#  -va: numero real con el valor acumulado total al 31 de diciembre
#  -interes: numero real con los intereses ganados totales
valor.acumulado <- function(df, tasa.anual, canal){
  # Filtramos las transacciones del canal indicado y extraemos el mes
  df.canal <- df[df$canal == canal, ]
  df.canal$mes <- as.integer(substr(as.character(df.canal$fecha), 6, 7))
  
  # Sumamos el monto total del canal por mes (Sm)
  res.mes <- df.canal |>
    dplyr::group_by(mes) |>
    dplyr::summarise(
      monto = sum(monto_crc),
      .groups = "drop"
    )
  
  # Aseguramos los 12 meses completos en orden (1 a 12)
  tabla.mes <- data.frame(mes = 1:12) |>
    dplyr::left_join(res.mes, by = "mes") |>
    dplyr::mutate(monto = dplyr::coalesce(monto, 0)) |>
    dplyr::arrange(mes)
  
  # Calculamos la tasa efectiva mensual equivalente y el factor de acumulacion
  tasa.mensual <- (1 + tasa.anual)^(1 / 12) - 1
  tabla.mes$factor <- (1 + tasa.mensual)^(12 - tabla.mes$mes)
  
  # Calculamos el valor acumulado al 31-dic y los intereses ganados
  va <- sum(tabla.mes$monto * tabla.mes$factor)
  interes <- va - sum(tabla.mes$monto)
  
  tabla.mes <- as.data.frame(tabla.mes)
  
  resultado <- list(
    tabla        = tabla.mes,
    tasa.mensual = tasa.mensual,
    va           = va,
    interes      = interes
  )
  
  return(resultado)
}

