# Ejercicio 1

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
