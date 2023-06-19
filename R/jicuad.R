#' Chi cuadrada
#'
#' Realiza el cálculo de chi cuadrada para diferenciar entre variables categóricas cualitativas
#'
#' @param w (fila) fila de una tabla de datos
#' @param x (fila) fila de una tabla de datos
#' @param y (columna) columna de una tabla de datos
#' @param z (columna) columna de una tabla de datos
#' @return Una lista con los Valores esperados para cado (A, B, C y D), el Valor de ji cuadrada calculada y los Grados de libertad para calcular ji cuadrada tabulada
#' @export
Chicuad <- function(a, b, c, d, w, x, y, z){
  #Cálculos preeliminares
  SumaW <- sum(w)
  SumaX <- sum(x)
  SumaY <- sum(y)
  SumaZ <- sum(z)

  ValObs <- sum(SumaW + SumaX + SumaY + SumaZ)

  #Cálculo de valores esperados para cada uno de los datos dentro de las filas x,w y columnas y,z
    VespA <- SumaY * SumaX / ValObs
    VespB <- SumaY * SumaW / ValObs
    VespC <- SumaZ * SumaX / ValObs
    VespD <- SumaZ * SumaW / ValObs

    ValEsp <- c(VespA, VespB, VespC, VespD)

  #Cálculo de ji cuadrada
  chicuad <- sum((a - VespA)^2/VespA + (b - VespB)^2/VespB + (c - VespC)^2/VespC + (d - VespD)^2/VespD)

  #Lista de resultados
  resfinal <- c(ValEsp, chicuad)

  #Cálculo de grados de libertad para filas y columnas
  glfila <- length(w) - 1
  glcol <- length(y) - 1
  Gradlib <- c(glfila, glcol)

  resultados <- list("Valores esperados A" = VespA,
                 "Valores esperados B" = VespB,
                 "Valores esperados C" = VespC,
                 "Valores esperados D" = VespD,
                 "Chi cuadrada calculada" = chicuad,
                 "Grados de libertad para calcular Xt" = Gradlib)

  return(resultados)

}


