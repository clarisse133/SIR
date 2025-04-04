#'calcula a capacidade vetorial (v) usando a fórmula de George Macdonald
#'
#' @param m  Densidade de vetores por humano
#' @param a  Taxa de picada do vetor por dia
#' @param P  Taxa de sobrevivência diária do vetor (0 < P < 1)
#' @param n  Período de incubação extrínseco do patógeno no vetor (em dias)
#'
#' @return   Estimativa da capacidade vetorial
#'
#' @export


calcular_v <- function(m, a, P, n){

  if(any(P<=0) | any(P>=1)) {

    stop("A taxa de sobrevivência (P) deve estar entre 0 e 1.")
  }

  est_v <- (m * a^2 * P^n) / (-log(P))

  return(est_v)
}
