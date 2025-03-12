#'calcula o número básico de reprodução (R0) usando a fórmula de Ronald Ross
#'
#' @param m Razão de mosquitos por humano
#' @param a Taxa de picadas por dia
#' @param b Probabilidade de transmissão vetor -> humano
#' @param c Probabilidade de transmissão humano -> vetor
#' @param g Taxa de mortalidade dos vetores
#' @param r Taxa de recuperação dos humanos
#' @param v Capacidade vetorial
#'
#' @return Estimativa de R0
#'
#' @examples
#' calcular_R0(10, 0.3, 0.1, 0.1, 0.1, 10, 5)
#'
#' @export

calcular_R0 <- function(m, a, b, c, g, v, r){

  if(v <= 0){
    stop("A capacidade vetorial (v) deve ser maior que 0.")
  }

  est_R0 <- (m * a^2 * b * c * exp(-g*r) *v)/(g * r)

  print(paste("O valor estimado de RO é:", round(est_R0, 3)))
  est_R0
  }
