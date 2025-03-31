#pacotes
library(ggplot2)
library(ggplot)

#' @param m densidade de vetores por hospedeiro
#' @param a taxa de picadas por vetor por dia
#' @param p taxa de sobrevivência diária do vetor
#' @param n período de incubação extrínseco do patógeno (Dias)
#' @param b competência vetorial
#'
#' @return estimativa da capacidade vetorial
#'
#' @export

calcula_cap_vet <- function(m, a, p, n, b){

  cap_v <- ((m*(a^2)*(p^n)*b))/(-log(p))

  return(round(cap_v, 3))
}
#vetores de parametros
m <- c(4, 3.6, 8.2, 4.8)
a <- c(0.75, 0.20, 0.14, 0.19)
p <- c(0.885, 0.801, 0.760, 0.780)
b <- c(0.64, 0.60, 0.76, 0.20)
n <- 10

#função geral
f <- calcula_cap_vet(m, a, p, n, b)

#data frame com os dados
df <- data.frame(Especie = c("Aedes Aegypti", "Aedes Albopictus", "Aedes Vigilax", "Aedes Notoscriptus"),
      Capacidade_vetorial = f)

#criação do gráfico de barras
ggplot(df, aes(x = Especie, y = Capacidade_vetorial, label = Capacidade_vetorial)) +

  geom_bar(stat = "identity", fill = "#9d19df", alpha = 0.8) + theme_bw() +

  labs(title = "Gráfico da capacidade vetorial para diferentes espécies de Aedes",
       x = "Espécie", y = "Capacidade vetorial") +
       coord_flip() +
       geom_label(size = 3.8, alpha = 0.75)
