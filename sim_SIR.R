#Pacotes utilizados:
library(deSolve)
library(ggplot2)
library(reshape2)

# Simula a dinâmica do modelo SIR

SIR <- function(tempo, estado, param){

  with(as.list(c(estado, param)), {

  dS <- -beta * S * I
  dI <- (beta * S * I) - (gamma * I)
  dR <- gamma * I

  return(list(c(dS, dI, dR))) })
}

# Vetores de condições iniciais e parâmetros

condiçoes_iniciais <- c(
  S=0.999, #Suscetíveis iniciais
  I=0.001,   #Infectados iniciais
  R=0      #Recuperados iniciais
)

param <- c(
  beta=0.3,   #taxa de transmissão (por contato)
  gamma=0.1   #taxa de recuperação (1/duração méda da doença)
)

# Tempo em dias

tempo <- seq(0, 200, by=1)

# Resolução do modelo usando ode

output <- ode(condiçoes_iniciais, tempo, SIR, param)

summary(output)

# Plotando os resultados

out_df <- as.data.frame(output)

out.m = melt(out_df, id.vars='time')

plot <- ggplot(out.m, aes(time, value, color= variable)) +

geom_line(linewidth= 1.2) +

labs(x="Tempo (dias)", y="Porcentagem populacional" ) +

ggtitle("Gráfico simulação SIR") +

scale_color_manual(values = c("S"= "black", "I"="red", "R"="blue"))

print(plot)


