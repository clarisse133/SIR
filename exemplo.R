source("SIR/calcula_V.r")

m <- c(2, 1.5, 3)
a <- c(1.3, 1.3, 1.3)
p <- c(0.8, 0.85, 0.75)
n <- c(8, 8, 8)

y <- calcular_v(2, 1.3, 0.8, 8)

yv <- calcular_v(m, a, p, n)

y
yv

df <- data.frame(especie = c("Aedes A", "Aedes B", "Aedes C"), capacidade_vetorial = yv)

ggplot(aes(x=especie, y= capacidade_vetorial), data = df) + geom_col() +
  coord_flip() +
  theme_bw()

p<0

source("calculo_R0")
calcular_R0(2, 1.3, 1, 1, -log(0.8), 8, 1.5)

b <- c(1,1,1)
c <- c(1,1,1)

r <- c(3,4,5)

calcular_R0(m, a, b, c, -log(p), n, r)


source("simulação_sir")

plot

plot <- ggplot(out.m, aes(x=time, y=value,
                          color = variable)) +  geom_line()

plot

plot <- ggplot(out.m, aes(x=time, y=value,
                          color = variable)) + geom_line(size=2) +
  coord_cartesian(xlim = c(0, 150))+
  theme_bw()

plot
