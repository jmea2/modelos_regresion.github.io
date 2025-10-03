# instalar libreria pacman
install.packages("pacman")

# instalar y/o activar librerias
pacman::p_load(rio, tidyverse, marginaleffects)

# carga de datos
lowbwt <- import("lowbwt.rds")

# Punto 1
ggplot(lowbwt, aes(x = apgar5, y = sbp))+
  geom_point()

# punto 2
modelo <- lm(formula = sbp ~gestage+apgar5, lowbwt)
summary(modelo)

# punto 3 y 4
predictions(modelo_low,newdata = data.frame(gestage = 31, apgar5 = 7))

# punto 5
summary(modelo)

# punto 6
# tasa de error
sigma(modelo)/mean(lowbwt$sbp)*100
# R^2
summary(modelo)$adj.r.squared

# punto 7
modelo1 <- lm(formula = sbp ~gestage, lowbwt)
anova(modelo1,modelo)

# punto 8
# graficos de residuos para diagnostico
plot(modelo)
# debe darle enter varias veces en la consola para que se produzca cada grafico
