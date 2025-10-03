# instalar libreria pacman
install.packages("pacman")

# instalar y/o activar librerias
pacman::p_load(rio, tidyverse, marginaleffects)

# carga de datos
lowbwt <- import("lowbwt.rds")
#####################################################################################

# Con covariable continua

# Punto 1
ggplot(lowbwt, aes(x = gestage, y = sbp))+
  geom_point()

# punto 2
modelo_low <- lm(formula = sbp ~gestage, lowbwt)
summary(modelo_low)


# punto 4 y 5 
predictions(modelo_low,newdata = data.frame(gestage = 31))

# punto 6
# tasa de error
sigma(modelo_low)/mean(lowbwt$sbp)*100
# R^2
summary(modelo_low)$r.squared
# graficos de residuos para diagnostico
plot(modelo_low)
# debe darle enter varias veces en la consola para que se produzca cada grafico

###############################################################################

# Con covariable categorica

# carga de datos
hers <- rio::import("https://regression.ucsf.edu/sites/g/files/tkssra16191/files/wysiwyg/home/data/hersdata.dta") %>% 
  filter(diabetes == 0)

# punto 1 y 2

modelo1 <- lm(formula = glucose ~ exercise, hers)
summary(modelo1)

# punto 3 
predictions(modelo1,newdata = data.frame(exercise = 0))

# punto 4 
predictions(modelo1,newdata = data.frame(exercise = 1))

# punto 5
# tasa de error
sigma(modelo1)/mean(hers$glucose)*100
# R^2
summary(modelo1)$r.squared
# graficos de residuos para diagnostico
plot(modelo1)
# debe darle enter varias veces en la consola para que se produzca cada grafico

