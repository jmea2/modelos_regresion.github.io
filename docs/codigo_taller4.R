
# Cargas de librerias
pacman::p_load(rio,tidyverse,marginaleffects)

# carga de datos
# no olvide descargar la base hers.rds desde la pagina web del curso

coronary <- import("coronary.rds")

############## CON VARIABLE CATEGORICA

# Punto 1 y 2
mod_cad <- glm(formula = cad~gender,family = binomial("logit"),data = coronary)
summary(mod_cad)

# punto 3
exp(summary(mod_cad)$coefficients)

exp(confint(mod_cad))

# pùnto 4

predictions(mod_cad, newdata = data.frame(gender = "Mujer"))

######## CON VARIABLE NUMERICA

# Punto 1 y 2
mod_cad_num <- glm(formula = cad~chol,family = binomial("logit"),data = coronary)
summary(mod_cad_num)

# punto 3
exp(summary(mod_cad_num)$coefficients)

exp(confint(mod_cad_num))

# pùnto 4
# prediccion de riesgo de cad para un colesterol de 248
predictions(mod_cad_num, newdata = data.frame(chol = 248))

#prediccion de riesgo de cad para un colesterol de 140
predictions(mod_cad_num, newdata = data.frame(chol = 140))

