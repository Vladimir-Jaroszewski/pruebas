install.packages("tidyverse")  # Para manejo de datos
install.packages("broom")      # Para organizar los resultados de la regresión
install.packages("writexl")
library(writexl)
library(tidyverse)
library(broom)
library(openxlsx)

data<-read.xlsx('C:/Users/Admin/Documents/GitHub/pruebas/eco pol/regresion.xlsx',sheet="regresion")

# Prediccion con preguntas economicas 

voto <- data$voto
P1E <- data$puntaje.1E
P2E <- data$puntaje.2E
P3E <- data$puntaje.3E
P4E <- data$puntaje.4E
P5E <- data$puntaje.5E
P6E <- data$puntaje.6E
P7E <- data$puntaje.7E
P8E <- data$puntaje.8E

P1S <- data$puntaje.1S
P2S <- data$puntaje.2S
P3S <- data$puntaje.3S
P4S <- data$puntaje.4S
P5S <- data$puntaje.5S
P6S <- data$puntaje.6S
P7S <- data$puntaje.7S
P8S <- data$puntaje.8S
P9S <- data$puntaje.9S


# Prediccion con preguntas economicas

modeloEco <- glm(voto ~ P1E + P2E + P3E + P4E + P5E + P6E + P7E + P8E , family = binomial )

modeloEc <- glm(voto ~ P5E  + P7E , family = binomial )


summary(modeloEc)

tidy(modelo)  # Para ver una tabla ordenada de los coeficientes y p-valores

datosFm_Ec <- data.frame(voto, P5E, P7E )

datosFm_Ec$predicciones <- predict(modeloEc, type = "response")

# Extraer las primeras 5 observaciones de cada pregunta
Ec_5obs <- lapply(datos_encuestaEc[, -1], head, 5)

# Convertir la lista a un dataframe (opcional)
Ec_5 <- as.data.frame(primeras_5_observaciones)


write_xlsx(datosFm_Ec, "prediccion_Ec.xlsx")


# prediccion con preguntas sociales

modelosSoc <- glm(voto ~ P1S + P2S + P3S + P4S + P5S + P6S 
                 + P7S + P8S + P9S , family = binomial)

modeloSo <- glm(voto ~  P2S + P6S , family = binomial )

summary(modeloSo)

datosFm_So <- data.frame(voto, P1S, P2S, P3S, P4S, P5S, P6S, P7S, P8S, P9S)

datosFm_So$predicciones <- predict(modeloSo, type = "response")


write_xlsx(datosFm_So, "prediccion_So.xlsx")


# prediccion con todas las preguntas

modeloTot <- glm(voto ~ P2S + P7E, family = binomial )

datosFm_Tot <- data.frame(voto, P2S, P7E)
datosFm_Tot$predicciones <- predict(modeloTot, type = "response")
write_xlsx(datosFm_Tot, "prediccion_Tot.xlsx")

summary(modeloTot)

# VER MULTICOLINEALIDAD 
install.packages("car")
library(car)

# Calcular el VIF
vif(modeloTot)




