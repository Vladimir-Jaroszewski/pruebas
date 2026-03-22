# Datos de ejemplo
x <- c(5, 7, 8, 6, 7, 6, 9, 10, 8, 6)
y <- c(7, 6, 5, 8, 9, 6, 7, 8, 7, 5)

# 1. Estimaciones puntuales
media <- mean(x)
mediana <- median(x)
varianza <- var(x)
desviacion_estandar <- sd(x)

# 2. Intervalo de confianza para la media (manual)
error_estandar <- sd(x) / sqrt(length(x))
t_value <- qt(0.975, df = length(x) - 1)  # Cuantil t para 95% de confianza
margen_error <- t_value * error_estandar
intervalo_confianza <- mean(x) + c(-1, 1) * margen_error

# También puedes usar t.test() para obtener el intervalo de confianza automáticamente
intervalo_confianza_ttest <- t.test(x)$conf.int

# 3. Pruebas de hipótesis
# Prueba t para una muestra (comparar la media con un valor)
prueba_t_una_muestra <- t.test(x, mu = 7)

# Prueba t para dos muestras independientes
prueba_t_dos_muestras <- t.test(x, y, var.equal = TRUE)

# Prueba de proporciones
# Supongamos que tenemos 45 éxitos en 100 ensayos
prueba_proporciones <- prop.test(45, 100)

# Prueba de chi-cuadrado (para tablas de contingencia)
tabla_contingencia <- matrix(c(20, 30, 10, 40), nrow = 2)
prueba_chi_cuadrado <- chisq.test(tabla_contingencia)

# 4. Otras medidas estadísticas útiles
rango_intercuartilico <- IQR(x)
quantiles <- quantile(x, probs = c(0.25, 0.5, 0.75))
correlacion <- cor(x, y)

# 5. Distribuciones y valores críticos
# Cuantil de la distribución t de Student
t_value_critico <- qt(0.975, df = length(x) - 1)

# Cuantil de la distribución normal
z_value_critico <- qnorm(0.975)

# Distribución normal: densidad y distribución acumulada
densidad_normal <- dnorm(x, mean = mean(x), sd = sd(x))
probabilidad_normal <- pnorm(x, mean = mean(x), sd = sd(x))

# 6. Pruebas no paramétricas
# Prueba de Mann-Whitney para dos muestras no paramétricas
prueba_mann_whitney <- wilcox.test(x, y)












library(openxlsx)
library(dplyr)

df <- read.xlsx("C:/Users/Admin/Documents/GitHub/pruebas/ddd.xlsx", sheet = 1)

media <- mean(df$datos)
media

De <- sd(df$datos)/sqrt(15)

qnorm(0.975)
qt(0.05,14)

Tcrit <- qt(0.05,14)
Tobs <- (media-5)/De

#Ho: u=> 5 , H1: u<5 
#No rechazo la Ho si Tcrit<Tobs

Criterio <- Tobs>Tcrit
#no Rechazo


#intervalo de confianza

Imax <- media-Tcrit*De
Imin <- media+Tcrit*De
Imax
Imin

qqnorm(df$datos, main="normalidad")
qqline(df$datos, col= "blue")
shapiro.test(df$datos)
plot(df$datos)








install.packages("mFilter")
library(mFilter)


# Crear una serie de ejemplo
serie <- ts(rnorm(120, mean = 100, sd = 10), frequency = 12)

# Aplicar el filtro Hodrick-Prescott con un factor de suavización freq = 1600 (usado comúnmente para datos trimestrales)
hp_fit <- hpfilter(serie, freq = 1600)

# Graficar la serie original, tendencia y ciclo
par(mfrow = c(2, 1))  # Dividir la ventana gráfica en 2 filas

# Graficar la tendencia
plot(hp_fit$trend, main = "Tendencia (Filtro Hodrick-Prescott)", col = "blue", ylab = "Valor", xlab = "Tiempo", lwd = 2)

# Graficar el ciclo
plot(hp_fit$cycle, main = "Ciclo (Desviaciones de la Tendencia)", col = "red", ylab = "Valor", xlab = "Tiempo", lwd = 2)
