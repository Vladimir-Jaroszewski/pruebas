
serie <- rnorm(100000, mean = 0.2, sd = 0.05) 
plot(density(serie))

ser <- rchisq(1000,5)
plot(density(ser))
media_chi <- mean(ser)
de <- sd(ser)

ser_min <- min(ser)
ser_max <- max(ser)
chi_nor <- (ser-ser_min)/(ser_max-ser_min)
plot(chi_nor)

mediano <- median(chi_nor)
plot(density(chi_nor), xlim = c(-0.1,1), main = "Preferencias de impuesto", 
     xlab = "Tasa impositiva", ylab = "Distribución"  )
# Agregar la línea de la mediana
abline(v = mediana, col = "red", lwd = 2, lty = 2)  # Línea discontinua en la mediana

# Agregar un texto para indicar la mediana
text(mediano, 0.5, paste("Mediano =", round(mediana, 2)), col = "red", pos = 4)


summary(chi_nor)




set.seed(123)
moda1 <- rnorm(110, mean = 0.20, sd = 0.05) 
moda2 <- rnorm(80, mean = 0.50, sd = 0.05)  
bimodal_data <- c(moda1, moda2)
plot(density(bimodal_data), xlim = c(-0.1,1),main = "Preferencias Impositivas", xlab = "Tasa impositiva", ylab = "Distribución")
abline(v = 0.20, col = "red", lwd = 2, lty = 2)  
abline(v = 0.50, col = "green", lwd = 2, lty = 2)
abline(v = median(bimodal_data), col = "black", lwd = 2, lty = 2)
# Agregar etiquetas a las líneas
legend("topright", legend = c("Politca 1", "Politca 2", "Mediano"),
       col = c("red", "green", "black"), lwd = 2, lty = 2)
