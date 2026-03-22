```{r}
# Librerías necesarias
library(ggplot2)

# Datos
candidatos <- c("Bregman", "Massa", "Schiaretti", "Bullrich", "Milei")
valoracion <- c(2, 3, 4, 5, 1)

# Crear dataframe
df <- data.frame(Candidatos = candidatos, Valoracion = valoracion)

# Gráfico
ggplot(df, aes(x = Candidatos, y = Valoracion)) +
  geom_line(aes(group = 1), color = "green") +
  geom_point(color = "green") +
  labs(x = "Candidatos", y = "Valoración") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA))
 
