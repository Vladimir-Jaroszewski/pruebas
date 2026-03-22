install.packages("cowplot")
library(ggplot2)
library(openxlsx)
library(corrplot)
library(cowplot)

data<-read.xlsx('C:/Users/Admin/Documents/GitHub/pruebas/eco pol/puntajes_voto.xlsx',sheet="ejes")

labels <- data.frame(label = c("Conservador","Liberal","Izquierda","Derecha"),
                     x = c(0, 0, -10, 10),
                     y = c(10, -10, 0, 0)
)
labels$angle <- ifelse(labels$label %in% c("Izquierda", "Derecha"), 90, 0)

# Pol compas
q <- ggplot(data, aes(eje.econ, eje.social)) +
  theme_minimal() +
  coord_fixed(ratio = 1, xlim = c(-10, 10), ylim = c(-10, 10)) +  # Solo coord_fixed, sin coord_cartesian
  geom_rect(aes(xmin = -9, xmax = 0,
                ymin = 0, ymax = 9),
            fill = "#ff7474") +
  geom_rect(aes(xmin = 0, xmax = 9,
                ymin = 0, ymax = 9),
            fill = "#44aafd") +
  geom_rect(aes(xmin = -9, xmax = 0,
                ymin = -9, ymax = 0),
            fill = "#98ec98") +
  geom_rect(aes(xmin = 0, xmax = 9,
                ymin = -9, ymax = 0),
            fill = "#c09aea")



# Crear el gráfico original
p <- q +
  geom_point(size = 3, colour = "red") +
  geom_point(size = 3, shape = 21) + 
  geom_text(data = labels, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle)) +
  labs(x = "", y = "")

# Mostrar el gráfico
plot(p)







# pol compas votos

va <- ggplot(data, aes(eje.econ, eje.social)) +
  theme_minimal() +
  coord_fixed(ratio = 1, xlim = c(-10, 10), ylim = c(-10, 10)) +  # Solo coord_fixed, sin coord_cartesian
  geom_rect(aes(xmin = -9, xmax = 0,
                ymin = 0, ymax = 9),
            fill = "#ff7474") +
  geom_rect(aes(xmin = 0, xmax = 9,
                ymin = 0, ymax = 9),
            fill = "#44aafd") +
  geom_rect(aes(xmin = -9, xmax = 0,
                ymin = -9, ymax = 0),
            fill = "#98ec98") +
  geom_rect(aes(xmin = 0, xmax = 9,
                ymin = -9, ymax = 0),
            fill = "#c09aea")



# Crear el gráfico 
v <- va +
  geom_point(aes(colour = voto), size = 3, shape = 19) +  # Asignar color según la variable 'voto'
  geom_text(data = labels, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle))+
  labs(x = "", y = "") +
  scale_color_manual(values = c("Javier Milei (La Libertad Avanza)" = "blue", 
                                "Patricia Bullrich (Juntos por el Cambio)" = "yellow", 
                                "Sergio Massa (Unión por la Patria)" = "red", 
                                "Juan Schiaretti (Hacemos por Nuestro País)" = "green", 
                                "En blanco" = "white"),
                                  guide = "none")  # Ocultar la leyenda de los candidatos

# gráfico
plot(v)





