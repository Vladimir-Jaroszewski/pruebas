library(gridExtra)
library(ggplot2)
library(openxlsx)
library(corrplot)
library(cowplot)

data<-read.xlsx('C:/Users/Admin/Documents/GitHub/pruebas/eco pol/Encuestas Filtradas.xlsx',sheet="ejes")

labels <- data.frame(label = c("Conservador","Liberal","Izquierda","Derecha"),
                     x = c(0, 0, -10, 10),
                     y = c(10, -10, 0, 0)
)
labels$angle <- ifelse(labels$label %in% c("Izquierda", "Derecha"), 90, 0)

# Cuadro Idiologia
I <- ggplot(data, aes(eje.econ1, eje.social1)) +
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
Id <- I +
  geom_point(size = 3, colour = "red") +
  geom_point(size = 3, shape = 21) + 
  geom_text(data = labels, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle)) +
  labs(x = "", y = "")

# Mostrar el gráfico
plot(Id)



# Ideologia + generales

va <- ggplot(data, aes(eje.econ1, eje.social1)) +
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
  geom_point(aes(colour = generales), size = 3, shape = 19) +  # Asignar color según la variable 'voto'
  geom_text(data = labels, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle))+
  labs(x = "", y = "") +
  scale_color_manual(values = c("Javier Milei (La Libertad Avanza)" = "blue", 
                                "Patricia Bullrich (Juntos por el Cambio)" = "yellow", 
                                "Sergio Massa (Unión por la Patria)" = "red", 
                                "Juan Schiaretti (Hacemos por Nuestro País)" = "green", 
                                "En blanco" = "white","mediano"="black"),
                     guide = "none")  # Ocultar la leyenda de los candidatos

# gráfico
plot(v)



# Ideologia + ballotage

# Defino las dos tendencias
tend1 <- geom_smooth(data = data, aes(eje.econ1, eje.social1), 
                     method = "lm", color = "black", se = FALSE)
tend2 <- geom_smooth(data = data, aes(eje.econ2, eje.social2), 
                     method = "lm", color = "yellow", se = FALSE)

# Base del gráfico
ba <- ggplot(data, aes(eje.econ1, eje.social1)) +
  theme_minimal() +
  coord_fixed(ratio = 1, xlim = c(-10, 10), ylim = c(-10, 10)) +
  geom_rect(aes(xmin = -9, xmax = 0, ymin = 0, ymax = 9), fill = "#ff7474") +
  geom_rect(aes(xmin = 0, xmax = 9, ymin = 0, ymax = 9), fill = "#44aafd") +
  geom_rect(aes(xmin = -9, xmax = 0, ymin = -9, ymax = 0), fill = "#98ec98") +
  geom_rect(aes(xmin = 0, xmax = 9, ymin = -9, ymax = 0), fill = "#c09aea") +
  tend1 +
  tend2

# Agregar elementos al gráfico
b <- ba +
  geom_point(aes(colour = ballotage), size = 3, shape = 19) +
  geom_text(data = labels, aes(x = x, y = y, label = label, angle = angle), 
            color = "azure4") +
  labs(x = "", y = "") +
  scale_color_manual(values = c(
    "Javier Milei (La Libertad Avanza)" = "blue",
    "Sergio Massa (Unión por la Patria)" = "red",
    "En blanco" = "white"
  ), guide = "none")  # Ocultar la leyenda de los candidatos

# Mostrar el gráfico
plot(b)




# Cuadro politicas

Po <- ggplot(data, aes(eje.econ2, eje.social2)) +
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
            fill = "#c09aea") +
  tend2




# Crear el gráfico original
Pol <- Po +
  geom_point(size = 3, colour = "red") +
  geom_point(size = 3, shape = 21) + 
  geom_text(data = labels, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle)) +
  labs(x = "", y = "")

# Mostrar el gráfico
plot(Pol)




# Politicas + generales

Pg <- ggplot(data, aes(eje.econ2, eje.social2)) +
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
Polg <- Pg +
  geom_point(aes(colour = generales), size = 3, shape = 19) +  # Asignar color según la variable 'voto'
  geom_text(data = labels, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle))+
  labs(x = "", y = "") +
  scale_color_manual(values = c("Javier Milei (La Libertad Avanza)" = "blue", 
                                "Patricia Bullrich (Juntos por el Cambio)" = "yellow", 
                                "Sergio Massa (Unión por la Patria)" = "red", 
                                "Juan Schiaretti (Hacemos por Nuestro País)" = "green", 
                                "En blanco" = "white","mediano"="black"),
                     guide = "none")  # Ocultar la leyenda de los candidatos

# gráfico
plot(Polg)




# Politicas + ballotage

Pb <- ggplot(data, aes(eje.econ2, eje.social2)) +
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
Polb <- Pb +
  geom_point(aes(colour = ballotage), size = 3, shape = 19) +  # Asignar color según la variable 'voto'
  geom_text(data = labels, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle))+
  labs(x = "", y = "") +
  scale_color_manual(values = c("Javier Milei (La Libertad Avanza)" = "blue",  
                                "Sergio Massa (Unión por la Patria)" = "red",  
                                "En blanco" = "white","mediano"="black"),
                     guide = "none")  # Ocultar la leyenda de los candidatos

# gráfico
plot(Polb)




#####  GRADIENTES desde graf ideologia

# Crear un gráfico con flechas que conectan las posiciones iniciales y finales

# Base del gráfico
ba <- ggplot(data, aes(eje.econ2, eje.social2)) +
  theme_minimal() +
  coord_fixed(ratio = 1, xlim = c(-10, 10), ylim = c(-10, 10)) +
  geom_rect(aes(xmin = -9, xmax = 0, ymin = 0, ymax = 9), fill = "#ff7474") +
  geom_rect(aes(xmin = 0, xmax = 9, ymin = 0, ymax = 9), fill = "#44aafd") +
  geom_rect(aes(xmin = -9, xmax = 0, ymin = -9, ymax = 0), fill = "#98ec98") +
  geom_rect(aes(xmin = 0, xmax = 9, ymin = -9, ymax = 0), fill = "#c09aea")

# Agregar las flechas para mostrar el movimiento
b <- ba +
  geom_segment(aes(
    x = eje.econ2, y = eje.social2,
    xend = eje.econ1, yend = eje.social1,
    color = ballotage
  ), arrow = arrow(length = unit(0.2, "cm")), size = 0.7) +
  geom_point(aes(color = ballotage), size = 3, shape = 19) +
  scale_color_manual(values = c(
    "Javier Milei (La Libertad Avanza)" = "blue",
    "Sergio Massa (Unión por la Patria)" = "red",
    "En blanco" = "white"
  ), guide = "none") +  # Ocultar la leyenda
  geom_text(data = labels, aes(x = x, y = y, label = label, angle = angle), 
            color = "azure4") +
  labs(x = "", y = "")

# Mostrar el gráfico
plot(b)



##### 



## revisar estos
# Pol compas Dolar  NO SIRVE


do <- ggplot(data, aes(eje.econ, eje.social)) +
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
d <- do +
  geom_point(aes(colour = Dolar), size = 3, shape = 19) +  # Asignar color según la variable 'voto'
  geom_text(data = labels, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle))+
  labs(x = "", y = "") +
  scale_color_manual(values = c("De Acuerdo" = "yellow", 
                                "En Desacuerdo" = "blue"),
                     guide = "none")  # Ocultar la leyenda de los candidatos

# gráfico
plot(d)



# Pol compas "Estoy dispuesto a sacrificar derechos políticos por más bienestar económico"


ec <- ggplot(data, aes(eje.econ, eje.social)) +
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
e <- ec +
  geom_point(aes(colour = sa_eco), size = 3, shape = 19) +  # Asignar color según la variable 'voto'
  geom_text(data = labels, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle))+
  labs(x = "", y = "") +
  scale_color_manual(values = c("De Acuerdo" = "yellow", 
                                "En Desacuerdo" = "blue"),
                     guide = "none")  # Ocultar la leyenda de los candidatos

# gráfico
plot(e)




# Polarizando SUMO =1 los muy y totalmente "Ejes2"
data2<-read.xlsx('C:/Users/Admin/Documents/GitHub/pruebas/eco pol/puntajes_voto.xlsx',sheet="ejes2")

labels2 <- data.frame(label = c("Conservador","Liberal","Izquierda","Derecha"),
                      x = c(0, 0, -10, 10),
                      y = c(10, -10, 0, 0)
)
labels$angle2 <- ifelse(labels2$label %in% c("Izquierda", "Derecha"), 90, 0)

# Pol compas
q2 <- ggplot(data2, aes(eje.econ2, eje.social2)) +
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
p2 <- q2 +
  geom_point(size = 3, colour = "red") +
  geom_point(size = 3, shape = 21) + 
  geom_text(data = labels, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle)) +
  labs(x = "", y = "")

# Mostrar el gráfico
plot(p2)

# las desviaciones estandar son mayores cuando polarizo
De_eje.econ2 <- sd(data2$eje.econ2)
De_eje.soc2 <- sd(data2$eje.social2)


# polarizados y votos "Ejes2"

# Definir el dataframe labels2 correctamente
labels2 <- data.frame(label = c("Conservador", "Liberal", "Izquierda", "Derecha"),
                      x = c(0, 0, -10, 10),
                      y = c(10, -10, 0, 0))

# Agregar la columna 'angle' a labels2
labels2$angle <- ifelse(labels2$label %in% c("Izquierda", "Derecha"), 90, 0)

# Crear el gráfico
va2 <- ggplot(data2, aes(eje.econ2, eje.social2)) +
  theme_minimal() +
  coord_fixed(ratio = 1, xlim = c(-10, 10), ylim = c(-10, 10)) +
  geom_rect(aes(xmin = -9, xmax = 0, ymin = 0, ymax = 9), fill = "#ff7474") +
  geom_rect(aes(xmin = 0, xmax = 9, ymin = 0, ymax = 9), fill = "#44aafd") +
  geom_rect(aes(xmin = -9, xmax = 0, ymin = -9, ymax = 0), fill = "#98ec98") +
  geom_rect(aes(xmin = 0, xmax = 9, ymin = -9, ymax = 0), fill = "#c09aea")

# Crear el gráfico final
v2 <- va2 +
  geom_point(aes(colour = voto2), size = 3, shape = 19) +
  geom_text(data = labels2, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle)) +  # Usar labels2 aquí
  labs(x = "", y = "") +
  scale_color_manual(values = c("Javier Milei (La Libertad Avanza)" = "blue", 
                                "Patricia Bullrich (Juntos por el Cambio)" = "yellow", 
                                "Sergio Massa (Unión por la Patria)" = "red", 
                                "Juan Schiaretti (Hacemos por Nuestro País)" = "green", 
                                "En blanco" = "white"),
                     guide = "none")

# Mostrar el gráfico
plot(v2)




# agrandando el centro "Ejes3"


data3<-read.xlsx('C:/Users/Admin/Documents/GitHub/pruebas/eco pol/puntajes_voto.xlsx',sheet="ejes3")

labels3 <- data.frame(label = c("Conservador","Liberal","Izquierda","Derecha"),
                      x = c(0, 0, -10, 10),
                      y = c(10, -10, 0, 0)
)
labels$angle3 <- ifelse(labels3$label %in% c("Izquierda", "Derecha"), 90, 0)

# Pol compas
q3 <- ggplot(data3, aes(eje.econ3, eje.social3)) +
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
p3 <- q3 +
  geom_point(size = 3, colour = "red") +
  geom_point(size = 3, shape = 21) + 
  geom_text(data = labels, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle)) +
  labs(x = "", y = "")

# Mostrar el gráfico
plot(p3)

# las desviaciones estandar son mayores cuando polarizo
De_eje.econ3 <- sd(data3$eje.econ3)
De_eje.soc3 <- sd(data3$eje.social3)




# agrandando el centr y votos "Ejes3"

# Definir el dataframe labels2 correctamente
labels3 <- data.frame(label = c("Conservador", "Liberal", "Izquierda", "Derecha"),
                      x = c(0, 0, -10, 10),
                      y = c(10, -10, 0, 0))

# Agregar la columna 'angle' a labels2
labels3$angle <- ifelse(labels3$label %in% c("Izquierda", "Derecha"), 90, 0)

# Crear el gráfico
va3 <- ggplot(data3, aes(eje.econ3, eje.social3)) +
  theme_minimal() +
  coord_fixed(ratio = 1, xlim = c(-10, 10), ylim = c(-10, 10)) +
  geom_rect(aes(xmin = -9, xmax = 0, ymin = 0, ymax = 9), fill = "#ff7474") +
  geom_rect(aes(xmin = 0, xmax = 9, ymin = 0, ymax = 9), fill = "#44aafd") +
  geom_rect(aes(xmin = -9, xmax = 0, ymin = -9, ymax = 0), fill = "#98ec98") +
  geom_rect(aes(xmin = 0, xmax = 9, ymin = -9, ymax = 0), fill = "#c09aea")

# Crear el gráfico final
v3 <- va3 +
  geom_point(aes(colour = voto3), size = 3, shape = 19) +
  geom_text(data = labels2, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle)) +  # Usar labels2 aquí
  labs(x = "", y = "") +
  scale_color_manual(values = c("Javier Milei (La Libertad Avanza)" = "blue", 
                                "Patricia Bullrich (Juntos por el Cambio)" = "yellow", 
                                "Sergio Massa (Unión por la Patria)" = "red", 
                                "Juan Schiaretti (Hacemos por Nuestro País)" = "green", 
                                "En blanco" = "white"),
                     guide = "none")

# Mostrar el gráfico
plot(v3)


# Polarizado mas centro "Ejes4"



data4<-read.xlsx('C:/Users/Admin/Documents/GitHub/pruebas/eco pol/puntajes_voto.xlsx',sheet="ejes4")

labels4 <- data.frame(label = c("Conservador","Liberal","Izquierda","Derecha"),
                      x = c(0, 0, -10, 10),
                      y = c(10, -10, 0, 0)
)
labels$angle4 <- ifelse(labels4$label %in% c("Izquierda", "Derecha"), 90, 0)

# Pol compas
q4 <- ggplot(data4, aes(eje.econ4, eje.social4)) +
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
p4 <- q4 +
  geom_point(size = 3, colour = "red") +
  geom_point(size = 3, shape = 21) + 
  geom_text(data = labels, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle)) +
  labs(x = "", y = "")

# Mostrar el gráfico
plot(p4)

# las desviaciones estandar son mayores cuando polarizo
De_eje.econ4 <- sd(data4$eje.econ4)
De_eje.soc4 <- sd(data4$eje.social4)




# agrandando el centr y votos "Ejes4"

# Definir el dataframe labels2 correctamente
labels4 <- data.frame(label = c("Conservador", "Liberal", "Izquierda", "Derecha"),
                      x = c(0, 0, -10, 10),
                      y = c(10, -10, 0, 0))

# Agregar la columna 'angle' a labels2
labels4$angle <- ifelse(labels4$label %in% c("Izquierda", "Derecha"), 90, 0)

# Crear el gráfico
va4 <- ggplot(data4, aes(eje.econ4, eje.social4)) +
  theme_minimal() +
  coord_fixed(ratio = 1, xlim = c(-10, 10), ylim = c(-10, 10)) +
  geom_rect(aes(xmin = -9, xmax = 0, ymin = 0, ymax = 9), fill = "#ff7474") +
  geom_rect(aes(xmin = 0, xmax = 9, ymin = 0, ymax = 9), fill = "#44aafd") +
  geom_rect(aes(xmin = -9, xmax = 0, ymin = -9, ymax = 0), fill = "#98ec98") +
  geom_rect(aes(xmin = 0, xmax = 9, ymin = -9, ymax = 0), fill = "#c09aea")

# Crear el gráfico final
v4 <- va4 +
  geom_point(aes(colour = voto4), size = 3, shape = 19) +
  geom_text(data = labels2, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle)) +  # Usar labels2 aquí
  labs(x = "", y = "") +
  scale_color_manual(values = c("Javier Milei (La Libertad Avanza)" = "blue", 
                                "Patricia Bullrich (Juntos por el Cambio)" = "yellow", 
                                "Sergio Massa (Unión por la Patria)" = "red", 
                                "Juan Schiaretti (Hacemos por Nuestro País)" = "green", 
                                "En blanco" = "white"),
                     guide = "none")

# Mostrar el gráfico
plot(v4)


# ballotage
data<-read.xlsx('C:/Users/Admin/Documents/GitHub/pruebas/eco pol/puntajes_voto.xlsx',sheet="ejes")

labels <- data.frame(label = c("Conservador","Liberal","Izquierda","Derecha"),
                     x = c(0, 0, -10, 10),
                     y = c(10, -10, 0, 0)
)
labels$angle <- ifelse(labels$label %in% c("Izquierda", "Derecha"), 90, 0)

Ba <- ggplot(data, aes(eje.econ, eje.social)) +
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
B <- Ba +
  geom_point(aes(colour = votoB), size = 3, shape = 19) +  # Asignar color según la variable 'voto'
  geom_text(data = labels, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle))+
  labs(x = "", y = "") +
  scale_color_manual(values = c("Javier Milei (La Libertad Avanza)" = "blue", 
                                "Sergio Massa (Unión por la Patria)" = "red", 
                                "En blanco" = "white","mediano"="black"),
                     guide = "none")  # Ocultar la leyenda de los candidatos

# gráfico
plot(B)


grid.arrange(v,v3,v4,v5, ncol = 2, nrow = 2)

plot(v)
plot(v2)
plot(v3)
plot(v4)



# distribuciones de los ejes

plot(density(data$eje.econ) , main= "economico")
abline(v = mean(data$eje.econ), col = "red", lwd = 2, lty = 2)
abline(v = median(data$eje.econ), col = "blue", lwd = 2, lty = 2)
median(data$eje.econ)

plot(density(data$eje.social))
abline(v = mean(data$eje.social), col = "red", lwd = 2, lty = 2)
abline(v = median(data$eje.social), col = "blue", lwd = 2, lty = 2)
median(data$eje.social)
median(data$eje.econ)

plot(density(data2$eje.econ2))
abline(v = mean(data$eje.econ2), col = "red", lwd = 2, lty = 2)
abline(v = median(data$eje.econ2), col = "blue", lwd = 2, lty = 2)

plot(density(data2$eje.social2))
abline(v = mean(data2$eje.social2), col = "red", lwd = 2, lty = 2)
abline(v = median(data2$eje.social2), col = "blue", lwd = 2, lty = 2)

plot(density(data3$eje.econ3))
abline(v = mean(data.$eje.econ3), col = "red", lwd = 2, lty = 2)
abline(v = median(data3$eje.econ3), col = "blue", lwd = 2, lty = 2)

plot(density(data3$eje.social3))
abline(v = mean(data3$eje.social3), col = "red", lwd = 2, lty = 2)
abline(v = median(data3$eje.social3), col = "blue", lwd = 2, lty = 2)

plot(density(data4$eje.econ4))
abline(v = mean(data4$eje.econ4), col = "red", lwd = 2, lty = 2)
abline(v = median(data4$eje.econ4), col = "blue", lwd = 2, lty = 2)

plot(density(data4$eje.social4))
abline(v = mean(data4$eje.social4), col = "red", lwd = 2, lty = 2)
abline(v = median(data4$eje.social4), col = "blue", lwd = 2, lty = 2)
median(data4$eje.social4)
median(data4$eje.econ4)


#con tendencia.


labels <- data.frame(label = c("Conservador","Liberal","Izquierda","Derecha"),
                     x = c(0, 0, -10, 10),
                     y = c(10, -10, 0, 0)
)
labels$angle <- ifelse(labels$label %in% c("Izquierda", "Derecha"), 90, 0)

# Pol compas + tendencia
qt <- ggplot(data, aes(eje.econ, eje.social)) +
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
            fill = "#c09aea")+
  geom_smooth(method = "lm", color = "black", se = FALSE)  # Línea de tendencia


# Crear el gráfico original
pt <- qt +
  geom_point(size = 3, colour = "red") +
  geom_point(size = 3, shape = 21) + 
  geom_text(data = labels, color = "azure4", 
            aes(x = x, y = y, label = label, angle = angle)) +
  labs(x = "", y = "")

# Mostrar el gráfico
plot(pt)
