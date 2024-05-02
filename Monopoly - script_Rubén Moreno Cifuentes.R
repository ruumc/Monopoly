# Rubén Moreno Cifuentes

## Monopoly Simulación R Avanzado:
rm(list=ls())

monopoly <- function(turnos = 100, time = 4) {
     par(mfcol = c(1, 2))
     
     # PRIMERO ASIGNAMOS VALORES DE PARTIDA A LOS JUGADORES ----
     jugador <- c("Jugador_1", "Jugador_2", "Jugador_3", "Jugador_4")
     
     
     # Creamos la variable estrategia, para asignarle una diferente a cada jugador:
     estrategia <- sample(c("base", "agresivo", "ahorrador", "estratega"), size = length(jugador))
     
     
     jugadores <- data.frame(n = 1:length(jugador), jugador, estrategia, 
                             posicion = rep(1, times = length(jugador)), # Colocamos a todos los jugadores en la casilla de salida
                             dinero = rep(15000, times = length(jugador)), # Les damos a todos los jugadores 1500€ para empezar
                             carcel = rep(0, times = length(jugador))) # Creamos la variable carcel
     
     
     # Creamos vectores para ir guardando las propiedades de los jugadores mientras juegan
     calles_jugador_1 <- c()
     calles_jugador_2 <- c()
     calles_jugador_3 <- c()
     calles_jugador_4 <- c()
     
    
     
     # SEGUNDO CREAMOS EL TABLERO ----
     
     nombre_casilla <- c("Salida", "Gdynia", "Caja de Comunidad", "Taipéi", "Impuesto sobre el Capital", "Ferrocarril", "Tokio", "Suerte", "Barcelona",  "Atenas", "Cárcel, solo visitas", "Estambul", "Energía Solar", "Kiev", "Toronto", "Aerolínea", "Roma", "Caja de Comunidad", "Shanghái", "Vancouver", "Parking Gratuito", "Sídney", "Suerte", "Nueva York", "Londres", "Crucero", "Pekín", "Hong Kong", "Energía Eólica", "Jerusalén", "Ve a la Cárcel", "París", "Belgrado", "Caja de Comunidad", "Ciudad del Cabo", "Astronave", "Suerte", "Riga", "Impuesto de Lujo", "Montreal") 
     
     
     
     barrio <- c("", "brown", "", "brown", "", "Estación", "cyan", "", "cyan", "cyan", "", "magenta", "Compañía", "magenta", "magenta", "Estación", "orange", "", "orange", "orange", "", "red", "", "red", "red", "Estación", "yellow", "yellow", "Compañía", "yellow", "", "green", "green", "", "green", "Estación", "", "blue", "", "blue")
     
     
     
     tipo_de_casilla <- c("Salida", "Calle", "Caja", "Calle", "Impuesto", "Estación", "Calle", "Suerte", "Calle", "Calle", "Visita", "Calle", "Compañía", "Calle", "Calle", "Estación", "Calle", "Caja", "Calle", "Calle", "Parking", "Calle", "Suerte", "Calle", "Calle", "Estación", "Calle", "Calle", "Compañía", "Calle", "Carcel", "Calle", "Calle", "Caja", "Calle", "Estación", "Suerte", "Calle", "Impuesto", "Calle")
     
     
     
     precio <- c(2000, 600, NA, 600, 2000, 2000, 1000, NA, 1000, 1200, NA, 1400, 1500, 1400, 1600, 2000, 1800, NA, 1800, 2000, NA, 2200, NA, 2200, 2400, 2000, 2600, 2600, 1500, 2800, NA, 3000, 3000, NA, 3200, 2000, NA, 3500, 1000, 4000)
     
     
     
     vendida <- rep("No", times = 40)
     vendida <- ifelse(tipo_de_casilla %in% c("Salida", "Caja", "Suerte", "Impuesto", "Parking", "Carcel", "Visita"), "", vendida)
     
     
     
     dueño <- rep("", times = 40)
     
     
     
     alquiler <- matrix(c(NA, NA, NA, NA, NA, NA, NA, NA,
                   20, 100, 300, 900, 1600, 2500, 300, 500,
                   NA, NA, NA, NA, NA, NA, NA, NA,
                   40, 200, 600, 1800, 3200, 4500, 300, 500,
                   NA, NA, NA, NA, NA, NA, NA, NA,
                   250, 500, 1000, 2000, NA, NA, NA, NA,
                   60, 300, 900, 2700, 4000, 5500, 500, 500,
                   NA, NA, NA, NA, NA, NA, NA, NA,
                   60, 300, 900, 2700, 4000, 5500, 500, 500,
                   80, 400, 1000, 3000, 4500, 6000, 600, 500,
                   NA, NA, NA, NA, NA, NA, NA, NA,
                   100, 500, 1500, 4500, 6250, 7500, 700, 1000,
                   NA, NA, NA, NA, NA, NA, NA, NA,
                   100, 500, 1500, 4500, 6250, 7500, 700, 1000,
                   120, 600, 1800, 5000, 7000, 9000, 800, 1000,
                   250, 500, 1000, 2000, NA, NA, NA, NA,
                   140, 700, 2000, 5500, 7500, 9500, 900, 1000,
                   NA, NA, NA, NA, NA, NA, NA, NA,
                   140, 700, 2000, 5500, 7500, 9500, 900, 1000,
                   160, 800, 2200, 6000, 8000, 10000, 1000, 1000,
                   NA, NA, NA, NA, NA, NA, NA, NA,
                   180, 900, 2500, 7000, 8750, 10500, 1100, 1500,
                   NA, NA, NA, NA, NA, NA, NA, NA,
                   180, 900, 2500, 7000, 8750, 10500, 1100, 1500,
                   200, 1000, 3000, 7500, 9250, 11000, 1200, 1500,
                   250, 500, 1000, 2000, NA, NA, NA, NA,
                   220, 1100, 3300, 8000, 9750, 11500, 1300, 1500,
                   220, 1100, 3300, 8000, 9750, 11500, 1300, 1500,
                   NA, NA, NA, NA, NA, NA, NA, NA,
                   240, 1200, 3600, 8500, 10250, 12000, 1400, 1500,
                   NA, NA, NA, NA, NA, NA, NA, NA,
                   260, 1300, 3900, 9000, 11000, 12750, 1500, 2000,
                   260, 1300, 3900, 9000, 11000, 12750, 1500, 2000,
                   NA, NA, NA, NA, NA, NA, NA, NA,
                   280, 1500, 4500, 10000, 12000, 14000, 1600, 2000,
                   250, 500, 1000, 2000, NA, NA, NA, NA,
                   NA, NA, NA, NA, NA, NA, NA, NA,
                   350, 1750, 5000, 11000, 13000, 15000, 17500, 2000,
                   NA, NA, NA, NA, NA, NA, NA, NA,
                   500, 2000, 6000, 14000, 17000, 20000, 2000, 2000), ncol = 8, byrow = TRUE, dimnames = list(c(1:40),
                              c("alquiler","casa1", "casa2", "casa3", "casa4", "hotel", "hipoteca", "money")))
    
     # Para que las fichas se puedan mover por el tablero les asignamos las coordenadas de la posicion del jugador uno a cada casilla:
     ancho <- (130-20)/9 # ancho de las casillas pequeñas
     pos_xy <- matrix(c(136, 14,
                          23 + 8*ancho, 11,
                          23 + 7*ancho, 11,
                          23 + 6*ancho, 11,
                          23 + 5*ancho, 11,
                          23 + 4*ancho, 11,
                          23 + 3*ancho, 11,
                          23 + 2*ancho, 11,
                          23 + ancho, 11,
                          23, 11,
                          6, 14,
                          4, 23 + ancho-6,
                          4, 23 + ancho-6 + ancho,
                          4, 23 + ancho-6 + 2*ancho,
                          4, 23 + ancho-6 + 3*ancho,
                          4, 23 + ancho-6 + 4*ancho,
                          4, 23 + ancho-6 + 5*ancho,
                          4, 23 + ancho-6 + 6*ancho,
                          4, 23 + ancho-6 + 7*ancho,
                          4, 23 + ancho-6 + 8*ancho,
                          6, 144,
                          23, 146,
                          23 + ancho, 146,
                          23 + 2*ancho, 146,
                          23 + 3*ancho, 146,
                          23 + 4*ancho, 146,
                          23 + 5*ancho, 146,
                          23 + 6*ancho, 146,
                          23 + 7*ancho, 146,
                          23 + 8*ancho, 146,
                          136, 144,
                          139, 23 + ancho-6 + 8*ancho,
                          139, 23 + ancho-6 + 7*ancho,
                          139, 23 + ancho-6 + 6*ancho,
                          139, 23 + ancho-6 + 5*ancho,
                          139, 23 + ancho-6 + 4*ancho,
                          139, 23 + ancho-6 + 3*ancho,
                          139, 23 + ancho-6 + 2*ancho,
                          139, 23 + ancho-6 + ancho,
                          139, 23 + ancho-6), ncol = 2, byrow = TRUE, dimnames = list(c(1:40), c("pos_x", "pos_y")))
     
     
     
     
     
     tablero <- data.frame(nombre_casilla, barrio, tipo_de_casilla, precio, vendida, dueño, alquiler, pos_xy, casas = rep(0, times = 40))
     
     
     
     # Creamos la funcion del grafico del tablero: ----
     board <- function(pos_x1, pos_y1, pos_x2, pos_y2, pos_x3, pos_y3, pos_x4, pos_y4) {
          par(mar = c(1, 0, 1, 0.2))
          plot(NA,
               yaxt = "n",
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = c(0, 150),
               ylim = c(0, 150))
          
          polygon(x = c(0, 0, 150, 150), 
                  y = c(0, 150, 150, 0),  col = "tan")
          
          
          abline(h = seq(20, 130, length = 10),
                 v = seq(20, 130, length = 10))
          polygon(x = c(20, 20, 130, 130), 
                  y = c(20, 130, 130, 20),  col = "tan3")
          
          text(x = 75, y = 75, "MONOPOLY", srt = 45, cex = 2.5, family = "mono", font = 2)
          text(x = 83, y = 67, "Edición Mundial", srt = 45, family = "mono")
          
          # Ancho de las casillas pequeñas:
          ancho <- (130-20)/9
          130-ancho
          
          # Marrones:
          polygon(x = c(130-ancho, 130-ancho, 130, 130),
                  y = c(15, 20, 20, 15), col = "brown")
          
          polygon(x = c(130-3*ancho, 130-3*ancho, 130-2*ancho, 130-2*ancho),
                  y = c(15, 20, 20, 15), col = "brown")
          
          
          # Cyan:
          polygon(x = c(20, 20, 20 + ancho, 20 + ancho),
                  y = c(15, 20, 20, 15), col = "cyan")
          
          polygon(x = c(20 + ancho, 20 + ancho, 20+2*ancho, 20+2*ancho),
                  y = c(15, 20, 20, 15), col = "cyan")
          
          polygon(x = c(20 + 3*ancho, 20 + 3*ancho, 20+4*ancho, 20+4*ancho),
                  y = c(15, 20, 20, 15), col = "cyan")
         
           
         # Magenta:
          polygon(x = c(15, 15, 20, 20),
                  y = c(20, 20 + ancho, 20 + ancho, 20), col = "magenta")
          
          polygon(x = c(15, 15, 20, 20),
                  y = c(20 + 2*ancho, 20 + 3*ancho, 20 + 3*ancho, 20 + 2*ancho), col = "magenta")
          
          polygon(x = c(15, 15, 20, 20),
                  y = c(20 + 3*ancho, 20 + 4*ancho, 20 + 4*ancho, 20 + 3*ancho), col = "magenta")
          
          
          # Naranja:
          polygon(x = c(15, 15, 20, 20),
                  y = c(20 + 5*ancho, 20 + 6*ancho, 20 + 6*ancho, 20 + 5*ancho), col = "orange")
          
          polygon(x = c(15, 15, 20, 20),
                  y = c(20 + 7*ancho, 20 + 8*ancho, 20 + 8*ancho, 20 + 7*ancho), col = "orange")
          
          polygon(x = c(15, 15, 20, 20),
                  y = c(20 + 8*ancho, 20 + 9*ancho, 20 + 9*ancho, 20 + 8*ancho), col = "orange")
          
          
          # Rojo:
          polygon(x = c(20, 20, 20 + ancho, 20 + ancho),
                  y = c(130, 135, 135, 130), col = "red")
          
          polygon(x = c(20 + 2*ancho, 20 + 2*ancho, 20 + 3*ancho, 20 + 3*ancho),
                  y = c(130, 135, 135, 130), col = "red")
          
          polygon(x = c(20 + 3*ancho, 20 + 3*ancho, 20 + 4*ancho, 20 + 4*ancho),
                  y = c(130, 135, 135, 130), col = "red")
          
          
          # Amarillo:
          polygon(x = c(130 - ancho, 130 - ancho, 130, 130),
                  y = c(130, 135, 135, 130), col = "yellow")
          
          polygon(x = c(130-3*ancho, 130-3*ancho, 130-2*ancho, 130-2*ancho),
                  y = c(130, 135, 135, 130), col = "yellow")
          
          polygon(x = c(130 - 4*ancho, 130 - 4*ancho, 130 - 3*ancho, 130 - 3*ancho),
                  y = c(130, 135, 135, 130), col = "yellow")
          
          
          # Verde:
          polygon(x = c(130, 130, 135, 135),
                  y = c(20 + 5*ancho, 20 + 6*ancho, 20 + 6*ancho, 20 + 5*ancho), col = "green3")
          
          polygon(x = c(130, 130, 135, 135),
                  y = c(20 + 7*ancho, 20 + 8*ancho, 20 + 8*ancho, 20 + 7*ancho), col = "green3")
          
          polygon(x = c(130, 130, 135, 135),
                  y = c(20 + 8*ancho, 20 + 9*ancho, 20 + 9*ancho, 20 + 8*ancho), col = "green3")
          
          
          # Azul:
          polygon(x = c(130, 130, 135, 135),
                  y = c(20, 20 + ancho, 20 + ancho, 20), col = "blue")
          
          polygon(x = c(130, 130, 135, 135),
                  y = c(20 + 2*ancho, 20 + 3*ancho, 20 + 3*ancho, 20 + 2*ancho), col = "blue")
          
          # Casillas suerte:
          text(x = 20 + 2.5*ancho, y = 10, "?", cex = 2, family = "sans", col = "magenta", font = 2)
          
          text(x = 20 + 1.5*ancho, y = 140, "?", cex = 2, family = "sans", col = "blue", font = 2, srt = 180)
          
          text(x = 140, y = 20 + 3.5*ancho, "?", cex = 2, family = "sans", col = "orange", font = 2, srt = 90)
          
          
          # Casillas de caja
          polygon(x = c(22 + 7*ancho, 22 + 7*ancho, 28 + 7*ancho, 28 + 7*ancho), y = c(5.89, 11.89, 11.89, 5.89), col = "ivory4"); polygon(x = c(28 + 7*ancho, 28 + 7*ancho, 30.222 + 7*ancho, 30.222 + 7*ancho), y = c(5.89, 11.89, 14.109, 8.112), col = "ivory4"); polygon(x = c(22 + 7*ancho, 24.222 + 7*ancho, 30.222 + 7*ancho, 28 + 7*ancho), y = c(11.89, 14.109, 14.109, 11.89), col = "red"); polygon(x = c(23 + 7*ancho, 24.2 + 7*ancho, 29.2 + 7*ancho, 28 + 7*ancho), y = c(11.89, 13.109, 13.109, 11.89), col = "gold"); text(x = 25 + 7*ancho, y = 5.89 + 3.2, "$", col = "gold")
          
          
          polygon(x = c(150 - 5.89, 150 - 11.89, 150 - 11.89, 150 - 5.89), y = c(22 + 6*ancho, 22 + 6*ancho, 28 + 6*ancho, 28 + 6*ancho), col = "ivory4"); polygon(x = c(150 - 5.89, 150 - 11.89, 150 - 14.109, 150 - 8.112), y = c(28 + 6*ancho, 28 + 6*ancho, 30.222 + 6*ancho, 30.222 + 6*ancho), col = "ivory4"); polygon(x = c(150 - 11.89, 150 - 14.109, 150 - 14.109, 150 - 11.89), y = c(22 + 6*ancho, 24.222 + 6*ancho, 30.222 + 6*ancho, 28 + 6*ancho), col = "red"); polygon(x = c(150 - 11.89, 150 - 13.109, 150 - 13.109, 150 - 11.89), y = c(23 + 6*ancho, 24.2 + 6*ancho, 29.2 + 6*ancho, 28 + 6*ancho), col = "gold"); text(x = 150 - 5.89 - 3.2, y = 25 + 6*ancho, "$", col = "gold", srt = 90)
          
          
          polygon(x = c(5.89, 11.89, 11.89, 5.89), y = c(130 - 2 - 2*ancho, 130 - 2 - 2*ancho, 130 - 8 - 2*ancho, 130 - 8 - 2*ancho), col = "ivory4");               polygon(x = c(5.89, 11.89, 14.109, 8.112), y = c(130 - 8 - 2*ancho, 130 - 8 - 2*ancho, 130 - 10.222 - 2*ancho, 130 - 10.222 - 2*ancho), col = "ivory4");               polygon(x = c(11.89, 14.109, 14.109, 11.89), y = c(130 - 2 - 2*ancho, 130 - 4.222 - 2*ancho, 130 - 10.222 - 2*ancho, 130 - 8 - 2*ancho), col = "red");               polygon(x = c(11.89, 13.109, 13.109, 11.89), y = c(130 - 3 - 2*ancho, 130 - 4.2 - 2*ancho, 130 - 9.2 - 2*ancho, 130 - 8 - 2*ancho), col = "gold");               text(x = 5.89 + 3.2, y = 130 - 5 - 2*ancho, "$", col = "gold", srt = -90)
          
          
          
          # Barco:
          polygon(x = c(22 + 4*ancho, 23 + 4*ancho, 17 + 5*ancho, 18 + 5*ancho), y = c(150 - 8, 150 - 5, 150 - 5, 150 - 8), col = "lightsalmon");               polygon(x = c(20 + 4.5*ancho, 17 + 5*ancho, 20 + 4.5*ancho),  y = c(150 - 8, 150 - 8, 150 - 15), col = "white");               polygon(x = c(23 + 4*ancho, 20 + 4.5*ancho, 20 + 4.5*ancho), y = c(150 - 8, 150 - 8, 150 - 15), col = "white")
          
          # Avion:
          polygon(x = c(7, 8.5, 14), y = c(18 + 5*ancho, 17 + 5*ancho, 22 + 4*ancho), col = "white");              polygon(x = c(7, 8.5, 14), y = c(14 + 5*ancho, 15 + 5*ancho, 22 + 4*ancho), col = "white");               polygon(x = c(7, 8.5, 14, 8.5), y = c(16 + 5*ancho, 17 + 5*ancho, 22 + 4*ancho, 15 + 5*ancho), col = "white")
          
          
          # Tren:
          points(x = 20 + 4.5*ancho, y = 10, pch = 16, cex = 3, col = "red"); points(x = 20 + 4.5*ancho, y = 10, pch = 16, cex = 2, col = "white"); points(x = 20 + 4.5*ancho, y = 10, pch = 16, cex = 1, col = "red"); polygon(x = c(16 + 4.5*ancho, 17 + 4.5*ancho, 20 + 4.5*ancho, 20 + 4.5*ancho), y = c(10, 13, 13, 10), col = "red", border = NA)
          
          # Nave:
          points(x = 150 - 10, y = 20 + 4.5*ancho, cex = 3.5, pch = 16, col = "blue"); text(x = 150 - 10, y = 20 + 4.5*ancho, "NASA", srt = 90, cex = .5, col = "white", family = "serif", font = 2)
          
          
          
          # impuesto:
          polygon(x = c(20 + 5.5*ancho, 23 + 5*ancho, 20 + 5.5*ancho, 17 + 6*ancho), y = c(6.889, 10, 20 - 6.889, 10)); polygon(x = c(20 + 5.5*ancho, 23 + 1.5 + 5*ancho, 20 + 5.5*ancho, 17 - 1.5 + 6*ancho), y = c(6.889 + 1.5, 10, 20 - 1.5 - 6.889, 10), col = "black")
          
          # Impuesto de lujo:
          polygon(x = c(150 - 15, 150 - 15, 150 - 12.5, 150 - 10, 150 - 10, 150 - 12.5), y = c(18 + 1.5*ancho, 22 + 1.5*ancho, 24 + 1.5*ancho, 22 + 1.5*ancho, 18 + 1.5*ancho, 16 + 1.5*ancho), col = "cyan", border = "blue"); points(x = 150 - 7, y = 20 + 1.5*ancho, cex = 2.7, lwd = 4, col = "gold")
          
          
          # Energía solar:
          points(x = 10, y = 20 + 1.5*ancho, cex = 2, col = "yellow", pch = 16); points(x = 10, y = 20 + 1.5*ancho, cex = 2.5, col = "yellow", pch = 8, lwd = 2.5)
          
          # Energía Eolica:
          polygon(x = c(20 + 7.5*ancho, 20 + 7.5*ancho), y = c(150 - 10, 150 - 2)); polygon(x = c(20 + 7.5*ancho, 20 + 7.5*ancho, 22 + 7.5*ancho), y = c(150 - 10, 150 - 14, 150 - 10), col = "red"); polygon(x = c(20 + 7.5*ancho, 24 + 7.5*ancho, 20 + 7.5*ancho), y = c(150 - 10, 150 - 10, 150 - 8), col = "blue"); polygon(x = c(20 + 7.5*ancho, 20 + 7.5*ancho, 18 + 7.5*ancho), y = c(150 - 10, 150 - 6, 150 - 10), col = "green"); polygon(x = c(20 + 7.5*ancho, 16 + 7.5*ancho, 20 + 7.5*ancho), y = c(150 - 10, 150 - 10, 150 - 12), col = "yellow")
          
          
          
          
          
          
          # Parking:
          text(x = 10, y = 140, "PARKING", srt = -135, cex = .8, family = "serif")
          
          # Ve a la carcel:
          text(x = 140, y = 140, "CÁRCEL", srt = 135, cex = .8, family = "serif")
          
          # Salida:
          text(x = 140, y = 10, "SALIDA", srt = 45, cex = .8, family = "serif")
          
          # Solo visitas:
          text(x = 10, y = 10, "VISITAS", srt = -45, cex = .8, family = "serif")
          
          # Ficha de los jugadores:
          points(x = pos_x1, y = pos_y1, cex = 1.5, pch = 16, col = "dodgerblue3"); text(x = pos_x1, y = pos_y1, "J1", cex = .5, family = "mono", font = 2)
          
          points(x = pos_x2 + 6.222, y = pos_y2, cex = 1.5, pch = 16, col = "red"); text(x = pos_x2 + 6.222, y = pos_y2, "J2", cex = .5, family = "mono", font = 2)
          
          points(x = pos_x3, y = pos_y3 - 7, cex = 1.5, pch = 16, col = "yellow"); text(x = pos_x3, y = pos_y3 - 7, "J3", cex = .5, family = "mono", font = 2)
          
          points(x = pos_x4 + 6.222, y = pos_y4 - 7, cex = 1.5, pch = 16, col = "green"); text(x = pos_x4 + 6.222, y = pos_y4 - 7, "J4", cex = .5, family = "mono", font = 2)
          
     }
     
     
     
     
     
     
     # CREAMOS FUNCIONES PARA IMPRIMIR LAS <- DE LOS DIFERENTES PROPIEDADES Y CASILLAS: ----
     
     ## Tarjetas de propiedad: ----
     tarjetas <- function(nombre, barrio, alquiler, casa1, casa2, casa3, casa4, hotel, hipoteca, money) {
          par(mar = c(1, 4, 1, 4))
          plot(NA,
               yaxt = "n",
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = c(0, 10),
               ylim = c(0, 30))
          polygon(x = c(0, 0, 10, 10), y = c(22, 30, 30, 22),  col = barrio)
          text(5, 28.5, labels = "TITULO DE PROPIEDAD", cex = 0.5, font = 2)
          text(5, 25.5, nombre, cex = 1.4, font = 2, family = "mono")
          text(5, 20.5, paste0("ALQUILER ", alquiler, "€"), cex = .7, font = 2)
          text(5, 18.5, paste0("Con 1 Casa     ", casa1, "€"), cex = .6)
          text(5, 17,paste0("Con 2 Casa     ", casa2, "€"), cex = .6)
          text(5, 15.5,paste0("Con 3 Casa     ", casa3, "€"), cex = .6)
          text(5, 14,paste0("Con 4 Casa     ", casa4, "€"), cex = .6)
          text(5, 12.5,paste0("Con HOTEL     ", hotel, "€"), cex = .6)
          text(5, 10, paste0("Valor de la Hipoteca ", hipoteca, "€"), cex = .7, font = 2)
          text(5, 8, paste0("Cada Casa cuesta ", money, "€"), cex = .6)
          text(5, 6.5, paste0("Cada HOTEL cuesta ", money, "€"), cex = .6)
          text(5, 5, "más 4 Casas", cex = .6)
          text(5, 2, "Si un jugador posee todos los Solares de\nun grupo de color, el precio del alquiler se duplica\nen los Solares sin edificar de ese grupo", cex = .4)
     }
     
     ## Tarjetas de Compañias: ----
     ### Energía solar:
     tarjeta_solar <- function() {
          par(mar = c(1, 4, 1, 4))
          plot(NA,
               yaxt = "n",
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = c(0, 10),
               ylim = c(0, 30))
          
          points(x = 5, y = 25, pch = 16, cex = 7, col = "gold", font = 2)
          points(x = 5, y = 25, cex = 7, pch = 8, lwd = 10, col = "gold", font = 2)
          
          text(x = 5, y = 19, "ENERGÍA SOLAR", cex = 1, font = 2)
          text(x = 5, y = 15, "Si se posee un\n'Servicio Público',\nel alquiler es 4 veces la\ncantidad mostrada en los dados.*", cex = .7)
          
          text(x = 5, y = 9, "Si se poseen los dos\n'Servicio Público',\nel alquiler es 10 veces la\ncantidad mostrada en los dados.*", cex = .7)
          
          text(x = 5, y = 5, paste0("VALOR DE LA HIPOTECA - 750€"), cex = .7)
          text(x = 5, y = 3, "*Multiplicado por 10", cex = .5)
          
     }
     
     ### Energía eolica:
     tarjeta_eolica <- function() {
          par(mar = c(1, 4, 1, 4))
          plot(NA,
               yaxt = "n",
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = c(0, 10),
               ylim = c(0, 30))
          
          polygon(x = c(20 + 7.5*ancho, 20 + 7.5*ancho),
                  y = c(150 - 10, 150 - 2))
          polygon(x = c(5, 7, 5),
                  y = c(25, 25, 26.5), col = "red")
          polygon(x = c(5, 5, 4),
                  y = c(25, 28, 25), col = "blue")
          polygon(x = c(5, 3, 5),
                  y = c(25, 25, 23.5), col = "green")
          polygon(x = c(5, 5, 6),
                  y = c(25, 22, 25), col = "yellow")
          
          text(x = 5, y = 19, "ENERGÍA EÓLICA", cex = 1, font = 2)
          text(x = 5, y = 15, "Si se posee un\n'Servicio Público',\nel alquiler es 4 veces la\ncantidad mostrada en los dados.*", cex = .7)
          
          text(x = 5, y = 9, "Si se poseen los dos\n'Servicio Público',\nel alquiler es 10 veces la\ncantidad mostrada en los dados.*", cex = .7)
          
          text(x = 5, y = 5, paste0("VALOR DE LA HIPOTECA - 750€"), cex = .7)
          text(x = 5, y = 3, "*Multiplicado por 10", cex = .5)
     }
     
     
     
     ## Tarjetas de trasnportes: ----
     ### Ferrocarril:
     tarjeta_ferrocarril <- function() {
          par(mar = c(1, 4, 1, 4))
          plot(NA,
               yaxt = "n",
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = c(0, 10),
               ylim = c(0, 30))
          
          
          points(x = 5, y = 25, pch = 16, cex = 12, col = "red")
          points(x = 5, y = 25, cex = 9, pch = 16, col = "white")
          points(x = 5, y = 25, pch = 16, cex = 5, col = "red")
          polygon(x = c(5, 3.5, 4, 5),
                  y = c(25, 25, 27.4, 28), col = "red", border = NA)
          
          
          text(x = 5, y = 18.5, "FERROCARRIL\nMONOPOLY", cex = 1, font = 2)
          text(x = 5, y = 15, "ALQUILER      250€", cex = 1)
          
          text(x = 2, y = 12, "Si se poseen\n2 trasportes", cex = .8, adj = 0)
          text(x = 2, y = 8.5, "Si se poseen\n3 trasportes", cex = .8, adj = 0)
          text(x = 2, y = 5, "Si se poseen\n4 trasportes", cex = .8, adj = 0)
          
          text(x = 8, y = 12, "500€", cex = .8, adj = 1)
          text(x = 8, y = 8.5, "1000€", cex = .8, adj = 1)
          text(x = 8, y = 5, "2000€", cex = .8, adj = 1)
          
          text(x = 5, y = 2, "VALOR DE LA HIPOTECA - 1000€", cex = .7)
          
     }
     
     ### Astronave:
     tarjeta_astronave <- function() {
          par(mar = c(1, 4, 1, 4))
          plot(NA,
               yaxt = "n",
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = c(0, 10),
               ylim = c(0, 30))
          
          
          points(x = 5, y = 25, pch = 16, cex = 12, col = "blue")
          text(x = 5, y = 25, "NASA", family = "serif", col = "white", cex = 1.5, font = 2)
          text(x = 4.5, y = 25, ".     .   . .  .   .\n    .  .   .   ..  .    .   \n  . .   .     . .   .  . .\n.     .   . .  .   .\n    .  .   .   ..  .    .   \n  . .   .     . .   .  . .\n.     .   . .  .   .\n    .  .   .   ..  .    .   \n  . .   .     . .   .  . .\n", family = "serif", col = "white", cex = .6, font = 2)
          text(x = 5.5, y = 24.3, ".     .   . .  .   .\n    .  .   .   ..  .    .   \n  . .   .     . .   .  . .\n.     .   . .  .   .\n    .  .   .   ..  .    .   \n  . .   .     . .   .  . .\n.     .   . .  .   .\n    .  .   .   ..  .    .   \n  . .   .     . .   .  . .\n", family = "serif", col = "white", cex = .6, font = 2)
          
          
          
          text(x = 5, y = 18.5, "ASTRONAVE\nMONOPOLY", cex = 1, font = 2)
          text(x = 5, y = 15, "ALQUILER      250€", cex = 1)
          
          text(x = 2, y = 12, "Si se poseen\n2 trasportes", cex = .8, adj = 0)
          text(x = 2, y = 8.5, "Si se poseen\n3 trasportes", cex = .8, adj = 0)
          text(x = 2, y = 5, "Si se poseen\n4 trasportes", cex = .8, adj = 0)
          
          text(x = 8, y = 12, "500€", cex = .8, adj = 1)
          text(x = 8, y = 8.5, "1000€", cex = .8, adj = 1)
          text(x = 8, y = 5, "2000€", cex = .8, adj = 1)
          
          text(x = 5, y = 2, "VALOR DE LA HIPOTECA - 1000€", cex = .7)
          
     }
     
     
     ### Aerolinea:
     tarjeta_aerolinea <- function() {
          par(mar = c(1, 4, 1, 4))
          plot(NA,
               yaxt = "n",
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = c(0, 10),
               ylim = c(0, 30))
          
          
          polygon(x = c(3, 3.5, 7),
                  y = c(22, 24, 28), col = "ivory2")
          polygon(x = c(7, 4.5, 4),
                  y = c(28, 24, 21.5), col = "ivory2")
          polygon(x = c(4.5, 7, 5),
                  y = c(24, 28, 22), col = "ivory2")
          polygon(x = c(3.5, 7, 4.5, 4),
                  y = c(24, 28, 24, 21.5), col = "ivory2")
          
          
          
          
          text(x = 5, y = 18.5, "AEROLINEA\nMONOPOLY", cex = 1, font = 2)
          text(x = 5, y = 15, "ALQUILER      250€", cex = 1)
          
          text(x = 2, y = 12, "Si se poseen\n2 trasportes", cex = .8, adj = 0)
          text(x = 2, y = 8.5, "Si se poseen\n3 trasportes", cex = .8, adj = 0)
          text(x = 2, y = 5, "Si se poseen\n4 trasportes", cex = .8, adj = 0)
          
          text(x = 8, y = 12, "500€", cex = .8, adj = 1)
          text(x = 8, y = 8.5, "1000€", cex = .8, adj = 1)
          text(x = 8, y = 5, "2000€", cex = .8, adj = 1)
          
          text(x = 5, y = 2, "VALOR DE LA HIPOTECA - 1000€", cex = .7)
          
     }
     
     ### Crucero:
     tarjeta_crucero <- function() {
          par(mar = c(1, 4, 1, 4))
          plot(NA,
               yaxt = "n",
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = c(0, 10),
               ylim = c(0, 30))
          
          
          polygon(x = c(3, 7, 6, 4),
                  y = c(24, 24, 22, 22), col = "lightsalmon")
          polygon(x = c(3.5, 5, 5),
                  y = c(24, 28, 24), col = "ivory2")
          polygon(x = c(5, 6.5, 5),
                  y = c(28, 24, 24), col = "ivory2")
          
          
          
          
          text(x = 5, y = 18.5, "CRUCERO\nMONOPOLY", cex = 1, font = 2)
          text(x = 5, y = 15, "ALQUILER      250€", cex = 1)
          
          text(x = 2, y = 12, "Si se poseen\n2 trasportes", cex = .8, adj = 0)
          text(x = 2, y = 8.5, "Si se poseen\n3 trasportes", cex = .8, adj = 0)
          text(x = 2, y = 5, "Si se poseen\n4 trasportes", cex = .8, adj = 0)
          
          text(x = 8, y = 12, "500€", cex = .8, adj = 1)
          text(x = 8, y = 8.5, "1000€", cex = .8, adj = 1)
          text(x = 8, y = 5, "2000€", cex = .8, adj = 1)
          
          text(x = 5, y = 2, "VALOR DE LA HIPOTECA - 1000€", cex = .7)
          
     }
     
     
     
     
     # CREAMOS FUNCIONES PARA IMPRIMIR DIFERENTES CASILLAS DEL TABLERO: ----
     ## Salida:
     tarjeta_salida <- function() {
          par(mar = c(1, 1, 1, 1))
          plot(NA,
               yaxt = "n",
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = c(0, 30),
               ylim = c(0, 30))
          polygon(x = c(0, 0, 30, 30),
                  y = c(0, 30, 30, 0), col = "tan")
          text(x = 15, y = 22.5, "SALIDA", font = 2, family = "mono", cex = 3)
          text(x = 15, y = 15, "Cobra 2000€ cada vez\nque pases por aquí", family = "mono", cex = 1.25)
          arrows(x0 = 10, y0 = 7.5, 
                 x1 = 20, y1 = 7.5, col = "red", lwd = 7, code = 1)
     }
     
     
     
     ## Parking:
     tarjeta_parking <- function() {
          par(mar = c(1, 1, 1, 1))
          plot(NA,
               yaxt = "n",
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = c(0, 30),
               ylim = c(0, 30))
          polygon(x = c(0, 0, 30, 30),
                  y = c(0, 30, 30, 0), col = "tan")
          polygon(x = c(10, 10, 20, 20),
                  y = c(10, 20, 20, 10), col = "blue")
          
          
          
          text(x = 15, y = 25, "PARKING", cex = 2, family = "mono")
          text(x = 15, y = 5, "GRATUITO", cex = 2, family = "mono")
          
          text(x = 15, y = 15, "P", font = 2, cex = 4, col = "white")
          
          
     }
     
     
     
     
     
     ## Ve a la carcel:
     tarjeta_carcel <- function() {
          par(mar = c(1, 1, 1, 1))
          plot(NA,
               yaxt = "n",
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = c(0, 30),
               ylim = c(0, 30))
          polygon(x = c(0, 0, 30, 30),
                  y = c(0, 30, 30, 0), col = "tan")
          
          points(x = 15, y = 15, pch = 17, cex = 22, col = "gold")
          points(x = 15, y = 15, pch = 25, cex = 22, col = "gold", bg = "gold")
          
          text(x = 15, y = 15, "VE A LA\nCÁRCEL", cex = 1.7, family = "mono")
          
     }
     
     
     
     
     ## Carcel solo visitas:
     tarjeta_visita <- function() {
          par(mar = c(1, 1, 1, 1))
          plot(NA,
               yaxt = "n",
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = c(0, 30),
               ylim = c(0, 30))
          polygon(x = c(0, 0, 30, 30),
                  y = c(0, 30, 30, 0), col = "tan")
          polygon(x = c(7, 7, 30, 30),
                  y = c(7, 30, 30, 7), col = "orange")
          
          
          
          text(x = 3.5, y = 20, "SÓLO", cex = 1.75, srt = -90, family = "mono")
          text(x = 20, y = 3.5, "VISITAS", cex = 1.75, family = "mono")
          
          text(x = 18.5, y = 8.8, "CÁRCEL", srt = 0, font = 2, family = "mono", cex = 1.25)
          
          
          
          polygon(x = c(10, 10, 27, 27), 
                  y = c(10, 27, 27, 10), col = "white", lwd = 2)
          
          segments(x0 = 10 + 4.25, x1 = 10 + 4.25,
                   y0 = 10, y1 = 27, lwd = 2)
          segments(x0 = 10 + 2*4.25, x1 = 10 + 2*4.25,
                   y0 = 10, y1 = 27, lwd = 2)
          segments(x0 = 10 + 3*4.25, x1 = 10 + 3*4.25,
                   y0 = 10, y1 = 27, lwd = 2)
          
     }
     
     
     
     
     ## Impuestos:
     ### Impuesto sobre el capital:
     tarjeta_impuesto_1 <- function() {
          par(mar = c(1, 4, 1, 4))
          plot(NA,
               yaxt = "n",
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = c(0, 10),
               ylim = c(0, 30))
          polygon(x = c(0, 0, 10, 10),
                  y = c(0, 30, 30, 0), col = "tan")
          
          text(x = 5, y = 25, "IMPUESTO\nSOBRE EL\nCAPITAL", cex = 2, family = "serif")
          
          points(x = 5, y = 10, pch = 5, cex = 10, lwd = 7)
          points(x = 5, y = 10, pch = 18, cex = 4, lwd = 10)
          
     }
     
     
     
     ### Impuesto de lujo:
     tarjeta_impuesto_2 <- function() {
          par(mar = c(1, 4, 1, 4))
          plot(NA,
               yaxt = "n",
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = c(0, 10),
               ylim = c(0, 30))
          polygon(x = c(0, 0, 10, 10),
                  y = c(0, 30, 30, 0), col = "tan")
          
          text(x = 5, y = 25, "IMPUESTO\nDE LUJO", cex = 2, family = "serif")
          
          polygon(x = c(4, 2.5, 3.5, 6.5, 7.5, 6),
                  y = c(14, 17, 19, 19, 17, 14), col = "turquoise")
          points(x = 5, y = 9, lwd = 14, cex = 16, col = "gold")
     }
     
     
     
     
     
     # BANCO DE TARJETAS <- SUERTE ----
     # 
     num <- c(2, 4, 6, 7, 9, 10, 12, 13, 14, 15, 16, 17, 19, 20, 22, 24, 25, 26, 27, 28, 29, 30, 32, 33, 35, 36, 38, 40)
     
     suerte <- c(rep("Colócate en la casilla de salida (cobra 2000€)", times = 2), # Ve a la salida
                 
                 # Ve a alguna calle
                 paste("Avanza hasta ", tablero[num, "nombre_casilla"], ", si pasas por la casilla de salida, cobra 2000€", sep = ""),
                 
                 # Ve a alguna calle, sin cobrar
                 paste("Avanza hasta ", tablero[num, "nombre_casilla"], sep = ""),
                 
                 # Recibe dinero
                 "Tu empresa organiza el festival de Burdeos, recibe un beneficio de 200€",
                 "Recibe un beneficio de 1500€ de tu club de danza del vientre en el corazón de El Cairo",
                 "Recibe un beneficio de 500€ por tu tienda de reparación de bicicletas en Pekín, China",
                 
                 # Paga dinero
                 "Paga 150€ para tener limpias y pulidas las 32 capsulas del London Eye",
                 "Gasta 500€ en el mercado de navidad de Munich",
                 
                 # Carcel
                 rep("La brigada internacional contra el fraude te descubre. Ve a la cárcel. Ve directamente sin pasar por la casilla de salida y sin cobrar los 2000€", times = 15)
                 )
     
     names(suerte) <- c(rep(1, times = 2),
                        num, 
                        -num,
                        200, 1500, 500,
                        -150, -500,
                        rep(0, times = 15))

     mezcla_suerte <- sample(suerte, length(suerte), replace = FALSE)
     cont_suerte <- 1
     
     
     # Tarjetas suerte:
     tarjeta_suerte <- function() {
          par(mar = c(1, 4, 1, 4))
          plot(NA,
               yaxt = "n",
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = c(0, 10),
               ylim = c(0, 30))
          polygon(x = c(0, 0, 10, 10),
                  y = c(0, 30, 30, 0), col = "orange")
          
          text(x = 2, y = 15, "S  U  E  R  T  E", srt = 90, cex = 2.5, col = "orange3", font = 2)
          text(x = 8, y = 15, "S  U  E  R  T  E", srt = 90, cex = 2.5, col = "orange3", font = 2)
          
          
          text(x = 4.75, y = 15.25, "?", srt = 90, cex = 12)
          text(x = 4.8, y = 15.20, "?", srt = 90, cex = 12)
          text(x = 4.85, y = 15.15, "?", srt = 90, cex = 12)
          text(x = 4.9, y = 15.1, "?", srt = 90, cex = 12)
          text(x = 4.95, y = 15.05, "?", srt = 90, cex = 12)
          text(x = 5, y = 15, "?", srt = 90, cex = 12, col = "darkorange3")
          
          text(x = 5, y = 15, "S  U  E  R  T  E", srt = 90, cex = 2.5, col = "orange3" , font = 2)
     }
     
     
     
     
     
     
     # BANCO DE TARJETA <- DE CAJA DE COMUNIDAD ----
     # 
     
     
     paga_caja <- seq(from = 250, to = 2000, by = 250)
     gana_caja <- seq(from = 250, to = 2000, by = 250)
     
     caja <- c(paste("Gasta ", paga_caja, "€ en el mercado de Navidad de Munich", sep = ""),
               paste("Paga ", paga_caja, "€ para organizar el festival del día de San Patricio en Dublín", sep = ""),
               paste("Recibes una devolución de impuestos. Cobra ", gana_caja, "€", sep = ""),
               paste("Organizas tu propio festival internacional de música y recibes ", gana_caja, "€ por la venta de entradas", sep = ""),
               paste("Paga ", paga_caja, "€ para celebrar una fiesta privada en la playa de Bondi, Sydney", sep = ""),
               paste("Recibe un beneficio de ", gana_caja, "€ de tu boutique de moda parisina", sep = ""))
     
     
     names(caja) <- c(-paga_caja,
                      -paga_caja,
                      gana_caja,
                      gana_caja,
                      -paga_caja,
                      gana_caja)
     
     
     mezcla_caja <- sample(caja, length(caja), replace = FALSE)
     cont_caja <- 1
     
     
     
     # Tarjetas caja de comunidad
     
     tarjeta_caja <- function() {
          par(mar = c(1, 4, 1, 4))
          plot(NA,
               yaxt = "n",
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = c(0, 10),
               ylim = c(0, 30))
          polygon(x = c(0, 0, 10, 10),
                  y = c(0, 30, 30, 0), col = "lightblue3")
          
          text(x = 2, y = 15, "C A J A  D E  C O M U N I D A D", srt = 90, cex = 1.3, col = "lightblue4", font = 2)
          text(x = 8, y = 15, "C A J A  D E  C O M U N I D A D", srt = 90, cex = 1.3, col = "lightblue4", font = 2)
          
          text(x = 4.75, y = 15.25, "$", srt = 90, cex = 12)
          text(x = 4.8, y = 15.20, "$", srt = 90, cex = 12)
          text(x = 4.85, y = 15.15, "$", srt = 90, cex = 12)
          text(x = 4.9, y = 15.1, "$", srt = 90, cex = 12)
          text(x = 4.95, y = 15.05, "$", srt = 90, cex = 12)
          text(x = 5, y = 15, "$", srt = 90, cex = 12, col = "gold")
          
          
          text(x = 5, y = 15, "C A J A  D E  C O M U N I D A D", srt = 90, cex = 1.3, col = "lightblue4", font = 2)
     }
     
     
     vuelta <- 1:turnos
     contador_vueltas <- 1
     
     cat("¡¡¡¡COMENCEMOS!!!!")
     
     # BUCLE DEL JUEGO ----
     # 
     
     for (p in rep(1:length(jugador), times = turnos)) {
          
          Sys.sleep(time)
          
          cat("\n\n--------------------------------\n")
          
          if (p == 1) {
               cat("\n")
               cat("\033[105m", "VUELTA NÚMERO:", vuelta[contador_vueltas], "\033[0m")
               contador_vueltas <- contador_vueltas + 1
               cat("\n\n--------------------------------\n")
          }
          
          ## Si estas en banca ropa -> NEXT ----
          if (jugadores[p, "dinero"] <= 0) {
               cat("\n")
               cat("\033[101m", jugadores[p, "jugador"], "estás en bancarrota", "\033[0m")
               next
          }
          
          
          
          
          # De quien es el turno
          cat("\nTurno de", jugadores[p, "jugador"])
          cat("\n")
          
          
          
          
          
          ## Comprar casas ----
          
          bar <- c("brown", "cyan", "magenta", "orange", "red", "yellow", "green", "blue")
          
          for (bb in 1:length(bar)) {
               if (all(tablero$dueño[tablero$barrio == bar[bb]] == jugadores[p, "jugador"]) & all(tablero$casas[tablero$barrio == bar[bb]] < 4)) {
                    
                    
                    # Probabilidad en funcion del precio de la calle
                    
                    # Si la estrategia es base o estratega:
                    if (jugadores[p, "estrategia"] %in% c("base", "estratega")) {
                         comp_casa <- ifelse(jugadores[p, "dinero"] <= length(tablero$dueño[tablero$barrio == bar[bb]]) * tablero$money[tablero$barrio == bar[bb]][1], "No", sample(c("Si", "No"), 1))
                    } 
                    
                    # Si la estrategia es agresiva:
                    if (jugadores[p, "estrategia"] == "agresivo") {
                         comp_casa <- ifelse(jugadores[p, "dinero"] <= length(tablero$dueño[tablero$barrio == bar[bb]]) * tablero$money[tablero$barrio == bar[bb]][1], "No", "Si")
                    } 
                    
                    # Si la estrategia es ahorrador:
                    if (jugadores[p, "estrategia"] == "ahorrador") {
                         comp_casa <- ifelse(jugadores[p, "dinero"] <= (length(tablero$dueño[tablero$barrio == bar[bb]]) * tablero$money[tablero$barrio == bar[bb]][1]) + 5000, "No", "Si")
                    } 
                    
                    
                    
                    cat("\nTienes todas las propiedades", bar[bb], "\n¿Quieres construir casas?\n", comp_casa, "\n")
                    
                    
                    if (comp_casa == "Si" & all(tablero$casas[tablero$barrio == bar[bb]] == 0)) {
                         # Le quitamos el dinero de las casas al jugador:
                         jugadores[p, "dinero"] <- jugadores[p, "dinero"] - length(tablero$dueño[tablero$barrio == bar[bb]]) * tablero$money[tablero$barrio == bar[bb]][1]
                         
                         # Cambiamos el nuevo alquiler con las casas
                         for (num_casas in 1:length(tablero$dueño[tablero$barrio == bar[bb]])) {
                              tablero$alquiler[tablero$barrio == bar[bb]][num_casas] <- tablero$casa1[tablero$barrio == bar[bb]][num_casas]
                         }
                         
                         
                         tablero$casas[tablero$barrio == bar[bb]] <- 1
                         
                    } else if (comp_casa == "Si" & all(tablero$casas[tablero$barrio == bar[bb]] == 1)) {
                         
                         # Le quitamos el dinero de las casas al jugador:
                         jugadores[p, "dinero"] <- jugadores[p, "dinero"] - length(tablero$dueño[tablero$barrio == bar[bb]]) * tablero$money[tablero$barrio == bar[bb]][1]
                         
                         # Cambiamos el nuevo alquiler con las casas
                         for (num_casas in 1:length(tablero$dueño[tablero$barrio == bar[bb]])) {
                              tablero$alquiler[tablero$barrio == bar[bb]][num_casas] <- tablero$casa2[tablero$barrio == bar[bb]][num_casas]
                         }
                         
                         tablero$casas[tablero$barrio == bar[bb]] <- 2
                         
                    } else if (comp_casa == "Si" & all(tablero$casas[tablero$barrio == bar[bb]] == 2)) {
                         
                         # Le quitamos el dinero de las casas al jugador:
                         jugadores[p, "dinero"] <- jugadores[p, "dinero"] - length(tablero$dueño[tablero$barrio == bar[bb]]) * tablero$money[tablero$barrio == bar[bb]][1]
                         
                         # Cambiamos el nuevo alquiler con las casas
                         for (num_casas in 1:length(tablero$dueño[tablero$barrio == bar[bb]])) {
                              tablero$alquiler[tablero$barrio == bar[bb]][num_casas] <- tablero$casa3[tablero$barrio == bar[bb]][num_casas]
                              
                         }
                         
                         tablero$casas[tablero$barrio == bar[bb]] <- 3
                         
                    } else if (comp_casa == "Si" & all(tablero$casas[tablero$barrio == bar[bb]] == 3)) {
                         
                         # Le quitamos el dinero de las casas al jugador:
                         jugadores[p, "dinero"] <- jugadores[p, "dinero"] - length(tablero$dueño[tablero$barrio == bar[bb]]) * tablero$money[tablero$barrio == bar[bb]][1]
                         
                         # Cambiamos el nuevo alquiler con las casas
                         for (num_casas in 1:length(tablero$dueño[tablero$barrio == bar[bb]])) {
                              tablero$alquiler[tablero$barrio == bar[bb]][num_casas] <- tablero$casa4[tablero$barrio == bar[bb]][num_casas]
                         }
                         
                         tablero$casas[tablero$barrio == bar[bb]] <- 4
                         
                    }
               } else if (all(tablero$dueño[tablero$barrio == bar[bb]] == jugadores[p, "jugador"]) & all(tablero$casas[tablero$barrio == bar[bb]] == 4)) {
                    
                    # Probabilidad en funcion del precio de la calle
                    
                    # Si la estrategia es base o estratega:
                    if (jugadores[p, "estrategia"] %in% c("base", "estratega")) {
                         comp_casa <- ifelse(jugadores[p, "dinero"] <= length(tablero$dueño[tablero$barrio == bar[bb]]) * tablero$money[tablero$barrio == bar[bb]][1], "No", sample(c("Si", "No"), 1))
                    } 
                    
                    # Si la estrategia es agresiva:
                    if (jugadores[p, "estrategia"] == "agresivo") {
                         comp_casa <- ifelse(jugadores[p, "dinero"] <= length(tablero$dueño[tablero$barrio == bar[bb]]) * tablero$money[tablero$barrio == bar[bb]][1], "No", "Si")
                    } 
                    
                    # Si la estrategia es ahorrador:
                    if (jugadores[p, "estrategia"] == "ahorrador") {
                         comp_casa <- ifelse(jugadores[p, "dinero"] <= (length(tablero$dueño[tablero$barrio == bar[bb]]) * tablero$money[tablero$barrio == bar[bb]][1]) + 5000, "No", "Si")
                    } 
                    
                    
                    cat("\nTienes todas las propiedades", bar[bb], "con 4 casas\n¿Quieres construir un HOTEL?\n", comp_casa)
                    if (comp_casa == "Si") {
                         
                         # Le quitamos el dinero de las casas al jugador:
                         jugadores[p, "dinero"] <- jugadores[p, "dinero"] - length(tablero$dueño[tablero$barrio == bar[bb]]) * tablero$money[tablero$barrio == bar[bb]][1]
                         
                         # Cambiamos el nuevo alquiler con las casas
                         for (num_casas in 1:length(tablero$dueño[tablero$barrio == bar[bb]])) {
                              tablero$alquiler[tablero$barrio == bar[bb]][num_casas] <- tablero$hotel[tablero$barrio == bar[bb]][num_casas]
                         }
                         
                         tablero$casas[tablero$barrio == bar[bb]] <- 5
                    }
               }
          }
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          ## SI ESTAS EN LA CARCEL ----
          ## 
          
          
          ### Primer turno ----
          if (tablero$nombre_casilla[jugadores[p, "posicion"]] == "Cárcel" & jugadores[p, "carcel"] == 1) {
               
               cat("\nEstas en la carcel, obten dobles o paga la fianza (500€)")
               
               
               # Si la estrategia es base o estratega:
               if (jugadores[p, "estrategia"] %in% c("base", "estratega")) {
                    fianza <- ifelse(jugadores[p, "dinero"] <= 500, 
                                     "No", sample(c("Si", "No"), 1))
               } 
               
               # Si la estrategia es agresiva:
               if (jugadores[p, "estrategia"] == "agresivo") {
                    fianza <- ifelse(jugadores[p, "dinero"] <= 500, "No", "Si")
               } 
               
               # Si la estrategia es ahorrador:
               if (jugadores[p, "estrategia"] == "ahorrador") {
                    fianza <- ifelse(jugadores[p, "dinero"] <= 2000, "No", "Si")
               }
               
               
               
               cat("\n¿Quieres pagar la fianza?\n", fianza)
               
               
               if (fianza == "No") {
                    
                    cat("\nTire los dados")
                    
                    dado <- replicate(2, sample(1:6, 1)) 
                    dados <- sum(dado)
                    
                    if(dado[1] != dado[2]) {
                         jugadores[p, "carcel"] <- 2
                         cat("\nDado1:", dado[1], "| Dado2:", dado[2], "\nNo has sacado dobles")
                         next
                    } else {
                         jugadores[p, "carcel"] <- 4
                         cat("\nDado1:", dado[1], "| Dado2:", dado[2], "\nHas sacado dobles, avanza")
                    }
               
                         
               } else {
                    jugadores[p, "dinero"] <- jugadores[p, "dinero"] - 500
                    
                    cat("\nTire los dados")
                    
                    dado <- replicate(2, sample(1:6, 1)) 
                    dados <- sum(dado)
               }
               
               
               
               
               
          ### Segundo turno ----  
          } else if (tablero$nombre_casilla[jugadores[p, "posicion"]] == "Cárcel" & jugadores[p, "carcel"] == 2) {
               cat("\nEstas en la carcel, obten dobles o paga la fianza (500€)")
               
               
               
               # Si la estrategia es base o estratega:
               if (jugadores[p, "estrategia"] %in% c("base", "estratega")) {
                    fianza <- ifelse(jugadores[p, "dinero"] <= 500, 
                                     "No", sample(c("Si", "No"), 1))
               } 
               
               # Si la estrategia es agresiva:
               if (jugadores[p, "estrategia"] == "agresivo") {
                    fianza <- ifelse(jugadores[p, "dinero"] <= 500, "No", "Si")
               } 
               
               # Si la estrategia es ahorrador:
               if (jugadores[p, "estrategia"] == "ahorrador") {
                    fianza <- ifelse(jugadores[p, "dinero"] <= 2000, "No", "Si")
               }
               
               
               
               cat("\n¿Quieres pagar la fianza?\n", fianza)
               
               if (fianza == "No") {
                    
                    cat("\nTire los dados")
                    
                    dado <- replicate(2, sample(1:6, 1)) 
                    dados <- sum(dado)
                    
                    if(dado[1] != dado[2]) {
                         jugadores[p, "carcel"] <- 3
                         cat("\nDado1:", dado[1], "| Dado2:", dado[2], "\nNo has sacado dobles")
                         next
                    } else {
                         jugadores[p, "carcel"] <- 4
                         cat("\nDado1:", dado[1], "| Dado2:", dado[2], "\nHas sacado dobles, avanza")
                    }
                    
                    
               } else {
                    jugadores[p, "dinero"] <- jugadores[p, "dinero"] - 500
                    
                    cat("\nTire los dados")
                    
                    dado <- replicate(2, sample(1:6, 1)) 
                    dados <- sum(dado)
               }
               
               
          ### Tercer turno ----  
          } else if (tablero$nombre_casilla[jugadores[p, "posicion"]] == "Cárcel" & jugadores[p, "carcel"] == 3) {
               
               cat("\nEstas en la carcel, obten dobles o paga la fianza (500€)")
               
               
               # Si la estrategia es base o estratega:
               if (jugadores[p, "estrategia"] %in% c("base", "estratega")) {
                    fianza <- ifelse(jugadores[p, "dinero"] <= 500, 
                                     "No", sample(c("Si", "No"), 1))
               } 
               
               # Si la estrategia es agresiva:
               if (jugadores[p, "estrategia"] == "agresivo") {
                    fianza <- ifelse(jugadores[p, "dinero"] <= 500, "No", "Si")
               } 
               
               # Si la estrategia es ahorrador:
               if (jugadores[p, "estrategia"] == "ahorrador") {
                    fianza <- ifelse(jugadores[p, "dinero"] <= 2000, "No", "Si")
               }
               
               
               
               cat("\n¿Quieres pagar la fianza?\n", fianza)
               
               if (fianza == "No") {
                    
                    cat("\nTire los dados")
                    
                    dado <- replicate(2, sample(1:6, 1)) 
                    dados <- sum(dado)
                    
                    if(dado[1] != dado[2]) {
                         jugadores[p, "carcel"] <- 3
                         cat("\nDado1:", dado[1], "| Dado2:", dado[2], "\nNo has sacado dobles")
                         cat("\033[31m", "\nPaga la fianza", "\033[0m")
                         jugadores[p, "dinero"] <- jugadores[p, "dinero"] - 500
                         
                    } else {
                         jugadores[p, "carcel"] <- 4
                         cat("\nDado1:", dado[1], "| Dado2:", dado[2], "\nHas sacado dobles, avanza")
                    }
                    
                    
               } else {
                    jugadores[p, "dinero"] <- jugadores[p, "dinero"] - 500
                    
                    cat("\nTire los dados")
                    
                    dado <- replicate(2, sample(1:6, 1)) 
                    dados <- sum(dado)
               }
          }
          
          
          
          
          
          
          
          ## TIRAMOS LOS DADOS ----
          
          if (jugadores[p, "carcel"] == 4) {
               jugadores[p, "carcel"] <- 0
               
          } else {
               
               dado <- replicate(2, sample(1:6, 1)) 
               dados <- sum(dado)
               
               if (dado[1] == dado[2]) {
                    
                    
                    cat("\nHas sacado dobles:", dado, "\nVuelve a tirar\n")
                    
                    dado <- replicate(2, sample(1:6, 1))
                    dados <- dados + sum(dado)
                    
                    if (dado[1] == dado[2]) {
                         
                         
                         cat("\nHas sacado dobles otra vez:", dado, "\nVuelve a tirar\n")
                         dado <- replicate(2, sample(1:6, 1))
                         dados <- dados + sum(dado)
                         
                         if (dado[1] == dado[2]) {
                              
                              cat("\nHas sacado dobles tres veces:", dado, "\nve a la cárcel\n")
                              
                              jugadores[p, "posicion"] <- 11
                              
                              jugadores[p, "carcel"] <- 1
                              
                              next
                              
                         } else {
                              cat("\nAvanzas", dados, "casillas\n")
                    }
                    } else {
                         
                         cat("\nAvanzas", dados, "casillas\n")
               }
               } else {
               
                    cat("\nAvanzas", dados, "casillas\n")
          }
          }
          
          
          
          
          # No pasa por la casilla de salida al avanzar 
          if (jugadores[p, "posicion"] + dados <= 40) {
               
               jugadores[p, "posicion"] <- jugadores[p, "posicion"] + dados
               
               
               
          # Pasa por la casilla de salida al avanzar     
          } else {
               
               jugadores[p, "posicion"] <- jugadores[p, "posicion"] <- jugadores[p, "posicion"] + dados
               jugadores[p, "posicion"] <- jugadores[p, "posicion"] - 40
               
               
               # Informa de que ha pasado por la casilla de salida y que recibe 2000 €
               cat("\033[32m", "\nHas pasado por la casilla de salida \nRecibes 2000€", "\033[0m")
               cat("\n")
               jugadores[p, "dinero"] <- jugadores[p, "dinero"] + 2000
          }
          
          
          # Te dice donde has caido
          
          cat("\n") 
          
          cat("\033[30;107m", "Has caído en", toupper(tablero[jugadores[p, "posicion"], "nombre_casilla"]), "\033[0m")
          cat("\n")
          
          
          
          if (tablero$tipo_de_casilla[jugadores[p, "posicion"]] == "Calle") {
               
               tarjetas(nombre = toupper(tablero[jugadores[p, "posicion"], "nombre_casilla"]),
                        barrio = tablero[jugadores[p, "posicion"], "barrio"], 
                        alquiler = tablero[jugadores[p, "posicion"], "alquiler"], 
                        casa1 = tablero[jugadores[p, "posicion"], "casa1"],
                        casa2 = tablero[jugadores[p, "posicion"], "casa2"], 
                        casa3 = tablero[jugadores[p, "posicion"], "casa3"], 
                        casa4 = tablero[jugadores[p, "posicion"], "casa4"], 
                        hotel = tablero[jugadores[p, "posicion"], "hotel"], 
                        hipoteca = tablero[jugadores[p, "posicion"], "hipoteca"], 
                        money = tablero[jugadores[p, "posicion"], "money"])
               
               
               board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], 
                     pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                     pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], 
                     pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                     pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], 
                     pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                     pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], 
                     pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
          
          }
          
          
          # Si cae en un trasporte
          if (tablero$nombre_casilla [jugadores[p, "posicion"]] == "Ferrocarril") {
               tarjeta_ferrocarril()
               board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], 
                     pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                     pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], 
                     pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                     pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], 
                     pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                     pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], 
                     pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
               
          } else if (tablero$nombre_casilla [jugadores[p, "posicion"]] == "Aerolínea") {
               tarjeta_aerolinea()
               board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], 
                     pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                     pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], 
                     pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                     pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], 
                     pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                     pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], 
                     pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
               
          } else if (tablero$nombre_casilla [jugadores[p, "posicion"]] == "Crucero") {
               tarjeta_crucero()
               board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], 
                     pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                     pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], 
                     pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                     pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], 
                     pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                     pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], 
                     pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
               
          } else if (tablero$nombre_casilla [jugadores[p, "posicion"]] == "Astronave") {
               tarjeta_astronave()
               board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], 
                     pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                     pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], 
                     pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                     pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], 
                     pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                     pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], 
                     pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
               
          } 
          
          
          # Si cae en una compañia
          if (tablero$nombre_casilla [jugadores[p, "posicion"]] == "Energía Solar") {
               tarjeta_solar()
               board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], 
                     pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                     pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], 
                     pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                     pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], 
                     pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                     pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], 
                     pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
               
          } else if (tablero$nombre_casilla [jugadores[p, "posicion"]] == "Energía Eólica") {
               tarjeta_eolica()
               board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], 
                     pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                     pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], 
                     pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                     pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], 
                     pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                     pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], 
                     pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
          }
          
          
          
          
          
          
          
          
          
          ## SI CAES EN: CARCEL (SOLO VISITAS) ----
          ##
          if (tablero$tipo_de_casilla[jugadores[p, "posicion"]] == "Visita") {
               tarjeta_visita()
               board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], 
                     pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                     pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], 
                     pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                     pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], 
                     pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                     pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], 
                     pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
               
               cat("\033[35m", "\n¡¡Solo visitas!!", "\033[0m")
          }
               
          
          
          
          
          
          
          ## SI CAES EN: PARKING GRATUITO ----
          ## 
          if (tablero$tipo_de_casilla[jugadores[p, "posicion"]] == "Parking") {
               tarjeta_parking()
               board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], 
                     pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                     pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], 
                     pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                     pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], 
                     pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                     pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], 
                     pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
               
               cat("\033[35m", "\nEs gratuito", "\033[0m")
          }
          
               
          
          
          
          
          
          ##SI CAES EN: SALIDA ----
          ##
          if (tablero$tipo_de_casilla[jugadores[p, "posicion"]] == "Salida") {
               tarjeta_salida()
               board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], 
                     pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                     pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], 
                     pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                     pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], 
                     pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                     pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], 
                     pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
               
               cat("\n\nTe quedan", jugadores[p, "dinero"], "€", jugadores[p, "jugador"])
          }
               
          
          
          
          
          
          
          
          ## SI CAES EN: SUERTE ----
          ## 
          
          if (tablero$tipo_de_casilla[jugadores[p, "posicion"]] == "Suerte") {
               tarjeta_suerte()
               board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], 
                     pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                     pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], 
                     pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                     pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], 
                     pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                     pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], 
                     pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
               
               
               cat("\033[33m", "\nCarta:", mezcla_suerte[cont_suerte], "\033[0m")
               cat("\n")
               
               # Si la carta te manda avanzar a otra casilla
               if (as.numeric(names(mezcla_suerte[cont_suerte])) < 41 & as.numeric(names(mezcla_suerte[cont_suerte])) > 0) {
                    
                    jugadores[p, "posicion"] <- as.numeric(names(mezcla_suerte[cont_suerte]))
                    
                    
                    if (tablero$tipo_de_casilla[jugadores[p, "posicion"]] == "Calle") {
                         tarjetas(nombre = toupper(tablero[jugadores[p, "posicion"], "nombre_casilla"]),
                                  barrio = tablero[jugadores[p, "posicion"], "barrio"], 
                                  alquiler = tablero[jugadores[p, "posicion"], "alquiler"], 
                                  casa1 = tablero[jugadores[p, "posicion"], "casa1"],
                                  casa2 = tablero[jugadores[p, "posicion"], "casa2"], 
                                  casa3 = tablero[jugadores[p, "posicion"], "casa3"], 
                                  casa4 = tablero[jugadores[p, "posicion"], "casa4"], 
                                  hotel = tablero[jugadores[p, "posicion"], "hotel"], 
                                  hipoteca = tablero[jugadores[p, "posicion"], "hipoteca"], 
                                  money = tablero[jugadores[p, "posicion"], "money"])
                         
                         board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], 
                               pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                               pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], 
                               pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                               pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], 
                               pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                               pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], 
                               pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
                    }
                         
                         
                    
                    # Si pasas por la casilla de salida recibes 2000€
                    if (jugadores[p, "posicion"] > as.numeric(names(mezcla_suerte[cont_suerte]))) {
                         jugadores[p, "dinero"] <- jugadores[p, "dinero"] + 2000
                         cat("\n\nTe quedan", jugadores[p, "dinero"], "€", jugadores[p, "jugador"])
                    }
                    
                    
               # Si la carta te hace pagar o te da dinero     
               } else if (abs(as.numeric(names(mezcla_suerte[cont_suerte]))) > 41) {
                    jugadores[p, "dinero"] <- jugadores[p, "dinero"] + as.numeric(names(mezcla_suerte[cont_suerte]))
                    
                    cat("\n\nTe quedan", jugadores[p, "dinero"], "€", jugadores[p, "jugador"])
               
                    
                    
                         
               } else if (as.numeric(names(mezcla_suerte[cont_suerte])) > -41 & as.numeric(names(mezcla_suerte[cont_suerte])) < 0) {
                    
                    jugadores[p, "posicion"] <- abs(as.numeric(names(mezcla_suerte[cont_suerte])))
                    
                    
                    if (tablero$tipo_de_casilla[jugadores[p, "posicion"]] == "Calle") {
                         tarjetas(nombre = toupper(tablero[jugadores[p, "posicion"], "nombre_casilla"]),
                                  barrio = tablero[jugadores[p, "posicion"], "barrio"], 
                                  alquiler = tablero[jugadores[p, "posicion"], "alquiler"], 
                                  casa1 = tablero[jugadores[p, "posicion"], "casa1"],
                                  casa2 = tablero[jugadores[p, "posicion"], "casa2"], 
                                  casa3 = tablero[jugadores[p, "posicion"], "casa3"], 
                                  casa4 = tablero[jugadores[p, "posicion"], "casa4"], 
                                  hotel = tablero[jugadores[p, "posicion"], "hotel"], 
                                  hipoteca = tablero[jugadores[p, "posicion"], "hipoteca"], 
                                  money = tablero[jugadores[p, "posicion"], "money"])
                         
                         
                         board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], 
                               pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                               pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], 
                               pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                               pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], 
                               pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                               pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], 
                               pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
                    }
                    
                    
               
                    
                    
               # Si la carta te manda a la carcel     
               } else {
                    jugadores[p, "posicion"] <- 11
                    
                    jugadores[p, "carcel"] <- 1
                    
                    next
                    }
               
               
               if (cont_suerte == length(mezcla_suerte)) {
                    cont_suerte <- 1
                    mezcla_suerte <- sample(suerte, length(suerte), replace = FALSE)
               } else {
                    cont_suerte <- cont_suerte + 1
               }
               
               
          }
          
          
          
          
          
          ## SI CAES EN: CAJA DE COMUNIDAD ----
          ## 
          
          if (tablero$tipo_de_casilla[jugadores[p, "posicion"]] == "Caja") {
               cat("\033[36m", "\nCarta:", mezcla_caja[cont_caja], "\033[0m")
               cat("\n")
               
               tarjeta_caja()
               board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], 
                     pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                     pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], 
                     pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                     pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], 
                     pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                     pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], 
                     pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
               
               jugadores[p, "dinero"] <- jugadores[p, "dinero"] + as.numeric(names(mezcla_caja[cont_caja]))
                    
               cat("\n\nTe quedan", jugadores[p, "dinero"], "€", jugadores[p, "jugador"])
               
               if (cont_caja == length(mezcla_caja)) {
                    cont_caja <- 1
                    mezcla_caja <- sample(caja, length(caja), replace = FALSE)
               } else {
                    cont_caja <- cont_caja + 1
               }
          }
          
          
          
          
          
          
          ## SI CAES EN: VE A LA CARCEL ----
          ## 
          
          
          if (tablero$tipo_de_casilla[jugadores[p, "posicion"]] == "Carcel") {
               tarjeta_carcel()
               board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], 
                     pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                     pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], 
                     pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                     pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], 
                     pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                     pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], 
                     pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
               
               jugadores[p, "posicion"] <- 11
               
               jugadores[p, "carcel"] <- 1
               
               next
          }
          
          
          
          
          
          
          
          
          ## SI CAES EN: IMPUESTO ----
          ## 
          
          
          if (tablero$tipo_de_casilla[jugadores[p, "posicion"]] == "Impuesto") {
               cat("\033[31m", "\nPaga el impuesto: ", tablero[jugadores[p, "posicion"], "precio"], "€", "\033[0m", sep = "")
               
               jugadores[p, "dinero"] <- jugadores[p, "dinero"] - tablero[jugadores[p, "posicion"], "precio"]
               
               cat("\n\nTe quedan", jugadores[p, "dinero"], "€", jugadores[p, "jugador"])
               
               if (tablero$nombre_casilla[jugadores[p, "posicion"]] =="Impuesto sobre el Capital") {
                    tarjeta_impuesto_1()
                    board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], 
                          pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                          pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], 
                          pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                          pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], 
                          pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                          pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], 
                          pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
                    
               }
          } else if (tablero$nombre_casilla[jugadores[p, "posicion"]] =="Impuesto de Lujo") {
               tarjeta_impuesto_2()
               board(pos_x1 = tablero$pos_x[jugadores[1, "posicion"]], pos_y1 = tablero$pos_y[jugadores[1, "posicion"]], 
                     pos_x2 = tablero$pos_x[jugadores[2, "posicion"]], pos_y2 = tablero$pos_y[jugadores[2, "posicion"]], 
                     pos_x3 = tablero$pos_x[jugadores[3, "posicion"]], pos_y3 = tablero$pos_y[jugadores[3, "posicion"]], 
                     pos_x4 = tablero$pos_x[jugadores[4, "posicion"]], pos_y4 = tablero$pos_y[jugadores[4, "posicion"]])
               
          }
          
          
          
          
          
          
          ## SI CAES EN: CALLE X ----
          
          ### Si la calle ya te pertenece ----
          if (tablero$vendida[jugadores[p, "posicion"]] == "Si" & tablero$dueño[jugadores[p, "posicion"]] == jugadores[p, "jugador"]) cat("\nLa calle te pertenece")
               
          
          ### Si no tiene dueño ----
          
          if (tablero$tipo_de_casilla[jugadores[p, "posicion"]] %in% c("Calle", "Compañía", "Estación") & tablero$vendida[jugadores[p, "posicion"]] == "No") { 
               
               # Informa de que la calle no tiene dueño
               cat("\nLa calle no tiene dueño")
               
               # Probabilidad en funcion del precio de la calle
               
               # Si la estrategia es base:
               if (jugadores[p, "estrategia"] == "base") probabilidad <- ifelse(tablero[jugadores[p, "posicion"], "precio"] > jugadores[p, "dinero"], 
                                                                           0, 
                                                                           1 - (tablero[jugadores[p, "posicion"], "precio"] / jugadores[p, "dinero"]))
               
               # Si la estrategia es agresiva:
               if (jugadores[p, "estrategia"] == "agresivo") probabilidad <- ifelse(tablero[jugadores[p, "posicion"], "precio"] > jugadores[p, "dinero"], 
                                                                           0, 
                                                                           1)
               
               # Si la estrategia es ahorrador:
               if (jugadores[p, "estrategia"] == "ahorrador") probabilidad <- ifelse(tablero[jugadores[p, "posicion"], "precio"] + 5000 > jugadores[p, "dinero"],
                                                                                  0, 
                                                                                  1 - ((tablero[jugadores[p, "posicion"], "precio"] + 5000) / jugadores[p, "dinero"]))
               
               
               # Si la estrategia es estratega:
               if (jugadores[p, "estrategia"] == "estratega") {
                    if (tablero[jugadores[p, "posicion"], "precio"] < jugadores[p, "dinero"]) {
                         if (jugadores[p, "posicion"] <= 20 & jugadores[p, "posicion"] >= 17) probabilidad <- 1
                         if (jugadores[p, "posicion"] < 17) probabilidad <- jugadores[p, "posicion"]/40 + 0.5
                         if (jugadores[p, "posicion"] > 20) probabilidad <- (40 - jugadores[p, "posicion"])/21
                    } else {
                         probabilidad <- 0
                    }
               }
                    
               
               
               # Pregunta si quieres comprarla
               decision <- sample(c("No", "Si"), size = 1, prob = c(1 - probabilidad, probabilidad))
               
               cat("\n¿Quieres comprarla?\n", decision)
               
               if(decision == "Si") {
                    
                    # Si decides comprar, quita el dinero del jugador
                    jugadores[p, "dinero"] <- jugadores[p, "dinero"] - tablero[jugadores[p, "posicion"], "precio"]
                    
                    # Cambia el estado de la calle a vendida SI
                    tablero$vendida[jugadores[p, "posicion"]] <- "Si"
                    
                    # Añade la calle a la lista del jugador
                    ifelse(p == 1, 
                           calles_jugador_1 <- cbind(calles_jugador_1, tablero$nombre_casilla[jugadores[p, "posicion"]]), 
                           ifelse(p == 2, calles_jugador_2 <- cbind(calles_jugador_2, tablero$nombre_casilla[jugadores[p, "posicion"]]),
                                  ifelse(p == 3, calles_jugador_3 <- cbind(calles_jugador_3, tablero$nombre_casilla[jugadores[p, "posicion"]]), 
                                         calles_jugador_4 <- cbind(calles_jugador_4, tablero$nombre_casilla[jugadores[p, "posicion"]]))))
                           
                           
                    
                    
                    # Asigna el dueño a la calle en el tablero
                    tablero$dueño[jugadores[p, "posicion"]] <- jugadores[p, "jugador"]
                    
                    # Dime cuanto dinero le queda al jugador despues de comprar
                    cat("\n\nTe quedan", jugadores[p, "dinero"], "€", jugadores[p, "jugador"])
               }
          } 
          
          
          ### Si es una calle o transporte, tiene dueño y no es tuya ----
          
          if (tablero$vendida[jugadores[p, "posicion"]] == "Si" & tablero$dueño[jugadores[p, "posicion"]] != jugadores[p, "jugador"] & tablero$tipo_de_casilla[jugadores[p, "posicion"]] %in% c("Calle", "Estación")) {
               
               if (jugadores$dinero[jugadores$n[jugadores$jugador == tablero$dueño[jugadores[p, "posicion"]]]] <= 0) {
                    
                    cat("\nLa propiedad pertenece a", tablero$dueño[jugadores[p, "posicion"]], "\nPero esta en bancarrota, no tienes que pagar alquiler")
                    next
                    
                    
                    
               } else {
                    # Informa de que la calle tiene dueño
                    
                    cat("\nLa propiedad pertenece a", tablero$dueño[jugadores[p, "posicion"]], "\nPaga el alquiler:", tablero[jugadores[p, "posicion"], "alquiler"], "€")
                    
                    # Si la calle ya le pertenece a otro jugador quita al jugador lo que cueste el alquiler
                    jugadores[p, "dinero"] <- jugadores[p, "dinero"] - tablero[jugadores[p, "posicion"], "alquiler"]
                    
                    # Cuanto dinero tiene el jugador que paga
                    cat("\033[31m", "\n", jugadores[p, "jugador"],"te quedan", jugadores[p, "dinero"], "€", "\033[0m")
                    
                    # Dale el dinero del alquiler al dueño de la calle
                    jugadores[jugadores$n[jugadores$jugador == tablero$dueño[jugadores[p, "posicion"]]], "dinero"] <- jugadores[jugadores$n[jugadores$jugador == tablero$dueño[jugadores[p, "posicion"]]], "dinero"] + tablero[jugadores[p, "posicion"], "alquiler"]
                    
                    # Cuanto dinero tiene el jugador que cobra
                    cat("\033[32m", "\n", tablero$dueño[jugadores[p, "posicion"]],"te quedan", jugadores[jugadores$n[jugadores$jugador == tablero$dueño[jugadores[p, "posicion"]]], "dinero"], "€", "\033[0m")
               }
             
          } 
          
          
          
          
          ### Si caes en una compañia con dueño: ----
          ###
          if (tablero$vendida[jugadores[p, "posicion"]] == "Si" & tablero$dueño[jugadores[p, "posicion"]] != jugadores[p, "jugador"] & tablero$tipo_de_casilla[jugadores[p, "posicion"]] == "Compañía") {
               
               if (jugadores$dinero[jugadores$n[jugadores$jugador == tablero$dueño[jugadores[p, "posicion"]]]] <= 0) {
                    cat("\nLa propiedad pertenece a", tablero$dueño[jugadores[p, "posicion"]], "\nPero esta en bancarrota, no tienes que pagar alquiler")
                    next
                    
                    
               } else {
                    
                    # Si la calle ya le pertenece a otro jugador quita al jugador lo que cueste el alquiler
                    if (sum(tablero$dueño[tablero$barrio == "Compañía"] == tablero$dueño[jugadores[p, "posicion"]]) == 1) {
                         # Informa de que la calle tiene dueño
                         
                         cat("\nLa propiedad pertenece a", tablero$dueño[jugadores[p, "posicion"]], "\nPaga el alquiler:", 40 * sum(dado), "€")
                         
                         jugadores[p, "dinero"] <- jugadores[p, "dinero"] - (40 * sum(dado))
                    } else {
                         # Informa de que la calle tiene dueño
                         cat("\nLa propiedad pertenece a", tablero$dueño[jugadores[p, "posicion"]], "\nPaga el alquiler:", 100 * sum(dado), "€")
                         
                         jugadores[p, "dinero"] <- jugadores[p, "dinero"] - (100 * sum(dado))
                    }
                    
                    
                    # Cuanto dinero tiene el jugador que paga
                    cat("\033[31m", "\n", jugadores[p, "jugador"],"te quedan", jugadores[p, "dinero"], "€", "\033[0m")
                    
                    
                    # Dale el dinero del alquiler al dueño de la calle
                    if (sum(tablero$dueño[tablero$barrio == "Compañía"] == tablero$dueño[jugadores[p, "posicion"]]) == 1) {
                         jugadores[jugadores$n[jugadores$jugador == tablero$dueño[jugadores[p, "posicion"]]], "dinero"] <- jugadores[jugadores$n[jugadores$jugador == tablero$dueño[jugadores[p, "posicion"]]], "dinero"] + (40 * sum(dado))
                         
                    } else {
                         jugadores[jugadores$n[jugadores$jugador == tablero$dueño[jugadores[p, "posicion"]]], "dinero"] <- jugadores[jugadores$n[jugadores$jugador == tablero$dueño[jugadores[p, "posicion"]]], "dinero"] + (100 * sum(dado))
                         
                    }
                    
                    
                    # Cuanto dinero tiene el jugador que cobra
                    cat("\033[32m", "\n", tablero$dueño[jugadores[p, "posicion"]],"te quedan", jugadores[jugadores$n[jugadores$jugador == tablero$dueño[jugadores[p, "posicion"]]], "dinero"], "€", "\033[0m")
               }
               
          } 
          
          
          
          
          
          
          
          # Si tienes varios trasportes aumenta el precio del alquiler: ----
          
          if (sum(tablero$dueño[tablero$barrio == "Estación"] == jugadores[p, "jugador"]) == 2) {
               tablero$alquiler[tablero$barrio == "Estación"][tablero$dueño[tablero$barrio == "Estación"] == jugadores[p, "jugador"]] <- tablero$casa1[tablero$barrio == "Estación"][tablero$dueño[tablero$barrio == "Estación"] == jugadores[p, "jugador"]]
               
          } else if (sum(tablero$dueño[tablero$barrio == "Estación"] == jugadores[p, "jugador"]) == 3) {
               tablero$alquiler[tablero$barrio == "Estación"][tablero$dueño[tablero$barrio == "Estación"] == jugadores[p, "jugador"]] <- tablero$casa2[tablero$barrio == "Estación"][tablero$dueño[tablero$barrio == "Estación"] == jugadores[p, "jugador"]]
               
          } else if (sum(tablero$dueño[tablero$barrio == "Estación"] == jugadores[p, "jugador"]) == 4) {
               tablero$alquiler[tablero$barrio == "Estación"][tablero$dueño[tablero$barrio == "Estación"] == jugadores[p, "jugador"]] <- tablero$casa3[tablero$barrio == "Estación"][tablero$dueño[tablero$barrio == "Estación"] == jugadores[p, "jugador"]]
          }
          
          
          
          
          if (jugadores[p, "dinero"] <= 0) {
               cat("\033[31m", "\n\nEstas en bancarrota", "\033[34m", jugadores[p, "jugador"], "\033[0m")
          }
          
          if (sum(jugadores[, "dinero"] <= 0) == 3) break      
          
     } 
     
     
     View(jugadores[-ncol(jugadores)], title = "Jugadores")
     View(tablero[c("nombre_casilla", "barrio", "precio", "dueño", "alquiler", "casas")], title = "Tablero")
     
     
     
     cat("\n\n\n\n")
     cat("\033[30;104m", "¡¡¡Fin de la partida!!!", "\033[0m")
     
     if (sum(jugadores[, "dinero"] <= 0) == 3) {
          cat("\n\nHa ganado", jugador[which(jugadores[, "dinero"] > 0)], "\nEs el único jugador que no está en bancarrota")
     }
     
     
     cat("\n\nRESUMEN PARTIDA:\n")
     cat("\nJugador 1 -> estrategia:", jugadores[1, "estrategia"])
     cat("\nPropiedades:", length(calles_jugador_1), "\n")
     cat(calles_jugador_1, sep = ", ")
     cat("\nDinero: ", jugadores[1, "dinero"], "€", sep = "")
     
     cat("\n\nJugador 2 -> estrategia:", jugadores[2, "estrategia"])
     cat("\nPropiedades:", length(calles_jugador_2), "\n")
     cat(calles_jugador_2, sep = ", ")
     cat("\nDinero: ", jugadores[2, "dinero"], "€", sep = "")
     
     cat("\n\nJugador 3 -> estrategia:", jugadores[3, "estrategia"])
     cat("\nPropiedades:", length(calles_jugador_3), "\n")
     cat(calles_jugador_3, sep = ", ")
     cat("\nDinero: ", jugadores[3, "dinero"], "€", sep = "")
     
     cat("\n\nJugador 4 -> estrategia:", jugadores[4, "estrategia"])
     cat("\nPropiedades:", length(calles_jugador_4), "\n")
     cat(calles_jugador_4, sep = ", ")
     cat("\nDinero: ", jugadores[4, "dinero"], "€", sep = "")
     
     
}



# JUGAR: ---- 

# Elige:
     # El numero de rondas: turnos = 1000
     # El tiempo de espera entre turnos: time = 4 

monopoly(turnos = 200, time = 0)


