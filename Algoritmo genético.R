# data: base de datos con las variables predictoras y criterio
# n: tamaño de la población
# v: número de variables de cada individuo
# p: probabilidad de mutación
# fin: número total de generaciones


alg_gen <- function(data, n, v, p, fin) {
     
     pob <- data.frame(t(replicate(n, sample(2:ncol(data), size = v))))
     names(pob) <- paste0("v", 1:v)
     
     generacion <- 1
     
     r2_max <- c()
     r2_mean <- c()
     
     
     repeat {
          
          # Calculamos la regresión multiple
          
          for (i in 1:nrow(pob)) {
               formula <- as.formula(paste("casas$price ~", 
                                           paste(paste0("casas[,", pob[i, 1:v], "]"), collapse = "+")))
               
               regre <- lm(formula)
               pob$r2[i] <- summary(regre)$adj.r.squared
               
          }
          
          
          r2_max[generacion] <- max(pob$r2)
          r2_mean[generacion] <- mean(pob$r2)
          
          
          # Ordenamos en funcion de su r2
          pob <- pob[order(pob$r2, decreasing = TRUE), ] 
          
          
          # Guardamos la población si es la mejor hasta el momento
          if (generacion > 1) {
               if (all(r2_mean[generacion] > (r2_mean[1:(generacion - 1)]))) pob_r2_mean <<- pob
               if (all(r2_max[generacion] > (r2_max[1:(generacion - 1)]))) pob_r2_max <<- pob
          }
          
          
          # Nos quedamos con la mitad mejor
          pob_2 <- pob[1:(nrow(pob)/2), ]
          
          
          # Reproducimos los individuos
          m <- 1:nrow(pob_2)
          
          
          prob <- (1 - (m - 1)/nrow(pob_2))
          
          hijos <- t(replicate(n, sample(m, 2, prob = prob)))
          
          rownames(pob) <- 1:n
          for (j in 1:nrow(hijos)) {
               domi <- sample(1:2, size = v, replace = TRUE)
               for (q in 1:v) {
                    pob[j, q] <- pob_2[hijos[j, domi[q]], q]
               }
               
               
          }
          
          
          # Mutacion
          muta <- t(replicate(n, rbinom(n = v, size = 1, prob = p)))
          
          
          pob[, 1:v][muta == 1] <- replicate(sum(muta == 1), sample(2:ncol(data), size = 1))
          
          
          if (generacion == fin) break
          
          
          generacion <- generacion + 1
     }
     
     
     # Gráficos del cambio en r2 a lo largo de las generaciones
     plot(r2_max, type = "l", main = "R2 máximo por generación", ylab = "R2 Max", xlab = "Generación")
     plot(r2_mean, type = "l", main = "R2 medio por generación", ylab = "R2 Max", xlab = "Generación")
     
     
     # Data frame que informa del R2 más alto encontrado, la generación en la que se encontró por primera vez, el conjunto de variables que lo consiguió, el R2 medio más alto y la generación en la que se encontró.
     
     return(data.frame(r2 = max(r2_max),
                       generacion = which(r2_max == max(r2_max))[1], 
                       variables = paste(pob_r2_max[1, -ncol(pob_r2_max)], collapse = ", "), 
                       r2_mean = max(r2_mean),
                       gen_mean = which(r2_mean == max(r2_mean))))
     
}




