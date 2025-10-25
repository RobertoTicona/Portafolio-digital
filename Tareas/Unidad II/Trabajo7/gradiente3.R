# ------------------------------------------------------------
# Método del gradiente descendente para f(x,y) = 3x^2y^3 + 6xy^2
# ------------------------------------------------------------

# Definimos la función y sus derivadas parciales
f  <- function(x, y) 3*x^2*y^3 + 6*x*y^2
fx <- function(x, y) 6*x*y^3 + 6*y^2
fy <- function(x, y) 9*x^2*y^2 + 12*x*y

# Parámetros
n <- 0.01    # tasa de aprendizaje
iter <- 20   # número de iteraciones

# Valores iniciales
x <- numeric(iter)
y <- numeric(iter)
z <- numeric(iter)

x[1] <- 2
y[1] <- 1

# Iteraciones del gradiente descendente
for (i in 1:iter) {
  z[i] <- f(x[i], y[i])  # valor de f(x,y)
  
  # Derivadas parciales en el punto actual
  dfx <- fx(x[i], y[i])
  dfy <- fy(x[i], y[i])
  
  # Actualización de variables
  if (i < iter) {
    x[i + 1] <- x[i] - n * dfx
    y[i + 1] <- y[i] - n * dfy
  }
}

# Crear tabla con resultados
tabla <- data.frame(
  iter = 1:iter,
  x = x,
  y = y,
  fxy = z
)

print(tabla)

# ------------------------------------------------------------
# Gráfico del recorrido sobre un mapa de calor de f(x,y)
# ------------------------------------------------------------

if(!require(ggplot2)) install.packages("ggplot2", repos = "https://cloud.r-project.org")
library(ggplot2)

# Crear grilla para graficar f(x,y)
x_vals <- seq(-2, 3, by = 0.1)
y_vals <- seq(-2, 3, by = 0.1)
grid <- expand.grid(x = x_vals, y = y_vals)
grid$f <- with(grid, f(x, y))

# Mapa de calor + trayectoria del gradiente descendente
ggplot() +
  geom_tile(data = grid, aes(x = x, y = y, fill = f)) +
  scale_fill_viridis_c(option = "plasma") +
  geom_path(data = tabla, aes(x = x, y = y), color = "red", linewidth = 1.2) +
  geom_point(data = tabla, aes(x = x, y = y), color = "white", size = 2) +
  labs(title = "Descenso del gradiente en f(x,y)",
       x = "x", y = "y", fill = "f(x,y)") +
  theme_minimal(base_size = 13)
