# ------------------------------------------------------------
# Simulación y gráfico de f(x), f'(x) y gradiente en R
# grad guardará el "nuevo" valor de x: x(i) - n * f'(x(i))
# ------------------------------------------------------------

# Parámetro n
n <- 0.01

# Definimos la función f(x) = x^2 y su derivada f'(x) = 2x
f <- function(x) x^2
f_deriv <- function(x) 2 * x

# Valor inicial y número de iteraciones
x0 <- 3
iter <- 21

# Vectores vacíos
x <- numeric(iter)
fx <- numeric(iter)
fpx <- numeric(iter)
grad <- numeric(iter)   # ahora contendrá el "nuevo x"

# Primer valor
x[1] <- x0

# Bucle de cálculo
for (i in 1:iter) {
  fx[i]  <- f(x[i])            # f(x)
  fpx[i] <- f_deriv(x[i])      # f'(x)
  
  # grad guarda el nuevo valor de x tras aplicar la corrección:
  grad[i] <- x[i] - n * fpx[i] # <-- aquí obtenemos 2.94 en la primera fila
  
  # siguiente valor de x (para la siguiente iteración)
  if (i < iter) {
    x[i + 1] <- grad[i]        # mejor usar grad[i] que recalcular
  }
}

# Creamos la tabla con nombres "seguros"
tabla <- data.frame(
  xo  = x,
  fx  = fx,
  fpx = fpx,
  grad = grad
)

print(tabla)

# ------------------------------------------------------------
# Gráfico con las tres curvas
# ------------------------------------------------------------

if(!require(ggplot2)) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if(!require(tidyr))  install.packages("tidyr",  repos = "https://cloud.r-project.org")

library(ggplot2)
library(tidyr)

datos_long <- tabla |>
  pivot_longer(cols = c(fx, fpx, grad),
               names_to = "variable",
               values_to = "valor")

ggplot(datos_long, aes(x = xo, y = valor, color = variable)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("blue", "red", "green"),
                     labels = c("f(x)", "f'(x)", "gradiente (x actualizado)")) +
  labs(title = "Comparación de f(x), f'(x) y gradiente (x actualizado)",
       x = "x",
       y = "Valor",
       color = "Variable") +
  theme_minimal(base_size = 13)

