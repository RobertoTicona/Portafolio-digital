import numpy as np
import matplotlib.pyplot as plt

# === 1. Ingreso de la función y su forma iterativa g(x) ===
print("=== MÉTODO DEL PUNTO FIJO ===")
print("Recuerde que f(x) = 0 se reescribe como x = g(x)")
print("Ejemplo: Si f(x) = cos(x) - x, entonces g(x) = cos(x)\n")

func_str = input("Ingrese la función original f(x): ")   # Ejemplo: np.cos(x) - x
g_str = input("Ingrese la función iterativa g(x): ")     # Ejemplo: np.cos(x)

# Definición de funciones
def f(x):
    return eval(func_str, {"np": np, "x": x})

def g(x):
    return eval(g_str, {"np": np, "x": x})

# === 2. Graficar la función f(x) ===
xmin = float(input("Ingrese el valor mínimo de x: "))
xmax = float(input("Ingrese el valor máximo de x: "))

x = np.linspace(xmin, xmax, 400)
y = f(x)

plt.figure(figsize=(8, 5))
plt.plot(x, y, label=f"f(x) = {func_str}", color='blue')
plt.axhline(0, color='black', linestyle='--')
plt.axvline(0, color='black', linestyle='--')
plt.title("Gráfico de la función ingresada")
plt.xlabel("x")
plt.ylabel("f(x)")
plt.legend()
plt.grid(True)
plt.show()

# === 3. Pregunta si desea aplicar el método de punto fijo ===
op = input("¿Desea aplicar el método de Punto Fijo? (s/n): ").lower()

if op == "s":
    # === 4. Ingreso de valor inicial ===
    x0 = float(input("Ingrese el valor inicial x0: "))

    tol = 1e-6
    max_iter = 100

    print("\nIteración |     x0      |     g(x0)     |     f(x0)     |   Error")
    print("---------------------------------------------------------------")

    for i in range(1, max_iter + 1):
        x1 = g(x0)
        error = abs(x1 - x0)

        print(f"{i:9d} | {x0:10.6f} | {x1:12.6f} | {f(x1):12.6f} | {error:10.6f}")

        if error < tol:
            print(f"\n✅ Raíz aproximada encontrada: {x1:.6f}")
            print(f"Iteraciones realizadas: {i}")
            break

        x0 = x1
    else:
        print("\n⚠️ No se alcanzó la convergencia después de", max_iter, "iteraciones.")
else:
    print("No se aplicó el método de Punto Fijo.")
