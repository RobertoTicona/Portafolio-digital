import numpy as np
import matplotlib.pyplot as plt

# === 1. Ingreso de la función ===
print("=== MÉTODO DE REGULA FALSI (FALSA POSICIÓN) ===")
func_str = input("Ingrese la función f(x): ")  # Ejemplo: x**3 - x - 1

# Definición de la función
def f(x):
    return eval(func_str, {"np": np, "x": x})

# === 2. Graficar la función ===
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

# === 3. Pregunta si desea aplicar el método de Regula Falsi ===
op = input("¿Desea aplicar el método de Regula Falsi? (s/n): ").lower()

if op == "s":
    # === 4. Ingreso de los extremos del intervalo ===
    a = float(input("Ingrese el valor de a (extremo izquierdo): "))
    b = float(input("Ingrese el valor de b (extremo derecho): "))

    # Comprobación del cambio de signo
    if f(a) * f(b) > 0:
        print("⚠️ La función no cambia de signo en el intervalo. No se puede aplicar el método.")
    else:
        tol = 1e-6
        max_iter = 100

        print("\nIteración |      a      |      b      |      c      |    f(c)     |   Error")
        print("----------------------------------------------------------------------------")

        c_old = a
        for i in range(1, max_iter + 1):
            # Fórmula de Regula Falsi
            c = b - (f(b) * (a - b)) / (f(a) - f(b))
            error = abs(c - c_old)
            c_old = c

            print(f"{i:9d} | {a:10.6f} | {b:10.6f} | {c:10.6f} | {f(c):10.6f} | {error:10.6f}")

            if abs(f(c)) < tol or error < tol:
                print(f"\n✅ Raíz aproximada encontrada: {c:.6f}")
                print(f"Iteraciones realizadas: {i}")
                break

            # Actualización de intervalos
            if f(a) * f(c) < 0:
                b = c
            else:
                a = c
        else:
            print("\n⚠️ No se alcanzó la convergencia después de", max_iter, "iteraciones.")
else:
    print("No se aplicó el método de Regula Falsi.")
