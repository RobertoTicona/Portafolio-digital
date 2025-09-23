# Función para preparar expresiones lineales
def preparar_expresion(expr: str) -> str:
    expr = expr.replace(" ", "")          # quitar espacios
    expr = expr.replace("^", "**")        # potencia
    expr = expr.replace("-x", "-1*x")     # caso -x
    expr = expr.replace("+x", "+1*x")     # caso +x
    if expr.startswith("x"):              # si empieza con x
        expr = "1*" + expr
    expr = expr.replace("x", "*x")        # poner multiplicación
    expr = expr.replace("**x", "*x")      # corregir si se duplicó
    return expr

# Pedimos las dos funciones
func1 = input("Ingrese la primera función (ejemplo: 2x+1): ")
func2 = input("Ingrese la segunda función (ejemplo: -x+3): ")

func1 = preparar_expresion(func1)
func2 = preparar_expresion(func2)

# Definimos el rango del gráfico (valores de X y Y)
xmin, xmax = -20, 20
ymin, ymax = -20, 20

# Recorremos los valores de Y de arriba hacia abajo
for y in range(ymax, ymin - 1, -1):
    linea = ""

    for x in range(xmin, xmax + 1):
        try:
            y1 = eval(func1)
        except:
            y1 = None
        try:
            y2 = eval(func2)
        except:
            y2 = None

        cond1 = (y1 is not None and abs(y - y1) < 0.5)
        cond2 = (y2 is not None and abs(y - y2) < 0.5)

        if cond1 and cond2:
            linea += "#"
        elif cond1:
            linea += "*"
        elif cond2:
            linea += "o"
        elif x == 0 and y == 0:
            linea += "+"
        elif x == 0:
            linea += "|"
        elif y == 0:
            linea += "-"
        else:
            linea += " "
    print(linea)

# Leyenda
print("\nLeyenda del gráfico:")
print("  * = Función 1")
print("  o = Función 2")
print("  # = Intersección")
print("  | = Eje Y")
print("  - = Eje X")
print("  + = Origen (0,0)")

