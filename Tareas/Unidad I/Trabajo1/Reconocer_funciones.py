import tkinter as tk
from tkinter import ttk, messagebox

def analizar():
    expresion = entrada.get()

    if not expresion.strip():
        messagebox.showwarning("Error", "Por favor ingresa una función matemática.")
        return

    variables = set()
    operaciones = 0

    for i, c in enumerate(expresion):
        if c.isalpha():
            variables.add(c)

        elif c in "+-*/^":
            operaciones += 1

        # Detectar multiplicación implícita
        if i < len(expresion) - 1:  
            siguiente = expresion[i+1]
            if c.isdigit() and siguiente.isalpha():
                operaciones += 1
            elif c.isalpha() and siguiente.isalpha():
                operaciones += 1

    resultado.set(
        f"Variables encontradas: {', '.join(sorted(variables))}\n"
        f"Número de variables: {len(variables)}\n"
        f"Número de operaciones: {operaciones}"
    )

# Crear ventana
ventana = tk.Tk()
ventana.title("Analizador de Funciones Matemáticas")
ventana.geometry("400x300")
ventana.configure(bg="#f4f6f7")

# Estilos
estilo = ttk.Style()
estilo.configure("TButton", font=("Arial", 12), padding=6)
estilo.configure("TLabel", font=("Arial", 12))

# Etiqueta de título
titulo = tk.Label(
    ventana, text="Analizador de Funciones", 
    font=("Arial", 16, "bold"), bg="#f4f6f7", fg="#2c3e50"
)
titulo.pack(pady=10)

# Entrada de texto
entrada = ttk.Entry(ventana, font=("Consolas", 14))
entrada.pack(pady=10, ipadx=20, ipady=5)

# Botón
btn = ttk.Button(ventana, text="Analizar", command=analizar)
btn.pack(pady=10)

# Resultado
resultado = tk.StringVar()
label_resultado = tk.Label(
    ventana, textvariable=resultado, 
    font=("Consolas", 12), bg="#ecf0f1", fg="#2c3e50", 
    relief="groove", justify="left", anchor="w"
)
label_resultado.pack(fill="both", expand=True, padx=20, pady=10)

# Iniciar
ventana.mainloop()
