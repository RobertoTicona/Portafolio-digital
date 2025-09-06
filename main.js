function mostrarSaludo() {
  const hora = new Date().getHours();
  let saludo = "";

  if (hora >= 6 && hora < 12) {
    saludo = "🌅 Buenos días, bienvenido a mi portafolio";
  } else if (hora >= 12 && hora < 18) {
    saludo = "☀️ Buenas tardes, bienvenido a mi portafolio";
  } else {
    saludo = "🌙 Buenas noches, bienvenido a mi portafolio";
  }

  document.getElementById("saludo").innerText = saludo;
}

// Ejecuta la función apenas se cargue la página
window.onload = mostrarSaludo;
