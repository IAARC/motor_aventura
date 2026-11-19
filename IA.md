#### Declaración de uso de IA:
Andres:
Se consultó al modelo Claude para abordar el modelado del estado dinámico del juego frente a la inmutabilidad del mundo, puntualmente para rastrear el movimiento de objetos al tomarlos o soltarlos. Entre las alternativas evaluadas —separar la ubicación entre un mapa de salas y una lista para el inventario (Opción A), duplicar y mutar la estructura completa de `Mundo` (Opción B), o centralizar el estado en un `Map IdObjeto Ubicacion` donde `Ubicacion = EnSala IdSala | EnInventario` (Opción C)— se optó por la Opción C, ya que garantiza por diseño que un objeto nunca pueda existir simultáneamente en dos lugares y simplifica significativamente la serialización y guardado de partidas.

Se utilizo Gemini para la generación de un README.md con las caracteristicas exigidas en el enunciado del proyecto. Ademas de la generacion de los mundos de prueba invalidos.

John:
Claude generó código y documentación a partir de mis indicaciones, del enunciado y de
los tipos y el reconocedor inicial que yo había escrito. Concretamente:
- Paquete cabal (motor-aventura.cabal), reorganización a Motor.* y esqueleto de tests.
- Ajustes a Tipos.hs (capacidad opcional con Maybe, errores de la variante peso).
- Combinadores del reconocedor y Mundo.hs: reconocimiento del archivo, acumulación
  de errores de sintaxis con un Applicative Validacion, y validación V2–V10 más
  la variante de peso.
- Comandos.hs (leerComando) y Nucleo.hs (estado inicial, ejecutar, meta).
- Persistencia.hs (guardar/cargar partidas con huella del mundo) y Main.hs.

