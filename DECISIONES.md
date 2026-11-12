# Decisiones de Diseño

## D1. ¿Cómo representan el mundo y los identificadores? 
Redactó: Andres.

**Contexto**
El modelado del mundo y sus identificadores presentaba el inconveniente de que se necesita parsear texto, seguidamente pasar una serie de validaciones sintacticas y semanticas lo que exige conservar numeros de lineas y tolerar datos incompletos o invalidos. Luego, el motor necesita consultas rapidas, evitar estados inconsistentes y prevenir confusiones humanas (intercambio de IDs)

---

**Opciones**
* *A:* Usar simples sinónimos de tipos (`type Id = String`), listas en todas las entidades (`[Sala]`, `[Objeto]`, `[(Direccion, String)]`) y una única estructura compartida desde el parseo hasta el juego. Hay un pro claro que es la simplicidad, pero una busqueda mas lenta, overhead de arrastrar datos innecesarios y riesgo de mezclar diferentes ID
* *B:* Separar la arquitectura en dos representaciones:
  1. Un modelo crudo (`MundoCrudo`) basado en listas, tuplas y `String` con números de línea para parseo y validación de errores.
  2. Un modelo final (`Mundo`) basado en `Data.Map` y tipos nominales (`newtype`) para la ejecución.

---

**Decisión**
Se adoptó un el diseño en dos etapas (opción 2): un modelo crudo (`MundoCrudo`) basado en listas con números de línea para reportar errores de sintaxis y contexto (v1 y v2+), y un modelo final (`Mundo`) estructurado con `Data.Map` para búsquedas eficientes en ejecución. Los IDs se encapsulan con `newtype` (`IdSala` e `IdObjeto`) para evitar mezclar salas y objetos en tiempo de compilación. Gracias a los tipos y a `Map`, se garantiza por construcción la unicidad de claves, la validez de las direcciones y que no haya salidas duplicadas en una misma dirección.

---

**Consecuencias:**
 * *Qué ganamos*: Seguridad en compilación, mensajes de error más precisos, rendimiento mayo en busquedas $O(\log n)$ y estructuras limpias de duplicados
* *Qué perdimos:* Simplicidad de codigo, hay que mantener dos modelos de datos paralelos, y desarrollar una función de tranformación del mundo crudo al final.

## D2. Qué guarda el Estado y dónde quedan los objetos
Redactó: Andrés

**Contexto:** el mundo es inmutable durante la partida, pero sí cambian el jugador (sala actual) y los objetos (se toman y se sueltan). Había que decidir qué parte vive en Estado, si se copia el mundo y dónde queda un objeto soltado.

---

**Opciones:**
- A. Copiar el Mundo completo dentro del estado y modificar las salas.
  Es más simple de escribir, pero mezcla lo estático con lo dinámico.
- B. `Map IdSala [IdObjeto]` más una lista aparte para el inventario. Es muy
  intuitivo, pero la posición de un objeto queda repartida en dos sitios y el código de `ejecutar` debe mantener a mano el invariante.
- C. `Map IdObjeto Ubicacion`, con `data Ubicacion = EnSala IdSala | EnInventario`.

---

**Decisión:** C. `Estado` guarda `estSala` (dónde está el jugador),
`estUbicacion` (dónde está cada objeto ahora) y `estMundo`, una referencia al
mundo inmutable que no se copia ni se modifica.

**Consecuencias:**
- Ganamos: cada objeto está en exactamente un lugar por construcción, así que
  no puede aparecer en dos sitios ni en ninguno. 
- Perdimos: 


---

## D3. Cómo se diseñó el reconocedor
Redactó: Andrés

**Contexto:** el formato del mundo se lee línea por línea y hay que construir los combinadores, dar el número de línea de cada error de sintaxis y reportar varios errores a la vez. Pero `ReadS` solo devuelve una lista de lecturas, sin explicar por qué falla.

---

**Opciones:**
- A. Usar `ReadS` directamente como alias de tipo. Es lo más corto, pero cada
  combinador queda con el manejo de listas a la vista y no se puede usar
  `do`, `<$>` ni `<|>`.
- B. Un `newtype Reconocedor a` sobre `ReadS` con instancias de `Functor`,
  `Applicative`, `Monad` y `Alternative`. 

---

**Decisión:** `newtype Reconocedor` sobre `ReadS`; `<|>` con todas las
lecturas; `munch` para clases de caracteres; primera lectura completa. Se
reconoce en dos niveles: primero funciones puras sobre las líneas (numerarlas,
quitar espacios, descartar vacías y comentarios, cortar en bloques) y luego
combinadores que reconocen el contenido de una sola línea.

---

**Consecuencias:**
- Ganamos: gramáticas legibles con `do` y `<|>`; la clase de caracteres da un
  único resultado con `munch`, y `<|>` no pierde lecturas por el orden en que
  se escriben las alternativas.
- Cada línea falla de manera independiente, lo que permite seguir y acumular
  todos los errores de sintaxis (V1).
- Perdimos: los mensajes de error no dicen en qué columna falló ni qué se
  esperaba, solo que la línea no se pudo reconocer.
