# Decisiones de Diseño

## D1. ¿Cómo representan el mundo y los identificadores?
Redactó: Andrés

**Contexto:** el mundo se lee de un archivo de texto y pasa por validaciones
sintácticas y semánticas, lo que exige conservar números de línea y tolerar
datos incompletos o inválidos mientras se revisan. Luego, durante la partida,
el motor necesita consultas rápidas por identificador y evitar confusiones
entre identificadores de salas y de objetos, que son ambos `String` en el
archivo.

---

**Opciones:**
- A. Sinónimos de tipo (`type Id = String`), listas para todas las entidades
  (`[Sala]`, `[Objeto]`, `[(Direccion, String)]`) y una única estructura desde
  el reconocimiento hasta el juego. Es lo más simple, pero las búsquedas son
  lineales, se arrastran los números de línea durante toda la partida y nada
  impide pasar el id de un objeto donde se esperaba una sala.
- B. Dos representaciones:
  1. Un modelo crudo (`MundoCrudo`, `SalaCruda`, `ObjetoCrudo` en
     `Motor.Tipos`) con listas, `String` y números de línea, para reconocer y
     validar.
  2. Un modelo final (`Mundo`) con `Data.Map` y `newtype` para jugar.

---

**Decisión:** B. `Motor.Mundo.reconocerMundo` produce el `MundoCrudo`,
`validarMundo` lo revisa y solo si no hay errores `construir` arma el `Mundo`.
Los ids van envueltos en `newtype IdSala` y `newtype IdObjeto`, así que el
compilador rechaza mezclarlos. Las salidas de una sala son un
`Map Direccion Salida`.

---

**Consecuencias:**
- Ganamos: el tipo garantiza que cada sala y cada objeto del `Mundo` tiene un
  id único (son claves de `Map`), que una sala no tiene dos salidas en la
  misma dirección y que las direcciones son solo las seis válidas
  (`data Direccion`). Las búsquedas son O(log n).
- El tipo **no** garantiza que los destinos de las salidas, los requisitos,
  `INICIO` y la meta existan, ni que cada objeto esté en una sola sala: eso lo
  asegura `validarMundo` antes de construir el `Mundo`. Por eso `Motor.Nucleo`
  todavía tiene que contemplar un `Map.lookup` que falle (`describirSala`).
- Perdimos simplicidad: hay dos modelos paralelos y una función de
  conversión (`construir`, `convertirSala`, `convertirObjeto`).
- Qué haríamos distinto: con más tiempo, exportaríamos `Mundo` sin su
  constructor para que solo `leerMundo` pudiera crearlo, y así el invariante
  "mundo validado" quedaría en manos del módulo y no de la disciplina.

---

## D2. Qué guarda el Estado y dónde quedan los objetos
Redactó: Andrés

**Contexto:** el mundo es inmutable durante la partida, pero cambian la sala
del jugador y la posición de los objetos (se toman y se sueltan). Había que
decidir qué vive en `Estado`, si se copia el mundo y dónde queda un objeto
soltado.

---

**Opciones:**
- A. Copiar el `Mundo` completo dentro del estado y modificar las salas.
  Es simple de escribir, pero mezcla lo estático con lo dinámico y guardar
  la partida implicaría guardar el mundo entero.
- B. `Map IdSala [IdObjeto]` más una lista aparte para el inventario. Es muy
  intuitivo, pero la posición de un objeto queda repartida en dos sitios y
  `ejecutar` debe mantener a mano que no esté en ambos.
- C. `Map IdObjeto Ubicacion`, con `data Ubicacion = EnSala IdSala | EnInventario`.

---

**Decisión:** C. `Estado` guarda `estSala` (dónde está el jugador),
`estUbicacion` (dónde está cada objeto ahora) y `estMundo`, una referencia al
mundo inmutable que no se copia ni se modifica. `estadoInicial` llena
`estUbicacion` con los `OBJETO:` de cada sala. Al soltar, `soltar` cambia la
ubicación a `EnSala (estSala e)`: el objeto queda en la sala actual y se puede
volver a tomar.

---

**Consecuencias:**
- Ganamos: un objeto no puede estar en dos lugares a la vez, porque es una
  sola clave del `Map`. Tomar y soltar son un único `Map.insert`.
- Un objeto definido pero no colocado en ninguna sala simplemente no aparece
  en el `Map`: no está en ningún lado y no se puede conseguir (V10 avisa si
  ese objeto hacía falta).
- Perdimos: para listar los objetos de una sala (`objetosAqui`) hay que
  recorrer todo el `Map`, O(n) en el número de objetos. Con los tamaños de
  mundo esperados no importa.
- Qué haríamos distinto: si los mundos fueran grandes, mantendríamos además
  un índice inverso sala → objetos, a costa de tener que mantenerlo
  sincronizado.

---

## D3. Cómo se diseñó el reconocedor
Redactó: Andrés

**Contexto:** hay que construir combinadores propios sobre `ReadS`, dar el
número de línea de cada error de sintaxis y reportar varios errores a la vez.
Pero `ReadS` solo devuelve una lista de lecturas, sin decir dónde ni por qué
falla, y no sabe en qué línea está.

---

**Opciones:**
- A. Usar `ReadS` directamente. Es lo más corto, pero cada combinador queda
  con el manejo de listas a la vista y no se puede usar `do`, `<$>` ni `<|>`.
- B. Un `newtype Reconocedor a` sobre `ReadS` con instancias de `Functor`,
  `Applicative`, `Monad` y `Alternative`.
- C. (para el número de línea) Llevar la posición dentro del estado del
  reconocedor, contando `'\n'` al consumir.

---

**Decisión:** B, y el número de línea sin C. Se reconoce en dos niveles:
1. Funciones puras sobre las líneas en `Motor.Mundo`: `lineasUtiles` numera
   con `zip [1 ..]` *antes* de descartar vacías y comentarios (así los números
   son los del archivo), y `bloques` corta por `---`.
2. Combinadores de `Motor.Reconocedor` que reconocen el contenido de una sola
   línea (`lineaClave`, `salidaCruda`, `metaCruda`, `identificador`,
   `direccion`). Como el número viaja junto a la línea, cada error se crea
   con él.

Ambigüedad: `<|>` concatena todas las lecturas y `reconocerLinea` se queda con
la primera que consume la línea completa. Por ejemplo, en `salidaCruda` el
`optional` del `requiere` produce dos lecturas y solo una llega al final.
`munch` es determinista (se queda con el prefijo más largo), lo que evita
explosiones de lecturas en identificadores.

---

**Consecuencias:**
- Ganamos: gramáticas legibles con `do` y `<|>`, y cada línea falla de forma
  independiente, lo que permite seguir y acumular todos los errores (D4).
- Para dar mensajes precisos, SALIDA y META se reconocen primero con una
  forma laxa (`palabra` en vez de `identificador`) y después se revisa cada
  pieza: así se distingue "dirección inexistente: noreste" de "identificador
  inválido" y de "SALIDA mal formada".
- Perdimos: el reconocedor no informa la columna ni qué esperaba; si la forma
  laxa falla, solo podemos decir que la línea está mal formada.
- Qué haríamos distinto: un reconocedor que devuelva `Either` con el error
  más lejano alcanzado daría mensajes mejores, a cambio de alejarse de
  `ReadS`.

---

## D4. Cómo se reportan todos los errores de validación
Redactó: John

**Contexto:** el enunciado pide reportar todos los errores juntos. Pero
`Either` con `do` se detiene en el primer `Left`: si la cabecera falla, nunca
se revisan los bloques siguientes.

---

**Opciones:**
- A. Encadenar `Either` con `do`. Simple, pero reporta un solo error.
- B. Que cada revisión devuelva una lista de errores y concatenarlas. Sirve
  cuando las revisiones son independientes y ya se tiene el dato completo.
- C. Un `Applicative` propio que, cuando los dos lados fallan, junte ambas
  listas de errores. Sirve para *construir* un valor (el `MundoCrudo`) a partir
  de partes que pueden fallar por separado.

---

**Decisión:** C para la sintaxis y B para la semántica.
- En `reconocerMundo` usamos `newtype Validacion a` (en `Motor.Mundo`). Su
  `<*>` concatena los errores de ambos lados. Así, en `cabecera`,
  `CabeceraCruda <$> ... <*> ...` revisa los cuatro campos aunque fallen
  varios, y `traverse bloque` revisa todos los bloques. Los errores se ordenan
  por línea al final.
- Hay una sola dependencia real: no tiene sentido revisar el identificador de
  un campo que falta o está repetido. Para eso existe `luego`, que es
  secuencial.
- En `validarMundo`, cada regla (`v2` … `v10`, `vPeso`) es una lista por
  comprensión sobre el `MundoCrudo` y el resultado es `concat` de todas.
- `leerMundo txt = reconocerMundo txt >>= validarMundo` *sí* encadena a propósito: si
  hay errores de sintaxis no se valida lo demás, porque el mundo crudo
  estaría incompleto y daría errores falsos.

---

**Consecuencias:**
- Ganamos: con `mundos/invalido_semantica.txt` salen los 9 errores, y con
  `mundos/invalido_sintaxis.txt` los 4, cada uno con su línea.
- Para no reportar lo mismo dos veces hubo que coordinar reglas: un objeto no
  definido da V6 pero no V7; si `INICIO` no existe no se marcan todas las
  salas como inalcanzables (V9); un objeto inexistente que hacía falta da V4
  o V8 pero no V10.
- Perdimos: `Validacion` no es una mónada lícita (no puede serlo y acumular a
  la vez), así que los pasos dependientes se escriben con `luego` en lugar de
  `do`.

---

## D5. Variante 1: peso y capacidad
Redactó: John

**Contexto:** la cabecera trae `CAPACIDAD: n` y cada objeto puede traer
`PESO: n` (1 si no se da). Pero `mundos/entrega.txt` no trae `CAPACIDAD` y aun
así debe poder jugarse. Además hay que decidir qué tan lejos llevar la
validación de "los objetos necesarios deben poder llevarse a la vez".

---

**Opciones (capacidad ausente):**
- A. Exigir `CAPACIDAD` como campo obligatorio. Rompe el mundo de ejemplo.
- B. `mundoCapacidad :: Maybe Int`, con `Nothing` como "sin límite".

**Opciones (validación conjunta):**
- A. Solo revisar que ningún objeto pese más que la capacidad.
- B. Búsqueda exacta sobre los estados (sala, inventario): no da falsos
  positivos ni negativos, pero es exponencial en la cantidad de objetos.
- C. Una condición necesaria barata: para cumplir `llevar o a d` hay que
  *entrar* a `d` con `o` en el inventario y, por lo tanto, también con el
  requisito de la salida por la que se entra.

---

**Decisión:** capacidad B y validación A + C.
- `ocPeso :: Maybe Int` en el modelo crudo, que `convertirObjeto` resuelve a
  1 si falta.
- `vPeso` reporta cada objeto que pese más que la capacidad
  (`ErrPesoExcedeCapacidad`). Si no hay ninguno, `entregaPosible` toma la
  entrada a la sala de la meta que pida menos peso
  (`peso o + peso requisito`) y, si aun así se pasa, reporta
  `ErrObjetosNoCabenJuntos`.
- No se revisa si `o` ya empieza colocado en `d`, porque en ese caso basta
  con tomarlo ahí.
- En el juego, `tomar` rechaza el objeto si `pesoInventario e + peso` supera
  la capacidad, e `inventario` muestra el peso cargado.

---

**Consecuencias:**
- Ganamos: la comprobación nunca rechaza un mundo ganable y detecta el caso
  más típico (meta pesada detrás de una puerta con llave pesada).
- Perdimos: no detecta todos los mundos imposibles. Por ejemplo, si para
  llegar a la llave hay que cargar otro objeto pesado antes.
- Si un objeto ya no cabe solo, no se emite además el error conjunto, porque
  solo repetiría el mismo problema.

---

## D6. Qué formas de los comandos se aceptan
Redactó: John

**Contexto:** `tomar memoria_usb` siempre debe funcionar, pero el jugador
escribe con mayúsculas, artículos y espacios. Además, el guion
`guiones/entrega.txt` usa solo la dirección (`norte`), sin `ir`.

---

**Opciones:**
- A. Aceptar solo la forma exacta del enunciado. Simple, pero el guion de
  entrega no funcionaría.
- B. Normalizar la entrada antes de reconocerla: minúsculas, quitar
  artículos, unir el nombre del objeto con `_`.
- C. Buscar el objeto por coincidencia parcial con los objetos del mundo
  (`tomar memoria` → `memoria_usb`).

---

**Decisión:** B, en `Motor.Comandos.leerComando`:
- la entrada pasa a minúsculas;
- la dirección sola o abreviada (`n s e o ar ab`) equivale a `ir`;
- `sinArticulos` quita `el la los las un una al a hacia` delante del objeto o
  la dirección, pero nunca la última palabra, para que un objeto llamado `la`
  siga funcionando;
- `objetoDe` une varias palabras con `_`, así que `tomar la Memoria USB` es
  `Tomar (IdObjeto "memoria_usb")`;
- los sinónimos `coger` e `inv`;
- el archivo de `guardar` y `cargar` se toma de la línea original, sin pasarlo
  a minúsculas.

Descartamos C porque `leerComando :: String -> Maybe Comando` no conoce el
mundo.

---

**Consecuencias:**
- Ganamos: `leerComando` sigue siendo pura e independiente del mundo, y
  acepta las formas naturales más comunes.
- Perdimos: `o` como abreviatura de oeste impide usarla para otra cosa, y un
  nombre mal escrito no se corrige: el núcleo responde "No hay … aquí".

---

## D7. Si una salida bloqueada queda abierta después de cruzarla
Redactó: John

**Contexto:** una salida con `requiere <objeto>` solo se cruza llevando el
objeto. Queda la duda de qué pasa si después se suelta.

---

**Opciones:**
- A. Recordar en el `Estado` las salidas ya abiertas, como una puerta que se
  destraba.
- B. No recordar nada: revisar el inventario cada vez que se intenta cruzar.

---

**Decisión:** B. En `Motor.Nucleo.ir`, una salida `Salida destino (Just o)`
se cruza solo si `lleva o e` en ese momento. `mirar` marca
`(requiere <objeto>)` solo en las salidas que el jugador no puede cruzar
ahora.

---

**Consecuencias:**
- Ganamos: el `Estado` no crece y guardar la partida no necesita registrar
  salidas abiertas. La regla es fácil de explicar: el objeto es una llave que
  hay que llevar.
- Perdimos: con la variante de peso, el jugador puede tener que volver a
  cargar una llave pesada para regresar por una puerta que ya cruzó. En
  mundos con capacidad ajustada esto puede ser molesto, pero también es parte
  del reto.
