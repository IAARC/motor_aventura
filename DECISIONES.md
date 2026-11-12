# Decisiones de Diseño

### ¿Cómo representan el mundo y los identificadores? (Map o listas, String o newtype, ¿qué invariantes garantiza el tipo y cuáles no?)

#### 1. Identificadores: `newtype` vs. `String`
Los identificadores de salas y objetos se modelan mediante `newtype` envolviendo a `String`:
- `newtype IdSala = IdSala String`
- `newtype IdObjeto = IdObjeto String`

**Justificación:**  
Aunque en su representación subyacente ambos son cadenas de texto, el uso de `newtype` añade una capa de seguridad en tiempo de compilación (*type safety*), imposibilitando confundir o intercambiar accidentalmente el identificador de una sala con el de un objeto (por ejemplo, intentar usar un `IdObjeto` como destino de una salida o un `IdSala` como requisito).

---

#### 2. Separación en dos fases: Modelo Crudo vs. Modelo Final
Para gestionar el ciclo de vida de los datos y el manejo de errores, se definieron dos estructuras bien diferenciadas:

1. **Estructuras Crudas (`MundoCrudo`, `SalaCruda`, `ObjetoCrudo`):**
   - Utilizan **listas** y preservan números de línea (`Int`) en cada campo, manteniendo los datos como `String`.
   - **Propósito:** Capturan fielmente el contenido del archivo de texto tras el parseo inicial (errores de sintaxis v1). Estas estructuras intermedias permiten realizar las validaciones contextuales posteriores (v2 en adelante) pudiendo reportar la línea exacta de cada error.

2. **Estructuras Finales (`Mundo`, `Sala`, `Objeto`):**
   - Utilizan **`Data.Map`** para almacenar salas (`Map IdSala Sala`) y objetos (`Map IdObjeto Objeto`), así como para las salidas de cada sala (`Map Direccion Salida`). Los objetos contenidos en salas se representan mediante listas de identificadores (`[IdObjeto]`).
   - **Propósito:** Representan el estado limpio y validado que utiliza el motor para la ejecución y jugabilidad, optimizando además el acceso a salas y objetos en $O(\log n)$.

---

#### 3. Invariantes

- **Garantiza el tipo:**
  - **La no confusion de ID:** No puedes pasar por error un objeto donde iba una sala o viceversa, gracias a los `newtype`.
  - **Sin duplicados:** El uso de `Map` garantiza que no haya salas u objetos repetidos con el mismo ID, ni dos salidas hacia la misma dirección en una sala.
  - **Direcciones válidas:** Solo se permiten las direcciones definidas en el tipo.
