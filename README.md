# CI-3661 — Laboratorio de Lenguajes de Programación I
## Proyecto 1: Motor de Aventuras de Texto en Haskell

### Integrantes
- **John Garrido** — Carnet: `20-10293`
- **Andrés Ramírez** — Carnet: `21-10520`

---

### Asignación de Variante

Siguiendo la fórmula de asignación establecida en la especificación del proyecto:

$$\text{Variante} = (\text{Carnet}_1 + \text{Carnet}_2) \pmod 4$$

1. Primer carnet sin guión: `2010293`
2. Segundo carnet sin guión: `2110520`
3. Suma: $2010293 + 2110520 = 4120813$
4. Módulo 4: $4120813 \pmod 4 = 1$

**Variante asignada:** **1 — Peso**

En esta variante:
- La cabecera del mundo puede definir `CAPACIDAD: n` (capacidad máxima de carga del jugador).
- Los bloques de objeto pueden definir `PESO: n` (entero positivo; por defecto 1 si se omite).
- El inventario no permite sobrepasar la capacidad especificada.
- Se implementan validaciones semánticas adicionales: ningún objeto puede pesar más que la capacidad máxima y los objetos necesarios para cumplir la meta o atravesar salidas bloqueadas deben poder cargarse simultáneamente sin exceder la capacidad.

---

### Requisitos

- **GHC** (probado con GHC 9.10+)
- **Cabal** (>= 2.4)
- Paquetes de Haskell: `base`, `containers`, `directory`, `filepath`, `HUnit` (para la suite de pruebas).

---

### Compilación

Para compilar todo el proyecto (biblioteca, ejecutable y suite de pruebas):

```bash
cabal build
```

---

### Ejecución

El ejecutable se denomina `aventura` (o ejecutable mediante `cabal run`).

#### 1. Jugar una partida interactiva

```bash
cabal run aventura -- mundos/entrega.txt
```

O con el mundo propio:

```bash
cabal run aventura -- mundos/mundo.txt
```

#### 2. Validar un archivo de mundo sin jugar

Permite verificar si la sintaxis y semántica del archivo son válidas según las reglas V1–V10 y las restricciones de peso:

```bash
cabal run aventura -- --validar mundos/entrega.txt
```

O también:

```bash
cabal run aventura -- validar mundos/mundo.txt
```

- Si el archivo es válido, imprime `Mundo válido.` y termina con código de salida `0`.
- Si contiene errores, los lista todos por línea (comenzando con `Error: `) y finaliza con código de salida `1`.

#### 3. Ejecución con guion redirigido

Para correr una partida automatizada mediante un archivo de comandos:

```bash
cabal run aventura -- mundos/entrega.txt < guiones/entrega.txt
```

O para el mundo propio con su guion:

```bash
cabal run aventura -- mundos/mundo.txt < guiones/mundo.txt
```

---

### Pruebas

Para ejecutar la suite de pruebas automatizadas con `HUnit`:

```bash
cabal test
```

La suite contiene al menos 18 casos de prueba automatizados que verifican:
- Reconocimiento y validación de mundos válidos (`mundos/entrega.txt` y `mundos/mundo.txt`).
- Conteo y detección precisa de errores en mundos inválidos sintáctica y semánticamente.
- Ejecución pura del núcleo del juego sin E/S (recorridos hasta la meta, choques con salidas bloqueadas y manejo del límite de peso).
- Reconocimiento del analizador de comandos (`leerComando`), soportando sinónimos, artículos y rechazando comandos inválidos.

---

### Estructura del Proyecto

- `src/Motor/`:
  - `Tipos.hs`: Tipos algebraicos del dominio (`Mundo`, `Sala`, `Objeto`, `Estado`, `Comando`, etc.).
  - `Reconocedor.hs`: Combinadores monádicos construidos sobre `ReadS`.
  - `Mundo.hs`: Parseo y validaciones semánticas (V1 a V10 + Variante Peso).
  - `Comandos.hs`: Parser léxico y sintáctico de los comandos del usuario.
  - `Nucleo.hs`: Lógica pura del juego (transición de estados sin E/S).
  - `Persistencia.hs`: Funciones de persistencia y serialización para guardar/cargar partidas.
- `app/Main.hs`: Entrada principal, interfaz CLI y ciclo interactivo (REPL).
- `test/Spec.hs`: Suite de pruebas unitarias (`HUnit`).
- `mundos/`: Archivos de mundos de ejemplo, propios e inválidos de prueba.
- `guiones/`: Guiones de comandos automatizados para ganar los mundos.
- `DECISIONES.md`: Registro de decisiones de arquitectura (ADR).
- `IA.md`: Declaración del uso de herramientas de asistencia basadas en IA.
