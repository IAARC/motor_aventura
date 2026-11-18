module Main (main) where

import qualified Data.Map as Map
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

import Motor.Comandos (leerComando)
import Motor.Mundo (leerMundo)
import Motor.Nucleo (estadoInicial, ejecutar, metaCumplida, pesoInventario, inventario)
import Motor.Tipos

-- =========================================================================
-- Helpers
-- =========================================================================

-- Lee un mundo desde un archivo y devuelve Right Mundo o Left [ErrorMundo].
leerMundoDesde :: FilePath -> IO (Either [ErrorMundo] Mundo)
leerMundoDesde ruta = leerMundo <$> readFile ruta

-- Número de errores que devuelve leerMundo para un archivo.
contarErrores :: FilePath -> IO Int
contarErrores ruta = do
  r <- leerMundoDesde ruta
  pure $ case r of
    Left es -> length es
    Right _ -> 0

-- Aplica una secuencia de comandos sobre un estado, devolviendo el estado
-- final y todos los mensajes acumulados.
ejecutarSecuencia :: [Comando] -> Estado -> ([String], Estado)
ejecutarSecuencia [] e = ([], e)
ejecutarSecuencia (c : cs) e =
  let (msgs, e')   = ejecutar c e
      (rest, e'')  = ejecutarSecuencia cs e'
  in  (msgs ++ rest, e'')

-- =========================================================================
-- 1. leerMundo — mundos válidos
-- =========================================================================

testEntregaValido :: Test
testEntregaValido = "leerMundo entrega.txt es válido" ~: do
  r <- leerMundoDesde "mundos/entrega.txt"
  case r of
    Right m -> do
      mundoTitulo m @?= "Entrega a última hora"
      mundoInicio m @?= IdSala "estacionamiento"
      mundoMeta m   @?= LlevarA (IdObjeto "proyecto_impreso") (IdSala "oficina_del_profesor")
      -- Sin CAPACIDAD
      mundoCapacidad m @?= Nothing
      -- 5 objetos, 9 salas
      Map.size (mundoObjetos m) @?= 5
      Map.size (mundoSalas m)   @?= 9
    Left es -> assertFailure ("se esperaba un mundo válido, pero hubo "
                              ++ show (length es) ++ " error(es)")

testMundoPropioValido :: Test
testMundoPropioValido = "leerMundo mundo.txt (propio) es válido" ~: do
  r <- leerMundoDesde "mundos/mundo.txt"
  case r of
    Right m -> do
      mundoTitulo m @?= "Apagón en el cuarto de servidores"
      mundoInicio m @?= IdSala "recepcion"
      mundoMeta m   @?= LlevarA (IdObjeto "bateria_ups") (IdSala "sala_de_servidores")
      mundoCapacidad m @?= Just 10
      Map.size (mundoObjetos m) @?= 6
      Map.size (mundoSalas m)   @?= 9
    Left es -> assertFailure ("se esperaba un mundo válido, pero hubo "
                              ++ show (length es) ++ " error(es)")

-- =========================================================================
-- 2. leerMundo — mundos inválidos (cantidad de errores)
-- =========================================================================

testInvalidoSintaxisProfesor :: Test
testInvalidoSintaxisProfesor = "invalido_sintaxis.txt tiene 4 errores V1" ~: do
  n <- contarErrores "mundos/invalido_sintaxis.txt"
  n @?= 4

testInvalidoSemanticaProfesor :: Test
testInvalidoSemanticaProfesor = "invalido_semantica.txt tiene 9 errores" ~: do
  n <- contarErrores "mundos/invalido_semantica.txt"
  n @?= 9

testSintaxisPropio :: Test
testSintaxisPropio = "sintaxis.txt tiene 8 errores V1" ~: do
  n <- contarErrores "mundos/invalidos/sintaxis.txt"
  n @?= 8

testReferencias :: Test
testReferencias = "referencias.txt tiene 9 errores" ~: do
  n <- contarErrores "mundos/invalidos/referencias.txt"
  n @?= 9

testGrafoYPeso :: Test
testGrafoYPeso = "grafo_y_peso.txt tiene 3 errores" ~: do
  n <- contarErrores "mundos/invalidos/grafo_y_peso.txt"
  n @?= 3

testNoCabenJuntos :: Test
testNoCabenJuntos = "no_caben_juntos.txt tiene 1 error" ~: do
  n <- contarErrores "mundos/invalidos/no_caben_juntos.txt"
  n @?= 1

-- =========================================================================
-- 3. Secuencias de ejecutar — llegar a la meta (entrega.txt)
-- =========================================================================

testSecuenciaGanaEntrega :: Test
testSecuenciaGanaEntrega = "secuencia que gana entrega.txt" ~: do
  r <- leerMundoDesde "mundos/entrega.txt"
  case r of
    Left _  -> assertFailure "entrega.txt debería ser válido"
    Right m -> do
      let e0 = estadoInicial m
          -- Recorrido que gana:
          -- 1. tomar carnet (en estacionamiento)
          -- 2. ir norte (-> plaza)
          -- 3. ir oeste (-> biblioteca, requiere carnet ✓)
          -- 4. tomar memoria_usb
          -- 5. ir este (-> plaza)
          -- 6. ir norte (-> pasillo_mys)
          -- 7. ir este (-> ldc, requiere carnet ✓)
          -- 8. tomar llave_del_ascensor
          -- 9. ir oeste (-> pasillo_mys)
          -- 10. ir oeste (-> fotocopiadora, requiere memoria_usb ✓)
          -- 11. tomar proyecto_impreso
          -- 12. ir este (-> pasillo_mys)
          -- 13. ir arriba (-> segundo_piso, requiere llave_del_ascensor ✓)
          -- 14. ir norte (-> oficina_del_profesor)
          cmds = [ Tomar (IdObjeto "carnet")
                 , Ir Norte
                 , Ir Oeste
                 , Tomar (IdObjeto "memoria_usb")
                 , Ir Este
                 , Ir Norte
                 , Ir Este
                 , Tomar (IdObjeto "llave_del_ascensor")
                 , Ir Oeste
                 , Ir Oeste
                 , Tomar (IdObjeto "proyecto_impreso")
                 , Ir Este
                 , Ir Arriba
                 , Ir Norte
                 ]
          (_, ef) = ejecutarSecuencia cmds e0
      -- Debe haber llegado a la meta
      metaCumplida ef @?= True
      estSala ef @?= IdSala "oficina_del_profesor"

-- =========================================================================
-- 4. Secuencia — salida bloqueada
-- =========================================================================

testSalidaBloqueada :: Test
testSalidaBloqueada = "salida bloqueada sin el objeto requerido" ~: do
  r <- leerMundoDesde "mundos/entrega.txt"
  case r of
    Left _  -> assertFailure "entrega.txt debería ser válido"
    Right m -> do
      let e0 = estadoInicial m
          -- Ir norte (estacionamiento -> plaza), ir oeste (plaza -> biblioteca)
          -- sin carnet: bloqueada
          cmds = [ Ir Norte, Ir Oeste ]
          (bloqMsgs, ef) = ejecutarSecuencia cmds e0
      -- El jugador debe seguir en plaza, no en biblioteca
      estSala ef @?= IdSala "plaza"
      -- El mensaje debe indicar que está bloqueada
      assertBool "el mensaje menciona que necesita carnet"
        (any (\s -> "bloqueada" `isInfixOf'` s || "carnet" `isInfixOf'` s) bloqMsgs)

-- Helper: comprobación de subcadena (sin importar Data.List.isInfixOf)
isInfixOf' :: String -> String -> Bool
isInfixOf' []    _  = True
isInfixOf' (_:_) [] = False
isInfixOf' xs    ys@(_:ys')
  | prefijo xs ys = True
  | otherwise     = isInfixOf' xs ys'
  where
    prefijo []     _      = True
    prefijo _      []     = False
    prefijo (a:as) (b:bs) = a == b && prefijo as bs

-- =========================================================================
-- 5. Secuencia — variante peso (mundo.txt propio)
-- =========================================================================

testVariantePeso :: Test
testVariantePeso = "peso: no se puede tomar un objeto que exceda la capacidad restante" ~: do
  r <- leerMundoDesde "mundos/mundo.txt"
  case r of
    Left _  -> assertFailure "mundo.txt debería ser válido"
    Right m -> do
      let e0 = estadoInicial m
          -- Escenario: ir a almacen, tomar escalera (peso 5), ir a entrepiso,
          -- intentar tomar bateria_ups (peso 7) => 5+7=12 > capacidad 10 => no cabe
          cmds = [ Tomar (IdObjeto "carnet")          -- peso 1
                 , Ir Norte                            -- recepcion -> pasillo
                 , Ir Oeste                            -- pasillo -> almacen
                 , Tomar (IdObjeto "escalera")         -- peso 5, total 6
                 , Ir Este                             -- almacen -> pasillo
                 , Ir Arriba                           -- pasillo -> entrepiso (requiere escalera ✓)
                 , Tomar (IdObjeto "bateria_ups")      -- peso 7, total 6+7=13 > 10 => rechazado
                 ]
          (_msgs, ef) = ejecutarSecuencia cmds e0
      -- La batería no debe estar en el inventario
      assertBool "bateria_ups no está en el inventario"
        (IdObjeto "bateria_ups" `notElem` inventario ef)
      -- El peso no debe exceder la capacidad
      assertBool "peso del inventario <= capacidad"
        (pesoInventario ef <= 10)
      -- Para ganar: hay que soltar escalera, luego tomar bateria_ups
      let cmds2 = [ Soltar (IdObjeto "escalera")
                  , Tomar (IdObjeto "bateria_ups")     -- peso 7, total 1+7=8 <= 10 ✓
                  ]
          (_, ef2) = ejecutarSecuencia cmds2 ef
      assertBool "ahora sí lleva bateria_ups"
        (IdObjeto "bateria_ups" `elem` inventario ef2)

-- =========================================================================
-- 6. Secuencia — ganar el mundo propio (mundo.txt)
-- =========================================================================

testSecuenciaGanaMundoPropio :: Test
testSecuenciaGanaMundoPropio = "secuencia completa que gana mundo.txt" ~: do
  r <- leerMundoDesde "mundos/mundo.txt"
  case r of
    Left _  -> assertFailure "mundo.txt debería ser válido"
    Right m -> do
      let e0 = estadoInicial m
          -- Siguiendo el guión de mundo_0.txt (adaptado):
          cmds = [ -- En recepcion: tomar carnet
                   Tomar (IdObjeto "carnet")
                   -- Ir a oficina_vigilante (requiere carnet ✓)
                 , Ir Oeste
                 , Tomar (IdObjeto "tarjeta_acceso")
                 , Ir Este              -- volver a recepcion
                 , Ir Norte             -- pasillo
                 , Ir Oeste             -- almacen
                 , Tomar (IdObjeto "escalera")
                 , Ir Este              -- pasillo
                 , Ir Arriba            -- entrepiso (requiere escalera ✓)
                 -- Aquí: carnet(1) + tarjeta_acceso(1) + escalera(5) = 7
                 -- bateria_ups pesa 7 → 7+7=14 > 10 → no cabe
                 -- Hay que soltar escalera primero
                 , Soltar (IdObjeto "escalera")
                 , Tomar (IdObjeto "bateria_ups")   -- 1+1+7=9 <= 10 ✓
                 , Ir Abajo             -- pasillo
                 , Ir Norte             -- laboratorio
                 , Ir Este              -- sala_de_servidores (requiere tarjeta_acceso ✓)
                 ]
          (_, ef) = ejecutarSecuencia cmds e0
      metaCumplida ef @?= True
      estSala ef @?= IdSala "sala_de_servidores"
      assertBool "lleva la batería" (IdObjeto "bateria_ups" `elem` inventario ef)

-- =========================================================================
-- 7. leerComando — comandos válidos
-- =========================================================================

testLeerComandoIr :: Test
testLeerComandoIr = "leerComando: ir norte" ~: do
  leerComando "ir norte"    @?= Just (Ir Norte)
  leerComando "ir sur"      @?= Just (Ir Sur)
  leerComando "ir este"     @?= Just (Ir Este)
  leerComando "ir oeste"    @?= Just (Ir Oeste)
  leerComando "ir arriba"   @?= Just (Ir Arriba)
  leerComando "ir abajo"    @?= Just (Ir Abajo)

testLeerComandoAtajosDireccion :: Test
testLeerComandoAtajosDireccion = "leerComando: atajos de dirección" ~: do
  leerComando "norte"  @?= Just (Ir Norte)
  leerComando "n"      @?= Just (Ir Norte)
  leerComando "s"      @?= Just (Ir Sur)
  leerComando "e"      @?= Just (Ir Este)
  leerComando "o"      @?= Just (Ir Oeste)

testLeerComandoTomar :: Test
testLeerComandoTomar = "leerComando: tomar y coger" ~: do
  leerComando "tomar carnet"      @?= Just (Tomar (IdObjeto "carnet"))
  leerComando "coger carnet"      @?= Just (Tomar (IdObjeto "carnet"))
  leerComando "tomar la memoria usb" @?= Just (Tomar (IdObjeto "memoria_usb"))

testLeerComandoOtros :: Test
testLeerComandoOtros = "leerComando: mirar, inventario, ayuda, salir" ~: do
  leerComando "mirar"      @?= Just Mirar
  leerComando "inventario" @?= Just Inventario
  leerComando "inv"        @?= Just Inventario
  leerComando "ayuda"      @?= Just Ayuda
  leerComando "salir"      @?= Just Salir

testLeerComandoGuardarCargar :: Test
testLeerComandoGuardarCargar = "leerComando: guardar y cargar" ~: do
  leerComando "guardar partida.txt" @?= Just (Guardar "partida.txt")
  leerComando "cargar partida.txt"  @?= Just (Cargar "partida.txt")

-- =========================================================================
-- 8. leerComando — comandos inválidos
-- =========================================================================

testLeerComandoInvalido :: Test
testLeerComandoInvalido = "leerComando: comandos inválidos devuelven Nothing" ~: do
  leerComando ""             @?= Nothing
  leerComando "volar"        @?= Nothing
  leerComando "ir"           @?= Nothing
  leerComando "tomar"        @?= Nothing
  leerComando "ir noreste"   @?= Nothing
  leerComando "guardar"      @?= Nothing

-- =========================================================================
-- Todas las pruebas
-- =========================================================================

pruebas :: Test
pruebas = TestList
  -- leerMundo — mundos válidos (2)
  [ testEntregaValido                  -- 1
  , testMundoPropioValido              -- 2
  -- leerMundo — mundos inválidos (6)
  , testInvalidoSintaxisProfesor       -- 3
  , testInvalidoSemanticaProfesor      -- 4
  , testSintaxisPropio                 -- 5
  , testReferencias                    -- 6
  , testGrafoYPeso                     -- 7
  , testNoCabenJuntos                  -- 8
  -- ejecutar — secuencias puras (4)
  , testSecuenciaGanaEntrega           -- 9
  , testSalidaBloqueada                -- 10
  , testVariantePeso                   -- 11
  , testSecuenciaGanaMundoPropio       -- 12
  -- leerComando (5)
  , testLeerComandoIr                  -- 13
  , testLeerComandoAtajosDireccion     -- 14
  , testLeerComandoTomar               -- 15
  , testLeerComandoOtros               -- 16
  , testLeerComandoGuardarCargar       -- 17
  , testLeerComandoInvalido            -- 18
  ]

main :: IO ()
main = do
  resultado <- runTestTT pruebas
  if errors resultado + failures resultado == 0
    then exitSuccess
    else exitFailure
