module Main (main) where

import Control.Monad (unless, when)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import System.IO

import Motor.Comandos (leerComando)
import Motor.Mundo (leerMundo, mostrarError)
import Motor.Nucleo (bienvenida, ejecutar, estadoInicial, metaCumplida)
import Motor.Persistencia (cargarPartida, guardarPartida, leerArchivoMundo)
import Motor.Tipos

-- Uso:
--   aventura MUNDO            carga el mundo y empieza el juego
--   aventura validar MUNDO    solo valida el mundo
main :: IO ()
main = do
  configurarConsola
  args <- getArgs
  case args of
    ["validar", ruta] -> do
      _ <- cargarMundo ruta
      putStrLn "Mundo válido."
      exitSuccess
    [ruta] -> do
      m <- cargarMundo ruta
      jugar m
    _ -> do
      hPutStrLn stderr "Uso: aventura MUNDO\n     aventura validar MUNDO"
      exitWith (ExitFailure 2)

-- UTF-8 en la consola sin importar el locale, para que las tildes y la ñ se
-- lean y se muestren bien (también con la entrada redirigida).
configurarConsola :: IO ()
configurarConsola = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  hSetBuffering stdout LineBuffering

-- Lee y valida el mundo. Si hay errores los imprime, uno por línea, y
-- termina con código 1.
cargarMundo :: FilePath -> IO Mundo
cargarMundo ruta = do
  r <- leerArchivoMundo ruta
  case r of
    Left msg -> fallar ["Error: " ++ msg]
    Right txt -> case leerMundo txt of
      Left errores -> fallar (map mostrarError errores)
      Right m      -> pure m
  where
    fallar ls = mapM_ putStrLn ls >> exitWith (ExitFailure 1)

jugar :: Mundo -> IO ()
jugar m = do
  -- Si la entrada no es una terminal (aventura mundo.txt < guion.txt), se
  -- repite cada comando después del indicador para que la salida se lea
  -- como una sesión.
  eco <- not <$> hIsTerminalDevice stdin
  let e0 = estadoInicial m
  mapM_ putStrLn (bienvenida e0)
  bucle eco e0

-- Se usa isEOF + getLine en lugar de getContents: así cada línea se procesa
-- apenas llega y el fin de la entrada se detecta explícitamente, sin
-- excepciones de getLine.
bucle :: Bool -> Estado -> IO ()
bucle eco e = do
  putStr "> "
  hFlush stdout
  fin <- isEOF
  if fin
    then putStrLn "" >> putStrLn "Fin de la entrada. Hasta luego."
    else do
      linea <- getLine
      when eco (putStrLn linea)
      case leerComando linea of
        Nothing -> do
          unless (all (`elem` " \t\r") linea) $
            putStrLn "No entiendo ese comando. Escribe \"ayuda\" para ver los comandos."
          bucle eco e
        Just cmd -> atender eco cmd e

-- Guardar, cargar y salir tienen efectos (archivos, terminar el programa),
-- así que se atienden aquí; todo lo demás lo resuelve el núcleo puro.
atender :: Bool -> Comando -> Estado -> IO ()
atender eco cmd e = case cmd of
  Salir -> putStrLn "Hasta luego."
  Guardar ruta -> do
    r <- guardarPartida ruta e
    putStrLn (either id (const ("Partida guardada en " ++ ruta ++ ".")) r)
    bucle eco e
  Cargar ruta -> do
    r <- cargarPartida ruta (estMundo e)
    case r of
      Left msg -> putStrLn msg >> bucle eco e
      Right e' -> do
        putStrLn ("Partida cargada de " ++ ruta ++ ".")
        continuar (fst (ejecutar Mirar e')) e'
  _ -> uncurry continuar (ejecutar cmd e)
  where
    continuar mensajes e' = do
      mapM_ putStrLn mensajes
      if metaCumplida e'
        then putStrLn "*** ¡Has ganado! ***"
        else bucle eco e'
