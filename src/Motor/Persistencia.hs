module Motor.Persistencia
  ( leerArchivoMundo
  , guardarPartida
  , cargarPartida
  ) where

import Control.Exception (IOException, evaluate, try)
import qualified Data.List as L
import qualified Data.Map as Map
import System.IO
import System.IO.Error (isAlreadyInUseError, isDoesNotExistError, isPermissionError)
import Text.Read (readMaybe)

import Motor.Tipos

-- Único módulo (junto con Main) que toca archivos. Las funciones devuelven
-- Either en lugar de dejar escapar excepciones: un archivo que no existe o no
-- se puede escribir es un error esperable y el juego debe seguir.

-- ---------------------------------------------------------------------------
-- Lectura y escritura en UTF-8

-- Lee el archivo completo en UTF-8 sin importar el locale del sistema. Se
-- fuerza todo el contenido antes de cerrar el archivo (hGetContents es
-- perezoso y, si no, se leería un archivo ya cerrado).
leerTexto :: FilePath -> IO (Either String String)
leerTexto ruta = do
  r <- try $ withFile ruta ReadMode $ \h -> do
    hSetEncoding h utf8
    s <- hGetContents h
    _ <- evaluate (length s)
    pure s
  pure $ case r of
    Left err -> Left (describirFallo ruta err)
    Right s  -> Right s

escribirTexto :: FilePath -> String -> IO (Either String ())
escribirTexto ruta s = do
  r <- try $ withFile ruta WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h s
  pure $ either (Left . describirFallo ruta) Right r

describirFallo :: FilePath -> IOException -> String
describirFallo ruta err = "No se pudo usar el archivo " ++ ruta ++ ": " ++ motivo
  where
    motivo
      | isDoesNotExistError err  = "no existe."
      | isPermissionError err    = "no hay permiso."
      | isAlreadyInUseError err  = "está en uso."
      | otherwise                = show err

leerArchivoMundo :: FilePath -> IO (Either String String)
leerArchivoMundo = leerTexto

-- ---------------------------------------------------------------------------
-- Partidas guardadas
--
-- Se guarda solo lo que cambia (la sala del jugador y dónde está cada objeto),
-- no el mundo, junto con una huella del mundo para reconocer una partida que
-- es de otro mundo o de una versión distinta del mismo archivo.

data Partida = Partida
  { ptHuella    :: Int
  , ptSala      :: IdSala
  , ptUbicacion :: [(IdObjeto, Ubicacion)]
  } deriving (Show, Read)

-- Suma de comprobación (djb2) del mundo completo. Cualquier cambio en el
-- mundo (una descripción, una salida) cambia la huella.
huella :: Mundo -> Int
huella = L.foldl' (\h c -> (h * 33 + fromEnum c) `mod` 1000000007) 5381 . show

guardarPartida :: FilePath -> Estado -> IO (Either String ())
guardarPartida ruta e = escribirTexto ruta (show partida ++ "\n")
  where
    partida = Partida
      { ptHuella    = huella (estMundo e)
      , ptSala      = estSala e
      , ptUbicacion = Map.toList (estUbicacion e)
      }

cargarPartida :: FilePath -> Mundo -> IO (Either String Estado)
cargarPartida ruta m = do
  r <- leerTexto ruta
  pure (r >>= interpretar)
  where
    interpretar txt = case readMaybe txt of
      Nothing -> Left ("El archivo " ++ ruta ++ " no es una partida guardada o está corrupto.")
      Just p  -> desdePartida m p

-- Revisa que la partida corresponda a este mundo y que sus datos tengan
-- sentido en él (la huella podría coincidir con un archivo editado a mano).
desdePartida :: Mundo -> Partida -> Either String Estado
desdePartida m p
  | ptHuella p /= huella m =
      Left "Esa partida es de otro mundo (o el mundo cambió desde que se guardó)."
  | not (salaExiste (ptSala p)) || not (all ubicacionValida (ptUbicacion p)) =
      Left "La partida está corrupta: nombra salas u objetos que no existen."
  | otherwise =
      Right Estado
        { estSala      = ptSala p
        , estUbicacion = Map.fromList (ptUbicacion p)
        , estMundo     = m
        }
  where
    salaExiste s = Map.member s (mundoSalas m)
    ubicacionValida (o, u) = Map.member o (mundoObjetos m) && case u of
      EnSala s     -> salaExiste s
      EnInventario -> True
