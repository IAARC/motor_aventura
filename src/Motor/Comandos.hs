module Motor.Comandos
  ( leerComando
  ) where

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, intercalate)

import Motor.Reconocedor (direccion, reconocerLinea)
import Motor.Tipos

-- Interpreta una línea escrita por el jugador. Es pura.
--
-- Formas aceptadas además de las del enunciado:
--   * mayúsculas o minúsculas indistintamente (TOMAR Carnet);
--   * la dirección sola como atajo de "ir" (norte), y las abreviaturas
--     n, s, e, o, ar, ab;
--   * artículos delante del objeto o la dirección (tomar la memoria usb,
--     ir al norte);
--   * objetos con espacios en lugar de guiones bajos (memoria usb);
--   * los sinónimos coger (tomar) e inv (inventario).
-- El nombre de archivo de guardar/cargar se toma tal cual (sin pasarlo a
-- minúsculas), porque los sistemas de archivos distinguen mayúsculas.
leerComando :: String -> Maybe Comando
leerComando linea = case words (map toLower linea) of
  []               -> Nothing
  [verbo]          -> sinArgumento verbo
  ("ir" : args)    -> Ir <$> dirDe args
  (verbo : args)
    | verbo `elem` ["tomar", "coger"] -> Tomar <$> objetoDe args
    | verbo == "soltar"               -> Soltar <$> objetoDe args
    | verbo == "examinar"             -> Examinar <$> objetoDe args
    | verbo == "guardar"              -> Guardar <$> archivo
    | verbo == "cargar"               -> Cargar <$> archivo
    | otherwise                       -> Nothing
  where
    -- Lo que sigue a la primera palabra en la línea original.
    archivo = case recortar (dropWhile isSpace (dropWhile (not . isSpace) (dropWhile isSpace linea))) of
      "" -> Nothing
      a  -> Just a

sinArgumento :: String -> Maybe Comando
sinArgumento verbo = case verbo of
  "mirar"      -> Just Mirar
  "inventario" -> Just Inventario
  "inv"        -> Just Inventario
  "ayuda"      -> Just Ayuda
  "salir"      -> Just Salir
  _            -> Ir <$> dirDe [verbo]

-- Una dirección escrita completa o abreviada, con artículo opcional.
dirDe :: [String] -> Maybe Direccion
dirDe args = case sinArticulos args of
  [d] -> lookup d abreviaturas `orElse` reconocerLinea direccion d
  _   -> Nothing
  where
    abreviaturas =
      [ ("n", Norte), ("s", Sur), ("e", Este), ("o", Oeste)
      , ("ar", Arriba), ("ab", Abajo) ]
    orElse (Just x) _ = Just x
    orElse Nothing  y = y

-- El objeto se nombra por su identificador; si viene en varias palabras se
-- unen con guiones bajos (memoria usb -> memoria_usb). Si el nombre no existe
-- en el mundo, el núcleo es quien lo dice.
objetoDe :: [String] -> Maybe IdObjeto
objetoDe args = case sinArticulos args of
  [] -> Nothing
  ps -> Just (IdObjeto (intercalate "_" ps))

-- Quita artículos iniciales, pero nunca la última palabra: así "tomar la"
-- sigue nombrando a un objeto que se llame "la".
sinArticulos :: [String] -> [String]
sinArticulos (p : ps@(_ : _))
  | p `elem` ["el", "la", "los", "las", "un", "una", "al", "a", "hacia"] = sinArticulos ps
sinArticulos ps = ps

recortar :: String -> String
recortar = dropWhileEnd isSpace . dropWhile isSpace
