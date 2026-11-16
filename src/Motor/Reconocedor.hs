module Motor.Reconocedor
  ( Reconocedor(..)
  , satisfy
  , caracter
  , cadena
  , munch
  , munch1
  , resto
  , espacios
  , lexema
  , reconocerLinea
    -- * Piezas del formato del mundo
  , espacios1
  , esCaracterId
  , identificador
  , palabra
  , entero
  , direccion
  , nombreDireccion
  , lineaClave
  , salidaCruda
  , metaCruda
  ) where
 
import Control.Applicative (Alternative(..), optional)
import Data.Char (digitToInt, isDigit, isSpace)

import Motor.Tipos (Direccion(..), MetaCruda(..))
 
-- recibe el texto pendiente y devuelve todas las lecturas posibles, 
-- cada una con el texto que sobra.
-- Una lista vacía significa fallo; varias entradas significan ambigüedad.
newtype Reconocedor a = Reconocedor { reconocer :: ReadS a }
 
instance Functor Reconocedor where
  fmap :: (a -> b) -> Reconocedor a -> Reconocedor b
  fmap f (Reconocedor p) = Reconocedor $ \s -> [ (f a, r) | (a, r) <- p s ]
 
instance Applicative Reconocedor where
  pure :: a -> Reconocedor a
  pure x = Reconocedor $ \s -> [(x, s)]
  (<*>) :: Reconocedor (a -> b) -> Reconocedor a -> Reconocedor b
  Reconocedor pf <*> Reconocedor pa =
    Reconocedor $ \s -> [ (f a, r2) | (f, r1) <- pf s, (a, r2) <- pa r1 ]
 
instance Monad Reconocedor where
  (>>=) :: Reconocedor a -> (a -> Reconocedor b) -> Reconocedor b
  Reconocedor p >>= f =
    Reconocedor $ \s -> concat [ reconocer (f a) r | (a, r) <- p s ]
 
instance Alternative Reconocedor where
  empty :: Reconocedor a
  empty = Reconocedor (const [])
  (<|>) :: Reconocedor a -> Reconocedor a -> Reconocedor a
  Reconocedor p <|> Reconocedor q = Reconocedor $ \s -> p s ++ q s
 
satisfy :: (Char -> Bool) -> Reconocedor Char
satisfy ok = Reconocedor $ \s -> case s of
  (c:cs) | ok c -> [(c, cs)]
  _             -> []

caracter :: Char -> Reconocedor Char
caracter = satisfy . (==)
 
cadena :: String -> Reconocedor String
cadena = traverse caracter
 
munch :: (Char -> Bool) -> Reconocedor String
munch ok = Reconocedor $ \s -> [span ok s]
 
munch1 :: (Char -> Bool) -> Reconocedor String
munch1 ok = do
  xs <- munch ok
  if null xs then empty else pure xs
  
resto :: Reconocedor String
resto = munch (const True)
 
espacios :: Reconocedor ()
espacios = () <$ munch (`elem` " \t")
 
lexema :: Reconocedor a -> Reconocedor a
lexema p = p <* espacios
 
-- Ejecuta un reconocedor exigiendo consumir la línea.
reconocerLinea :: Reconocedor a -> String -> Maybe a
reconocerLinea p s = case [ a | (a, "") <- reconocer p s ] of
  (a:_) -> Just a
  []    -> Nothing

-- ---------------------------------------------------------------------------
-- Piezas del formato del mundo. Cada una reconoce el contenido de una sola
-- línea (o de un valor); Motor.Mundo se encarga de numerar las líneas y de
-- cortar el archivo en bloques.

-- Al menos un espacio o tabulador.
espacios1 :: Reconocedor ()
espacios1 = () <$ munch1 (`elem` " \t")

-- Letras minúsculas (incluidas á é í ó ú ñ ü), dígitos y guion bajo.
-- isDigit solo acepta 0-9, a diferencia de isNumber.
esCaracterId :: Char -> Bool
esCaracterId c = (c >= 'a' && c <= 'z') || c `elem` "áéíóúñü" || isDigit c || c == '_'

identificador :: Reconocedor String
identificador = munch1 esCaracterId

-- Cualquier secuencia sin espacios. Se usa para reconocer la forma de SALIDA
-- y META antes de revisar cada pieza, y así dar un mensaje más preciso
-- ("dirección inexistente", "identificador inválido") que "mal formada".
palabra :: Reconocedor String
palabra = munch1 (not . isSpace)

-- Entero sin signo. Se devuelve como Integer para que el que llama pueda
-- revisar el rango sin desbordamientos; no se usa read.
entero :: Reconocedor Integer
entero = foldl (\acc c -> acc * 10 + toInteger (digitToInt c)) 0 <$> munch1 isDigit

nombreDireccion :: Direccion -> String
nombreDireccion d = case d of
  Norte  -> "norte"
  Sur    -> "sur"
  Este   -> "este"
  Oeste  -> "oeste"
  Arriba -> "arriba"
  Abajo  -> "abajo"

-- Una alternativa por cada dirección. Ningún nombre es prefijo de otro, así
-- que como mucho una alternativa consume la entrada completa.
direccion :: Reconocedor Direccion
direccion = foldr ((<|>) . opcion) empty [minBound .. maxBound]
  where
    opcion d = d <$ cadena (nombreDireccion d)

-- "CLAVE: valor". La clave son mayúsculas ASCII; el valor es el resto de la
-- línea (puede contener ':' y está vacío si no se escribió nada).
lineaClave :: Reconocedor (String, String)
lineaClave = (,) <$> munch1 esMayuscula <* espacios <* caracter ':' <* espacios <*> resto
  where
    esMayuscula c = c >= 'A' && c <= 'Z'

-- "<dirección> -> <sala>" o "<dirección> -> <sala> requiere <objeto>".
-- Devuelve las tres piezas como texto; Motor.Mundo revisa que la dirección
-- exista y que los identificadores sean válidos.
-- `optional` produce las dos lecturas (con y sin requisito); reconocerLinea
-- se queda con la que consume la línea completa.
salidaCruda :: Reconocedor (String, String, Maybe String)
salidaCruda = do
  d <- munch1 (\c -> not (isSpace c) && c /= '-')
  espacios
  _ <- cadena "->"
  espacios
  destino <- palabra
  requisito <- optional (espacios1 *> cadena "requiere" *> espacios1 *> palabra)
  espacios
  pure (d, destino, requisito)

-- "llegar a <sala>" o "llevar <objeto> a <sala>", con las piezas sin revisar.
metaCruda :: Reconocedor MetaCruda
metaCruda = llegar <|> llevar
  where
    llegar = LlegarACruda <$> (cadena "llegar" *> espacios1 *> cadena "a" *> espacios1 *> palabra)
    llevar = LlevarACruda
      <$> (cadena "llevar" *> espacios1 *> palabra)
      <*> (espacios1 *> cadena "a" *> espacios1 *> palabra)
