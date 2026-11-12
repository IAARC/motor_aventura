module Reconocedor
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
  ) where
 
import Control.Applicative (Alternative(..))
 
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
 