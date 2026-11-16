module Motor.Mundo
  ( reconocerMundo
  ) where

import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.List (dropWhileEnd)

import Motor.Reconocedor
import Motor.Tipos

-- ---------------------------------------------------------------------------
-- Acumulación de errores
--
-- Either con do se detiene en el primer Left. Para reportar todos los errores
-- de sintaxis a la vez usamos un Applicative propio: cuando los dos lados
-- fallan, se juntan las dos listas de errores.

newtype Validacion a = Validacion { aEither :: Either [ErrorMundo] a }

instance Functor Validacion where
  fmap f (Validacion e) = Validacion (fmap f e)

instance Applicative Validacion where
  pure = Validacion . Right
  Validacion (Left e1) <*> Validacion (Left e2) = Validacion (Left (e1 ++ e2))
  Validacion (Left e1) <*> Validacion (Right _) = Validacion (Left e1)
  Validacion (Right f) <*> Validacion r         = Validacion (fmap f r)

falla :: Int -> String -> Validacion a
falla n msg = Validacion (Left [ErrSintaxis n msg])

conErrores :: [ErrorMundo] -> Validacion ()
conErrores [] = pure ()
conErrores es = Validacion (Left es)

-- Paso secuencial: solo se revisa el valor si el campo se pudo obtener
-- (por ejemplo, no tiene sentido revisar el identificador de un INICIO que
-- falta).
luego :: Validacion a -> (a -> Validacion b) -> Validacion b
luego v f = Validacion (aEither v >>= aEither . f)

-- ---------------------------------------------------------------------------
-- Líneas y bloques

type Linea = (Int, String)          -- número de línea, contenido recortado
type Campo = (Int, String, String)  -- número de línea, clave, valor

recortar :: String -> String
recortar = dropWhileEnd isSpace . dropWhile isSpace

-- Numera las líneas antes de filtrar, para que los números sean los del
-- archivo. Se quita un BOM de UTF-8 al inicio si lo hay; recortar también
-- quita el '\r' de los archivos con fin de línea de Windows.
lineasUtiles :: String -> [Linea]
lineasUtiles = filter util . zip [1 ..] . map recortar . lines . quitarBOM
  where
    util (_, l) = not (null l) && take 1 l /= "#"
    quitarBOM ('\xFEFF' : r) = r
    quitarBOM r              = r

esSeparador :: String -> Bool
esSeparador l = not (null l) && all (== '-') l

-- Corta en bloques por las líneas separadoras. Cada bloque se da como su
-- primera línea y el resto, así no hay bloques vacíos por construcción
-- (dos separadores seguidos o uno al final no producen nada).
bloques :: [Linea] -> [(Linea, [Linea])]
bloques ls = case break (esSeparador . snd) ls of
  (b, sep) -> conCabeza b ++ case sep of
    []      -> []
    (_ : r) -> bloques r
  where
    conCabeza []       = []
    conCabeza (x : xs) = [(x, xs)]

-- Reconoce cada línea como CLAVE: valor. Las que no tienen esa forma se
-- reportan y se descartan, para seguir revisando el resto del bloque.
camposDe :: [Linea] -> ([ErrorMundo], [Campo])
camposDe = partitionEithers . map uno
  where
    uno (n, l) = case reconocerLinea lineaClave l of
      Just (k, v) -> Right (n, k, recortar v)
      Nothing     -> Left (ErrSintaxis n ("la línea no tiene la forma CLAVE: valor: \"" ++ l ++ "\""))

conClave :: String -> [Campo] -> [(Int, String)]
conClave k cs = [ (n, v) | (n, k', v) <- cs, k' == k ]

clavesConocidas :: String -> [String] -> [Campo] -> Validacion ()
clavesConocidas donde permitidas = traverse_ revisar
  where
    revisar (n, k, _)
      | k `elem` permitidas = pure ()
      | otherwise           = falla n ("clave desconocida " ++ k ++ " en " ++ donde)

repetidosDe :: String -> [(Int, String)] -> Validacion a
repetidosDe k reps = Validacion (Left [ ErrSintaxis n ("campo " ++ k ++ " repetido") | (n, _) <- reps ])

-- Campo que debe aparecer exactamente una vez en el bloque que empieza en ini.
unico :: Int -> String -> [Campo] -> Validacion (Int, String)
unico ini k cs = case conClave k cs of
  [x]      -> pure x
  []       -> falla ini ("falta el campo " ++ k)
  (_ : rs) -> repetidosDe k rs

-- Campo que puede aparecer a lo sumo una vez.
aLoSumoUno :: String -> [Campo] -> Validacion (Maybe (Int, String))
aLoSumoUno k cs = case conClave k cs of
  []       -> pure Nothing
  [x]      -> pure (Just x)
  (_ : rs) -> repetidosDe k rs

conLinea :: (Int -> String -> Validacion a) -> (Int, String) -> Validacion (Int, a)
conLinea f (n, v) = (,) n <$> f n v

-- ---------------------------------------------------------------------------
-- Valores

textoLibre :: String -> Int -> String -> Validacion String
textoLibre k n v
  | null v    = falla n ("el campo " ++ k ++ " está vacío")
  | otherwise = pure v

ident :: String -> Int -> String -> Validacion String
ident que n v = case reconocerLinea identificador v of
  Just i  -> pure i
  Nothing -> falla n ("identificador inválido en " ++ que ++ ": \"" ++ v ++ "\"")

enteroPositivo :: String -> Int -> String -> Validacion Int
enteroPositivo k n v = case reconocerLinea entero v of
  Just x | x >= 1 && x <= toInteger (maxBound :: Int) -> pure (fromInteger x)
  _ -> falla n (k ++ " debe ser un entero positivo: \"" ++ v ++ "\"")

metaDe :: Int -> String -> Validacion MetaCruda
metaDe n v = case reconocerLinea metaCruda v of
  Nothing -> falla n ("META mal formada (se espera \"llegar a <sala>\" o \"llevar <objeto> a <sala>\"): \"" ++ v ++ "\"")
  Just (LlegarACruda s)   -> LlegarACruda <$> ident "la sala de META" n s
  Just (LlevarACruda o s) -> LlevarACruda <$> ident "el objeto de META" n o <*> ident "la sala de META" n s

salidaDe :: Int -> String -> Validacion (Int, Direccion, String, Maybe String)
salidaDe n v = case reconocerLinea salidaCruda v of
  Nothing -> falla n ("SALIDA mal formada (se espera \"<dirección> -> <sala>\" o \"<dirección> -> <sala> requiere <objeto>\"): \"" ++ v ++ "\"")
  Just (d, s, r) ->
    (,,,) n
      <$> dir d
      <*> ident "el destino de SALIDA" n s
      <*> traverse (ident "el requisito de SALIDA" n) r
  where
    dir d = maybe (falla n ("dirección inexistente: \"" ++ d ++ "\"")) pure (reconocerLinea direccion d)

-- ---------------------------------------------------------------------------
-- Bloques

cabecera :: (Linea, [Linea]) -> Validacion CabeceraCruda
cabecera (primera@(ini, _), ls) =
  conErrores malas
    *> clavesConocidas "la cabecera" ["MUNDO", "INICIO", "META", "CAPACIDAD"] cs
    *> ( CabeceraCruda
           <$> (unico ini "MUNDO" cs `luego` uncurry (textoLibre "MUNDO"))
           <*> (unico ini "INICIO" cs `luego` conLinea (ident "INICIO"))
           <*> (unico ini "META" cs `luego` conLinea metaDe)
           <*> (aLoSumoUno "CAPACIDAD" cs `luego` traverse (conLinea (enteroPositivo "CAPACIDAD")))
       )
  where
    (malas, cs) = camposDe (primera : ls)

-- La primera línea decide si es un bloque OBJETO o SALA.
bloque :: (Linea, [Linea]) -> Validacion (Either ObjetoCrudo SalaCruda)
bloque (primera@(ini, _), ls) = conErrores malas *> segun tipo
  where
    (malas, cs) = camposDe (primera : ls)
    tipo = case cs of
      ((n, k, _) : _) | n == ini -> Just k
      _                          -> Nothing
    segun (Just "OBJETO") = Left <$> bloqueObjeto ini cs
    segun (Just "SALA")   = Right <$> bloqueSala ini cs
    segun (Just k)        = falla ini ("el bloque debe empezar con OBJETO o SALA, no con " ++ k)
    -- La primera línea no tenía forma CLAVE: valor, así que ya está en
    -- `malas` (que no es vacía); no se agrega otro error para la misma línea.
    segun Nothing         = Validacion (Left [])

bloqueObjeto :: Int -> [Campo] -> Validacion ObjetoCrudo
bloqueObjeto ini cs =
  clavesConocidas "un bloque OBJETO" ["OBJETO", "DESC", "PESO"] cs
    *> ( ObjetoCrudo ini
           <$> (unico ini "OBJETO" cs `luego` uncurry (ident "OBJETO"))
           <*> (unico ini "DESC" cs `luego` uncurry (textoLibre "DESC"))
           <*> (aLoSumoUno "PESO" cs `luego` traverse (uncurry (enteroPositivo "PESO")))
       )

bloqueSala :: Int -> [Campo] -> Validacion SalaCruda
bloqueSala ini cs =
  clavesConocidas "un bloque SALA" ["SALA", "DESC", "SALIDA", "OBJETO"] cs
    *> ( SalaCruda ini
           <$> (unico ini "SALA" cs `luego` uncurry (ident "SALA"))
           <*> (unico ini "DESC" cs `luego` uncurry (textoLibre "DESC"))
           <*> traverse (uncurry salidaDe) (conClave "SALIDA" cs)
           <*> traverse (conLinea (ident "OBJETO")) (conClave "OBJETO" cs)
       )

-- ---------------------------------------------------------------------------
-- Archivo completo

-- Reconoce el texto del archivo y devuelve el mundo crudo (sin validar las
-- referencias entre salas y objetos) o todos los errores de sintaxis (V1).
reconocerMundo :: String -> Either [ErrorMundo] MundoCrudo
reconocerMundo txt = case bloques (lineasUtiles txt) of
  [] -> Left [ErrSintaxis 1 "el archivo está vacío (faltan MUNDO, INICIO y META)"]
  (cab : demas) -> aEither (armar <$> cabecera cab <*> traverse bloque demas)
  where
    armar c bs = let (os, ss) = partitionEithers bs in MundoCrudo c os ss
