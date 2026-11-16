module Motor.Mundo
  ( leerMundo
  , reconocerMundo
  , validarMundo
  , mostrarError
  ) where

import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.List (dropWhileEnd, intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Motor.Reconocedor
import Motor.Tipos

-- ---------------------------------------------------------------------------
-- Punto de entrada

-- Reconoce y valida. Aquí sí se encadena con >>= a propósito: si hay errores
-- de sintaxis (V1) no se intenta validar lo demás, porque el mundo crudo
-- estaría incompleto y se reportarían errores falsos.
leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo txt = reconocerMundo txt >>= validarMundo

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

-- ---------------------------------------------------------------------------
-- Validación (V2-V10 y variante de peso)
--
-- Cada revisión es independiente y produce una lista de errores (vacía si
-- todo está bien); el resultado es la concatenación de todas. Así se reportan
-- todos los errores juntos, no solo el primero.

validarMundo :: MundoCrudo -> Either [ErrorMundo] Mundo
validarMundo mc
  | null errores = Right (construir mc)
  | otherwise    = Left errores
  where
    errores = concat [v2, v3, v4, v5, v6, v7, v8, v9, v10, vPeso]

    cab      = mcCabecera mc
    salas    = mcSalas mc
    objetos  = mcObjetos mc
    idsSala  = Set.fromList (map scId salas)
    idsObj   = Set.fromList (map ocId objetos)
    (lIni, ini)   = hcInicio cab
    (lMeta, meta) = hcMeta cab

    -- V2: identificadores repetidos (se reporta cada aparición después de la primera).
    v2 = [ ErrIdSalaRepetido n (IdSala i) | (n, i) <- repetidos [ (scLinea x, scId x) | x <- salas ] ]
      ++ [ ErrIdObjetoRepetido n (IdObjeto i) | (n, i) <- repetidos [ (ocLinea x, ocId x) | x <- objetos ] ]

    -- V3: salidas hacia salas que no existen.
    v3 = [ ErrSalidaASalaInexistente n (IdSala (scId x)) (IdSala d) dir
         | x <- salas, (n, dir, d, _) <- scSalidas x, d `Set.notMember` idsSala ]

    -- V4: salidas que requieren objetos que no existen.
    v4 = [ ErrRequisitoInexistente n (IdSala (scId x)) (IdObjeto r)
         | x <- salas, (n, _, _, Just r) <- scSalidas x, r `Set.notMember` idsObj ]

    -- V5: dos salidas en la misma dirección dentro de una sala.
    v5 = [ ErrDireccionDuplicada n (IdSala (scId x)) dir
         | x <- salas, (n, dir) <- repetidos [ (m, d) | (m, d, _, _) <- scSalidas x ] ]

    -- V6: objetos colocados que no están definidos.
    v6 = [ ErrObjetoNoDefinidoEnSala n (IdSala (scId x)) (IdObjeto o)
         | x <- salas, (n, o) <- scObjetos x, o `Set.notMember` idsObj ]

    -- V7: un objeto colocado más de una vez (en salas distintas o en la misma).
    -- Solo se cuentan los objetos definidos; los otros ya dieron V6.
    colocaciones = [ (n, o) | x <- salas, (n, o) <- scObjetos x, o `Set.member` idsObj ]
    colocados    = Set.fromList (map snd colocaciones)
    v7 = [ ErrObjetoEnVariasSalas n (IdObjeto o) | (n, o) <- repetidos colocaciones ]

    -- V8: INICIO o META con salas u objetos inexistentes.
    v8 = [ ErrInicioInexistente lIni (IdSala ini) | ini `Set.notMember` idsSala ]
      ++ case meta of
           LlegarACruda d   -> salaMeta d
           LlevarACruda o d -> [ ErrMetaObjetoInexistente lMeta (IdObjeto o) | o `Set.notMember` idsObj ]
                               ++ salaMeta d
    salaMeta d = [ ErrMetaSalaInexistente lMeta (IdSala d) | d `Set.notMember` idsSala ]

    -- V9: salas no alcanzables desde INICIO ignorando requisitos. Si INICIO
    -- no existe ya se reportó en V8 y no se marcan todas las salas.
    grafo :: Map String [String]
    grafo = Map.fromListWith (++) [ (scId x, [ d | (_, _, d, _) <- scSalidas x ]) | x <- salas ]
    alcanzables = alcanzar grafo ini
    v9 | ini `Set.member` idsSala =
           [ ErrSalaInalcanzable (IdSala i) | i <- unicos (map scId salas), i `Set.notMember` alcanzables ]
       | otherwise = []

    -- V10: objetos necesarios (meta o requisitos) que no están colocados en
    -- ninguna sala. Los que ni siquiera están definidos ya dieron V4 u V8.
    necesarios = unicos ([ o | LlevarACruda o _ <- [meta] ]
                         ++ [ r | x <- salas, (_, _, _, Just r) <- scSalidas x ])
    v10 = [ ErrObjetoInconseguible (IdObjeto o)
          | o <- necesarios, o `Set.member` idsObj, o `Set.notMember` colocados ]

    -- Variante 1 (peso).
    pesos :: Map String Int
    pesos  = Map.fromList [ (ocId x, fromMaybe 1 (ocPeso x)) | x <- reverse objetos ]
    peso o = Map.findWithDefault 1 o pesos

    vPeso = case snd <$> hcCapacidad cab of
      Nothing  -> []
      Just cap ->
        [ ErrPesoExcedeCapacidad (ocLinea x) (IdObjeto (ocId x)) p cap
        | x <- objetos, let p = fromMaybe 1 (ocPeso x), p > cap ]
        ++ entregaPosible cap

    -- Comprobación de "llevar a la vez": para cumplir "llevar o a d" hay que
    -- entrar a d con o en el inventario, y por lo tanto también con el
    -- requisito de la salida por la que se entra. Si ninguna entrada a d
    -- permite llevar ambos sin pasar la capacidad, el mundo no se puede
    -- ganar. Es una comprobación segura (nunca rechaza un mundo ganable),
    -- aunque no detecta todos los casos imposibles.
    entregaPosible cap = case meta of
      LlevarACruda o d
        | o `Set.member` idsObj, d `Set.member` idsSala
        , peso o <= cap
        , not (colocadoEn o d)
        , not (null opciones) ->
            -- minimum no falla: la guarda de arriba exige opciones no vacía.
            let (total, objs) = minimum opciones
            in [ ErrObjetosNoCabenJuntos (map IdObjeto objs) total cap | total > cap ]
        where
          opciones =
            [ (sum (map peso objs), objs)
            | x <- salas, (_, _, destino, r) <- scSalidas x, destino == d
            , let objs = unicos (o : [ q | Just q <- [r], q `Set.member` idsObj ]) ]
      _ -> []

    colocadoEn o d = or [ o == q | x <- salas, scId x == d, (_, q) <- scObjetos x ]

-- Las apariciones de cada clave después de la primera, en orden.
repetidos :: Ord k => [(Int, k)] -> [(Int, k)]
repetidos = go Set.empty
  where
    go _ [] = []
    go vistos ((n, k) : r)
      | k `Set.member` vistos = (n, k) : go vistos r
      | otherwise             = go (Set.insert k vistos) r

-- Quita duplicados conservando la primera aparición.
unicos :: Ord a => [a] -> [a]
unicos = go Set.empty
  where
    go _ [] = []
    go vistos (x : r)
      | x `Set.member` vistos = go vistos r
      | otherwise             = x : go (Set.insert x vistos) r

-- Recorrido en profundidad desde una sala.
alcanzar :: Map String [String] -> String -> Set String
alcanzar grafo desde = go Set.empty [desde]
  where
    go vistos [] = vistos
    go vistos (x : pendientes)
      | x `Set.member` vistos = go vistos pendientes
      | otherwise = go (Set.insert x vistos) (Map.findWithDefault [] x grafo ++ pendientes)

-- Solo se llama cuando el mundo crudo ya pasó todas las validaciones, así que
-- los identificadores son únicos y todas las referencias existen.
construir :: MundoCrudo -> Mundo
construir mc = Mundo
  { mundoTitulo    = hcTitulo cab
  , mundoInicio    = IdSala (snd (hcInicio cab))
  , mundoMeta      = case snd (hcMeta cab) of
      LlegarACruda d   -> LlegarA (IdSala d)
      LlevarACruda o d -> LlevarA (IdObjeto o) (IdSala d)
  , mundoCapacidad = snd <$> hcCapacidad cab
  , mundoObjetos   = Map.fromList [ (objId o, o) | o <- map convertirObjeto (mcObjetos mc) ]
  , mundoSalas     = Map.fromList [ (salaId s, s) | s <- map convertirSala (mcSalas mc) ]
  }
  where
    cab = mcCabecera mc

convertirObjeto :: ObjetoCrudo -> Objeto
convertirObjeto o = Objeto
  { objId   = IdObjeto (ocId o)
  , objDesc = ocDesc o
  , objPeso = fromMaybe 1 (ocPeso o)   -- si el archivo no da PESO, pesa 1
  }

convertirSala :: SalaCruda -> Sala
convertirSala s = Sala
  { salaId      = IdSala (scId s)
  , salaDesc    = scDesc s
  , salaSalidas = Map.fromList [ (d, Salida (IdSala dst) (IdObjeto <$> r)) | (_, d, dst, r) <- scSalidas s ]
  , salaObjetos = map (IdObjeto . snd) (scObjetos s)
  }

-- ---------------------------------------------------------------------------
-- Mensajes

mostrarError :: ErrorMundo -> String
mostrarError e = "Error: " ++ case e of
  ErrSintaxis n m ->
    enLinea n m
  ErrIdSalaRepetido n s ->
    enLinea n ("la sala " ++ nSala s ++ " ya estaba definida")
  ErrIdObjetoRepetido n o ->
    enLinea n ("el objeto " ++ nObj o ++ " ya estaba definido")
  ErrSalidaASalaInexistente n s d dir ->
    enLinea n ("la salida " ++ nombreDireccion dir ++ " de " ++ nSala s ++ " lleva a la sala inexistente " ++ nSala d)
  ErrRequisitoInexistente n s o ->
    enLinea n ("una salida de " ++ nSala s ++ " requiere el objeto inexistente " ++ nObj o)
  ErrDireccionDuplicada n s dir ->
    enLinea n ("la sala " ++ nSala s ++ " tiene más de una salida en dirección " ++ nombreDireccion dir)
  ErrObjetoNoDefinidoEnSala n s o ->
    enLinea n ("la sala " ++ nSala s ++ " contiene el objeto no definido " ++ nObj o)
  ErrObjetoEnVariasSalas n o ->
    enLinea n ("el objeto " ++ nObj o ++ " ya estaba colocado en una sala")
  ErrInicioInexistente n s ->
    enLinea n ("INICIO nombra la sala inexistente " ++ nSala s)
  ErrMetaSalaInexistente n s ->
    enLinea n ("META nombra la sala inexistente " ++ nSala s)
  ErrMetaObjetoInexistente n o ->
    enLinea n ("META nombra el objeto inexistente " ++ nObj o)
  ErrSalaInalcanzable s ->
    "la sala " ++ nSala s ++ " no es alcanzable desde INICIO"
  ErrObjetoInconseguible o ->
    "el objeto " ++ nObj o ++ " hace falta pero no está colocado en ninguna sala"
  ErrPesoExcedeCapacidad n o p c ->
    enLinea n ("el objeto " ++ nObj o ++ " pesa " ++ show p ++ ", más que la CAPACIDAD (" ++ show c ++ ")")
  ErrObjetosNoCabenJuntos os t c ->
    "para cumplir la META hay que llevar a la vez " ++ intercalate ", " (map nObj os)
      ++ " (peso " ++ show t ++ "), más que la CAPACIDAD (" ++ show c ++ ")"
  where
    enLinea n m = "línea " ++ show n ++ ": " ++ m
    nSala (IdSala s) = s
    nObj (IdObjeto o) = o
