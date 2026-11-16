module Motor.Nucleo
  ( estadoInicial
  , ejecutar
  , bienvenida
  , metaCumplida
  , inventario
  , pesoInventario
  ) where

import Data.List (intercalate)
import qualified Data.Map as Map

import Motor.Reconocedor (nombreDireccion)
import Motor.Tipos

-- Todo este módulo es puro: recibe el estado y devuelve el estado nuevo con
-- los mensajes para el jugador. Main se encarga de mostrarlos.

estadoInicial :: Mundo -> Estado
estadoInicial m = Estado
  { estSala      = mundoInicio m
  , estUbicacion = Map.fromList
      [ (o, EnSala (salaId s)) | s <- Map.elems (mundoSalas m), o <- salaObjetos s ]
  , estMundo     = m
  }

-- Lo que se muestra al arrancar: el título y la sala inicial.
bienvenida :: Estado -> [String]
bienvenida e = ("*** " ++ mundoTitulo (estMundo e) ++ " ***") : "" : describirSala e

-- Guardar, Cargar y Salir tocan archivos o terminan el programa, así que los
-- atiende Main; aquí no cambian el estado ni dan mensajes.
ejecutar :: Comando -> Estado -> ([String], Estado)
ejecutar cmd e = case cmd of
  Ir d         -> ir d e
  Mirar        -> (describirSala e, e)
  Tomar o      -> tomar o e
  Soltar o     -> soltar o e
  Examinar o   -> (examinar o e, e)
  Inventario   -> (mostrarInventario e, e)
  Ayuda        -> (ayuda, e)
  Guardar _    -> ([], e)
  Cargar _     -> ([], e)
  Salir        -> ([], e)

-- "llegar a s": estar en s. "llevar o a s": estar en s con o en el inventario.
metaCumplida :: Estado -> Bool
metaCumplida e = case mundoMeta (estMundo e) of
  LlegarA s   -> estSala e == s
  LlevarA o s -> estSala e == s && lleva o e

-- ---------------------------------------------------------------------------
-- Consultas sobre el estado

salaActual :: Estado -> Maybe Sala
salaActual e = Map.lookup (estSala e) (mundoSalas (estMundo e))

lleva :: IdObjeto -> Estado -> Bool
lleva o e = Map.lookup o (estUbicacion e) == Just EnInventario

estaAqui :: IdObjeto -> Estado -> Bool
estaAqui o e = Map.lookup o (estUbicacion e) == Just (EnSala (estSala e))

inventario :: Estado -> [IdObjeto]
inventario e = [ o | (o, EnInventario) <- Map.toList (estUbicacion e) ]

objetosAqui :: Estado -> [IdObjeto]
objetosAqui e = [ o | (o, u) <- Map.toList (estUbicacion e), u == EnSala (estSala e) ]

peso :: Estado -> IdObjeto -> Int
peso e o = maybe 1 objPeso (Map.lookup o (mundoObjetos (estMundo e)))

pesoInventario :: Estado -> Int
pesoInventario e = sum (map (peso e) (inventario e))

-- ---------------------------------------------------------------------------
-- Comandos

ir :: Direccion -> Estado -> ([String], Estado)
ir d e = case salaActual e >>= Map.lookup d . salaSalidas of
  Nothing -> (["No hay salida hacia " ++ hacia d ++ "."], e)
  Just (Salida destino requisito) -> case requisito of
    Just o | not (lleva o e) ->
      (["La salida hacia " ++ hacia d ++ " está bloqueada. Necesitas " ++ nombre o ++ "."], e)
    _ ->
      let e' = e { estSala = destino }
      in (describirSala e', e')

tomar :: IdObjeto -> Estado -> ([String], Estado)
tomar o e
  | lleva o e         = (["Ya llevas " ++ nombre o ++ "."], e)
  | not (estaAqui o e) = (["No hay " ++ nombre o ++ " aquí."], e)
  | otherwise = case mundoCapacidad (estMundo e) of
      Just cap | pesoInventario e + peso e o > cap ->
        ( [ "No puedes con " ++ nombre o ++ ": pesa " ++ show (peso e o)
            ++ " y ya llevas " ++ show (pesoInventario e) ++ " de " ++ show cap ++ "." ]
        , e )
      _ ->
        ( ["Tomaste " ++ nombre o ++ "."]
        , e { estUbicacion = Map.insert o EnInventario (estUbicacion e) } )

-- El objeto soltado queda en la sala actual (y se puede volver a tomar).
soltar :: IdObjeto -> Estado -> ([String], Estado)
soltar o e
  | lleva o e =
      ( ["Soltaste " ++ nombre o ++ "."]
      , e { estUbicacion = Map.insert o (EnSala (estSala e)) (estUbicacion e) } )
  | otherwise = (["No llevas " ++ nombre o ++ "."], e)

examinar :: IdObjeto -> Estado -> [String]
examinar o e
  | lleva o e || estaAqui o e =
      [maybe "No tiene nada de especial." objDesc (Map.lookup o (mundoObjetos (estMundo e)))]
  | otherwise = ["No ves " ++ nombre o ++ " por aquí."]

mostrarInventario :: Estado -> [String]
mostrarInventario e = case inventario e of
  [] -> ["No llevas nada."]
  os -> ("Llevas: " ++ lista os ++ ".") : cargaActual
  where
    cargaActual = case mundoCapacidad (estMundo e) of
      Just cap -> ["Peso: " ++ show (pesoInventario e) ++ " de " ++ show cap ++ "."]
      Nothing  -> []

describirSala :: Estado -> [String]
describirSala e = case salaActual e of
  -- No ocurre con un mundo validado (INICIO y los destinos existen), pero
  -- se responde algo en lugar de usar una función parcial.
  Nothing -> ["Estás en un lugar que no existe."]
  Just s ->
    [ "[" ++ nombreSala (salaId s) ++ "]"
    , salaDesc s
    ]
    ++ [ "Ves aquí: " ++ lista os ++ "." | let os = objetosAqui e, not (null os) ]
    ++ [ salidas (Map.toList (salaSalidas s)) ]
  where
    salidas [] = "No hay salidas."
    salidas ss = "Salidas: " ++ intercalate ", " (map salida ss) ++ "."
    salida (d, Salida _ req) = nombreDireccion d ++ case req of
      Just o | not (lleva o e) -> " (requiere " ++ nombre o ++ ")"
      _                        -> ""

ayuda :: [String]
ayuda =
  [ "Comandos:"
  , "  ir <dirección>      (o solo la dirección: norte, sur, este, oeste, arriba, abajo; n, s, e, o, ar, ab)"
  , "  mirar               describe la sala"
  , "  tomar <objeto>      (o coger)"
  , "  soltar <objeto>"
  , "  examinar <objeto>"
  , "  inventario          (o inv)"
  , "  guardar <archivo>"
  , "  cargar <archivo>"
  , "  ayuda"
  , "  salir"
  ]

-- ---------------------------------------------------------------------------
-- Texto

hacia :: Direccion -> String
hacia d = case d of
  Arriba -> "arriba"
  Abajo  -> "abajo"
  _      -> "el " ++ nombreDireccion d

nombre :: IdObjeto -> String
nombre (IdObjeto o) = o

nombreSala :: IdSala -> String
nombreSala (IdSala s) = s

lista :: [IdObjeto] -> String
lista = intercalate ", " . map nombre
