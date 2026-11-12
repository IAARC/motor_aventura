module Motor.Tipos where

import Data.Map (Map)

-- Se colocan newtype para que no se confundan los Ids de salas y objetos
newtype IdSala   = IdSala String   deriving (Eq, Ord, Show, Read)
newtype IdObjeto = IdObjeto String deriving (Eq, Ord, Show, Read)

data Direccion = Norte | Sur | Este | Oeste | Arriba | Abajo
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

 -- Se agrega el peso como atributo variante por la suma de nuestro carnets
data Objeto = Objeto
  { objId   :: IdObjeto
  , objDesc :: String
  , objPeso :: Int
  } deriving (Eq, Show)

data Salida = Salida
  { salDestino  :: IdSala
  , salRequiere :: Maybe IdObjeto
  } deriving (Eq, Show)

data Sala = Sala
  { salaId      :: IdSala
  , salaDesc    :: String
  , salaSalidas :: Map Direccion Salida
  , salaObjetos :: [IdObjeto]       -- objetos que hay al empezar
  } deriving (Eq, Show)

-- Se agregan el tipo de datos crudos para
-- poder verificar en primera instacia los errores tipo v1 de sintaxis, y si es valido para
-- luego transformarlo a los tipos de datos finales donde se pasan el resto de verificaciones

data SalaCruda = SalaCruda
  { scLinea   :: Int
  , scId      :: String
  , scDesc    :: String
  , scSalidas :: [(Int, Direccion, String, Maybe String)]  -- línea, dir, destino, requiere
  , scObjetos :: [(Int, String)]                           -- línea, id de objeto
  } deriving (Eq, Show)

data ObjetoCrudo = ObjetoCrudo
  { ocLinea :: Int
  , ocId    :: String
  , ocDesc  :: String
  , ocPeso  :: Maybe Int   -- Nothing = el archivo no lo dijo
  } deriving (Eq, Show)

data MetaCruda
  = LlegarACruda String               
  | LlevarACruda String String         
  deriving (Eq, Show)

-- Cada campo lleva su línea para poder reportar errores.
data CabeceraCruda = CabeceraCruda
  { hcTitulo    :: String
  , hcInicio    :: (Int, String)
  , hcMeta      :: (Int, MetaCruda)
  , hcCapacidad :: (Int, Int)          
  } deriving (Eq, Show)

data MundoCrudo = MundoCrudo
  { mcCabecera :: CabeceraCruda
  , mcObjetos  :: [ObjetoCrudo]
  , mcSalas    :: [SalaCruda]
  } deriving (Eq, Show)

data Meta
  = LlegarA IdSala
  | LlevarA IdObjeto IdSala
  deriving (Eq, Show)

data Mundo = Mundo
  { mundoTitulo    :: String
  , mundoInicio    :: IdSala
  , mundoMeta      :: Meta
  , mundoCapacidad :: Int
  , mundoObjetos   :: Map IdObjeto Objeto
  , mundoSalas     :: Map IdSala Sala
  } deriving (Eq, Show)