module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

-- Por ahora solo hay un caso de humo para que la test-suite corra.
-- Los casos reales se agregan a medida que se implementan los módulos.
pruebas :: Test
pruebas = TestList
  [ "la suite corre" ~: True @?= True
  ]

main :: IO ()
main = do
  resultado <- runTestTT pruebas
  if errors resultado + failures resultado == 0
    then exitSuccess
    else exitFailure
