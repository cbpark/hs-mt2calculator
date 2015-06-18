module Main where

import           HEP.Kinematics.Variable.MT2

main :: IO ()
main = do
  let visA = [423,410,20,-20]
      visB = [398,-210,-300,44]
      ptmiss = [-200,280]
      mInvisible = 100

  print $ symmMT2 visA visB ptmiss mInvisible
