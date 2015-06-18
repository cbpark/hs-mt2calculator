module Main where

import           HEP.Kinematics.Variable.MT2

main :: IO ()
main = do
  let visA = [100, 410, 20]
      visB = [150, -210, -300]
      ptmiss = [-200,280]
      mInvisible = 100

  print $ mT2AsymmBisect visA visB ptmiss mInvisible mInvisible
