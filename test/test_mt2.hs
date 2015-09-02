module Main where

import           HEP.Kinematics.Vector.LorentzVector (setXYZT)
import           HEP.Kinematics.Vector.TwoVector     (setXY)

import           HEP.Kinematics.Variable.MT2

main :: IO ()
main = do
  let visA = setXYZT 410.0 20.0 0.0 422.493
      visB = setXYZT (-210.0) (-300.0) 0.0 395.727
      ptmiss = setXY (-200.0) 280.0
      mInvisible = 100.0

  print $ mT2Symm visA visB ptmiss mInvisible
  print $ mT2AsymmBisect visA visB ptmiss mInvisible mInvisible
