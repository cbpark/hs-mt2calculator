{- Copyright (c) 2014-2015, 2017 Chan Beom Park <cbpark@gmail.com>

   This file is part of hs-mt2calculator, which is released under the GNU
   General Public License. See file LICENSE in the top directory of this
   project or go to <http://www.gnu.org/licenses/> for full license details.
-}

module Main where

import HEP.Kinematics.Variable.MT2

import HEP.Kinematics.Vector.LorentzVector (setXYZT)
import HEP.Kinematics.Vector.TwoVector     (setXY)

main :: IO ()
main = do
    let visA = setXYZT 410.0 20.0 0.0 422.493
        visB = setXYZT (-210.0) (-300.0) 0.0 395.727
        ptmiss = setXY (-200.0) 280.0
        mInvisible = 100.0

    print $ mT2SymmMinuit2 visA visB ptmiss mInvisible
    print $ mT2SymmChengHanBisect visA visB ptmiss mInvisible
    print $ mT2SymmLesterBisect visA visB ptmiss mInvisible
    print $ mT2AsymmLesterBisect visA visB ptmiss mInvisible mInvisible
