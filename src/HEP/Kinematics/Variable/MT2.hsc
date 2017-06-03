{- Copyright (c) 2014-2015, 2017 Chan Beom Park <cbpark@gmail.com>

   This file is part of hs-mt2calculator, which is released under the GNU
   General Public License. See file LICENSE in the top directory of this
   project or go to <http://www.gnu.org/licenses/> for full license details.
-}

{-# LANGUAGE ForeignFunctionInterface #-}

module HEP.Kinematics.Variable.MT2
    (
      mT2SymmMinuit2
    , mT2SymmChengHanBisect
    , mT2SymmLesterBisect
    , mT2AsymmLesterBisect
    ) where

import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

import HEP.Kinematics   (FourMomentum, HasFourMomentum (..), TransverseMomentum)

#include "mt2_cwrapper/symm_mt2_minuit2_c.h"
#include "mt2_cwrapper/mt2_bisect_c.h"
#include "mt2_cwrapper/lester_mt2_bisect_c.h"

data MT2Value = MT2Value { mT2 :: CDouble
                         , kx  :: CDouble
                         , ky  :: CDouble
                         }

instance Storable MT2Value where
    sizeOf    _ = (#size mt2calc_result)

    alignment _ = alignment (undefined :: CDouble)

    peek ptr = do
      mt2value <- (#peek mt2calc_result, mt2) ptr
      qxvalue  <- (#peek mt2calc_result, qx ) ptr
      qyvalue  <- (#peek mt2calc_result, qy ) ptr
      return MT2Value { mT2 = mt2value, kx  = qxvalue, ky  = qyvalue }

    poke ptr (MT2Value mt2value qxvalue qyvalue) = do
                             (#poke mt2calc_result, mt2) ptr mt2value
                             (#poke mt2calc_result, qx ) ptr qxvalue
                             (#poke mt2calc_result, qy ) ptr qyvalue

foreign import ccall unsafe "mt2_cwrapper/symm_mt2_minuit2_c.h run_symm_mt2_minuit2"
    c_SymmMT2Minuit2 :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CDouble
                     -> Ptr MT2Value -> IO CInt

mT2SymmPrim :: [Double]
            -> [Double]
            -> [Double]
            -> Double
            -> Either String (Double, [Double], [Double])
mT2SymmPrim visA visB ptmiss mInvisible = System.IO.Unsafe.unsafePerformIO $
    alloca $ \mt2ValPtr -> do
        va <- newArray . map realToFrac $ visA
        vb <- newArray . map realToFrac $ visB
        pX <- newArray . map realToFrac $ ptmiss
        status <- c_SymmMT2Minuit2 va vb pX (realToFrac mInvisible) mt2ValPtr
        if status == 0
            then do (MT2Value mt2 qX qY) <- peek mt2ValPtr
                    return $ Right ( realToFrac mt2
                                   , [realToFrac qX, realToFrac qY]
                                   , [        head ptmiss - realToFrac qX
                                   , (head . tail) ptmiss - realToFrac qY ])
            else return (Left "MT2 calculation failed.")

-- | calculates MT2 using Minuit2.
mT2SymmMinuit2 :: FourMomentum -> FourMomentum -> TransverseMomentum -> Double
               -> Either String (Double, [Double], [Double])
mT2SymmMinuit2 visA visB ptmiss mInvisible =
    let (eA, pxA, pyA, pzA) = epxpypz visA
        (eB, pxB, pyB, pzB) = epxpypz visB
        (pxX, pyX) = pxpy ptmiss
    in mT2SymmPrim [eA, pxA, pyA, pzA] [eB, pxB, pyB, pzB] [pxX, pyX] mInvisible

foreign import ccall unsafe "mt2_cwrapper/lester_mt2_bisect_c.h asymm_mt2_lester_bisect"
    c_AsymmMT2LesterBisect :: CDouble -> CDouble -> CDouble
                           -> CDouble -> CDouble -> CDouble
                           -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble

mT2AsymmLesterBisectPrim :: [Double] -> [Double] -> [Double]
                         -> Double -> Double -> Double
mT2AsymmLesterBisectPrim visA visB ptMiss mInvisA mInvisB =
    let (mVisA:pxVisA:pyVisA:_) = map realToFrac visA
        (mVisB:pxVisB:pyVisB:_) = map realToFrac visB
        (pxMiss:pyMiss:_) = map realToFrac ptMiss
    in realToFrac $ c_AsymmMT2LesterBisect mVisA pxVisA pyVisA mVisB pxVisB pyVisB
                                           pxMiss pyMiss
                                           (realToFrac mInvisA) (realToFrac mInvisB)

-- | calculates MT2 using the lester_mt2_bisect algorithm.
mT2AsymmLesterBisect :: FourMomentum       -- ^ four-momentum of the first visible systme
                     -> FourMomentum       -- ^ four-momentum of the second visible system
                     -> TransverseMomentum -- ^ missing transverse momentum
                     -> Double             -- ^ invariant mass of the first invisible system
                     -> Double             -- ^ invariant mass of the second invisible system
                     -> Double
mT2AsymmLesterBisect visA visB ptmiss mInvisA mInvisB =
    let mVisA = mass visA
        (pxA, pyA) = pxpy visA
        mVisB = mass visB
        (pxB, pyB) = pxpy visB
        (pxX, pyX) = pxpy ptmiss
    in mT2AsymmLesterBisectPrim [mVisA, pxA, pyA] [mVisB, pxB, pyB] [pxX, pyX]
                                mInvisA mInvisB

mT2SymmLesterBisect :: FourMomentum       -- ^ four-momentum of the first visible system
                    -> FourMomentum       -- ^ four-momentum of the second visible system
                    -> TransverseMomentum -- ^ missing transverse momentum
                    -> Double             -- ^ invariant mass of each invisible system
                    -> Double
mT2SymmLesterBisect visA visB ptmiss mInvisible =
    mT2AsymmLesterBisect visA visB ptmiss mInvisible mInvisible

foreign import ccall unsafe "mt2_cwrapper/lester_mt2_bisect_c.h symm_mt2_chenghan_bisect"
    c_SymmMT2ChengHanBisect :: CDouble -> CDouble -> CDouble
                            -> CDouble -> CDouble -> CDouble
                            -> CDouble -> CDouble -> CDouble -> CDouble

mT2SymmChengHanBisectPrim :: [Double] -> [Double] -> [Double] -> Double -> Double
mT2SymmChengHanBisectPrim visA visB ptMiss mInvis =
    let (mVisA:pxVisA:pyVisA:_) = map realToFrac visA
        (mVisB:pxVisB:pyVisB:_) = map realToFrac visB
        (pxMiss:pyMiss:_) = map realToFrac ptMiss
    in realToFrac $ c_SymmMT2ChengHanBisect mVisA pxVisA pyVisA mVisB pxVisB pyVisB
                                            pxMiss pyMiss (realToFrac mInvis)

-- | calculates MT2 using the lester_mt2_bisect algorithm.
mT2SymmChengHanBisect :: FourMomentum       -- ^ four-momentum of the first visible system
                      -> FourMomentum       -- ^ four-momentum of the second visible system
                      -> TransverseMomentum -- ^ missing transverse momentum
                      -> Double             -- ^ invariant mass of each invisible system
                      -> Double
mT2SymmChengHanBisect visA visB ptmiss mInvis =
    let mVisA = mass visA
        (pxA, pyA) = pxpy visA
        mVisB = mass visB
        (pxB, pyB) = pxpy visB
        (pxX, pyX) = pxpy ptmiss
    in mT2SymmChengHanBisectPrim [mVisA, pxA, pyA] [mVisB, pxB, pyB] [pxX, pyX]
                                 mInvis
