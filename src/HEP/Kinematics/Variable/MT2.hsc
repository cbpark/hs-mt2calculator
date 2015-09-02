{-# LANGUAGE ForeignFunctionInterface #-}

module HEP.Kinematics.Variable.MT2 (mT2Symm, mT2AsymmBisect) where

import           Foreign
import           Foreign.C.Types
import           System.IO.Unsafe (unsafePerformIO)

import           HEP.Kinematics   (FourMomentum, HasFourMomentum (..),
                                   TransverseMomentum)

#include <mt2_cwrapper/symm_mt2_minuit2_c.h>
#include <mt2_cwrapper/lester_mt2_bisect_c.h>

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
    c_SymmMT2Minuit2 :: Ptr CDouble
                     -> Ptr CDouble
                     -> Ptr CDouble
                     -> CDouble
                     -> Ptr MT2Value
                     -> IO CInt

-- | calculates MT2 using Minuit2.
mT2SymmPrim :: [Double]
            -> [Double]
            -> [Double]
            -> Double
            -> Either String (Double, [Double], [Double])
mT2SymmPrim visA visB ptmiss mInvisible =
    System.IO.Unsafe.unsafePerformIO $
          alloca $ \mt2ValPtr -> do
            va <- newArray . map realToFrac $ visA
            vb <- newArray . map realToFrac $ visB
            px <- newArray . map realToFrac $ ptmiss
            status <- c_SymmMT2Minuit2 va vb px (realToFrac mInvisible) mt2ValPtr
            if status == 0
            then do (MT2Value mt2 qx qy) <- peek mt2ValPtr
                    return $ Right ( realToFrac mt2
                                   , [realToFrac qx, realToFrac qy]
                                   , [        head ptmiss - realToFrac qx
                                   , (head . tail) ptmiss - realToFrac qy ]
                                   )
            else return $ Left "MT2 calculation failed."

mT2Symm :: FourMomentum -> FourMomentum -> TransverseMomentum -> Double
        -> Either String (Double, [Double], [Double])
mT2Symm visA visB ptmiss mInvisible =
  let (eA, pxA, pyA, pzA) = epxpypz visA
      (eB, pxB, pyB, pzB) = epxpypz visB
      (pxX, pyX) = pxpy ptmiss
  in mT2SymmPrim [eA, pxA, pyA, pzA] [eB, pxB, pyB, pzB] [pxX, pyX] mInvisible

foreign import ccall unsafe "mt2_cwrapper/lester_mt2_bisect_c.h asymm_mt2_bisect"
  c_AsymmMT2Bisect :: CDouble -> CDouble -> CDouble
                   -> CDouble -> CDouble -> CDouble
                   -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble

-- | calculates MT2 using the lester_mt2_bisect algorithm.
mT2AsymmBisectPrim :: [Double] -> [Double] -> [Double] -> Double -> Double -> Double
mT2AsymmBisectPrim visA visB ptMiss mInvisA mInvisB =
  let (mVisA:pxVisA:pyVisA:_) = map realToFrac visA
      (mVisB:pxVisB:pyVisB:_) = map realToFrac visB
      (pxMiss:pyMiss:_) = map realToFrac ptMiss
  in realToFrac $ c_AsymmMT2Bisect mVisA pxVisA pyVisA mVisB pxVisB pyVisB
                                   pxMiss pyMiss
                                   (realToFrac mInvisA) (realToFrac mInvisB)

mT2AsymmBisect :: FourMomentum -> FourMomentum -> TransverseMomentum
               -> Double  -- ^ invariant mass of the first invisible system
               -> Double  -- ^ invariant mass of the second invisible system
               -> Double
mT2AsymmBisect visA visB ptmiss mInvisA mInvisB =
  let mVisA = mass visA
      (pxA, pyA) = pxpy visA
      mVisB = mass visB
      (pxB, pyB) = pxpy visB
      (pxX, pyX) = pxpy ptmiss
  in mT2AsymmBisectPrim [mVisA, pxA, pyA] [mVisB, pxB, pyB] [pxX, pyX]
                        mInvisA mInvisB
