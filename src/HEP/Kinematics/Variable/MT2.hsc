{-# LANGUAGE ForeignFunctionInterface #-}

module HEP.Kinematics.Variable.MT2 (symmMT2) where

import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

#include <mt2_cwrapper/symm_mt2_minuit2_c.h>

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

symmMT2 :: [Double]
        -> [Double]
        -> [Double]
        -> Double
        -> Either String (Double, [Double], [Double])
symmMT2 visA visB ptmiss mInvisible =
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
                                   , [          head ptmiss - realToFrac qx
                                   , (head . tail) ptmiss - realToFrac qy ]
                                   )
            else return $ Left "MT2 calculation failed."
