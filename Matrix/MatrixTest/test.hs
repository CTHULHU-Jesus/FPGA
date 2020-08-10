module Main where

import Clash.Prelude hiding (transpose)
import Matrix

import qualified Data.List as L

-- identity Tests
identityTest01 :: Bool
identityTest01 =
    let
        idM = Matrix $
            (1 :> 0 :> 0 :> Nil) :>
            (0 :> 1 :> 0 :> Nil) :>
            (0 :> 0 :> 1 :> Nil) :> Nil
    in
        (identity d3 d3) == idM

identityTest02 :: Bool
identityTest02 =
    let
        idM = Matrix $
            (1 :> 0 :> 0:> 0 :> Nil) :>
            (0 :> 1 :> 0:> 0 :> Nil) :>
            (0 :> 0 :> 1:> 0 :> Nil) :> Nil
    in
        (identity d3 d4) == idM
            

identityTest03 :: Bool
identityTest03 =
    let
        idM = Matrix $
            (1 :> Nil) :>
            (0 :> Nil) :>
            (0 :> Nil) :> Nil
    in
        (identity d3 d1) == idM

-- identityI Tests
identityITest01 :: Bool
identityITest01 =
    let
        idM :: Matrix 3 3 Int
        idM = Matrix $
            (1 :> 0 :> 0 :> Nil) :>
            (0 :> 1 :> 0 :> Nil) :>
            (0 :> 0 :> 1 :> Nil) :> Nil
    in
        identityI == idM

identityITest02 :: Bool
identityITest02 =
    let
        idM :: Matrix 3 4 Int
        idM = Matrix $
            (1 :> 0 :> 0:> 0 :> Nil) :>
            (0 :> 1 :> 0:> 0 :> Nil) :>
            (0 :> 0 :> 1:> 0 :> Nil) :> Nil
    in
        identityI == idM
            

identityITest03 :: Bool
identityITest03 =
    let
        idM :: Matrix 3 1 Int
        idM = Matrix $
            (1 :> Nil) :>
            (0 :> Nil) :>
            (0 :> Nil) :> Nil
    in
        identityI  == idM


-- vSlice Tests

vSliceTest01 :: Bool
vSliceTest01 = 
    let
        vec = 1 :> 2 :>  3 :> 4 :> Nil
        vec2= 2 :> 3 :> Nil
    in
        (vSlice 1 d2 vec) == vec2


vSliceTest02 :: Bool
vSliceTest02 =
    let
        vec = 1 :> 2 :> 3 :> 4 :> Nil
        vec2= 1 :>2 :>3 :> Nil
    in
        (vSlice 0 d3 vec) == vec2

vSliceTest03 :: Bool
vSliceTest03 =
    let
        vec = 1 :> 2 :> Nil
        vec2= Nil
    in
        (vSlice 2 d0 vec) == vec2

-- Mslice Tests
mSliceTest01 :: Bool
mSliceTest01 =
    let
        m1 = Matrix $
            (1 :> 2 :> 3 :> 4 :> Nil) :>
            (5 :> 6 :> 7 :> 8 :> Nil) :>
            (9 :> 10:> 11:> 12:> Nil) :> Nil
        m2 = Matrix $
            (1 :> 2 :> Nil) :>
            (5 :> 6 :> Nil) :> Nil
    in
        (mSlice 0 d2 0 d2 m1) == m2

mSliceTest02 :: Bool
mSliceTest02 =
    let
        m1 = Matrix $
            (1 :> 2 :> Nil) :>
            (3 :> 4 :> Nil) :> Nil
    in
        (mSlice 0 d0 0 d0 m1) == Matrix Nil

mSliceTest03 :: Bool
mSliceTest03 =
    let
        m1 = Matrix $
            (1 :> 2 :> 3 :> 4 :> Nil) :>
            (5 :> 6 :> 7 :> 8 :> Nil) :>
            (9 :> 10:> 11:> 12:> Nil) :> Nil
        m2 = Matrix $
            (7 :> 8 :> Nil) :>
            (11:>12 :> Nil) :> Nil
    in
        (mSlice 1 d2 2 d2 m1) == m2

mSliceTest04 :: Bool
mSliceTest04 =
    let
        m1 = Matrix $
            (0 :> 1 :> 2 :> Nil) :>
            (3 :> 4 :> 5 :> Nil) :>
            (6 :> 7 :> 8 :> Nil) :> Nil
        m2 = Matrix $
            (3 :> 4 :> Nil) :>
            (6 :> 7 :> Nil) :> Nil
    in
        (mSlice 1 d2 0 d2 m1) == m2

-- applyToSubMatrix Tests
applyToSubMatrixTest01 :: Bool
applyToSubMatrixTest01 =
    let
        m1 :: Matrix 5 5 Int
        m1 = pure 0
        m2 :: Matrix 5 5 Int
        m2 = Matrix $
            (0 :> 0 :> 0 :> 0 :> 0 :> Nil) :>
            (0 :> 0 :> 1 :> 1 :> 0 :> Nil) :>
            (0 :> 0 :> 1 :> 1 :> 0 :> Nil) :>
            (0 :> 0 :> 0 :> 0 :> 0 :> Nil) :>
            (0 :> 0 :> 0 :> 0 :> 0 :> Nil) :> Nil
        f :: Matrix 2 2 Int -> Matrix 2 2 Int
        f x = (+1) <$> x
    in
        (applyToSubMatrix f 1 2 m1) == m2

applyToSubMatrixTest02 :: Bool
applyToSubMatrixTest02 =
    let
        m1 :: Matrix 3 3 Int
        m1 = pure 0
        m2 :: Matrix 3 3 Int
        m2 = Matrix $
            (0 :> 0 :> 0:> Nil) :>
            (0 :> 1 :> 2:> Nil) :>
            (0 :> 2 :> 3:> Nil) :> Nil
        f :: Matrix 2 2 Int -> Matrix 2 2 Int
        f = iimap (\(i1,i2) _ -> 1+fromIntegral i1+fromIntegral i2)
    in
        (applyToSubMatrix f 1 1 m1) == m2

-- replaceSubVector Tests
replaceSubVectorTest01 :: Bool
replaceSubVectorTest01 =
    (replaceSubVector 1 (1 :> 1 :> Nil) (pure 0 :: Vec 5 Int)) == (0 :> 1 :> 1 :> 0 :> 0 :> Nil)

replaceSubVectorTest02 :: Bool
replaceSubVectorTest02 =
    (replaceSubVector 0 Nil Nil) == (Nil :: Vec 0 Int)


-- replaceSubMatrix Tests
replaceSubMatrixTest01 :: Bool
replaceSubMatrixTest01 =
    let
        m1 :: Matrix 2 2 Int
        m1 = pure 1
        m2 :: Matrix 4 5 Int
        m2 = pure 0
        m3 = Matrix $
            (0 :> 0 :> 0 :> 0 :> 0 :> Nil) :>
            (1 :> 1 :> 0 :> 0 :> 0 :> Nil) :>
            (1 :> 1 :> 0 :> 0 :> 0 :> Nil) :>
            (0 :> 0 :> 0 :> 0 :> 0 :> Nil) :> Nil
    in
        (replaceSubMatrix 1 0 m1 m2) == m3

replaceSubMatrixTest02 :: Bool
replaceSubMatrixTest02 =
    let
        m1 :: Matrix 2 2 Int
        m1 = pure 1
        m2 :: Matrix 3 4 Int
        m2 = pure 0
        m3 :: Matrix 3 4 Int
        m3 = Matrix $
            (0 :> 0 :> 0 :> 0 :> Nil) :>
            (0 :> 0 :> 1 :> 1 :> Nil) :>
            (0 :> 0 :> 1 :> 1 :> Nil) :> Nil
    in
        (replaceSubMatrix 1 2 m1 m2) == m3

-- mZipWith Test
mZipWithTest01 :: Bool
mZipWithTest01 =
    let
        m1 = Matrix $
            (1 :> 2 :> Nil) :>
            (2 :> 1 :> Nil) :> Nil
        m2 = Matrix $
            (3 :> 4 :> Nil) :>
            (0 :> 1 :> Nil) :> Nil
        m3 = Matrix $
            (4 :> 6 :> Nil) :>
            (2 :> 2 :> Nil) :> Nil
    in
        (mZipWith (+) m1 m2) == m3

-- mZip Tests
mZipTest01 :: Bool
mZipTest01 =
    let
        m1 :: Matrix 3 2 Int
        m1 = pure 1
        m2 :: Matrix 3 2 Int
        m2 = pure 2
        m3 :: Matrix 3 2 (Int,Int)
        m3 = pure (1,2)
    in
        (mZip m1 m2) == m3

mZipTest02 :: Bool
mZipTest02 =
    let
        m1 = Matrix $
            (1 :> 2 :> Nil) :>
            (3 :> 4 :> Nil) :> Nil
        m2 = Matrix $
            (5 :> 6 :> Nil) :>
            (7 :> 8 :> Nil) :> Nil
        m3 = Matrix $
            ((1,5) :> (2,6) :> Nil) :>
            ((3,7) :> (4,8) :> Nil) :> Nil
    in
        (mZip m1 m2) == m3

-- getVal Tests
getValTest01 :: Bool
getValTest01 =
    let
        m1 :: Matrix 4 4 Int
        m1 = pure 4
    in
        L.foldl (&&) True [m1 !!! (x,y) == 4 | x <- [0..3], y <- [0..3]]

getValTest02 :: Bool
getValTest02  =
    let
        m1 = Matrix $
            (1 :> 2 :> Nil) :>
            (3 :> 4 :> Nil) :> Nil
    in
        (m1 !!! (0,0) == 1) &&
        (m1 !!! (0,1) == 2) &&
        (m1 !!! (1,0) == 3) &&
        (m1 !!! (1,1) == 4) 

-- sizeS Test
sizeSTest01 :: Bool
sizeSTest01 =
    let
        m1 :: Matrix 3 2 ()
        m1 = pure ()
        tt = (d3,d2)
    in
        sizeS m1 == tt

sizeSTest02 :: Bool
sizeSTest02 =
    let
        m1 :: Matrix 0 0 ()
        m1 = pure ()
        tt = (d0,d0)
    in
        sizeS m1 == tt

-- dot test
dotTest01 :: Bool
dotTest01 =
    let
        v1 :: Vec 2 Int
        v1 = pure 2
        a = 8
    in
        (v1 `dot` v1) == a

main :: IO ()
main =
    do
        putStrLn $ "identityTest01:" L.++ show identityTest01
        putStrLn $ "identityTest02:" L.++ show identityTest02
        putStrLn $ "identityTest03:" L.++ show identityTest03
        putStrLn $ "identityITest01:" L.++ show identityITest01
        putStrLn $ "identityITest02:" L.++ show identityITest02
        putStrLn $ "identityITest03:" L.++ show identityITest03
        putStrLn $ "vSliceTest01:" L.++ show vSliceTest01
        putStrLn $ "vSliceTest01:" L.++ show vSliceTest01
        putStrLn $ "vSliceTest02:" L.++ show vSliceTest02
        putStrLn $ "vSliceTest03:" L.++ show vSliceTest03
        putStrLn $ "mSliceTest01:" L.++ show mSliceTest01
        putStrLn $ "mSliceTest02:" L.++ show mSliceTest02
        putStrLn $ "mSliceTest03:" L.++ show mSliceTest03
        putStrLn $ "mSliceTest04:" L.++ show mSliceTest04
        putStrLn $ "applyToSubMatrixTest01:" L.++ show applyToSubMatrixTest01
        putStrLn $ "applyToSubMatrixTest02:" L.++ show applyToSubMatrixTest02
        putStrLn $ "replaceSubVectorTest01:" L.++ show replaceSubVectorTest01
        putStrLn $ "replaceSubVectorTest02:" L.++ show replaceSubVectorTest02
        putStrLn $ "replaceSubMatrixTest01:" L.++ show replaceSubMatrixTest01
        putStrLn $ "mZipWithTest01:" L.++ show mZipWithTest01
        putStrLn $ "mZipTest01:" L.++ show mZipTest01
        putStrLn $ "mZipTest02:" L.++ show mZipTest02
        putStrLn $ "getValTest01:" L.++ show getValTest01
        putStrLn $ "getValTest02:" L.++ show getValTest02
        putStrLn $ "sizeSTest01:" L.++ show sizeSTest01
        putStrLn $ "sizeSTest02:" L.++ show sizeSTest02
        putStrLn "test over"
