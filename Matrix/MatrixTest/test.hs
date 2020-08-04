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
        putStrLn "test over"
