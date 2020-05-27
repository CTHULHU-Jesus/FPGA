{-# LANGUAGE TypeSynonymInstances  #-}
module Matrix where

import Clash.Prelude

type Matrix m n t = Vec m (Vec n t)


identity :: (Num t,KnownNat a,KnownNat b) => SNat a -> SNat b -> Matrix a b t
identity a b =
    let
        zero = replicate a (replicate b 0)
    in
        imap (\i v -> replace i 1 v) zero


scaleM
    :: (Num n,KnownNat a,KnownNat b) 
    => n -> Matrix a b n 
    -> Matrix a b n
scaleM n mat =
    mmap (n*) mat

vSlice 
    :: KnownNat n
    => CmpNat (n+l+1) l ~ GT
    => SNat f -> SNat l
    -> Vec (f+l+n) t -> Vec l t
vSlice f l v = select f (SNat :: SNat 1) l v

mSlice
    :: (KnownNat a, KnownNat b)
    => CmpNat (a+l1+1) l1 ~ GT
    => CmpNat (b+l2+1) l2 ~ GT
    => SNat f1 -> SNat l1
    -> SNat f2 -> SNat l2 
    -> Matrix (f1+l1+a) (f2+l2+b) t -> Matrix l1 l2 t
mSlice f1 l1 f2 l2 mat =
    map (vSlice f2 l2)
    . vSlice f1 l1 
    $ mat 

replaceSubVector
    :: KnownNat n
    => KnownNat s
    => KnownNat a
    => SNat s -> Vec a t
    -> Vec (s+a+n) t -> Vec (s+a+n) t
replaceSubVector s sub major =
    let 
        selector 
            :: (KnownNat n,KnownNat l)
            => Vec l t -> Index n 
            -> t -> t
        selector v i a =
            let
                i' = fromIntegral i
                s' = snatToNum s
                len = length v
            in
            if (s' <= i') && (i' < s'+len)  then
                v !! (i'-s')
            else
                a
    in
    imap (selector sub) major

replaceSubMatrix 
    :: (KnownNat n1,KnownNat n2)
    => (KnownNat a,KnownNat b)
    => (KnownNat x,KnownNat y)
    => SNat x -> SNat y
    -> Matrix a b t -> Matrix (x+a+n1) (y+b+n2) t
    -> Matrix (x+a+n1) (y+b+n2) t
replaceSubMatrix x y sub major =
    let
        selector
            :: (KnownNat a,KnownNat b)
            => (KnownNat y,KnownNat l)
            => KnownNat n
            => SNat y -> Matrix a b t 
            -> Index n -> Vec (y+b+l) t
            -> Vec (y+b+l) t
        selector y m i r =
            let
                i' = fromIntegral i
                x' = snatToNum x
                y' = snatToNum y
                len= length m
            in
            if (x' <= i') && (i' < x'+len) then
                replaceSubVector y (m !! (i' - x')) r
            else
                r
    in
    imap (selector y sub) major

mZip 
    :: (KnownNat a,KnownNat b)
    => Matrix a b t1 -> Matrix a b t2
    -> Matrix a b(t1,t2)
mZip m1 m2 =
        imap (\ i v -> zip (m1 !! fromIntegral i) v) m2

-- instance Functor (Matrix a b)  where
--      fmap f m = map (map f) m

mmap f m = map (map f) m 

instance (Num t,KnownNat a,KnownNat b) => Num (Matrix a b t ) where
    negate = map (map negate)
    (+) m1 m2 = mmap (\(a,b)-> a+b) (mZip m1 m2)
    (*) m1 m2 = mmap (\(a,b)-> a*b) (mZip m1 m2)
    fromInteger i = unconcatI $ fmap (\x->fromInteger i) indicesI :: Matrix a b t
    abs = mmap abs
    signum = mmap signum

-- the following algorithms were coppied from: https://clash-lang.org/blog/0001-matrix-multiplication/

-- vector vector multiplication
dot
  :: KnownNat n
  -- ^ Constraint 1: Store length information at runtime too
  => 1 <= n
  -- ^ Constraint 2: Vectors must be at least of length one
  => Num t
  => Vec n t
  -> Vec n t
  -> t
dot vec1 vec2 = 
  sum (zipWith (*) vec1 vec2)


--matrix vector multiplocation
mvMult
  :: KnownNat n
  => 1 <= n
  -- ^ Constraints needed for `dot`
  => Num t
  => Matrix m n t
  -- ^ Matrix with `m` rows, `n` columns
  -> Vec n t
  -- ^ Vector with `n` integers
  -> Vec m t
mvMult mat vec = 
  map (dot vec) mat

-- matrix matrix multiplication
mmMult 
  :: an ~ bm
  -- ^ Number of columns of matrix A must be
  -- equal to the number of rows in matrix B.
  => 1 <= bm
  => KnownNat bn
  => KnownNat bm
  => Num t 
  => Matrix am an t
  -> Matrix bm bn t
  -> Matrix am bn t
mmMult mat1 mat2 = 
  map (mvMult (transpose mat2)) mat1