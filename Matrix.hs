{-# LANGUAGE TypeSynonymInstances  #-}
module Matrix where

import Clash.Prelude hiding (transpose)
import Clash.Prelude as TR (transpose)


data Matrix m n t = Matrix (Vec m (Vec n t))
                  deriving (Eq,Show)


identity :: (Num t,KnownNat a,KnownNat b) => SNat a -> SNat b -> Matrix a b t
identity a b =
    let
        zero = replicate a (replicate b 0)
    in
        Matrix $ imap (\i v -> replace i 1 v) zero


iimap 
    :: (KnownNat r, KnownNat c)
    => ((Index r,Index c) -> t1 -> t2)
    -> Matrix r c t1 -> Matrix r c t2
iimap f (Matrix m) =
    Matrix $
        imap (\i1 v -> imap (\i2 x -> f (i1,i2) x) v) m 


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
mSlice f1 l1 f2 l2 (Matrix mat) =
    Matrix $
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
replaceSubMatrix x y sub (Matrix major) =
    let
        selector
            :: (KnownNat a,KnownNat b)
            => (KnownNat y,KnownNat l)
            => KnownNat n
            => SNat y -> Matrix a b t 
            -> Index n -> Vec (y+b+l) t
            -> Vec (y+b+l) t
        selector y (Matrix m) i r =
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
        Matrix $
        imap (selector y sub) major

-- zip to matraces of the same size together
mZip 
    :: (KnownNat a,KnownNat b)
    => Matrix a b t1 -> Matrix a b t2
    -> Matrix a b(t1,t2)
mZip (Matrix m1) (Matrix m2) =
    Matrix $
        imap (\ i v -> zip (m1 !! fromIntegral i) v) m2

mmap f (Matrix m) = Matrix $ map (map f) m 

getVal 
    :: KnownNat b => KnownNat a
    => Matrix a b t -> (Nat,Nat)
    -> t
getVal (Matrix m) (x,y) = (m !! x) !! y

( %% ) = getVal
infixl 8 %%

instance Functor (Matrix a b)  where
     fmap = mmap

instance (KnownNat a,KnownNat b) => Applicative (Matrix a b) where
    pure a = Matrix $ repeat (repeat a)
    f <*> (Matrix m) =
        Matrix $ 
        imap (\i1 v -> 
                imap (\i2 x->
                      (f %% (fromIntegral i1,fromIntegral i2)) x) v)
        m

instance (Num t,KnownNat a,KnownNat b) => Num (Matrix a b t ) where
    negate = fmap negate
    (+) m1 m2 = (\(a,b)-> a+b) <$> (mZip m1 m2)
    (*) m1 m2 = (\(a,b)-> a*b) <$> (mZip m1 m2)
    fromInteger i = pure $ fromInteger i
    abs = fmap abs
    signum = fmap signum

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
mvMult (Matrix mat) vec = 
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
mmMult (Matrix mat1) mat2 = 
    Matrix $
        map (mvMult $ Matrix.transpose mat2) mat1


-- These are my own additions

transpose (Matrix m) = Matrix $ TR.transpose m

zeroI 
    :: (KnownNat n,KnownNat m)
    => Num t
    => Matrix n m t
zeroI = pure 0 

-- infix notation for matrix multiplication
( .@ ) = mvMult
(  @. ) v m = (Matrix.transpose $ Matrix (v:>Nil)) .@. m
( .@. ) = mmMult 
infixl 7 .@. 
infixl 7  @.
infixl 7 .@