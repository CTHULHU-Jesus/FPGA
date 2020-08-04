-- {-# LANGUAGE TypeSynonymInstances #-}
module Matrix where

-- I need to make my own transpose for
-- matraces so I import the default one
-- with the TR prefix
-- import qualified Prelude as PPPP ()
import Clash.Prelude hiding (transpose)
import qualified Clash.Prelude as TR (transpose)

-- I need these for strings they are used anywhere other than show.
import qualified Data.List as LIST

-- A matrix is just a vector of vectors of some
-- arbitrary types
data Matrix m n t = Matrix (Vec m (Vec n t))
                    deriving (Eq)

-- Create the identity matrix, the one that looks like:
-- [ 1 , 0 , 0 ]
-- [ 0 , 1 , 0 ]
-- [ 0 , 0 , 1 ]
-- But of any size
identity :: (Num t,KnownNat a,KnownNat b) => SNat a -> SNat b -> Matrix a b t
identity a b =
    let
        zero = replicate a (replicate b 0)
    in
        Matrix $ imap (\i v -> replace i 1 v) zero

-- The same as Identity but the size is deduced from context
identityI :: (Num t, KnownNat a,KnownNat b) => Matrix a b t
identityI =
    let
        fun :: (KnownNat a,KnownNat b,Num t)
            => (Index a, Index b) -> t
            -> t
        fun (i1,i2) _ =  if (toInteger i1 == toInteger i2) then 
                            1
                         else 
                            0
    in
        iimap fun $ pure 0

-- imap for matrixes but insted of takeing one
-- index it takes a tuple of inexes
iimap 
    :: (KnownNat r, KnownNat c)
    => ((Index r,Index c) -> t1 -> t2)
    -> Matrix r c t1 -> Matrix r c t2
iimap f (Matrix m) =
    Matrix $
        imap (\i1 v -> imap (\i2 x -> f (i1,i2) x) v) m 

-- Get a sub vector from a lager vector.
-- Example:
-- vSlice 1 3 <1,2,3,4,5,6,7,8,9> = <2,3,4>
vSlice 
    -- :: KnownNat n
    :: KnownNat l
    => KnownNat (n+l)
    => Int -> SNat l
    -> Vec (l+n) t -> Vec l t
vSlice k l vs = 
    map (\i -> vs !! (k+i)) $  iterate l (+1) (0 :: Int)
    {-
vSlice f l v =
    let
        length :: Int
        length = fromIntegral . snatToInteger $ l
        fun :: KnownNat n
            => Index n -> Int 
        fun i = (f+length-(fromIntegral i)-1) 
    in
        imap (\i _ -> v !! (fun i)) $ replicate l 0
-}


-- Get a sub matrix fom a larger matrix.
-- Example:
-- mSlice 1 2 0 2 [ 0 , 1 , 2 ] = [ 3 , 4 ]
--                [ 3 , 4 , 5 ]   [ 6 , 7 ]
--                [ 6 , 7 , 8 ]
mSlice
    :: (KnownNat a, KnownNat b)
    => (KnownNat l1,KnownNat l2)
    => CmpNat (a+l1+1) l1 ~ GT
    => CmpNat (b+l2+1) l2 ~ GT
    => Int -> SNat l1
    -> Int -> SNat l2 
    -> Matrix (l1+a) (l2+b) t -> Matrix l1 l2 t
mSlice f1 l1 f2 l2 (Matrix mat) =
    Matrix $
    map (vSlice f2 l2)
    . vSlice f1 l1 
    $ mat 

-- applys a function to a subsection of a matrix
-- Example:
-- applyToSubMatrix 1 2 ( <$> (+1) :: Matrix 2 2 Int -> Matrix 2 2 Int)
--                         (pure 0 :: Matrix  5 5 Int)
--     =
--     [ 0 , 0 , 0 , 0 , 0 ]
--     [ 0 , 0 , 1 , 1 , 0 ]
--     [ 0 , 0 , 1 , 1 , 0 ]
--     [ 0 , 0 , 0 , 0 , 0 ]
--     [ 0 , 0 , 0 , 0 , 0 ]
applyToSubMatrix :: (KnownNat m, KnownNat n)
                 => (KnownNat a, KnownNat b)
                 => (Matrix a b t -> Matrix a b t)
                 -> Int -> Int
                 -> Matrix (a+m) (b+n) t
                 -> Matrix (a+m) (b+n) t
applyToSubMatrix f row col mat =
    let
        new = f $ iimap (\(i1,i2) _ -> mat !!! (row+fromIntegral i1,col+fromIntegral i2)) $ pure 0 
    in
        replaceSubMatrix row col new mat 


-- Replaces the values of a sub vector with the values of another vector
-- Example:
-- replaceSubVector 1 <1,1> <0,0,0,0,0> = <0,1,1,0,0>
replaceSubVector
    :: KnownNat n
    => KnownNat a
    => Int -> Vec a t
    -> Vec (a+n) t -> Vec (a+n) t
replaceSubVector s sub major =
    let 
        selector 
            :: (KnownNat n,KnownNat l)
            => Vec l t -> Index n 
            -> t -> t
        selector v i a =
            let
                i' = fromIntegral i
                len = fromIntegral . length $ v
            in
            if (s <= i') && (i' < s+len)  then
                v !! (i'-s)
            else
                a
    in
    imap (selector sub) major

-- Replaces the values of some sub matrix with the values of another matrix
-- Example: 
-- replaceSubMatrix 1 0 [ 1 , 1 ] [ 0 , 0 , 0 , 0 , 0 ] = [ 0 , 0 , 0 , 0 , 0 ]
--                      [ 1 , 1 ] [ 0 , 0 , 0 , 0 , 0 ]   [ 1 , 1 , 0 , 0 , 0 ]
--                                [ 0 , 0 , 0 , 0 , 0 ]   [ 1 , 1 , 0 , 0 , 0 ]
--                                [ 0 , 0 , 0 , 0 , 0 ]   [ 0 , 0 , 0 , 0 , 0 ]
replaceSubMatrix 
    :: (KnownNat n1,KnownNat n2)
    => (KnownNat a,KnownNat b)
    => Int -> Int
    -> Matrix a b t -> Matrix (a+n1) (b+n2) t
    -> Matrix (a+n1) (b+n2) t
replaceSubMatrix x y sub (Matrix major) =
    let
        selector
            :: (KnownNat a,KnownNat b)
            => (KnownNat n,KnownNat y)
            => Int -> Matrix a b t 
            -> Index n -> Vec (y+b) t
            -> Vec (y+b) t
        selector y (Matrix m) i r =
            let
                i' = fromIntegral i
                len= fromIntegral . length $ m
            in
            if (x <= i') && (i' < x+len) then
                replaceSubVector y (m !! (i' - x)) r
            else
                r
    in
        Matrix $
        imap (selector y sub) major


-- zipWith for matrixes
-- Example:
-- mZipWith + [ 1 , 2 ] [ 3 , 4 ] = [ 4 , 6 ]
--            [ 2 , 1 ] [ 0 , 1 ]   [ 2 , 2 ]
mZipWith
    :: (KnownNat a, KnownNat b)
    => (t1 -> t2 -> t3) -> Matrix a b t1 
    -> Matrix a b t2 -> Matrix a b t3
mZipWith f (Matrix m1) (Matrix m2) =
    Matrix $
        imap (\ i v -> zipWith f (m1 !! fromIntegral i) v) m2

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
    => Matrix a b t -> (Int,Int)
    -> t
getVal (Matrix m) (x,y) = (m !! x) !! y

( !!! ) = getVal
infixl 8 !!!

sizeS :: (KnownNat a, KnownNat b)
      => Matrix a b t -> (SNat a, SNat b)
sizeS (Matrix m) =
    (lengthS m,lengthS $ m !! 0)

size :: (KnownNat a, KnownNat b)
     => Matrix a b t -> (Int, Int)
size m =
    let
        (a,b) = sizeS m
    in
        (fromIntegral . snatToInteger $ a,fromIntegral . snatToInteger $ b)


instance Functor (Matrix a b)  where
     fmap = mmap

instance (KnownNat a,KnownNat b) => Applicative (Matrix a b) where
    pure a = Matrix $ repeat (repeat a)
    f <*> (Matrix m) =
        Matrix $ 
        imap (\i1 v -> 
                imap (\i2 x->
                      (f !!! (fromIntegral i1,fromIntegral i2)) x) v)
        m

instance (Num t,KnownNat a,KnownNat b) => Num (Matrix a b t ) where
    negate = fmap negate
    (+) m1 m2 = mZipWith (+) m1 m2
    (*) m1 m2 = mZipWith (*) m1 m2
    fromInteger i = pure $ fromInteger i
    abs = fmap abs
    signum = fmap signum

instance (Show t,KnownNat a, KnownNat b) => Show (Matrix a b t) where
    show m =
        let (Matrix vecs) = m in
        let
            row :: [t] -> String 
            row v = case LIST.length v of
                0 -> ""
                1 -> show $ v LIST.!! 0
                n | n > 1 -> (LIST.foldl (\ s e -> s LIST.++" ,"LIST.++show e ) "" . LIST.init $ v)
                             LIST.++ (show . LIST.last $ v)
        in
        case fst . size $ m of
            0 -> ""
            1 -> "[["LIST.++(row . toList $ vecs !! 0 )LIST.++"]]"
            2 -> "[["LIST.++(row . toList $ vecs !! 0 )LIST.++"],\n"LIST.++
                 " ["LIST.++(row . toList $ vecs !! 1 )LIST.++"]]"
            n -> 
                let
                    rows :: [String]
                    rows = LIST.map (\ v -> "["LIST.++(row . toList $ v)LIST.++"]") . toList $ vecs
                    h = "["LIST.++(LIST.head rows) LIST.++",\n"
                    mid = LIST.map (\ r -> " "LIST.++r LIST.++",\n") . LIST.init . LIST.tail $ rows
                    t = " "LIST.++ (LIST.last rows) LIST.++"]"
                in
                    LIST.foldl (LIST.++) "" $ h : mid LIST.++[t] 

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
