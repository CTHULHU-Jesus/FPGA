module EKF(SLAM) where
-- {-# LANGUAGE TypeSynonymInstances #-}

import Matrix
import Clash.Prelude hiding (transpose)

data SLAM a = SLAM (Index a,Vec (2*a+3) Float,Matrix (2*a+3) (2*a+3) Float)
            deriving (Show,Eq)

-- noise matrix
r :: Matrix 3 3 Float
r = (0.1*) <$> identity d3 d3


mesumentNoise :: Matrix 2 2 Float
mesumentNoise = identityI

-- newtype MaxSize :: SNat (3+2*100) -- the robot and 100 landmarks

init :: SLAM 203 -- the robot and 100 landmarks
init =
    let 
        numLands = 0
        means = iterateI (+0) 0
        cov = replaceSubMatrix d0 d0 (identity d3 d3) zeroI 
    in
    SLAM (
        numLands,
        means,
        cov
        )

-- takes in current state (velocity forward,angular velocity,dt)
update_move :: SLAM 203 -> Vec 3 Float -> SLAM 203
update_move (SLAM (numLands,means,cov)) u =
    let
        -- g is the motial model
        g 
            :: KnownNat n 
            => Vec n Float -> Vec 3 Float 
            -> Vec n Float
        g means (v :> omega :> dt :> Nil) =
            let 
                x = means !! 0
                y = means !! 1
                theta = means !! 2
            in
            let
                x' = x + v*cos(theta)*dt
                y' = y + v*sin(theta)*dt
                theta' = theta+omega*dt
            in
            replace 0 x' 
            . replace 1 y'
            . replace 2 theta'
            $ means
        -- jac_G is the jacobian of g
        jac_G 
            :: Float -> Vec 3 Float
            -> Matrix 203 203 Float
        jac_G theta (v :> omega :> dt :> Nil) =
            let 
                base =Matrix $
                    (1 :> 0 :> -v*sin(theta)*dt :> Nil) :>
                    (0 :> 1 :> v*cos(theta)*dt  :> Nil) :>
                    (0 :> 0 :> 1 :> Nil) :> Nil
            in
                replaceSubMatrix d0 d0 base zeroI
    in
    let
        j = jac_G (means !! 2) u
        cov' :: Matrix 203 203 Float
        cov' = j .@. cov .@. (transpose j)
    in
        SLAM (numLands,g means u,cov')

getCovOfLand :: Int -> SLAM b -> Matrix 2 2 Float
getCovOfLand l (SLAM (_,_,cov)) =
    let
        n = 2+2*l
    in
        mSlice n d2 n d2 cov

getMeanOfLand :: Int -> SLAM a -> Vec 2 Float
getMeanOfLand l (SLAM (_,means,_)) =
    let
        n = 2+2*l
    in
        vSlice n d2 means

toPolar :: SLAM a -> Vec 2 Float -> Vec 2 Float
toPolar s@(SLAM (_,means,cov)) v1@(x:>y:>Nil) =
    let
        robotXY = vSlice 0 d2 means
        delta = v1-robotXY
        q = edist delta
        theta = means !! 2
    in
        q :>
        (atan2 (delta !! 1) (delta !! 0)) - theta :> Nil

toPolarJ :: SLAM a -> Vec 2 Float -> Matrix 2 5 Float
toPolarJ = 
    let
        robotXY = vSlice 0 d2 means
        delta = v1-robotXY
        deltaX = delta !! 0
        deltaY = delta !! 1
        q = edist delta
        q2= q**2
        theta = means !! 2
    in
        Matrix $
           ((-deltaX)/q :> (-deltaY)/q  :> 0  :> deltaX/q     :> deltaY/q  :> Nil) :>
           (deltaY/q2   :> (-deltaX)/q2 :> -1 :> (-deltaY)/q2 :> deltaX/q2 :> Nil) :> Nil


-- returns a vector of numbers that match up with the mesuments. The first number in the vector is the landmark that coresponds to the first mesument. A zero means it is a new landmark
dataAssoc :: SLAM 203 -> Matrix a 2 Float -> Vec a (Index (1+a))
dataAssoc s@(SLAM (numLands,means,cov)) (Matrix mes) = 
    let
        -- how many standatd deviations is v1 from v2?
        dist :: Vec 2 Float -> Vec 2 Float -> Matrix 2 2 Float -> Float
        dist v1 v2 cov = 
            let
                diff  = v1 - v2
                diffT = transpose diff
            in
               edist $ cov <@ (v2-v1)
        min = 1
        same :: Vec 2 Float -> Index a -> Bool
        same v1 l =
            let
                -- v1 is in polar coords so we need to change the landmark and covaricance to the same
                land = toPolar s $ getMeanOfLand l
                jac  = toPolarJ s $ getMeanOfLand l
                covLand = jac .@. cov .@. (transpose jac) + mesumentNoise
                d = dist v1 land covLand
            in
                d <= min
        
    in
        iimap 
