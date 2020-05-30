module EKF(SLAM) where
-- {-# LANGUAGE TypeSynonymInstances #-}

import Matrix
import Clash.Prelude hiding (transpose)

data SLAM a = SLAM (Nat,Vec a Float,Matrix a a Float)
            deriving (Show,Eq)

-- noise matrix
r :: Matrix 3 3 Float
r = (0.1*) <$> identity d3 d3

-- newtype MaxSize :: SNat (3+2*100) -- the robot and 100 landmarks

init :: SLAM 203 -- the robot and 100 landmarks
init =
    let 
        startSize = 3
        means = iterateI (+0) 0
        cov = replaceSubMatrix d0 d0 (identity d3 d3) zeroI :: Matrix 203 203 Float
    in
    SLAM (
        startSize,
        means,
        cov
        )

-- takes in current state (velocity forward,angular velocity,dt)
update_move :: SLAM 203 -> Vec 3 Float -> SLAM 203
update_move (SLAM (size,means,cov)) u =
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
        SLAM (size,g means u,cov')