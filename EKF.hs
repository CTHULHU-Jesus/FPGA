module EKF(SLAM) where
-- {-# LANGUAGE TypeSynonymInstances #-}

import Matrix
import Clash.Prelude

data SLAM a = SLAM (Nat,Vec a Float,Matrix a a Float)
            deriving (Show,Eq)

-- noise matrix
r :: Matrix 3 3 Float
r = scaleM 0.1 $ identity d3 d3

maxSize = SNat :: SNat (3+2*100) -- the robot and 100 landmarks

init :: SLAM (3+2*100) -- the robot and 100 landmarks
init =
    let 
        startSize = 3
        means = iterateI (+0) 0
        cov = unconcatI (iterateI (+0) 0) :: Matrix 203 203 Float
    in
    SLAM (
        startSize,
        means,
        replaceSubMatrix d0 d0 (identity d3 d3 :: Matrix 3 3 Float) cov 
        )

-- takes in current state (velocity forward,angular velocity,dt)
update_move :: SLAM 203 -> Vec 3 Float -> SLAM 203
update_move (SLAM (size,means,cov)) u =
    let
        -- g is the motial model
        g :: KnownNat n => Vec n Float -> Vec 3 Float -> Vec n Float
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
        jac_G :: Float -> Vec 3 Float -> Matrix 203 203 Float
        jac_G theta (v :> omega :> dt :> Nil) =
            let 
                base =
                    (1 :> 0 :> -v*sin(theta)*dt :> Nil) :>
                    (0 :> 1 :> v*cos(theta)*dt  :> Nil) :>
                    (0 :> 0 :> 1 :> Nil) :> Nil
                sizeingM = identity d3 maxSize :: Matrix 3 203 Float
            in
                mmMult (transpose sizeingM) $ mmMult base sizeingM
    in
    let
        j = jac_G (means !! 2) u
        cov' :: Matrix 203 203 Float
        cov' = mmMult j $ mmMult cov $ transpose j 
    in
        SLAM (size,g means u,cov')