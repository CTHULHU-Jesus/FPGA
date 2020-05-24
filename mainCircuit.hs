module MainCircuit where

import Clash.Prelude


data Input = SensorInfo (Set Vec Nat Float) 
           | BCommand (Vec Nat Float) -- command from brain
           | Query String
           deriving (Show, Eq)

-- circut State
data CState = CState (EKFSLAM,FASTSLAM)
                deriving (Show, Eq)

data Output = CCommand (Float,Float) -- controll command 
            | Response String
            | PureUpdate -- just an update of state, no output needed
            deriving (Show, Eq)

initalState :: CState
-- initalState = CState {ekf = ?? , patical = ??}

mainT :: CState -> Input -> (CState,Output)
mainT s (SensorInfo set) = (s',o)
    where
        s'= s
        o = PureUpdate

mainT s (BCommand v) = (s',o)
    where
        s' = s
        o  = CCommand (0,0)

mainT s (Query str) = (s,o)
    where
        o = Response ""

main = mealy mainT initalState

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Input
  -> Signal System Output
topEntity = exposeClockResetEnable main