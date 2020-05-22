module MainCircuit where

import Clash.Prelude


type Input = SensorInfo Set Vec Nat Float
           | Command Vec Nat Float
           | Query String

-- circut State
type CState = CState {ekf :: EKFSLAM, partical :: FASTSLAM}

type Output = Command (Float,Float)
            | Response String
            | PureUpdate -- just an update of state, no output needed

initalState :: CState
initalState = 0

mainT :: CState -> Input -> (CState,Output)
mainT s i = (s',o)
    where
        s' = 
            case i of
                SensorInfo set -> s
                Command u -> s
                Query str -> s
        o =
            case i of
                SensorInfo set -> PureUpdate
                Command u -> Command ??
                Query str -> Response str

main = mealy mainT initalState

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Input
  -> Signal System Output
topEntity = exposeClockResetEnable main