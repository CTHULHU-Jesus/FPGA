module MatrixTest where

import Matrix
import Clash.Prelude hiding (transpose)


type State = ()

type Input = (Matrix 3 2 Int, Matrix 2 3 Int)

type Output = IO ()

mainT :: State -> Input -> (State,Output)
mainT s (m1,m2) = (s', print o)
    where
        s' = s
        o = m1 .@. m2

main = mealy mainT

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> State
  -> Signal System Input
  -> Signal System Output
topEntity = exposeClockResetEnable main
