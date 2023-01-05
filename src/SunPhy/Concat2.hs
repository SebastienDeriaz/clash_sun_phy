{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module SunPhy.Concat2 where

import Clash.Prelude

-- State machine
data State
    = A
    | B
    deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
    deriving anyclass (NFDataX)

data AxiForward = AxiForward
    { _valid :: Bit
    , _data :: Bit
    , _last :: Bit
    }

-- state, slaveWrite, a_last_i, b_last_i
nextState :: State -> Bit -> Bit -> Bit -> State
nextState A 1 1 _ = B -- to B if the slave did write and a_last_i is 1
nextState B 1 _ 1 = A -- to A if the slave did write and b_last_i is 1
nextState x _ _ _ = x

concat2
    :: forall dom
     . HiddenClockResetEnable dom
    => Signal dom AxiForward -- a
    -> Signal dom AxiForward -- b
    -> Signal dom Bit -- ready
    -> Signal dom (Bit, Bit, AxiForward)
concat2 a b ready = bundle (readyA, readyB, out)
  where
    slaveWrite = (_valid <$> out) * ready

    state =
        register A $
            nextState
                <$> state
                <*> slaveWrite
                <*> (_last <$> a)
                <*> (_last <$> b)

    readyA = ready * (boolToBit <$> (state .==. pure A))
    readyB = ready * (boolToBit <$> (state .==. pure B))

    out = do
        _valid <- mux (state .==. pure A) (_valid <$> a) (_valid <$> b)
        _data <- mux (state .==. pure A) (_data <$> a) (_data <$> b)
        _last <- _last <$> b
        pure AxiForward{..}
