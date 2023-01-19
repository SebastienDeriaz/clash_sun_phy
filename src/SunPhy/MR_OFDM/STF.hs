module SunPhy.MR_OFDM.STF where

import Clash.Prelude
import SunPhy.MR_OFDM.Constants
import SunPhy.MR_OFDM.STF_constants

-- The STF symbols are repeated 4 times (each with its own cyclic prefix)
-- The maximum length for a single symbol is 160 (OFDM Option 1)

data State = Idle
           | Running
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX

stf
  :: forall dom . (HiddenClockResetEnable dom)
  => Signal dom (Unsigned 3) -- OFDM Option
  -> Signal dom Bit -- ready_i
  -> Signal dom Bit -- valid_i
  -> Signal dom (Bit, Subcarrier, Bit) -- valid_o, data_o, last_o
stf ofdmOption ready_i valid_i = bundle(valid_o, data_o, last_o)
  where
    slaveWrite = valid_o * ready_i

    nextState :: State -> Bit -> Bit -> Bit -> Bit -> State
    nextState Idle    1 _ _ _ = Running
    nextState Running _ 1 1 1 = Idle
    nextState x       _ _ _ _ = x

    state = register (Idle) $ nextState <$> state <*> valid_i <*> slaveWrite <*> bitCounterEnd <*> symbolCounterEnd

    nextBitCounter :: State -> Bit -> Bit -> Bit -> Unsigned 8 -> Unsigned 8
    --             ┌state
    --             │       ┌valid_i 
    --             │       │ ┌slaveWrite
    --             │       │ │ ┌bitCounterEnd
    --             │       │ │ │ ┌bitCounter
    --             │       │ │ │ │
    -- Idle        │       │ │ │ │
    nextBitCounter Idle    _ _ _ _ = 0
    nextBitCounter Running _ 1 1 _ = 0
    nextBitCounter Running _ 1 0 x = x + 1
    nextBitCounter _       _ _ _ x = x

    bitCounter = register (0 :: Unsigned 8) (nextBitCounter <$> state <*> valid_i <*> slaveWrite <*> bitCounterEnd <*> bitCounter)
    bitCounterEnd = boolToBit <$> (bitCounter .==. ((stfLength <$> ofdmOption) - 1))

    last_o = boolToBit <$> (bitCounterEnd .==. 1 .&&. symbolCounterEnd .==. 1)


    stfLength :: Unsigned 3 -> Unsigned 8
    stfLength 1 = fromIntegral $ length stf_1
    stfLength 2 = fromIntegral $ length stf_2
    stfLength 3 = fromIntegral $ length stf_3
    stfLength 4 = fromIntegral $ length stf_4
    stfLength _ = 0

    nextSymbolCounter :: State -> Bit -> Bit -> Unsigned 2 -> Unsigned 2
    nextSymbolCounter Idle    _ _ _ = 0
    nextSymbolCounter Running 1 1 x = x + 1
    nextSymbolCounter _       _ _ x = x
    symbolCounter = register (0 :: Unsigned 2) (nextSymbolCounter <$> state <*> bitCounterEnd <*> slaveWrite <*> symbolCounter)
    symbolCounterEnd = boolToBit <$> (symbolCounter .==. 3)

    dataOut :: Unsigned 3 -> Unsigned 8 -> Subcarrier
    dataOut 1 i = stf_1 !! i
    dataOut 2 i = stf_2 !! i
    dataOut 3 i = stf_3 !! i
    dataOut 4 i = stf_4 !! i


    dataFlip :: Unsigned 3 -> Subcarrier -> Unsigned 8 -> Unsigned 2 -> Subcarrier
    dataFlip o (a,b) i 3
      | i >= resize (stfLength o) `div` 5 * 3 = (-a,-b)
      | otherwise = (a,b)
    dataFlip _ x _ _ = x

    data_o = dataFlip <$> ofdmOption <*> (dataOut <$> ofdmOption <*> bitCounter) <*> bitCounter <*> symbolCounter
    valid_o = boolToBit <$> (state .==. pure Running)