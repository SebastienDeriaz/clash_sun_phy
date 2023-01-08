{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Sun_phy.MR_FSK.PHR where

import Clash.Prelude

reverseBits :: KnownNat n => BitVector n -> BitVector n
reverseBits a = v2bv $ reverse $ bv2v a

data PHR = PHR
  { phrModeSwitch :: Bit
  , phrFCS :: Bit
  , phrDataWhitening :: Bit
  , phrFrameLength :: Unsigned 11
  }

instance BitPack PHR where
  type BitSize PHR = 16
  pack PHR{..} =
    (reverseBits $ pack phrFrameLength)
     ++# (pack phrDataWhitening)
     ++# (pack phrFCS)
     ++# (0b00 :: BitVector 2)
     ++# (pack phrModeSwitch)

  unpack $(bitPattern ("a..bcddddddddddd")) =
    PHR
      { phrModeSwitch = unpack a
      , phrFCS = unpack b
      , phrDataWhitening = unpack c
      , phrFrameLength = unpack ddddddddddd
      }

data State = Idle
           | Running
           | Finish
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX


nextState :: State -> Bit -> Bit -> State
-- state, slaveWrite, last
--        ┌state
--        │       ┌slaveWrite 
--        │       │ ┌last
-- Idle   │       │ │
nextState Idle    0 _ = Idle
nextState Idle    1 _ = Running
nextState Running 1 1 = Finish
nextState Running _ _ = Running
nextState Finish  1 _ = Finish
nextState Finish  0 _ = Idle

nextBitIndex :: State -> Bit -> Unsigned 4 -> Unsigned 4
-- state, slaveWrite, bitIndex
--           ┌state
--           │       ┌valid_i 
--           │       │ ┌ready_o
-- Idle      │       │ │
nextBitIndex Running 0 x = x
nextBitIndex _       1 x = x + 1
nextBitIndex _       _ x = 0

phr
  :: forall dom . (HiddenClockResetEnable dom)
  => Signal dom Bit -- phrModeSwitch :: Bit
  -> Signal dom Bit -- macFCSType :: Bit
  -> Signal dom Bit -- phyFSKScramblePSDU :: Bit
  -> Signal dom (Unsigned 11) -- phrFrameLength :: Unsigned 11
  -> Signal dom Bit -- ready_i
  -> Signal dom Bit -- valid_i
  -> Signal dom (Bit, Bit, Bit) -- valid_o, data_o, last_o
phr ms fcs dw len ready_i valid_i = bundle(valid_o, data_o, last_o)
  where
    state = register Idle (nextState <$> state <*> slaveWrite <*> last_o)

    slaveWrite = ready_i * valid_o


    -- Index of the PHR bit (start at 0)
    bitIndex = register (0 :: Unsigned 4) (nextBitIndex <$> state <*> slaveWrite <*> bitIndex)

    bitIndexMax = pure 15

    last_o = boolToBit <$> (bitIndex .==. bitIndexMax)

    valid_o = register 0 valid_i

    data_o = boolToBit <$> (testBit <$> header <*> (fromIntegral <$> bitIndex))
    header = pack <$> (PHR <$> ms <*> fcs <*> dw <*> len)