{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module MR_FSK.PHR where

import Clash.Prelude

data PHR = PHR
  { phrModeSwitch :: Bit
  , phrFCS :: Bit
  , phrDataWhitening :: Bit
  , phrFrameLength :: Unsigned 11
  }

instance BitPack PHR where
  type BitSize PHR = 16
  pack PHR{..} =
    (pack phrModeSwitch)
      ++# (0b00 :: BitVector 2)
      ++# (pack phrFCS)
      ++# (pack phrDataWhitening)
      ++# (pack phrFrameLength)

  unpack $(bitPattern "a..bcddddddddddd") =
    PHR
      { phrModeSwitch = unpack a
      , phrFCS = unpack b
      , phrDataWhitening = unpack c
      , phrFrameLength = unpack ddddddddddd
      }

phr
  :: forall dom . (HiddenClockResetEnable dom)
  => Signal dom Bit -- phrModeSwitch :: Bit
  -> Signal dom Bit -- phrFCS :: Bit
  -> Signal dom Bit -- phrDataWhitening :: Bit
  -> Signal dom (Unsigned 11) -- phrFrameLength :: Unsigned 11
  -> Signal dom Bit -- start
  -> Signal dom Bit -- busy_i
  -> Signal dom (Bit, Bit, Bit) -- end_o, valid_o, data_o
phr ms fcs dw len start busy_i = bundle(end_o, valid_o, data_o)
  where
    end_o = pure 0
    valid_o = pure 0
    data_o = boolToBit <$> (testBit <$> header <*> (fromIntegral <$> bitIndex))
    bitIndex = register (0 :: Unsigned 4) (bitIndex + 1) 
    header = pack <$> (PHR <$> ms <*> fcs <*> dw <*> len)