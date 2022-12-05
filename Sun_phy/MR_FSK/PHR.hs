{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module MR_FSK.PHR where

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

phr
  :: forall dom . (HiddenClockResetEnable dom)
  => Signal dom Bit -- phrModeSwitch :: Bit
  -> Signal dom Bit -- phrFCS :: Bit
  -> Signal dom Bit -- phrDataWhitening :: Bit
  -> Signal dom (Unsigned 11) -- phrFrameLength :: Unsigned 11
  -> Signal dom Bit -- start
  -> Signal dom Bit -- busy
  -> Signal dom (Bit, Bit, Bit) -- end_o, valid_o, data_o
phr ms fcs dw len start busy = bundle(end, valid, data_out)
  where
    -- Index of the PHR bit (start at 0)
    -- bitIndex++ if !busy && (bitIndex == 0 or start)
    bitIndex = register (0 :: Unsigned 4) nextBitIndex
    nextBitIndex = mux (running .&&. (busy .==. 0)) (bitIndex + 1) bitIndex

    running = register (False :: Bool) nextRunning
    nextRunning = (nextBitIndex .>. 0) .||. (start .==. 1) --(busy .==. 0) .&&. ((nextBitIndex .>. 0) .||. (start .==. 1))

    --bitIndexMax :: Signal dom (Unsigned 4)
    bitIndexMax = pure (0b1111 :: Unsigned 4)

    -- End of the transmission
    -- end=0 when start == 1
    -- end=1 when bitIndex == 15
    end :: Signal dom Bit
    end = register (1 :: Bit) nextEnd

    valid = complement <$> end
    
    nextEnd = mux (bitToBool <$> end) (1 - start) (boolToBit <$> (bitIndex .==. bitIndexMax))
    data_out = boolToBit <$> (testBit <$> header <*> (fromIntegral <$> bitIndex))
    header = pack <$> (PHR <$> ms <*> fcs <*> dw <*> len)