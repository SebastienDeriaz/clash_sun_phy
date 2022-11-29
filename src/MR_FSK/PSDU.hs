{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module MR_FSK.PSDU where

import Clash.Prelude

-- TODO : Add cross-domain FIFO

psdu
  :: forall dom . (HiddenClockResetEnable dom)
  => Signal dom Bit -- start
  -> Signal dom Bit -- busy
  -> Signal dom (Bit, Bit, Bit) -- end_o, valid_o, data_o
psdu start busy = bundle(end, valid, data_out)
  where
    -- bitIndex++ if !busy && (bitIndex == 0 or start)
    bitIndex = register (0 :: Unsigned 5) nextBitIndex
    nextBitIndex = mux (running .&&. (busy .==. 0)) (bitIndex + 1) bitIndex

    running = register (False :: Bool) nextRunning
    nextRunning = (nextBitIndex .>. 0) .||. (start .==. 1) --(busy .==. 0) .&&. ((nextBitIndex .>. 0) .||. (start .==. 1))

    --bitIndexMax :: Signal dom (Unsigned 4)
    bitIndexMax = pure (0b11111 :: Unsigned 5)

    psdu_data = pure (0b1111_0000_1111_0000_1111_0000_1111_0011 :: BitVector 32)

    -- End of the transmission
    -- end=0 when start == 1
    -- end=1 when bitIndex == 15
    end :: Signal dom Bit
    end = register (1 :: Bit) nextEnd

    valid = complement <$> end
    
    nextEnd = mux (bitToBool <$> end) (1 - start) (boolToBit <$> (bitIndex .==. bitIndexMax))
    data_out = boolToBit <$> (testBit <$> psdu_data <*> (fromIntegral <$> bitIndex))