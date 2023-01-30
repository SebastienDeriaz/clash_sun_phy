module SunPhy.MR_OFDM.Modulator where

import Clash.Prelude
import SunPhy.MR_OFDM.Constants

-- The modulator takes a bitstream (data_i) and converts it into a subcarrier stream based on
-- MCS value
--
--                  ┏━━━━━━━━━━━━━━━┓
--                  ┃   Modulator   ┃
--             MCS  ┣>             <┨  ready_i
--         ready_o <┨               ┠> valid_o
--         valid_i  ┠>              ┣> data_o
--          data_i  ┠>              ┠> last_o
--          last_i  ┠>              ┃
--                  ┃               ┃
--                  ┗━━━━━━━━━━━━━━━┛


-- data State = Idle
--            | Piped
--            | Buffer
--   deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
--   deriving anyclass NFDataX

singleBPSK :: Bit -> MFixed
singleBPSK 0 = -1.0
singleBPSK 1 = 1.0

applyModulation :: Modulation -> Bit -> Bit -> Bit -> Bit -> Subcarrier
--              ┌modulation        16-QAM  QPSK  BPSK
--              │     ┌data_i      ->b3    ->b1  ->b0
--              │     │ ┌m[-1]     ->b2    ->b0
--              │     │ │ ┌m[-2]   ->b1
--              │     │ │ │ ┌m[-3] ->b0
-- BPSK         │     │ │ │ │
applyModulation BPSK  a _ _ _ = (singleBPSK a, 0.0)
-- QPSK
-- 00 -> -1-1j
-- 01 -> -1+1j
-- 10 -> +1-1j
-- 11 -> +1+1j
applyModulation QPSK  a b _ _ = (singleBPSK b, singleBPSK a)
-- QAM16
applyModulation QAM16 a b c d = ((2.0 - (singleBPSK c)) * (singleBPSK d), (2.0 - (singleBPSK a)) * (singleBPSK b))

modulator
    :: forall dom . HiddenClockResetEnable dom
    => Signal dom MCS
    -> Signal dom Bit -- valid_i
    -> Signal dom Bit -- data_i
    -> Signal dom Bit -- last_i
    -> Signal dom Bit -- ready_i
    -> Signal dom (Bit, Bit, Subcarrier, Bit) -- valid_o, data_o, last_o, ready_o
modulator mcs valid_i data_i last_i ready_i = bundle(ready_o, valid_o, data_o, last_o)
  where    
    slaveWrite = ready_i * valid_o
    
    masterWrite = ready_o * valid_i


    nextDataCounter :: Bit -> Bit -> Bit -> Unsigned 3 -> Unsigned 3
    -- masterWrite, slaveWrite
    nextDataCounter 1 0 0 x = x + 1
    nextDataCounter _ 1 1 _ = 0
    nextDataCounter _ _ _ x = x

    dataCounter = register (0 :: Unsigned 3) $ nextDataCounter
      <$> masterWrite
      <*> slaveWrite
      <*> full
      <*> dataCounter

    full = boolToBit <$> (dataCounter .==. ((nbpsc_mod <$> _modulation)))

    -- Holds the necessary amount of previous data bits for the modulation
    m0 = register (0 :: Bit) (mux (bitToBool <$> masterWrite) data_i m0)
    -- data_i[-1]
    m1 = register (0 :: Bit) (mux (bitToBool <$> masterWrite) m0 m1)
    -- data_i[-2]
    m2 = register (0 :: Bit) (mux (bitToBool <$> masterWrite) m1 m2)
    -- data_i[-3]
    m3 = register (0 :: Bit) (mux (bitToBool <$> masterWrite) m2 m3)


    _modulation = mcsModulation <$> mcs
    
    data_o = applyModulation <$> _modulation <*> m0 <*> m1 <*> m2 <*> m3
    valid_o = boolToBit <$> (full .==. 1)
    last_o = boolToBit <$> (lastFlag .==. 1 .&&. full .==. 1)

    lastFlag = register (0 :: Bit) (nextLastFlag <$> last_i <*> masterWrite <*> slaveWrite <*> lastFlag)

    nextLastFlag :: Bit -> Bit -> Bit -> Bit -> Bit
    --           ┌last_i
    --           │ ┌masterWrite
    --           │ │ ┌slaveWrite
    --           │ │ │ ┌lastFlag
    -- Idle      │ │ │ │
    nextLastFlag 1 1 _ _ = 1
    nextLastFlag _ _ 1 1 = 0
    nextLastFlag _ _ _ x = x

    -- Outputs
    ready_i_reg = register (0 :: Bit) ready_i
    ready_o = boolToBit <$> (full .==. 0)





    
