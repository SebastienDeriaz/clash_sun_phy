module SunPhy.MR_OFDM.STF where

stf
    :: forall dom
     . HiddenClockResetEnable dom
    => Signal dom Bit -- start_i
    -> Signal dom (Unsigned 3) -- OFDM Option
    -> Signal dom (Bit, Bit, Bit, Bit) -- data_o, valid_o, last_o
stf start_i ofdmOption = bundle (data_o, valid_o, last_o)
  where

--
