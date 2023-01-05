module SunPhy.MR_OFDM.LTF where

ltf
    :: forall dom
     . HiddenClockResetEnable dom
    => Signal dom Bit -- start_i
    -> Signal dom (Unsigned 3) -- OFDM Option
    -> Signal dom (Bit, Bit, Bit, Bit) -- data_o, valid_o, last_o
ltf start_i ofdmOption = bundle (data_o, valid_o, last_o)
  where

--
