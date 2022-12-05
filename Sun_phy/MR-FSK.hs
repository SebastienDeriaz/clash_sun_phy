module MR_FSK(mr_fsk) where
import Clash.Prelude
import Clash.Explicit.Testbench

{-# ANN mr_fsk
  (Synthesize
    { t_name   = "mr_fsk"
    , t_inputs =
        [ PortName "CLOCK_50"
        , PortName "SW_0"
        , PortName "valid"
        , PortName "data"
        ]
    , t_output = PortName "ready"
    }
  )#-}

mr_fsk
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (
        Bit -- valid
        ,Bit -- data
    )
    -> Signal System Bit
mr_fsk _ _ _ input = fst $ unbundle input