module SUN_PHY where

import Clash.Prelude
import PN9


{-# ANN sun_phy
  (Synthesize
    { t_name   = "sun_phy"
    , t_inputs =
        [ PortName "CLOCK_50"
        , PortName "reset"
        , PortName "enable"
        , PortName "input"
        ]
    , t_output = PortProduct "" [
        PortName "A"
        ,PortName "B"
      ]
    }
  )#-}

sun_phy
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Bit)
  -> Signal System (
    Bit -- A
    ,BitVector 7 -- B
    )
sun_phy clk rst en input =  bundle (a, b)
    where
        a = withClockResetEnable clk rst en $ pn9 input
        b = withClockResetEnable clk rst en $ pure 0