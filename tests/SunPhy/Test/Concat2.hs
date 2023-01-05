module SunPhy.Test.Concat2 where

import Clash.Explicit.Testbench
import Clash.Prelude
import SunPhy.Concat2
import Clash.WaveDrom

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System AxiForward -- a
    -> Signal System AxiForward -- b
    -> Signal System Bit -- ready
    -> Signal System (Bit, Bit, AxiForward)
topEntity clk rst en = exposeClockResetEnable concat2 clk rst en



testBench :: Signal System Bool
testBench = done
  where
    testInput = stimuliGenerator clk rst (2 :> 3 :> (-2) :> 8 :> Nil)
    expectedOutput = outputVerifier' clk rst (4 :> 12 :> 1 :> 20 :> Nil)
    done = expectedOutput (topEntity clk rst enableGen testInput)
    clk = tbSystemClockGen (not <$> done)
    rst = systemResetGen
