module SunPhy.PN9 where

import Clash.Prelude
import Data.Bits

pn9Next :: BitVector 9 -> BitVector 9
pn9Next a = (a `shiftR` 1) .|. next
  where
    next = ((a `shiftL` 3) `xor` (a `shiftL` 8)) .&. bit 8


regInit = 0b1_1111_1111 :: BitVector 9

regNext :: BitVector 9 -> Bit -> Bit -> BitVector 9 -> BitVector 9
--      ┌seed
--      │ ┌next_i
--      │ │ ┌reset_i
--      │ │ │ ┌reg
--      │ │ │ │
regNext i _ 1 _ = i
regNext _ 1 0 x = pn9Next x
regNext _ 0 0 x = x


pn9 :: forall dom . HiddenClockResetEnable dom
    => Signal dom (BitVector 9) -- pn9_seed
    -> Signal dom Bit -- next_i
    -> Signal dom Bit -- reset_i
    -> Signal dom (Bit, BitVector 9) -- data_o
pn9 pn9_seed next_i reset_i = bundle (data_o, reg)
  where
    data_o = msb <$> (pn9Next <$> reg)
    reg = register regInit $ regNext
      <$> pn9_seed
      <*> next_i
      <*> reset_i
      <*> reg