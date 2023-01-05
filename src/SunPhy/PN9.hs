module SunPhy.PN9 where

import Clash.Prelude
import Data.Bits

pn9_next :: Bits a => a -> a
pn9_next a = (a `shiftR` 1) .|. next
  where
    next = ((a `shiftL` 3) `xor` (a `shiftL` 8)) .&. (bit 8)

-- reg_init = 511 :: BitVector 9

reg_next :: BitVector 9 -> Bit -> Bit -> BitVector 9 -> BitVector 9
-- seed next_i reset_i reg
reg_next i _ 1 _ = i
reg_next _ 1 0 x = pn9_next x
reg_next _ 0 0 x = x

pn9
    :: forall dom
     . HiddenClockResetEnable dom
    => Signal dom (BitVector 9) -- pn9_seed
    -> Signal dom Bit -- next_i
    -> Signal dom Bit -- reset_i
    -> Signal dom Bit -- data_o
pn9 pn9_seed next_i reset_i = data_o
  where
    data_o = msb <$> regNext
    reg = register (511) regNext
    regNext = reg_next <$> pn9_seed <*> next_i <*> reset_i <*> reg
