--import PN9
import Data.Bits


pn9_next :: Int -> Int
pn9_next a = (a `shiftR` 1) .|. next
  where
    next = ((a `shiftL` 3) `xor` (a `shiftL` 8)) .&. (bit 8)

pn9 :: Int -> [Int]
pn9 seed = tail [x `shiftR` 8 | x <- iterate pn9_next seed]


scrambler :: Int -> [Int] -> [Int]
scrambler seed message = [(fst x) `xor` (snd x) | x <- zip message (pn9 seed)]