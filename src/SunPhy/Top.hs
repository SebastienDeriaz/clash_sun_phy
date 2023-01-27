module SunPhy.Top where

import Clash.Prelude

import SunPhy.MR_FSK.FEC
import SunPhy.MR_FSK.PHR
import SunPhy.MR_FSK.PSDU

ma acc (x, y) = acc + x * y

macT acc (x, y) = (acc', o)
 where
  acc' = ma acc (x, y)
  o = acc

mac inp = mealy macT 0 inp

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Signed 9, Signed 9) ->
  Signal System (Signed 9)
topEntity = exposeClockResetEnable mac
