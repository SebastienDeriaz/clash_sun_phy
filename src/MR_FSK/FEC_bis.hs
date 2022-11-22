{-# LANGUAGE ApplicativeDo #-}

module MR_FSK.FEC where

import Clash.Prelude hiding (foldr)
import Data.Foldable (foldr)

data EncoderType = NrnscEncoder | RscEncoder
  deriving (Generic, BitPack)

fec
  :: (HiddenClockResetEnable System)
  => EncoderType
  -> Signal System Bit
  -> Signal System (Bit, Bit)
fec et bv = (encoder et) bv
  where
    encoder NrnscEncoder = rscEncoder
    encoder RscEncoder = rscEncoder

  --   pureBv <- bv
  --   pure $ (pack pureBv) ++# (pack pureBv)
  -- where
  --   getEncoder NrnscEncoder = nrnscEncoder
  --   getEncoder RscEncoder = rscEncoder
    
  --   encoder = getEncoder <$> et
  --   bitPairs = encoder <*> bv
  --   catPairs = foldr (\(a,b) xs -> a:b:xs) ([])



rscEncoder
  :: HiddenClockResetEnable dom
  => Signal dom Bit
  -> Signal dom (Bit, Bit)
rscEncoder b = bundle (ui1, ui0)
  where
    (m0, m1, m2) = (register 0 nextM0, register 0 nextM1, register 0 nextM2)
    nextM0 = foldr (liftA2 xor) b [m0, m1, m2]
    nextM1 = m0
    nextM2 = m1
    ui0 = b
    ui1 = foldr (liftA2 xor) b [m1, m2]

nrnscEncoder
  :: forall dom
   . HiddenClockResetEnable dom
  => Signal dom Bit
  -> Signal dom (Bit, Bit)
nrnscEncoder b = bundle (ui1, ui0)
  where
    m0 = register 0 nextM0
    m1 = register 0 nextM1
    m2 = register 0 nextM2
    nextM0 = b
    nextM1 = m0
    nextM2 = m1
    ui0 = complement <$> foldr (liftA2 xor) b [m0, m1, m2]
    ui1 = complement <$> foldr (liftA2 xor) b [m1, m2]
