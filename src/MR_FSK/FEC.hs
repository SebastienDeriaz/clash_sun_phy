{-# LANGUAGE ApplicativeDo #-}

module MR_FSK.FEC where

import Clash.Prelude hiding (foldr)
import Data.Foldable (foldr)

data EncoderType = NrnscEncoder | RscEncoder
  deriving (Generic, BitPack, Eq)

singleFec
  :: forall dom
   . HiddenClockResetEnable dom
  => Signal dom EncoderType
  -> Signal dom Bit
  -> Signal dom (Bit, Bit)
singleFec et b = mux bo rsc nrnsc
  where
    bo = et .==. pure RscEncoder

    rsc = rscEncoder b
    nrnsc = nrnscEncoder b

multiFec
  :: (HiddenClockResetEnable dom, KnownNat n)
  => Signal dom EncoderType
  -> Signal dom (BitVector n)
  -> Signal dom (BitVector (n*2))
multiFec = undefined

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
