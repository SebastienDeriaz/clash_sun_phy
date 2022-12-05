{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}

module Sun_phy.MR_FSK.FEC_B where

import Clash.Prelude hiding (foldr)
import Data.Foldable (foldr)




-- State machine
data FecEncoderState = Idle
           | Out1
           | Out1W -- Out1 wait
           | Out0
           | Out0W -- Out0 wait
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX




nextState :: FecEncoderState -> Bit -> Bit -> FecEncoderState 
--        State ready_i valid_i
-- Idl
nextState Idle  1 1 = Out1
nextState Idle  _ _ = Idle
-- Out1
nextState Out1  1 _ = Out0
nextState Out1  0 _ = Out1
-- Out0
nextState Out0  1 _ = Idle
nextState Out0  0 _ = Out0

data_out :: FecEncoderState -> Bit -> Bit -> Bit
data_out Out0  ui0 _   = ui0
data_out Out1  _   ui1 = ui1
data_out Idle  _   _   = 0

ready_out :: FecEncoderState -> Bit
ready_out Idle  = 1
ready_out _     = 0


valid_out :: FecEncoderState -> Bit
valid_out Idle  = 0
valid_out Out0  = 1
valid_out Out1  = 1

fecEncoder
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom Bit -- phyFSKFECScheme
  -> Signal dom Bit -- ready_i
  -> Signal dom Bit -- valid_i
  -> Signal dom Bit -- Input
  -> Signal dom (BitVector 3, Bit, Bit, Bit) -- m, ready_o, data_out, valid_o
fecEncoder phyFSKFECScheme ready_i valid_i inp = bundle (m, ready_o, data_o, valid_o)
  where
    state = register (Idle) (nextState <$> state <*> ready_i <*> valid_i)

    (m, ui0, ui1) = unbundle $ mux (bitToBool <$> phyFSKFECScheme) rsc nrnsc

    en = boolToBit <$> (state .==. (pure Idle) .&&. valid_i .==. (pure 1))
    rsc = rscEncoder en inp
    nrnsc = nrnscEncoder en inp

    -- Outputs
    ready_o = ready_out <$> state
    data_o = data_out <$> state <*> ui0 <*> ui1
    valid_o = valid_out <$> state



pack3 :: Bit -> Bit -> Bit -> BitVector 3
pack3 a b c = (pack a) ++# (pack b) ++# (pack c)


-- RSC Encoder
rscEncoder
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom Bit -- valid_i
  -> Signal dom Bit -- Input
  -> Signal dom (BitVector 3, Bit, Bit) -- m, ui1, ui0
rscEncoder en b = bundle (m, ui0, ui1)
  where
    m = pack3 <$> m2 <*> m1 <*> m0
    enb = bitToBool <$> en
    (m0, m1, m2) = (register 0 nextM0, register 0 nextM1, register 0 nextM2)
    nextM0 = mux enb (foldr (liftA2 xor) b [m0, m1, m2]) m0
    nextM1 = mux enb m0 m1
    nextM2 = mux enb m1 m2
    ui0 = register (0 :: Bit) nextui0 
    nextui0 = mux enb (b) ui0
    ui1 = register (0 :: Bit) nextui1
    -- Instead of remaking the big xor, use nextM0 directly
    nextui1 = mux enb (foldr (liftA2 xor) nextM0 [m1, m2]) ui1

-- NRNSC Encoder
nrnscEncoder
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom Bit -- valid_i
  -> Signal dom Bit -- input
  -> Signal dom (BitVector 3, Bit, Bit) -- m, ui0, ui1
nrnscEncoder en b = bundle (m, ui0, ui1)
  where
    m = pack3 <$> m2 <*> m1 <*> m0
    enb = bitToBool <$> en
    m0 = register 0 nextM0
    m1 = register 0 nextM1
    m2 = register 0 nextM2
    nextM0 = mux enb b m0
    nextM1 = mux enb m0 m1
    nextM2 = mux enb m1 m2
    ui0 = register (0 :: Bit) nextui0
    nextui0 = mux enb (complement <$> foldr (liftA2 xor) b [m0, m1, m2]) ui0
    ui1 = register (0 :: Bit) nextui1
    nextui1 = mux enb (complement <$> foldr (liftA2 xor) b [m1, m2]) ui1