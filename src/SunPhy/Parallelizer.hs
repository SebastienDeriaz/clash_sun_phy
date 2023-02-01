{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module SunPhy.Parallelizer where

import Clash.Prelude
import Data.Functor ((<&>))
import SunPhy.AXI
import Data.Proxy

-- A module to convert an AXI serial stream to a vector

data ParallelizerInput a = ParallelizerInput
    { axiInput :: AxiForward a
    , axiOutputFeedback :: AxiBackward 
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data ParallelizerOutput n a = ParallelizerOutput 
    { _data :: Vec n a
    , valid :: Bit
    , axiInputFeedback :: AxiBackward
    }
    deriving stock (Generic, Show)
    deriving anyclass (NFDataX)

instance (KnownNat n, Eq a) => Eq (ParallelizerOutput n a) where
    o1 == o2 = (o1.valid == o2.valid)
            && (o1.axiInputFeedback.ready == o2.axiInputFeedback.ready)
            && (o1.valid == 0 || o1._data == o2._data)

parallelizer
    :: forall n a dom
     . HiddenClockResetEnable dom
    => KnownNat n
    => NFDataX a
    => Num a
    => Signal dom (ParallelizerInput a)
    -> Signal dom (ParallelizerOutput n a)
parallelizer input = do
    _data <- _data
    valid <- negate <$> reading
    axiInputFeedback <- do
      ready <- reading
      pure AxiBackward {..}
    pure $ ParallelizerOutput {..}
    where

        masterWrite = ((input <&> (.axiInput) <&> (.valid)) .==. 1) .&&. (reading .==. 1)
        slaveWrite = ((input <&> (.axiOutputFeedback) <&> (.ready)) .==. 1) .&&. (reading .==. 0)

        reading = register 1 $ mux
          ((input <&> (.axiInput) <&> (.last)) .==. 1 .&&. masterWrite)
          (pure 0)
          $ mux
            (slaveWrite .&&. reading .==. 0)
            (pure 1)
            reading

        _data :: Signal dom (Vec n a)
        _data = register (repeat 0 :: Vec n a) $ mux
          masterWrite
          (replace
            <$> bitCounter
            <*> (input <&> (.axiInput) <&> (._data))
            <*> _data)
          _data

        n = fromIntegral $ natVal (Proxy :: Proxy n)
        
        bitCounter :: Signal dom (Index n)
        bitCounter = register (0 :: Index n) $ mux
          masterWrite
          (mux (bitCounter .==. n - 1)
            bitCounter
            (bitCounter + 1))
          $ mux
            slaveWrite
            0
            bitCounter
