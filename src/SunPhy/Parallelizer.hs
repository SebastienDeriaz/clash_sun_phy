{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module SunPhy.Parallelizer where

import Clash.Prelude
import Data.Functor ((<&>))

-- A module to convert an AXI serial stream to a vector

data ParallelizerInput a = ParallelizerInput
    { ready :: Bit
    , valid :: Bit
    , _data :: a
    , last :: Bit
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data ParallelizerOutput n a = ParallelizerOutput 
    { _data :: Vec n a
    , valid :: Bit
    , ready :: Bit
    }
    deriving stock (Generic, Show)
    deriving anyclass (NFDataX)

instance (KnownNat n, Eq a) => Eq (ParallelizerOutput n a) where
    o1 == o2 = (o1.valid == o2.valid)
            && (o1.ready == o2.ready)
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
    ready <- reading
    valid <- negate <$> reading
    pure $ ParallelizerOutput {..}
    where

        masterWrite = ((input <&> (.valid)) .==. 1) .&&. (reading .==. 1)
        slaveWrite = ((input <&> (.ready)) .==. 1) .&&. (reading .==. 0)

        reading = register 1 $ mux
          ((input <&> (.last)) .==. 1 .&&. masterWrite)
          (pure 0)
          $ mux
            (slaveWrite .&&. reading .==. 0)
            (pure 1)
            reading

        _data :: Signal dom (Vec n a)
        _data = register (repeat (0) :: Vec n a) $ mux
          masterWrite
          (replace
            <$> bitCounter
            <*> (input <&> (._data))
            <*> _data)
          _data

        bitCounter :: Signal dom (Index n)
        bitCounter = register (0 :: Index n) $ mux
          masterWrite
          (bitCounter + 1)
          $ mux
            slaveWrite
            0
            bitCounter
