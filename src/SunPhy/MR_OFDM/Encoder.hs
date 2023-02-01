module SunPhy.MR_OFDM.Encoder where

import Clash.Prelude hiding (B0, B1, foldr)
import Data.Foldable (foldr)
import Data.Functor ((<&>))
import SunPhy.AXI
import SunPhy.Bypass (bypass)
import SunPhy.MR_OFDM.Constants

data EncoderInput = EncoderInput
    { axiInput :: AxiForward Bit
    , axiOutputFeedback :: AxiBackward
    , reset :: Bit
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data EncoderOutput = EncoderOutput
    { axiInputFeedback :: AxiBackward
    , axiOutput :: AxiForward Bit
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data State
    = Buffering -- Waiting for input data
    | OutputA -- Output first value
    | OutputB -- Output second value
    deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
    deriving anyclass (NFDataX)

nextState :: State -> Bool -> Bool -> State
-- Input data has been read
nextState Buffering True _ = OutputA
-- First value has been outputed
nextState OutputA _ True = OutputB
-- Second value has been outputed
nextState OutputB _ True = Buffering
-- otherwise
nextState s _ _ = s

newM :: BitVector 6 -> Bit -> BitVector 6
newM m n = shiftR m 1 .|. (pack n ++# 0b00000)

encoder
    :: forall dom
     . HiddenClockResetEnable dom
    => Signal dom EncoderInput
    -> Signal dom EncoderOutput
encoder input = do
    axiInputFeedback <- do
        ready <- boolToBit <$> readyOut
        pure AxiBackward {..}

    axiOutput <- do
        valid <- boolToBit <$> validOut
        _data <- mux (state .==. pure OutputA) a b
        last <- boolToBit <$> (state .==. pure OutputB .&&. isLast)
        pure AxiForward {..}
    pure $ EncoderOutput {..}
    where
        readyOut = state .==. pure Buffering
        validOut = state ./=. pure Buffering
        axiInput = input <&> (.axiInput)

        masterWrite = readyOut .&&. (axiInput <&> (.valid)) .==. 1
        slaveWrite = (input <&> (.axiOutputFeedback) <&> (.ready)) .==. 1 .&&. validOut

        state =
            register Buffering $
                nextState
                    <$> state
                    <*> masterWrite
                    <*> slaveWrite
        m =
            register (0 :: BitVector 6)
                $ mux
                    ((input <&> (.reset)) .==. 1)
                    0
                $ mux
                    masterWrite
                    (newM <$> m <*> (axiInput <&> (._data)))
                    m

        mBits i = boolToBit <$> (testBit <$> m <*> i)

        a =
            register (0 :: Bit) $
                mux
                    masterWrite
                    (foldr (liftA2 xor) (axiInput <&> (._data)) (mBits <$> [0, 1, 3, 4]))
                    a

        b =
            register (0 :: Bit) $
                mux
                    masterWrite
                    (foldr (liftA2 xor) (axiInput <&> (._data)) (mBits <$> [0, 3, 4, 5]))
                    b

        isLast =
            register (False) $
                mux
                    masterWrite
                    (bitToBool <$> (axiInput <&> (.last)))
                    isLast