{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module SunPhy.MR_OFDM.PHR where

import Clash.Prelude
import Data.Functor ((<&>))
import SunPhy.AXI
import SunPhy.MR_OFDM.Constants

-- PHR is 36 bits long, the rest if the PHR is bigger is 0s
-- 0-4   : RA4 - RA0   Rate
-- 5     : R           Reserved
-- 6-16  : L10 - L0    Frame Length
-- 17-18 : R1  - R0    Reserved
-- 19-20 : S1  - S0    Scrambler
-- 21    : R           Reserved
-- 22-29 : H7  - H0    HCS
-- 30-35 : T5  - T0    Tail
--
-- Since the Tail is always 0s, the PHR is technically 30 bits + padding
-- The maximum (total) PHR length is 72 bits

data PHRInput = PHRInput
    { mcs :: MCS
    , frameLength :: Unsigned 11
    , scramblerSeed :: Unsigned 2
    , phrLength :: Unsigned 7
    , start :: Bit
    , axiOutputFeedback :: AxiBackward
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data PHROutput = PHROutput
    { axiOutput :: AxiForward Bit
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

reverseBits :: KnownNat n => BitVector n -> BitVector n
reverseBits a = v2bv $ reverse $ bv2v a

data PHR = PHR
    { mcs :: Unsigned 5
    , frameLength :: Unsigned 11
    , scramblerSeed :: Unsigned 2
    }

instance BitPack PHR where
    type BitSize PHR = 22
    pack PHR {..} =
        (0b0 :: BitVector 1)
            ++# reverseBits (pack scramblerSeed)
            ++# (0b00 :: BitVector 2)
            ++# reverseBits (pack frameLength)
            ++# (0b0 :: BitVector 1)
            ++# reverseBits (pack mcs)

    unpack $(bitPattern "aaaaa.bbbbbbbbbbb..cc.") =
        PHR
            { mcs = unpack aaaaa
            , frameLength = unpack bbbbbbbbbbb
            , scramblerSeed = unpack cc
            }

data State
    = Idle
    | Running
    deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
    deriving anyclass (NFDataX)

nextBitIndex :: State -> Bit -> Unsigned 4 -> Unsigned 4
--           ┌state
--           │       ┌start_i
--           │       │ ┌ready_o
-- Idle      │       │ │
nextBitIndex Running 0 x = x
nextBitIndex _ 1 x = x + 1
nextBitIndex _ _ x = 0

serialHCS
    :: forall dom
     . (HiddenClockResetEnable dom)
    => Signal dom Bit -- start_i
    -> Signal dom Bit -- data_i
    -> Signal dom (BitVector 8) -- HCS out
serialHCS start_i data_i = output
    where
        nextB :: Bit -> Bit -> BitVector 8 -> BitVector 8
        --    ┌start_i
        --    │ ┌data_i
        --    │ │ ┌b
        --    │ │ │
        nextB 0 _ x = x
        nextB 1 d x = shiftL x 1 `xor` (0b0000_0111 * (resize (pack d) `xor` resize msb))
            where
                msb = shiftR x 7

        -- msb = (B & (1<<7)) >> 7
        -- return (B << 1 ^ (0b111 * (inp ^ msb))) & 0b1111_1111

        b :: Signal dom (BitVector 8)
        b = register 0 $ nextB <$> start_i <*> data_i <*> b

        a :: Signal dom (BitVector 8)
        a = pure 0b0100_1011

        output = complement <$> (xor <$> b <*> a)

phr
    :: forall dom
     . (HiddenClockResetEnable dom)
    => Signal dom PHRInput
    -> Signal dom PHROutput
phr input = do
    -- Output
    axiOutput <- do
        valid <- valid_o
        _data <- data_o
        last <- bitCounterEnd
        pure AxiForward {..}
    pure PHROutput {..}
    where
        slaveWrite = (input <&> (.axiOutputFeedback) <&> (.ready)) * valid_o

        nextState :: State -> Bit -> Bit -> Bit -> State
        --        ┌state
        --        │       ┌counterEnd
        --        │       │ ┌start_i
        --        │       │ │ ┌slaveWrite
        -- Idle   │       │ │ │
        nextState Idle _ 1 _ = Running
        nextState Idle _ _ _ = Idle
        nextState Running 1 _ 1 = Idle
        nextState Running _ _ _ = Running

        state =
            register Idle $
                nextState
                    <$> state
                    <*> bitCounterEnd
                    <*> (input <&> (.start))
                    <*> slaveWrite

        nextBitCounter :: State -> Bit -> Unsigned 7 -> Unsigned 7
        -- state, slaveWrite
        nextBitCounter Idle _ _ = 0
        nextBitCounter Running 1 x = x + 1
        nextBitCounter _ _ x = x

        bitCounter = register (0 :: Unsigned 7) (nextBitCounter <$> state <*> slaveWrite <*> bitCounter)

        bitCounterEnd = boolToBit <$> (bitCounter .==. (input <&> (.phrLength)))

        valid_o = boolToBit <$> (state .==. pure Running)

        dataOut :: State -> PHR -> BitVector 8 -> Unsigned 7 -> Bit
        -- state, phr, hcs, index
        dataOut Running p h i
            -- PHR
            | i < 22 = boolToBit $ testBit (pack p) (fromIntegral i) -- TODO : Find a way to define the 22 in a cleaner way
            -- HCS
            | i < 30 = boolToBit $ testBit (h) hindex
            -- Padding
            | otherwise = 0
            where
                hindex = fromIntegral (29 - i)
        dataOut _ _ _ _ = 0

        data_o = dataOut <$> state <*> phr <*> hcs <*> bitCounter

        -- TODO : Check if it's possible to replace "data_o" with "phr" and
        -- thus only declare data in the output do
        serialHCS_start_i = boolToBit <$> (bitCounter .<. 22 .&&. slaveWrite .==. 1)
        hcs = serialHCS serialHCS_start_i data_o

        phr =
            PHR
                <$> (resize <$> (input <&> (.mcs)))
                <*> (input <&> (.frameLength))
                <*> (input <&> (.scramblerSeed))
