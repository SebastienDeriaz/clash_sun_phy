{-# LANGUAGE FlexibleInstances #-}

module SunPhy.MR_OFDM.Interleaver where

import Clash.Prelude
import Data.Functor ((<&>))
import SunPhy.AXI
import SunPhy.Bypass (bypass)
import SunPhy.MR_OFDM.Constants

data InterleaverInput = InterleaverInput
    { ofdmOption :: OFDM_Option
    , mcs :: MCS
    , phyOFDMInterleaving :: Bit
    , axiInput :: AxiForward Bit
    , axiOutputFeedback :: AxiBackward
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data InterleaverOutput = InterleaverOutput
    { axiInputFeedback :: AxiBackward
    , axiOutput :: AxiForward Bit
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

-- State machine
data InterleaverState
    = Buffering
    | Output
    deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
    deriving anyclass (NFDataX)

nextState :: InterleaverState -> Bit -> Bit -> Bit -> InterleaverState
--        ┌state
--        │         ┌valid_i
--        │         │ ┌counterEnd
--        │         │ │ ┌ready_i
--        │         │ │ │
nextState Buffering 1 1 _ = Output
nextState Output _ 1 1 = Buffering
-- otherwise
nextState x _ _ _ = x

nextCounter :: InterleaverState -> Bit -> Bit -> Bit -> Bit -> Unsigned 9 -> Unsigned 9
--          ┌state
--          │         ┌valid_i
--          │         │ ┌ready_o
--          │         │ │ ┌counterEnd
--          │         │ │ │ ┌ready_i
--          │         │ │ │ │ ┌counter
-- Idle     │         │ │ │ │ │
nextCounter Buffering 1 _ 1 _ _ = 0
nextCounter Buffering 1 _ 0 _ x = x + 1
nextCounter Output _ _ 1 1 _ = 0
nextCounter Output _ _ 0 1 x = x + 1
nextCounter _ _ _ _ _ x = x

nextBuffer :: InterleaverState -> Bit -> Bit -> Unsigned 9 -> Bit -> Bit -> BitVector 384 -> BitVector 384
--         ┌state
--         │         ┌masterWrite
--         │         │ ┌slaveWrite
--         │         │ │ ┌counter
--         │         │ │ │ ┌counterEnd
--         │         │ │ │ │ ┌data_i
--         │         │ │ │ │ │ ┌buffer
-- Idle    │         │ │ │ │ │ │
nextBuffer Buffering 1 _ i _ 1 buffer = setBit buffer (fromEnum i)
nextBuffer Output _ 1 _ 1 _ buffer = 0
nextBuffer _ _ _ _ _ _ buffer = buffer

ready' :: InterleaverState -> Bit
ready' Buffering = 1
ready' Output = 0

nextLastStore :: InterleaverState -> Bit -> Bit -> Bit
--            ┌state
--            │         ┌last_i
--            │         │ ┌last_store
nextLastStore Buffering 1 _ = 1
nextLastStore Output _ _ = 0
nextLastStore _ _ x = x

-- The maximum value for ncbps could happen when
-- N_FFT = 128, N_bpsc = 4, phyOFDMInterleaving = 1
-- Therefore a maximum of 384 ()
ncbps :: Bit -> Unsigned 8 -> Unsigned 3 -> Unsigned 3 -> Unsigned 9
-- phyOFDMInterleaving N_fft N_bpsc SF
-- Divide first to avoid having to deal with big numbers (or overflows)
-- This can always be done because nfft is 16 at minimum and sf is 4 at maximum
ncbps int _nfft _nbpsc _sf = nfft `div` 4 `div` sf * nbpsc * 3
    where
        nfft = resize _nfft :: Unsigned 9
        nbpsc = resize _nbpsc :: Unsigned 9
        sf = case int of
            0 -> resize _sf :: Unsigned 9
            1 -> 1 :: Unsigned 9

-- S as a function of N_bpsc
s :: Unsigned 3 -> Unsigned 2
s 4 = 2
s _ = 1

i :: Unsigned 9 -> Unsigned 4 -> Unsigned 9 -> Unsigned 2 -> Unsigned 9
i ncbps _nrow k _s = ncbps `div` nrow * (k `mod` nrow) + k `div` nrow
    where
        nrow = resize _nrow :: Unsigned 9
        s = resize _s :: Unsigned 9

j :: Unsigned 9 -> Unsigned 4 -> Unsigned 9 -> Unsigned 2 -> Unsigned 9
j ncbps _nrow k _s = s * (k `div` s) + (k + ncbps - (resize a :: Unsigned 9)) `mod` s
    where
        nrow = resize _nrow :: Unsigned 9
        s = resize _s :: Unsigned 9

        a = (resize nrow :: Unsigned 12) * (resize k :: Unsigned 12) `div` (resize ncbps :: Unsigned 12)

n_row :: Unsigned 3 -> Unsigned 4
-- sf
n_row 1 = 12
n_row 2 = 6
n_row 4 = 3

interleaver
    :: forall dom
     . HiddenClockResetEnable dom
    => Signal dom InterleaverInput
    -> Signal dom InterleaverOutput
interleaver input = do
    -- Output
    axiOutput <- do
        valid <- valid_o
        _data <- boolToBit <$> (testBit <$> buffer <*> (fromEnum <$> counter))
        last <- boolToBit <$> (state .==. pure Output .&&. (lastStore .==. 1) .&&. counterEnd)
        pure AxiForward {..}
    -- Input feedback
    axiInputFeedback <- do
        ready <- ready_o
        pure AxiBackward {..}
    pure InterleaverOutput {..}
    where
        -- valid_i data_i last_i ready_i = bundle (ready_o, valid_o, data_o, last_o)

        valid_i = input <&> (.axiInput) <&> (.valid)
        ready_i = input <&> (.axiOutputFeedback) <&> (.ready)
        _sf :: Signal dom (Unsigned 3)
        _sf = frequencySpreading <$> (input <&> (.mcs))

        _nbpsc = nbpsc <$> (input <&> (.mcs))
        _n_fft = n_fft <$> (input <&> (.ofdmOption))

        _n_row :: Signal dom (Unsigned 4)
        _n_row = n_row <$> _sf

        _ncbps =
            ncbps
                <$> (input <&> (.phyOFDMInterleaving))
                <*> _n_fft
                <*> _nbpsc
                <*> _sf
        _s :: Signal dom (Unsigned 2)
        _s = s <$> _nbpsc

        _j = j <$> _ncbps <*> _n_row <*> counter <*> _s
        idx = i <$> _ncbps <*> _n_row <*> _j <*> _s

        counter = register (0 :: Unsigned 9) $ nextCounter <$> state <*> valid_i <*> ready_o <*> (boolToBit <$> counterEnd) <*> ready_i <*> counter
        counterEnd = counter .==. (_ncbps - 1)

        lastStore =
            register (0 :: Bit) $
                nextLastStore
                    <$> state
                    <*> (input <&> (.axiInput) <&> (.last))
                    <*> lastStore

        slaveWrite = boolToBit <$> ((bitToBool <$> ready_i) .&&. (bitToBool <$> valid_o))
        masterWrite = boolToBit <$> ((bitToBool <$> ready_o) .&&. (bitToBool <$> valid_i))

        state = register Buffering $ nextState <$> state <*> valid_i <*> (boolToBit <$> counterEnd) <*> ready_i

        buffer =
            register (0 :: BitVector 384) $
                nextBuffer
                    <$> state
                    <*> masterWrite
                    <*> slaveWrite
                    <*> idx
                    <*> (boolToBit <$> counterEnd)
                    <*> (input <&> (.axiInput) <&> (._data))
                    <*> buffer

        ready_o = ready' <$> state
        valid_o = boolToBit <$> (state .==. pure Output)
