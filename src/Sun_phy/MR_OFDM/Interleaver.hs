module Sun_phy.MR_OFDM.Interleaver where

import Clash.Prelude
import Sun_phy.MR_OFDM.Constants
import Sun_phy.Bypass (bypass)


-- State machine
data State = Idle
            | Write
            | Read
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX



nextState :: State -> Bit -> Bit -> State
--        ┌state
--        │     ┌valid_i 
--        │     │ ┌counterEnd
-- Idle   │     │ │
nextState Idle  0 _ = Idle
nextState Idle  1 _ = Write
-- Write
nextState Write _ 0 = Write
nextState Write _ 1 = Read
-- Read
nextState Read  _ 0 = Read
nextState Read  _ 1 = Idle


nextCounter :: State -> Bit -> Bit -> Bit -> Bit -> Unsigned 9 -> Unsigned 9
--          ┌state
--          │     ┌valid_i 
--          │     │ ┌ready_o
--          │     │ │ ┌ready_i
--          │     │ │ │ ┌counterEnd      
--          │     │ │ │ │ ┌counter              
-- Idle     │     │ │ │ │ │
nextCounter Idle  1 1 _ _ _ = 1
nextCounter Idle  _ _ _ _ _ = 0
nextCounter Write _ _ 1 _ _ = 0
nextCounter Write 1 1 _ _ x = x + 1
nextCounter Write _ _ _ _ x = x
nextCounter Read  _ _ 1 _ _ = 0
nextCounter Read  _ _ _ 1 x = x + 1
nextCounter Read  _ _ _ 0 x = x

nextBuffer :: State -> Bit -> Bit -> Unsigned 9 -> Bit -> Bit -> BitVector 384 -> BitVector 384
--         ┌state
--         │     ┌masterWrite 
--         │     │ ┌slaveWrite
--         │     │ │ ┌counter
--         │     │ │ │ ┌counterEnd      
--         │     │ │ │ │ ┌data_i              
--         │     │ │ │ │ │ ┌buffer              
-- Idle    │     │ │ │ │ │ │
nextBuffer Idle  1 _ i _ 1 buffer = setBit buffer (fromEnum i)
nextBuffer Write 1 _ i _ 1 buffer = setBit buffer (fromEnum i)
nextBuffer Read  _ 1 _ 1 _ buffer = 0
nextBuffer _     _ _ _ _ _ buffer = buffer

ready :: State -> Bit
ready Idle = 1
ready Write = 1
ready Read = 0

nextLastStore :: State -> Bit -> Bit -> Bit
--            ┌state
--            │     ┌last_i 
--            │     │ ┌last_store
nextLastStore Write 1 _ = 1
nextLastStore Idle  _ _ = 0
nextLastStore _     _ x = x

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
    sf    = case int of
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
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom Bit -- bypass
  -> Signal dom (Unsigned 3) -- MCS
  -> Signal dom (Unsigned 3) -- OFDM option
  -> Signal dom Bit -- phyOFDMInterleaving
  -> Signal dom Bit -- valid_i
  -> Signal dom Bit -- data_i
  -> Signal dom Bit -- last_i
  -> Signal dom Bit -- ready_i
  -> Signal dom (Bit, Bit, Bit, Bit, Unsigned 9) -- ready_o, data_o, valid_o, last_o, test
interleaver bp mcs ofdmOption _phyOFDMInterleaving valid_i data_i last_i ready_i = bundle(ready_o, data_o, valid_o, last_o, idx)
  where
    _sf = frequencySpreading <$> mcs
    _nbpsc = nbpsc <$> mcs
    _nfft = nfft <$> ofdmOption

    _n_row :: Signal dom (Unsigned 4)
    _n_row = n_row <$> _sf

    _ncbps = ncbps <$> _phyOFDMInterleaving <*> _nfft <*> _nbpsc <*> _sf
    _s :: Signal dom (Unsigned 2)
    _s = s <$> _nbpsc

    _j = j <$> _ncbps <*> _n_row <*> counter <*> _s
    idx = i <$> _ncbps <*> _n_row <*> _j <*> _s

    counter = register (0 :: Unsigned 9) $ nextCounter <$> state <*> valid_i <*> ready_o <*> (boolToBit <$> counterEnd) <*> ready_i <*> counter
    counterEnd = counter .==. (_ncbps - 1)

    lastStore = register (0 :: Bit) (nextLastStore <$> state <*> last_i <*> lastStore)

    slaveWrite = boolToBit <$> ((bitToBool <$> ready_i) .&&. (bitToBool <$> valid_out))
    masterWrite = boolToBit <$> ((bitToBool <$> ready_out) .&&. (bitToBool <$> valid_i))

    state = register Idle $ nextState <$> state <*> valid_i <*> (boolToBit <$> counterEnd)

    buffer = register (0 :: BitVector 384) $ nextBuffer <$> state <*> masterWrite <*> slaveWrite <*> idx <*> (boolToBit <$> counterEnd) <*>  data_i <*> buffer

    output = boolToBit <$> (testBit <$> buffer <*> (fromEnum <$> counter))

    ready_out = ready <$> state
    valid_out = boolToBit <$> (state .==. pure Read)
    -- Bypass
    (bypassValid_o, bypassData_o, bypassLast_o, bypassReady_o) = unbundle $ bypass valid_i data_i last_i ready_i
    -- Outputs
    bpb = bitToBool <$> bp
    ready_o = mux bpb bypassReady_o ready_out
    valid_o = mux bpb bypassValid_o valid_out
    data_o  = mux bpb bypassData_o (mux (state .==. (pure Read)) output (pure 0))
    last_o  = mux bpb bypassLast_o $ boolToBit <$> (state .==. pure Read .&&. (lastStore .==. 1) .&&. counterEnd)