{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}

module MR_FSK.FEC where

import Clash.Prelude hiding (foldr)
import Data.Foldable (foldr)


tailVec :: (BitVector 3) -> Bit -> BitVector 3
--       m     phyFSKFECScheme
tailVec _     0 = 0b000
tailVec 0b000 _ = 0b000
tailVec 0b001 _ = 0b001
tailVec 0b010 _ = 0b011
tailVec 0b011 _ = 0b010
tailVec 0b100 _ = 0b111
tailVec 0b101 _ = 0b110
tailVec 0b110 _ = 0b100
tailVec 0b111 _ = 0b101




-- State machine
data State = Idle
           | Start 
           | Read
           | RdW -- Read Wait
           | RdE -- Read End
           | Tail
           | TailW -- Tail wait
           | TailE -- Tail End
           | Pad
           | PadW  -- Pad Wait
           | PadE  -- Pad end
           | End
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX

-- state ready_i valid_i last_i tailCounterEnd padCounterEnd
f_nextState :: State -> Bit -> Bit -> Bit -> Bit -> Bit -> State
-- Idle
--f_nextState _ _ _ _ _ _ = Idle
f_nextState Idle  1 1 _ _ _ = Start -- ready_i=valid_i=1
f_nextState Idle  _ _ _ _ _ = Idle
-- Start 
f_nextState Start _ _ _ _ _ = RdW
-- Read
f_nextState Read  _ _ 0 _ _ = RdW -- last_i = 0
f_nextState Read  _ _ 1 _ _ = RdE -- last_i = 1
-- Wait
f_nextState RdW   _ _ _ _ _ = Read
-- EndRead
f_nextState RdE   _ _ _ _ _ = Tail
-- Tail
f_nextState Tail  _ _ _ 0 _ = TailW
f_nextState Tail  _ _ _ 1 _ = TailE
-- Tail Wait
f_nextState TailW _ _ _ _ _ = Tail
-- Tail End
f_nextState TailE _ _ _ _ _ = Pad
-- Pad
f_nextState Pad   _ _ _ _ 0 = PadW -- padCounterEnd = 0
f_nextState Pad   _ _ _ _ 1 = PadE -- padCounterEnd = 0
-- Pad wait
f_nextState PadW  _ _ _ _ _ = Pad
-- Pad End
f_nextState PadE  _ _ _ _ _ = End
-- End
f_nextState End   _ _ _ _ _ = Idle


f_ready :: State -> Bit
f_ready Read = 1
f_ready Start = 1
f_ready _    = 0


f_nextTailCounter :: State -> Unsigned 2 -> Unsigned 2
f_nextTailCounter Tail x = x + 1
f_nextTailCounter TailW x = x
f_nextTailCounter _ _ = 0


f_encoderInput :: State -> Bit -> Bit -> Bit -> Bit
f_encoderInput state data_i tail pad = case state of
    Start -> data_i
    Read  -> data_i
    Tail  -> tail
    Pad   -> pad
    _     -> 0

f_encoderEnable :: State -> Bit
f_encoderEnable Read = 1
f_encoderEnable Start = 1
f_encoderEnable Tail = 1
f_encoderEnable Pad  = 1
f_encoderEnable _    = 0




-- 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1
pad_bits :: BitVector 13
pad_bits = 0b1101000011010

-- nextPadCounter :: State -> Bool -> Unsigned 4 -> Unsigned 4
-- nextPadCounter Pad _ 0 = 0
-- nextPadCounter Pad _ counter = counter - 1   -- TODO : try (-1) alone

nextPadCounter :: State -> Unsigned 4 -> Unsigned 4
nextPadCounter Pad x = x + 1
nextPadCounter PadW x = x
nextPadCounter _ _ = 0

padCounterMax :: Bool -> Unsigned 4
padCounterMax True  = 12 -- even -> 13 bits
padCounterMax False = 4  -- odd  -> 5 bits

f_dataOut :: State -> Bit -> Bit -> Bit
f_dataOut state ui0 ui1 = case state of
  Idle  -> ui0
  Read  -> ui0
  RdE   -> ui1
  Start -> ui0
  Tail  -> ui0
  Pad   -> ui0
  RdW   -> ui1
  TailW -> ui1
  PadW  -> ui1
  PadE  -> ui1
  End   -> ui0
  _     -> 0

f_last :: State -> Bit
f_last End = 1
f_last _   = 0

f_valid :: State -> Bit
f_valid Start = 0
f_valid Idle = 0
f_valid _ = 1

f_nextBitCounter :: State -> Unsigned 4 -> Unsigned 4
f_nextBitCounter Read x = x + 1
f_nextBitCounter Start x = x + 1
f_nextBitCounter Idle _ = 0
f_nextBitCounter _ x = x


encoder
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom Bit -- phyFSKFECScheme
  -> Signal dom Bit -- enable
  -> Signal dom Bit -- input
  -> Signal dom (BitVector 3, Bit, Bit) -- output
encoder phyFSKFECScheme en inp = mux (bitToBool <$> phyFSKFECScheme) rsc nrnsc
  where
    rsc = rscEncoder en inp
    nrnsc = nrnscEncoder en inp

fec
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom Bit -- phyFSKFECScheme : 1 -> RSC, 0 -> NRNSC
  -> Signal dom Bit -- valid_i
  -> Signal dom Bit -- data_i
  -> Signal dom Bit -- last_i
  -> Signal dom Bit -- ready_i
  -> Signal dom (Bit, Bit, Bit, Bit, State, Bit, Bit, Bit, Bit, Unsigned 4) -- ready_o, valid_o, data_o, last_o
fec phyFSKFECScheme valid_i data_i last_i ready_i = bundle(ready_o, valid_o, data_o, last_o, state, ui0, ui1, encoderEnable, encoderInput, padCounter)
  where
    state = register (Idle :: State) nextState
    nextState = f_nextState <$> state <*> ready_i <*> valid_i <*> last_i <*> tailCounterEnd <*> padCounterEnd


    -- Bit counter (to determine if there's an even or odd number of bytes)
    bitCounter = register (0 :: Unsigned 4) (f_nextBitCounter <$> state <*> bitCounter)
    -- Even number of bytes when the 4th bit of bitCounter is low
    evenNBytes = not <$> (testBit <$> bitCounter <*> 3)
    
    -- Tail counter
    tailCounter = register (0 :: Unsigned 2) (f_nextTailCounter <$> state <*> tailCounter)
    tailCounterEnd = boolToBit <$> (tailCounter .==. 2)

    -- Pad counter (4->0 or 12->0)
    padCounter = register (0 :: Unsigned 4) (nextPadCounter <$> state <*> padCounter)
    padCounterEnd = boolToBit <$> (padCounter .==. (padCounterMax <$> evenNBytes))

    pad = boolToBit <$> (testBit pad_bits <$> (fromEnum <$> padCounter))

    
    tail = boolToBit <$> (testBit <$> (tailVec <$> m <*> phyFSKFECScheme) <*> (fromEnum <$> tailCounter))

    encoderInput = f_encoderInput <$> state <*> data_i <*> tail <*> pad
    encoderEnable = f_encoderEnable <$> state
    -- tail = pure 0
    

    (m, ui0, ui1) = unbundle $ encoder phyFSKFECScheme encoderEnable encoderInput
    -- Output

    ready_o = f_ready <$> state
    valid_o = f_valid <$> state
    data_o = f_dataOut <$> state <*> ui0 <*> ui1
    last_o = f_last <$> state


pack3 :: Bit -> Bit -> Bit -> BitVector 3
pack3 a b c = (pack a) ++# (pack b) ++# (pack c)

-- RSC Encoder
rscEncoder
  :: forall dom . HiddenClockResetEnable dom
  => Signal dom Bit -- Enable
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
  => Signal dom Bit -- enable
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
