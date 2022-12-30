{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Sun_phy.MR_FSK.SHR where

import Clash.Prelude

preambleSequence :: Bit -> BitVector 32
-- The order is reversed to traverse the BitVector starting at 0
-- modulation 
preambleSequence 1 = 0b0000_0000_0000_0000_0000_0000_1010_1010 -- 2FSK
preambleSequence 0 = 0b0000_0000_0000_0000_1110_1110_1110_1110 -- 4FSK

preambleSequenceLength :: Bit -> Unsigned 5
-- modulation
preambleSequenceLength 1 = 7
preambleSequenceLength 0 = 15

-- SFD Values as a function of modulation, phyMRFSKSFD and coded/uncoded
-- Key is (modulation, phyMRFSKSFD, is_coded)
-- See tables 131 and 132 of 802.15.4g-2012
-- Note that the order is reversed so that the BitVector
-- can be traversed starting at 0
sfd :: Bit -> Bit -> Bit -> BitVector 32
-- modulation, phyMRFSKSFD, phyFSKFECEnabled
-- Table 131
sfd 1 0 1 = 0b0000_0000_0000_0000_0111_0010_1111_0110
sfd 1 0 0 = 0b0000_0000_0000_0000_0111_0010_0000_1001
sfd 1 1 1 = 0b0000_0000_0000_0000_1011_0100_1100_0110
sfd 1 1 0 = 0b0000_0000_0000_0000_0111_0000_0101_1110
-- Table 132
sfd 0 0 1 = 0b1011_1111_1010_1110_1111_1111_1011_1110
sfd 0 0 0 = 0b1011_1111_1010_1110_1010_1010_1110_1011
sfd 0 1 1 = 0b1110_1111_1011_1010_1111_1010_1011_1110
sfd 0 1 0 = 0b1011_1111_1010_1010_1011_1011_1111_1110

sfdLength :: Bit -> Unsigned 5
-- 2FSK = 1
-- 4FSK = 0
sfdLength 1 = 15
sfdLength 0 = 31

-- State machine
data State = Idle
           | Preamble
           | SFD
           | Finish
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX

nextState :: State -> Bit -> Bool -> Bool -> State
-- State, ready_i, preambleCounterEnd, bitCounterEnd
nextState Idle     1 _    _     = Preamble
nextState Idle     _ _    _     = Idle
nextState Preamble 1 True True  = SFD
nextState Preamble _ _    _     = Preamble
nextState SFD      1 _    True  = Finish
nextState SFD      _ _    _     = SFD
nextState Finish   1 _    _     = Finish
nextState Finish   0 _    _     = Finish


bitCounterMax :: State -> Bit -> Unsigned 5
bitCounterMax Preamble x = preambleSequenceLength x
bitCounterMax SFD      x = sfdLength x

nextBitCounter :: State -> Bool -> Bit -> Unsigned 5 -> Unsigned 5
-- State, bitCounterEnd, slaveWrite, bitCounter
-- Idle
nextBitCounter Finish _   _ _ = 0
nextBitCounter Idle _     0 _ = 0
nextBitCounter Idle _     1 x = x + 1
-- Other state
nextBitCounter _    False 1 x = x + 1
nextBitCounter _    True  1 _ = 0
nextBitCounter _    _     _ x = x

nextPreambleCounter :: State -> Bool -> Bit -> Unsigned 10 -> Unsigned 10
-- state, bitCounterEnd, ready_i, counter
nextPreambleCounter Preamble True  1 x = x + 1
nextPreambleCounter Preamble _     _ x = x
nextPreambleCounter _        _     _ _ = 0


data_out :: State -> Bit -> Bit -> Bit
-- state, bitCounter, preamble, sfd
data_out Preamble preamble _   = preamble
data_out SFD      _        sfd = sfd
data_out _        _ _          = 0

shr
  :: forall dom . (HiddenClockResetEnable dom)
  => Signal dom Bit -- ready_i
  -> Signal dom Bit -- modulation
  -> Signal dom Bit -- phyMRFSKSFD
  -> Signal dom Bit -- phyFSKFECEnabled
  -> Signal dom (Unsigned 10) -- phyFSKPreambleLength
  -> Signal dom Bit -- valid_i
  -> Signal dom (Bit, Bit, Bit) -- valid_o, data_o, last_o
shr
  -- Inputs
  ready_i
  fsk2_nfsk4
  phyMRFSKSFD
  phyFSKFECEnabled
  phyFSKPreambleLength
  valid_i = bundle (
    -- Outputs
    valid_o,
    data_o,
    last_o)
  where
    state = register (Idle :: State) (nextState <$> state <*> ready_i <*> preambleCounterEnd <*> bitCounterEnd)

    slaveWrite = ready_i * valid_o

    preambleCounter = register (0 :: Unsigned 10) (nextPreambleCounter <$> state <*> bitCounterEnd <*> ready_i <*> preambleCounter)
    preambleCounterEnd = preambleCounter .==. (phyFSKPreambleLength - 1)

    bitCounter = register (0 :: Unsigned 5) (nextBitCounter <$> state <*> bitCounterEnd <*> slaveWrite <*> bitCounter)
    bitCounterEnd = bitCounter .==. (bitCounterMax <$> state <*> fsk2_nfsk4)

    sfd_bit = boolToBit <$> (testBit <$> (sfd <$> fsk2_nfsk4 <*> phyMRFSKSFD <*> phyFSKFECEnabled) <*> (fromEnum <$> bitCounter))
    preamble_bit = boolToBit <$> (testBit <$> (preambleSequence <$> fsk2_nfsk4) <*> (fromEnum <$> bitCounter))

    data_o = data_out <$> state <*> preamble_bit <*> sfd_bit

    last_o = boolToBit <$> (state .==. (pure SFD) .&&. bitCounterEnd)

    valid_o = register 0 valid_i