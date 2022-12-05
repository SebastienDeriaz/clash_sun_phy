{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module CONCAT2 where

import Clash.Prelude

-- State machine
data State = Idle
           | ReadA
           | EndA
           | WaitB
           | ReadB
           | EndB
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX



-- Idle :
--   -> ReadA when A_valid = 1
-- ReadA : 
--   -> EndA when A_last = 1
-- EndA
--   -> WaitB 
-- WaitB
--   -> ReadB when B_valid = 1
-- ReadB :
--   -> EndB when B_last = 1
-- EndB : 
--   -> Idle

nextState
    :: State -- Input State
    -> Bit -- A_valid
    -> Bit -- A_last
    -> Bit -- B_valid
    -> Bit -- B_last
    -> State -- Output state
-- Idle
nextState Idle 0 _ _ _ = Idle -- A_valid = 0
nextState Idle 1 _ _ _ = ReadA  -- A_valid = 1
-- Read A
nextState ReadA _ 0 _ _ = ReadA -- A_last = 0 
nextState ReadA _ 1 _ _ = EndA -- A_last = 1
-- End A
nextState EndA  _ _ _ _ = WaitB -- always
-- WaitB
nextState WaitB _ _ 0 _ = WaitB -- B_valid = 0
nextState WaitB _ _ 1 _ = ReadB -- B_valid = 1
-- Read B
nextState ReadB _ _ _ 0 = ReadB -- B_last = 0
nextState ReadB _ _ _ 1 = EndB -- B_last = 1
-- End B
nextState EndB  _ _ _ _ = Idle -- always

-- If ready is low (on the right, then it is always propagated) for A or B
-- TODO : Check this... it is certainly causing a long combinational path

-- A_ready
f_a_ready :: State -> Bit -> Bit
f_a_ready s ready
    | s == Idle = ready
    | s == ReadA = ready
    | otherwise = 0

-- B_ready
f_b_ready :: State -> Bit -> Bit
f_b_ready s ready
    | s == EndA = ready
    | s == WaitB = ready
    | s == ReadB = ready
    | otherwise = 0

-- valid
f_valid :: State -> Bit
f_valid s
    | s == ReadA = 1
    | s == EndA = 1
    | s == ReadB = 1
    | s == EndB = 1
    | otherwise = 0

f_last :: State -> Bit
f_last s
    | s == EndB = 1
    | otherwise = 0

-- Return which data stream to choose
dataSel :: State -> Bool -- A_nB
dataSel s
    | s <= EndA = True
    | otherwise = False


concat2
    :: forall dom . (HiddenClockResetEnable dom)
    => Signal dom Bit -- A_valid
    -> Signal dom Bit -- A_data
    -> Signal dom Bit -- A_last
    -> Signal dom Bit -- B_valid
    -> Signal dom Bit -- B_data
    -> Signal dom Bit -- B_last
    -> Signal dom Bit -- ready (input)
    -> Signal dom (Bit, Bit, Bit, Bit, Bit, State) -- a_ready, b_ready, valid, data, last, state (test)
concat2 a_valid a_data a_last b_valid b_data b_last ready = bundle (a_ready, b_ready, valid, data_out, last, state)
  where
    state = register (Idle :: State)
      (nextState
        <$> state
        <*> a_valid
        <*> a_last
        <*> b_valid
        <*> b_last)

    a_ready = f_a_ready <$> state <*> ready
    b_ready = f_b_ready <$> state <*> ready

    valid = f_valid <$> state
    last = f_last <$> state
    
    data_out = register (0 :: Bit) (mux (dataSel <$> state) a_data b_data)