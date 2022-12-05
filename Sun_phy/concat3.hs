{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module CONCAT3 where

import Clash.Prelude

-- State machine
data State = Idle
           | ReadA
           | EndA
           | WaitB
           | ReadB
           | EndB
           | WaitC
           | ReadC
           | EndC
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
--   -> WaitC
-- WaitC
--   -> ReadC when C_valid = 1
-- ReadC
--   -> EndC when B_last = 1
-- EndC
--   -> Idle

nextState
    :: State -- Input State
    -> Bit -- A_valid
    -> Bit -- A_last
    -> Bit -- B_valid
    -> Bit -- B_last
    -> Bit -- C_valid
    -> Bit -- C_last
    -> State -- Output state
-- Idle
nextState Idle 0 _ _ _ _ _ = Idle -- A_valid = 0
nextState Idle 1 _ _ _ _ _ = ReadA  -- A_valid = 1
-- ReadA
nextState ReadA _ 0 _ _ _ _ = ReadA -- A_last = 0 
nextState ReadA _ 1 _ _ _ _ = EndA -- A_last = 1
-- EndA
nextState EndA  _ _ _ _ _ _ = WaitB -- always
-- WaitB
nextState WaitB _ _ 0 _ _ _ = WaitB -- B_valid = 0
nextState WaitB _ _ 1 _ _ _ = ReadB -- B_valid = 1
-- ReadB
nextState ReadB _ _ _ 0 _ _ = ReadB -- B_last = 0
nextState ReadB _ _ _ 1 _ _ = EndB -- B_last = 1
-- EndB
nextState EndB  _ _ _ _ _ _ = WaitC -- always
-- WaitC
nextState WaitC _ _ _ _ 0 _ = WaitC -- C_valid = 0
nextState WaitC _ _ _ _ 1 _ = ReadC -- C_valid = 1
-- ReadC
nextState ReadC _ _ _ _ _ 0 = ReadC -- C_last = 0
nextState ReadC _ _ _ _ _ 1 = EndC -- C_last = 1
-- EndC
nextState EndC  _ _ _ _ _ _ = Idle -- always


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

-- C_ready
f_c_ready :: State -> Bit -> Bit
f_c_ready s ready
    | s == EndB = ready
    | s == WaitC = ready
    | s == ReadC = ready
    | otherwise = 0

-- valid
f_valid :: State -> Bit
f_valid s
    | s == ReadA = 1
    | s == EndA = 1
    | s == ReadB = 1
    | s == EndB = 1
    | s == ReadC = 1
    | s == EndC = 1
    | otherwise = 0

f_last :: State -> Bit
f_last s
    | s == EndC = 1
    | otherwise = 0

-- Return which data stream to choose
dataSel :: State -> Bit -> Bit -> Bit -> Bit -- State, a, b, c
dataSel s a b c = case s of
    Idle -> a
    ReadA -> a
    EndA -> a
    WaitB -> b
    ReadB -> b
    EndB -> b
    WaitC -> c
    ReadC -> c
    EndC -> c


concat3
    :: forall dom . (HiddenClockResetEnable dom)
    => Signal dom Bit -- A_valid
    -> Signal dom Bit -- A_data
    -> Signal dom Bit -- A_last
    -> Signal dom Bit -- B_valid
    -> Signal dom Bit -- B_data
    -> Signal dom Bit -- B_last
    -> Signal dom Bit -- C_valid
    -> Signal dom Bit -- C_data
    -> Signal dom Bit -- C_last
    -> Signal dom Bit -- ready (input)
    -> Signal dom (Bit, Bit, Bit, Bit, Bit, Bit, State) -- a_ready, b_ready, c_ready, valid, data, last, state (test)
concat3 a_valid a_data a_last b_valid b_data b_last c_valid c_data c_last ready = bundle (a_ready, b_ready, c_ready, valid, data_out, last, state)
  where
    state = register (Idle :: State)
      (nextState
        <$> state
        <*> a_valid
        <*> a_last
        <*> b_valid
        <*> b_last
        <*> c_valid
        <*> c_last)

    a_ready = f_a_ready <$> state <*> ready
    b_ready = f_b_ready <$> state <*> ready
    c_ready = f_c_ready <$> state <*> ready
    
    valid_reg = register (0 :: Bit) ready

    valid = boolToBit <$> ((bitToBool <$> (f_valid <$> state)) .&&. (bitToBool <$> valid_reg))
    last = f_last <$> state
    
    data_out = register (0 :: Bit) (dataSel <$> state <*> a_data <*> b_data <*> c_data)