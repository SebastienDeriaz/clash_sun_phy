module SunPhy.Padder where

import SunPhy.AXI
import Clash.Prelude
import Data.Functor ((<&>))


-- The padder takes an input bitstreams and does two operations :
-- 1) Add N tail bits
-- 2) Pad the input+tail bitstream so that it becomes a multiple of M bits
-- The padder will wait for the "last" signal to add the tail and pad bits
--
--                  ┏━━━━━━━━━━━━━━━┓
--                  ┃   Puncturer   ┃
--        tailSize  ┣>              ┃
--    sizeMultiple  ┣>             <┨  ready_i 
--         ready_o <┨               ┠> valid_o
--         valid_i  ┠>              ┣> data_o
--          data_i  ┠>              ┠> last_o 
--          last_i  ┠>              ┃
--                  ┃               ┃
--                  ┗━━━━━━━━━━━━━━━┛

data PadderInput = PadderInput
    { axiInput :: AxiForward Bit
    , axiOutputFeedback :: AxiBackward
    , tailSize :: Unsigned 4
    , sizeMultiple :: Unsigned 9
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data PadderOutput = PadderOutput
    { axiInputFeedback :: AxiBackward
    , axiOutput :: AxiForward Bit
    , counter :: Unsigned 9
    , state :: Unsigned 3
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data State = Idle -- No data in the buffer
           | Piped -- There's data in the buffer, but it's coming out as new data is coming in
           | Buffer -- Data has accumulated in the input buffer, it must come out before new data can be accepted
           | Tail   -- Tail output (zeros)
           | Padding -- Adding zeros to the data
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX

stateNum :: State -> Unsigned 3
stateNum Idle =  0
stateNum Piped =  1
stateNum Buffer =  2
stateNum Tail =  3
stateNum Padding =  4

padder
    :: forall dom . HiddenClockResetEnable dom
    => Signal dom PadderInput
    -> Signal dom PadderOutput
padder input = do
    axiInputFeedback <- do
        ready <- ready_o
        pure AxiBackward {..}
    axiOutput <- do
        valid <- valid_o
        _data <- data_o
        last <- last_o
        pure AxiForward {..}
    counter <- counter
    state <- stateNum <$> state
            
    pure PadderOutput {..}
  where    
    --ready_o = boolToBit <$> (ready_i_reg .==. 1 .&&. last_o .==. 0)
    ready_o = boolToBit <$> (state .==. pure Piped .||. state .==. pure Idle)
    slaveWrite = (input <&> (.axiOutputFeedback) <&> (.ready)) * valid_o
    masterWrite = ready_o * (input <&> (.axiInput) <&> (.valid))

    nextState :: State -> Bit -> Bit -> Unsigned 9 -> Bit -> Bit -> Bit -> State
    --        ┌state
    --        │       ┌masterWrite
    --        │       │ ┌slaveWrite
    --        │       │ │ ┌counter (this is when the counter is zero)
    --        │       │ │ │ ┌tailCounterEnd
    --        │       │ │ │ │ ┌counterEnd (this is before the counter gets to zero)
    --        │       │ │ │ │ │ ┌lastFlag
    --        │       │ │ │ │ │ │
    -- Idle   │       │ │ │ │ │ │
    nextState Idle    1 0 _ _ _ _ = Piped -- Data in only
    -- Piped
    nextState Piped   1 0 _ _ _ _ = Buffer -- Bufferize the data
    nextState Piped   0 1 _ _ _ 1 = Tail -- End of incoming data (data out only) (no padding)
    -- Tail
    nextState Tail    _ 1 0 1 _ _ = Idle
    nextState Tail    _ 1 _ 1 _ _ = Padding
    -- Padding (0 output only)
    nextState Padding _ 1 _ _ 1 _ = Idle
    nextState Padding _ _ _ _ _ _ = Padding
    -- Buffer
    nextState Buffer  0 1 _ _ _ _ = Piped -- Data out (return to normal)
    -- Otherwise (no change)
    nextState x       _ _ _ _ _ _ = x

    state = register Idle $ nextState
        <$> state
        <*> masterWrite
        <*> slaveWrite
        <*> counter
        <*> tailCounterEnd
        <*> (boolToBit <$>counterEnd)
        <*> lastFlag

    a = register (0 :: Bit) nextA
    nextA = mux
        (bitToBool <$> masterWrite)
        (input <&> (.axiInput) <&> (._data))
        a

    b = register (0 :: Bit) nextB
    nextB = mux (bitToBool <$> masterWrite) a b

    -- Last management
    -- A last flag is raised whenever the last_i input is high
    lastFlag = register (0 :: Bit) $ nextLastFlag
        <$> state
        <*> (input <&> (.axiInput) <&> (.last))
        <*> lastFlag

    nextLastFlag :: State -> Bit -> Bit -> Bit
    --           ┌state
    --           │    ┌last_i
    --           │    │ ┌lastFlag
    -- Idle      │    │ │
    nextLastFlag Idle _ _ = 0
    nextLastFlag _    1 _ = 1
    nextLastFlag _    _ x = x


    -- tailCounter, used to count the number of tail bit to insert
    tailCounter = register (0 :: Unsigned 4) $ mux
        (state ./=. pure Tail)
        0 $ mux
            (slaveWrite .==. 1)
            (tailCounter + 1)
            tailCounter
    tailCounterEnd = boolToBit <$> (tailCounter .==. (input <&> (.tailSize)) - 1)

    -- dataCounter, it counts the number of bits already sent out and
    -- overflows at moduleSize
    -- When the input bitstream is exhausted, 0s are outputed to return the counter to zero

    -- TODO : try a do block
    counter = register (0 :: Unsigned 9) $ mux
        (slaveWrite .==. 0)
        counter $ mux
            counterEnd
            0
            (counter + 1)
    counterEnd = counter .==. (input <&> (.sizeMultiple)) - 1

    -- Outputs
    ready_i_reg = register (0 :: Bit) $ input <&> (.axiOutputFeedback) <&> (.ready)
    valid_o = boolToBit <$> (state ./=. pure Idle)
    -- TODO : Try a do block
    data_o = mux
        (state .==. pure Padding)
        0 $ mux
            (state ./=. pure Buffer)
            a
            b
    last_o = boolToBit <$> (
        ((state .==. pure Padding .&&. counterEnd)
        .||. (state .==. pure Tail .&&. counter .==. 0)) -- TODO : check this one
        .&&. slaveWrite .==. 1
        .&&. lastFlag .==. 1)
