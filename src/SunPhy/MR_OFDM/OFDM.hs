module SunPhy.MR_OFDM.OFDM where

import Clash.Prelude
import SunPhy.MR_OFDM.Constants
import Clash.DSP.FFT.Example
import Clash.DSP.Complex
import Clash.DSP.FFT.Parallel
import SunPhy.PN9
import SunPhy.MR_OFDM.OFDM_Pilots

-- NOTE : No modulation factor because it can be encoded inside
-- the modulation parameter, the output modulation must be multiplied
-- by the modulation factor as such :
-- BPSK  1
-- QPSK  1/np.sqrt(2)
-- QAM16 1/np.sqrt(10)

-- NOTE : The CP value is removed, it is now
-- either on (1/4) or off (0)

-- NOTE : Only the 128-FFT is implemented. How to calculate 16,32 and 64 FFT then ?
-- # Fill only the first N values of the input array
-- subcarriers = 0,0,...,0 x 128 
-- subcarriers[0:16] = input
-- # Calculate the IFFT normally
-- temp = ifft(subcarriers)
-- step = 128 / N
-- # The N-ifft will be one value every 128 / N value
-- output = temp[::step]

-- NOTE : The frequency spreading can happen worry-free. If a pilot were to be copied it would always land on another one
-- Therefore all the pilots can simply be ignored when data is written

ifftshift :: Unsigned 8 -> Unsigned 7 -> Unsigned 7
ifftshift nfft i 
  | i >= (resize nfft) = i
  | i < n2 = i + n2
  | otherwise = i - n2
  where
    n2 :: Unsigned 7
    n2 = resize $ nfft `div` 2




data State = Init -- Initial state
           | Idle -- Wait for data
           | WPlt
           | BufD
           | WDat
           | WrSF
           | Skip
           | Outp
           | OuCP
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX

-- Convert a counter value to a position on the subcarrier vector
-- 0 corresponds to the first (lowest negative frequency) value
-- active_tones - 1 corresponds to the last (highest positive frequency) value
-- the DC tone is skipped
subcarrierCounterToIndex :: Unsigned 8 -> Unsigned 8 -> Unsigned 7 -> Unsigned 7
-- _n_fft, active_tones, counter
subcarrierCounterToIndex nfft at i = i + offset + (dcOffset i)
  where
    offset = resize $ (nfft - (resize at)) `div` 2 :: Unsigned 7
    dcOffset :: Unsigned 7 -> Unsigned 7
    dcOffset i
      | i >= (resize $ at `div` 2) = 1
      | otherwise        = 0

-- Convert a counter value to an index when accounting for SF effect
counterToSpreadIndex :: Unsigned 3 -> Unsigned 8 -> Unsigned 8 -> Unsigned 7 -> Unsigned 7
-- 1x (no change)
counterToSpreadIndex 1 nfft at i = subcarrierCounterToIndex nfft at i
-- 2x and 4x
-- 2x bbbbbbbbDmmmmmmmm
-- 4x ccccddddDmmmmbbbb
counterToSpreadIndex s nfft at i = out (it + offset s m)
  where
    at2 = resize $ at `div` 2 :: Unsigned 7
    at4 = resize $ at `div` 4  :: Unsigned 7
    it = i `div` (resize s)

    m = i `mod` (resize s)
    --offset s m
    offset 2 0 = 0
    offset 2 1 = at2
    offset 4 0 = 0
    offset 4 1 = at4
    offset 4 2 = at2
    offset 4 3 = at2 + at4

    out j
     | j < at2 = subcarrierCounterToIndex nfft at (j + at2)
     | otherwise = subcarrierCounterToIndex nfft at (j - at2)


pilotsIndicesInitial = repeat (-1) :: Vec 128 (Unsigned 7)
-- OFDM steps :
-- 1) Load the pilot values and indices
-- 2) Load the data (+ apply modulation)
--    The input stream is stopped whenever the modulation is anything
--    other than BPSK (to have enough time to load everything).
--    The input stream is also stopped whenever a pilot is encountered
--    so that it can be skipped
-- 3) Read output data
ofdm
  :: forall dom . HiddenClockResetEnable dom
  -- OFDM Settings
  => Signal dom OFDM_Option -- OFDM_option
  -> Signal dom MCS -- MCS
  -> Signal dom CP -- CP
  -- Input data
  -> Signal dom Bit -- data_valid_i  
  -> Signal dom Bit -- data_i
  -> Signal dom Bit -- data_last_i
  -- Pilot and PN9 values
  -> Signal dom (Unsigned 4) -- pilotset_index_i
  -> Signal dom Bit -- pilotset_write_i
  -> Signal dom (BitVector 9) -- pn9seed_data_i (pn9 starting seed)
  -> Signal dom Bit -- pn9seed_write_i
  -> Signal dom Bit -- ready_i
  -> Signal dom (Bit, Bit, IQ, Bit, State, Bit, Unsigned 3, Bit, Unsigned 7, Unsigned 7, Unsigned 4, IQ)
ofdm
  -- Inputs
  ofdmOption
  mcs
  cp
  payload_valid_i
  payload_data_i
  payload_last_i
  pilotset_index_i
  pilotset_write_i
  pn9seed_data_i
  pn9seed_write_i
  ready_i
  -- Outputs
  = bundle (
    data_ready_o,
    valid_o,
    data_o,
    last_o,
    state,
    pilotCounterEnd,
    pilotCounter,
    pilotNext,
    subcarrierIndex',
    subcarrierCounter,
    pilotSetCounter,
    dataSubcarrier
    )
  where
    at x = x !! 5


    _n_fft = n_fft <$> ofdmOption

    -- ╔══════════════════════════════╗
    -- ║ Outputs and output functions ║
    -- ╚══════════════════════════════╝
    -- pilot_ready_o
    pilot_ready_o = boolToBit <$> (state .==. pure WPlt)
    -- data_ready_o
    dataReady :: State -> Bit
    dataReady WDat  = 1
    dataReady BufD  = 1
    dataReady _     = 0

    data_ready_o  = dataReady <$> state
    -- data_o
    dataOut :: Enum n => State -> Vec 128 IQ -> n -> IQ
    --      ┌state
    --      │    ┌iq vector
    --      │    │ ┌index
    --      │    │ │
    dataOut OuCP v i = v !! i
    dataOut Outp v i = v !! i
    dataOut _    _ _ = (0,0)

    data_o        = dataOut <$> state <*> fftOutput <*> subcarrierIndex'
    -- valid_o

    valid :: State -> Bit
    valid Outp = 1
    valid OuCP = 1
    valid _    = 0

    valid_o       = valid <$> state
    -- last_o
    last_o        = boolToBit <$> (
      subcarrierReadEnd .==. 1 .&&.
      isLast .==. 1 .&&.
      state .==. pure Outp)

    cp_enabled    = boolToBit <$> (cp ./=. pure CP_NONE)

    _modulation = mcsModulation <$> mcs

    _frequencySpreading = frequencySpreading <$> mcs

    -- ╔═══════════╗
    -- ║ Shortcuts ║
    -- ╚═══════════╝

    dataWrite = data_ready_o * payload_valid_i
    slaveWrite = valid_o * ready_i

    _dataTones = dataTones <$> ofdmOption
    _pilotTones = pilotTones <$> ofdmOption
    _activeTones = _dataTones + _pilotTones

    -- ╔═══════════════╗
    -- ║ State machine ║
    -- ╚═══════════════╝

    -- State
    nextState :: State -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Unsigned 2 -> Unsigned 2 -> Bit -> Bit -> State
    --        ┌state
    --        │    ┌pilotset_write_i
    --        │    │ ┌pilotCounterEnd
    --        │    │ │ ┌data_valid_i
    --        │    │ │ │ ┌data_last_i
    --        │    │ │ │ │ ┌dataWrite
    --        │    │ │ │ │ │ ┌subcarrierWriteEnd
    --        │    │ │ │ │ │ │ ┌subcarrierReadEnd
    --        │    │ │ │ │ │ │ │ ┌pilotNext
    --        │    │ │ │ │ │ │ │ │ ┌spreadCounter
    --        │    │ │ │ │ │ │ │ │ │ ┌bufferCounter
    --        │    │ │ │ │ │ │ │ │ │ │ ┌cp_enabled
    --        │    │ │ │ │ │ │ │ │ │ │ │ ┌slaveWrite
    -- Init   │    │ │ │ │ │ │ │ │ │ │ │ │
    nextState Init _ _ _ _ _ _ _ _ _ _ _ _ = WPlt -- Write the pilots
    -- Idle
    nextState Idle 1 _ _ _ _ _ _ _ _ _ _ _ = WPlt -- Write the pilots
    nextState Idle _ _ 1 _ _ _ _ 1 _ _ _ _ = Skip
    nextState Idle _ _ 1 _ _ _ _ 0 _ 0 _ _ = WDat
    nextState Idle _ _ 1 _ _ _ _ 0 _ _ _ _ = BufD
    nextState Idle _ _ _ _ _ _ _ _ _ _ _ _ = Idle
    -- Write pilot                             
    nextState WPlt _ 1 _ _ _ _ _ _ _ _ _ _ = Idle
    nextState WPlt _ 0 _ _ _ _ _ _ _ _ _ _ = WPlt
    -- Buffer data                             
    nextState BufD _ _ _ _ 1 _ _ 1 _ 0 _ _ = Skip
    nextState BufD _ _ _ _ 1 _ _ 0 _ 0 _ _ = WDat
    nextState BufD _ _ _ _ _ _ _ _ _ _ _ _ = BufD
    -- Write data                             
    nextState WDat _ _ _ _ 1 _ _ _ 0 _ 0 _ = Outp
    nextState WDat _ _ _ _ 1 _ _ _ 0 _ 1 _ = OuCP
    nextState WDat _ _ _ _ 1 _ _ _ _ _ _ _ = WrSF
    nextState WDat _ _ _ _ 0 _ _ 0 _ _ _ _ = WDat
    nextState WDat _ _ _ _ _ _ _ 1 0 _ _ _ = Skip
    nextState WDat _ _ _ _ _ _ _ 0 0 _ _ _ = WDat
    nextState WDat _ _ _ _ _ _ _ _ _ _ _ _ = WrSF
    -- Write frequency spreading             
    nextState WrSF _ _ _ _ _ 0 _ 0 0 0 _ _ = WDat
    nextState WrSF _ _ _ _ _ 0 _ 1 0 _ _ _ = Skip
    nextState WrSF _ _ _ _ _ 0 _ 0 0 _ _ _ = BufD
    nextState WrSF _ _ _ _ _ 1 _ _ _ _ 0 _ = Outp -- the spread counter must be 0, no need to check it
    nextState WrSF _ _ _ _ _ 1 _ _ _ _ 1 _ = OuCP -- the spread counter must be 0, no need to check it
    nextState WrSF _ _ _ _ _ _ _ _ _ _ _ _ = WrSF
    -- Pilot skip             
    --nextState Skip _ _ _ _ _ 1 _ _ _ _ 0 _ = Outp
    nextState Skip _ _ _ _ _ 1 _ _ _ _ 1 _ = OuCP
    nextState Skip _ _ _ _ _ _ _ 1 _ _ _ _ = Skip
    nextState Skip _ _ _ _ _ _ _ 0 _ 0 _ _ = WDat
    nextState Skip _ _ _ _ _ _ _ 0 _ _ _ _ = BufD
    -- Output CP                             
    nextState OuCP _ _ _ _ _ _ 1 _ _ _ _ 1 = Outp
    nextState OuCP _ _ _ _ _ _ _ _ _ _ _ _ = OuCP
    -- Output                             
    nextState Outp _ _ _ _ _ _ 1 _ _ _ _ 1 = WPlt
    nextState Outp _ _ _ _ _ _ _ _ _ _ _ _ = Outp

    nextState' = nextState
      <$> state
      <*> pilotset_write_i
      <*> pilotCounterEnd
      <*> payload_valid_i
      <*> payload_last_i
      <*> dataWrite
      <*> subcarrierWriteEnd
      <*> subcarrierReadEnd
      <*> pilotNext
      <*> spreadCounter
      <*> bufferCounter
      <*> cp_enabled
      <*> slaveWrite

    state = register (Init :: State) nextState'

    -- ╔════════════════╗
    -- ║ Data Buffering ║
    -- ╚════════════════╝
    -- Only when mod != BPSK
    nextBufferCounter :: State -> Modulation -> Unsigned 2 -> Unsigned 2
    --                ┌next state
    --                │     ┌modulation
    --                │     │   ┌counter
    --                │     │   │
    nextBufferCounter WDat  mod 0 = (nbpsc_mod mod) - 1
    nextBufferCounter BufD  _   x = x - 1
    nextBufferCounter _     mod _ = (nbpsc_mod mod) - 1

    bufferCounter = register (0 :: Unsigned 2) (nextBufferCounter <$> nextState' <*> _modulation <*> bufferCounter)
    bufferReady = boolToBit <$> (bufferCounter .==. 0 .&&. state .==. pure WDat)

    -- ╔══════════════╗
    -- ║ Data Counter ║
    -- ╚══════════════╝
    -- Counts the index of data (k)
    nextdataCounter :: State -> Bit -> Unsigned 2 -> Unsigned 7 -> Unsigned 7
    --              ┌next state
    --              │    ┌subcarrierWrite'
    --              │    │ ┌spreadCounter
    --              │    │ │ ┌counter
    --              │    │ │ │
    nextdataCounter Idle _ _ _ = 0
    nextdataCounter WDat 1 0 x = x + 1
    nextdataCounter WrSF 1 0 x = x + 1
    nextdataCounter _    _ _ x = x
    dataCounter = register (0 :: Unsigned 7) (nextdataCounter <$> state <*> subcarrierWrite' <*> spreadCounter <*> dataCounter)

    -- ╔════════════╗
    -- ║ Last check ║
    -- ╚════════════╝
    -- Check if the input is the last piece of data. In that case, the last flag will be raised
    -- when outputing the data
    nextIsLast :: State -> Bit -> Bit -> Bit
    --         ┌state
    --         │    ┌last_i
    --         │    │ ┌isLast
    --         │    │ │ 
    nextIsLast Idle _ _ = 0
    nextIsLast WDat l _ = l
    nextIsLast _    _ x = x

    isLast = register (0 :: Bit) (nextIsLast <$> state <*> payload_last_i <*> isLast)

    -- ╔═════════════════╗
    -- ║ Data subcarrier ║
    -- ╚═════════════════╝
    -- Subcarrier that results from one or more data bits received

    applyModulation :: Modulation -> Bit -> Bit -> Bit -> Bit -> Subcarrier
    --         ┌modulation
    --         │     ┌data_i
    --         │     │ ┌m[-1]
    --         │     │ │ ┌m[-2]
    --         │     │ │ │ ┌m[-3]
    -- BPSK    │     │ │ │ │
    applyModulation BPSK  0 _ _ _ = (-1.0, 0.0)
    applyModulation BPSK  1 _ _ _ = (1.0, 0.0)
    -- QPSK
    -- 00 -> -1-1j
    applyModulation QPSK  0 0 _ _ = (-1.0, -1.0)
    -- 01 -> -1+1j
    applyModulation QPSK  1 0 _ _ = (-1.0, 1.0)
    -- 10 -> +1-1j
    applyModulation QPSK  0 1 _ _ = (1.0, -1.0)
    -- 11 -> +1+1j
    applyModulation QPSK  1 1 _ _ = (1.0, 1.0)
    -- QAM16
    applyModulation QAM16 _ _ _ _ = (0.0, 0.0)

    k_mod :: Modulation -> Subcarrier -> Subcarrier
    k_mod BPSK x     = x
    k_mod QPSK (a,b) = (a * kModQPSK, b * kModQPSK)
    k_mod QAM16 (a,b) = (a * kModQAM16, b * kModQAM16)

    dataSubcarrier = k_mod <$> _modulation <*> (applyModulation <$> _modulation <*> payload_data_i <*> m1 <*> m2 <*> m3)

    -- Holds the necessary amount of previous data bits for the modulation
    -- data_i[-1]
    m1 = register (0 :: Bit) (mux (bitToBool <$> dataWrite) payload_data_i m1)
    -- data_i[-2]
    m2 = register (0 :: Bit) (mux (bitToBool <$> dataWrite) m1 m2)
    -- data_i[-3]
    m3 = register (0 :: Bit) (mux (bitToBool <$> dataWrite) m2 m3)

    -- ╔═════════════════════╗
    -- ║ Frequency spreading ║
    -- ╚═════════════════════╝
    -- Write the data to the "message location" in the subcarriers vector
    -- At the same time, store a copy of the subcarrier
    -- If necessary, the copy is used to fill the spreading locations
    nextDataSubcarrierStore :: State -> Bit -> Subcarrier -> Subcarrier -> Subcarrier
    --                      ┌state
    --                      │    ┌dataWrite
    --                      │    │ ┌new subcarrier
    --                      │    │ │ ┌old subcarrier
    --                      │    │ │ │
    nextDataSubcarrierStore WDat 1 n _ = n -- new
    nextDataSubcarrierStore _    _ _ o = o -- old

    dataSubcarrierStore = register ((0.0,0.0) :: Subcarrier) (nextDataSubcarrierStore <$> state <*> dataWrite <*> dataSubcarrier <*> dataSubcarrierStore)

    -- Frequency spread counter
    nextSpreadCounter :: State -> Unsigned 3 -> Bit -> Unsigned 2 -> Unsigned 2
    -- next state, frequencySpreading, dataWrite, counter
    --nextSpreadCounter WDat s 1 0 = resize $ s - 1
    nextSpreadCounter WrSF _ _ x = x - 1
    nextSpreadCounter _    s _ _ = resize $ s - 1

    spreadCounter = register (0 :: Unsigned 2) (nextSpreadCounter <$> nextState' <*> _frequencySpreading <*> dataWrite <*> spreadCounter)

    
    sfPhase :: Unsigned 3 -> Unsigned 2 -> Unsigned 7 -> Subcarrier -> Subcarrier
    -- frequencySpreading, spreadCounter, index, old carrier, new carrier
    -- 1x, no change
    sfPhase 1 _ _ x = x
    -- 2x, * np.exp(1j*2*np.pi*(2*k-1)/4)
    sfPhase 2 1 _ x = (fst x, snd x)
    sfPhase 2 0 d x
      -- *(-1j) (second, fourth, ...)
      | even k      = (snd x, -fst x)
      -- *(+1j) (first, third, ...)
      | otherwise   = (- snd x, fst x)
      where
        -- k is dataCounter + 1 because the tone index start at 1. See 18.2.3.6.1 of 802.15.4g-2012
        k = d + 1
    -- 4x
    sfPhase 4 0 k x = x
  
    -- ╔══════════════════════════════╗
    -- ║ Subcarrier writing + counter ║
    -- ╚══════════════════════════════╝
    -- 

    subcarrierWrite :: State -> Bit -> Bit
    --              ┌state
    --              │     ┌dataWrite
    -- Idle (reset) │     │ 
    subcarrierWrite Idle  _ = 0
    -- Write pilot
    subcarrierWrite WPlt  _ = 1
    -- Write data
    subcarrierWrite WDat  x = x
    -- Write frequency spreading
    subcarrierWrite WrSF  _ = 1
    subcarrierWrite _     _ = 0

    subcarrierWrite' = subcarrierWrite <$> state <*> dataWrite

    -- Counts the number of subcarrier written ("true message" and frequency spreading)
    -- Also counts the output index
    nextSubcarrierCounter :: State -> CP -> Unsigned 8 -> Bit -> Bit -> Bit -> Bit -> Unsigned 7 -> Unsigned 7
    --                    ┌state
    --                    │    ┌CP
    --                    │    │          ┌_n_fft 
    --                    │    │          │ ┌subcarrierWrite
    --                    │    │          │ │ ┌write counter end
    --                    │    │          │ │ │ ┌slaveWrite
    --                    │    │          │ │ │ │ ┌read counter end
    --                    │    │          │ │ │ │ │ ┌counter
    -- Idle -> reset      │    │          │ │ │ │ │ │ 
    nextSubcarrierCounter Idle _          _ _ _ _ _ _ = 0
    nextSubcarrierCounter WPlt _          _ _ _ _ _ _ = 0 -- Do not count pilot write, skip will do it
    -- Reading
    nextSubcarrierCounter Outp _          _ _ _ 1 _ x = x + 1
    nextSubcarrierCounter Outp _          _ _ _ 0 _ x = x
    -- Reading (CP)
    nextSubcarrierCounter OuCP _          _ _ _ 1 1 _ = 0 -- Reset for (not CP this time)
    nextSubcarrierCounter OuCP _          _ _ _ 1 _ x = x + 1
    nextSubcarrierCounter OuCP _          _ _ _ 0 _ x = x
    -- Writing
    nextSubcarrierCounter Skip _          _ _ 0 _ _ x = x + 1
    nextSubcarrierCounter _    CP_NONE    _ 1 1 _ _ _ = 0 -- Reset for output (CP or not)
    nextSubcarrierCounter _    CP_HALF    n 1 1 _ _ _ = resize $ n `div` 2 -- Go at Half width
    nextSubcarrierCounter _    CP_QUARTER n 1 1 _ _ _ = resize $ n `div` 4 * 3 -- Go at 3/4
    nextSubcarrierCounter Skip CP_NONE    _ _ 1 _ _ _ = 0 -- Reset for output (CP or not)
    nextSubcarrierCounter Skip CP_HALF    n _ 1 _ _ _ = resize $ n `div` 2 -- Go at Half width
    nextSubcarrierCounter Skip CP_QUARTER n _ 1 _ _ _ = resize $ n `div` 4 * 3 -- Go at 3/4
    nextSubcarrierCounter _    _          _ 1 _ _ _ x = x + 1
    -- Default
    nextSubcarrierCounter _    _          _ 0 _ _ _ x = x

    -- Output step, this is for getting a N/step sized FFT out of the 128FFT
    outputStep :: Unsigned 8 -> Unsigned 7
    outputStep a = resize $ 128 `div` a

    nextSubcarrierCounter' = nextSubcarrierCounter <$> state <*> cp <*> _n_fft <*> subcarrierWrite' <*> subcarrierWriteEnd <*> slaveWrite <*> subcarrierReadEnd <*> subcarrierCounter
    subcarrierCounter      = register (0 :: Unsigned 7) nextSubcarrierCounter'
    subcarrierWriteEnd     = boolToBit <$> (subcarrierCounter .==. (resize <$> _activeTones - 1))
    subcarrierReadEnd      = boolToBit <$> (subcarrierCounter .==. (resize <$> (_n_fft - 1)))
    

    subcarrierIndex :: State -> Unsigned 7 -> Unsigned 7 -> Unsigned 7 -> Unsigned 7 -> Unsigned 7
    --              ┌state
    --              │    ┌pilotIndex
    --              │    │ ┌subcarrierWriteIndex
    --              │    │ │ ┌counter
    --              │    │ │ │ ┌outputStep
    --              │    │ │ │ │
    subcarrierIndex WPlt p _ _ _ = p
    subcarrierIndex Outp _ _ c s = c * s
    subcarrierIndex OuCP _ _ c s = c * s
    subcarrierIndex _    _ i _ _ = i

    subcarrierWriteIndex = counterToSpreadIndex <$> _frequencySpreading <*> _n_fft <*> _activeTones <*> subcarrierCounter

    subcarrierIndex'     = subcarrierIndex <$> state <*> pilotIndex <*> subcarrierWriteIndex <*> subcarrierCounter <*> (outputStep <$> _n_fft) 
    nextSubcarrierIndex' = subcarrierIndex <$> state <*> pilotIndex <*> (counterToSpreadIndex <$> _frequencySpreading <*> _n_fft <*> _activeTones <*> nextSubcarrierCounter') <*> subcarrierCounter <*> (outputStep <$> _n_fft)


    -- ╔════════╗
    -- ║ Pilots ║
    -- ╚════════╝
    -- Store pilots position
    -- 
    nextPilotFlags :: State -> Unsigned 7 -> Unsigned 3 -> BitVector 128 -> BitVector 128
    --             ┌state
    --             │    ┌pilotIndex
    --             │    │ ┌pilotCounter
    --             │    │ │ ┌flags
    --             │    │ │ │ 
    nextPilotFlags WPlt i 0 x = bit $ fromEnum i -- Initialize the vector with the first value
    nextPilotFlags WPlt i _ x = setBit x $ fromEnum i -- Add the other values
    nextPilotFlags _    _ _ x = x

    pilotsFlags = register (0 :: BitVector 128) (nextPilotFlags <$> state <*> pilotIndex <*> pilotCounter <*> pilotsFlags)

    -- Index of which pilot set is used
    nextPilotSetCounter :: State -> Bit -> Unsigned 4 -> Bit -> Bit -> Unsigned 4 -> Unsigned 4
    --                  ┌state
    --                  │    ┌pilotset_write_i
    --                  │    │ ┌pilotset_index_i
    --                  │    │ │ ┌pilotCounterEnd
    --                  │    │ │ │ ┌pilotSetCounterEnd
    --                  │    │ │ │ │ ┌counter
    --                  │    │ │ │ │ │
    nextPilotSetCounter _    1 i _ _ _ = i
    nextPilotSetCounter WPlt _ _ 1 1 _ = 0
    nextPilotSetCounter WPlt _ _ 1 0 x = x + 1
    nextPilotSetCounter _    _ _ _ _ x = x
    pilotSetCounter = register (0 :: Unsigned 4) (nextPilotSetCounter
      <$> state
      <*> pilotset_write_i
      <*> pilotset_index_i
      <*> pilotCounterEnd
      <*> pilotSetCounterEnd
      <*> pilotSetCounter)
    pilotSetCounterEnd = boolToBit <$> (pilotSetCounter .==. ((pilotSets <$> ofdmOption) - 1))

    -- Index of the pilot (inside a set)
    nextPilotCounter :: State -> Bit -> Unsigned 3 -> Unsigned 3
    --               ┌state
    --               │    ┌pilotCounterEnd
    --               │    │ ┌pilotCounter
    --               │    │ │
    nextPilotCounter WPlt 0 x = x + 1
    nextPilotCounter _    _ _ = 0
    pilotCounter = register (0 :: Unsigned 3) $ nextPilotCounter <$> state <*> pilotCounterEnd <*> pilotCounter
    pilotCounterEnd = boolToBit <$> (pilotCounter .==. (resize <$> (_pilotTones - 1)))


    --pilotHere = boolToBit <$> (testBit <$> pilotsFlags <*> (fromEnum <$> subcarrierIndex'))
    pilotNext = boolToBit <$> (testBit <$> pilotsFlags <*> (fromEnum <$> nextSubcarrierIndex'))

    -- Output the pilot position based on ofdmOption, pilotSetCounter and pilotCounter
    pilot :: OFDM_Option -> Unsigned 4 -> Unsigned 3 -> Unsigned 7
    pilot 1 s p = (pilots_indices_1 !! s) !! p
    pilot 2 s p = (pilots_indices_2 !! s) !! p
    pilot 3 s p = (pilots_indices_3 !! s) !! p
    pilot 4 s p = (pilots_indices_4 !! s) !! p

    pilotIndex = pilot <$> ofdmOption <*> pilotSetCounter <*> pilotCounter

    -- PN9 Sequence for pilot generation
    nextPn9_seed :: BitVector 9 -> BitVector 9
    nextPn9_seed x = x
    pn9_seed = register (0b1_1111_1111 :: BitVector 9) $ nextPn9_seed <$> pn9_seed
    pn9_next = boolToBit <$> (state .==. pure WPlt)
    pn9_reset = pure 0
    pilotValue = pn9 pn9_seed pn9_next pn9_reset



    -- ╔════════════════════╗
    -- ║ Subcarriers vector ║
    -- ╚════════════════════╝
    -- IFFT input buffer (subcarriers)
    nextSubcarriers :: State -> Bit -> Unsigned 7 -> Subcarrier -> Unsigned 8 -> Vec 128 Subcarrier -> Vec 128 Subcarrier
    --              ┌state
    --              │    ┌subcarrierWrite'
    --              │    │ ┌subcarrierIndex'
    --              │    │ │ ┌subcarrierValue'
    --              │    │ │ │ ┌_n_fft
    --              │    │ │ │ │ ┌subcarriers
    --              │    │ │ │ │ │
    -- Idle         │    │ │ │ │ │
    nextSubcarriers Init _ _ _ _ _ = repeat (0.0,0.0) :: Vec 128 Subcarrier
    -- WPlt / WDat
    nextSubcarriers _    1 i v n x = replace (ifftshift n i) v x
    nextSubcarriers _    _ _ _ _ x = x

    subcarriers = register (repeat (0.0,0.0) :: Vec 128 Subcarrier) (nextSubcarriers
      <$> state
      <*> (subcarrierWrite')
      <*> (subcarrierIndex')
      <*> (subcarrierValue')
      <*> _n_fft
      <*> subcarriers)

    subcarrierValue :: State -> Unsigned 3 -> Unsigned 2 -> Unsigned 7 -> Bit -> Subcarrier -> Subcarrier
    -- state, pilot_value, modData
    --              ┌state
    --              │          ┌pilot value
    --              │          │ ┌modData
    --              │          │ │
    --              │          │ │ 
    --              │          │ │ 
    -- Idle         │          │ │ 
    subcarrierValue Idle _  _  _ _ _ = (0.0, 0.0)
    -- WPlt
    subcarrierValue WPlt _  _  _ 0 _ = (-1.0, 0.0)
    subcarrierValue WPlt _  _  _ 1 _ = (1.0, 0.0)
    -- WDat
    subcarrierValue WDat sf sc k _ x = sfPhase sf sc k x
    subcarrierValue WrSF sf sc k _ x = sfPhase sf sc k x
    subcarrierValue _    _  _  _ _ _ = (0.0, 0.0)

    -- Conjugate of an IQ pair, this is to calculate the IFFT with an FFT (IFFT chapter)
    iqConj :: IQ -> IQ
    iqConj (a,b) = (a,-b)

    -- spreadCounter, index, old carrier, new carrier
    -- Value to put in the subcarriers
    subcarrierValue' = iqConj <$> (subcarrierValue <$> state <*> _frequencySpreading <*> spreadCounter <*> dataCounter <*> pilotValue <*> (
      mux (state .==. pure WDat)
      dataSubcarrier
      dataSubcarrierStore))

    -- ╔══════╗
    -- ║ IFFT ║
    -- ╚══════╝

    toCplx :: IQ -> Complex MFixed
    toCplx (a,b) = a :+ b

    fromCplx :: Complex MFixed -> IQ
    fromCplx (a :+ b) = (a,b)

    twiddles :: Vec 64 (Complex MFixed)
    twiddles = $(listToVecTH (twiddleFactors 64))

    toCplxVec :: Vec 128 (IQ) -> Vec 128 (Complex MFixed)
    toCplxVec v = toCplx <$> v

    fromCplxVec :: Vec 128 (Complex MFixed) -> Vec 128 (IQ)
    fromCplxVec v = fromCplx <$> v

    -- NOTE : Only the FFT is made by Adam Walker, how to do an IFFT ?
    -- A handy formula exists (source needed)
    -- 
    -- IFFT(x) = 1/N * conj(fft(conj(x)))


    (<$$>) = fmap . fmap

    ifftScale :: Unsigned 8 -> IQ -> IQ
    ifftScale nfft (a,b) = (a `shiftR` s, b `shiftR` s)
      where
        s = case nfft of
          128 -> 5 -- TODO : Check the values
          64 -> 4
          32 -> 3
          16 -> 2
          otherwise -> 0

    scaleIfft :: Unsigned 8 -> Vec 128 IQ -> Vec 128 IQ
    scaleIfft nfft v = ifftScale nfft <$> v

    fftOutputConj :: Signal dom (Vec 128 (Complex MFixed))
    fftOutputConj = fftDITIter128 twiddles <$> (toCplxVec <$> subcarriers)

    rawFftOutput :: Signal dom (Vec 128 IQ)
    rawFftOutput = iqConj <$$> (fromCplxVec <$> fftOutputConj)

    fftOutput :: Signal dom (Vec 128 (IQ))
    fftOutput = scaleIfft <$> _n_fft <*> rawFftOutput