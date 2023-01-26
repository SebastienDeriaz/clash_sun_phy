module SunPhy.MR_OFDM.OFDM where

import Clash.Prelude
import SunPhy.MR_OFDM.Constants
import Clash.DSP.FFT.Example
import Clash.DSP.Complex
import Clash.DSP.FFT.Parallel
import SunPhy.PN9
import SunPhy.MR_OFDM.OFDM_Pilots
import SunPhy.MR_OFDM.Modulator

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
           | WPlt
           | Wait -- Wait for the pilots to be set for 1 clock cycle
           | WDat
           | WrSF
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

-- -- Convert a counter value to an index when accounting for SF effect
-- counterToSpreadIndex :: Unsigned 3 -> Unsigned 8 -> Unsigned 8 -> Unsigned 7 -> Unsigned 7
-- -- 1x (no change)
-- counterToSpreadIndex 1 nfft at i = subcarrierCounterToIndex nfft at i
-- -- 2x and 4x
-- -- 2x bbbbbbbbDmmmmmmmm
-- -- 4x ccccddddDmmmmbbbb
-- counterToSpreadIndex s nfft at i = out (it + offset s m)
--   where
--     at2 = resize $ at `div` 2 :: Unsigned 7
--     at4 = resize $ at `div` 4  :: Unsigned 7
--     it = i `div` (resize s)

--     m = i `mod` (resize s)
--     --offset s m
--     offset 2 0 = 0
--     offset 2 1 = at2
--     offset 4 0 = 0
--     offset 4 1 = at4
--     offset 4 2 = at2
--     offset 4 3 = at2 + at4

--     out j
--      | j < at2 = subcarrierCounterToIndex nfft at (j + at2)
--      | otherwise = subcarrierCounterToIndex nfft at (j - at2)


counterToSpreadIndex :: Unsigned 3 -> Unsigned 8 -> Unsigned 8 -> Unsigned 2 -> Unsigned 7 -> Unsigned 7 
-- spreadingFactor, nfft, activeTones, spreadIndex, index -> new index
-- 1x
counterToSpreadIndex 1 nfft at _  i = subcarrierCounterToIndex nfft at i
-- 2x
counterToSpreadIndex s nfft at si i = out (i + offset s si)
  where
    at2 = resize $ at `div` 2 :: Unsigned 7
    at4 = resize $ at `div` 4  :: Unsigned 7

    --offset s m
    offset 2 0 = 0
    offset 2 1 = at2
    offset 4 0 = 0
    offset 4 1 = at4
    offset 4 2 = at2
    offset 4 3 = at2 + at4
    offset _ _ = 0

    out j
     | j < at2 = subcarrierCounterToIndex nfft at (j + at2)
     | otherwise = subcarrierCounterToIndex nfft at (j - at2)

ofdm
  :: forall dom . HiddenClockResetEnable dom
  -- OFDM Settings
  => Signal dom OFDM_Option -- OFDM_option
  -> Signal dom MCS -- MCS
  -> Signal dom Bit -- enable CP (1/4)
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
  -> Signal dom (Bit, Bit, IQ, Bit, Unsigned 4, BitVector 9, State, Unsigned 7)
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
    mod_ready_o,
    valid_o,
    data_o,
    last_o,
    pilotSetCounter,
    pn9_reg,
    state,
    subcarrierIndex
    )
  where
    _n_fft = n_fft <$> ofdmOption

    (mod_ready_o, mod_valid_o, mod_data_o, mod_last_o) = unbundle $ modulator
      mcs
      payload_valid_i
      payload_data_i
      payload_last_i
      mod_ready_i
      
      
    mod_ready_i = (mod_ready <$> state)

    -- ╔══════════════════════════════╗
    -- ║ Outputs and output functions ║
    -- ╚══════════════════════════════╝
    -- pilot_ready_o
    pilot_ready_o = boolToBit <$> (state .==. pure WPlt)

    mod_ready :: State -> Bit
    mod_ready WDat  = 1
    mod_ready _     = 0

    -- data_o
    dataOut :: Enum n => State -> Vec 128 IQ -> n -> IQ
    --      ┌state
    --      │    ┌iq vector
    --      │    │ ┌index
    --      │    │ │
    dataOut OuCP v i = v !! i
    dataOut Outp v i = v !! i
    dataOut _    _ _ = (0,0)

    data_o        = dataOut <$> state <*> fftOutput <*> subcarrierIndex

    valid :: State -> Bit
    valid Outp = 1
    valid OuCP = 1
    valid _    = 0

    valid_o       = valid <$> state
    -- last_o
    last_o        = boolToBit <$> (
      subcarrierCounterEnd' .==. 1 .&&.
      isLast .==. 1 .&&.
      state .==. pure Outp)


    -- ╔═══════════╗
    -- ║ Shortcuts ║
    -- ╚═══════════╝

    mod_write = mod_ready_i * mod_valid_o
    slaveWrite = valid_o * ready_i

    _modulation = mcsModulation <$> mcs
    _frequencySpreading = frequencySpreading <$> mcs
    _dataTones = dataTones <$> ofdmOption
    _pilotTones = pilotTones <$> ofdmOption
    _activeTones = _dataTones + _pilotTones

    n_dbps :: Unsigned 8 -> Unsigned 3 -> Unsigned 7
    -- dataTones, frequencySpreading
    n_dbps dt sf = (resize dt) `div` (resize sf)

    _ndbps = n_dbps <$> _dataTones <*> _frequencySpreading

    -- ╔═══════════════╗
    -- ║ State machine ║
    -- ╚═══════════════╝

    -- State
    nextState :: State -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Unsigned 2 -> Bit -> Bit -> Bit -> State
    --        ┌state
    --        │    ┌pilotset_write_i
    --        │    │ ┌pilotCounterEnd
    --        │    │ │ ┌data_valid_i
    --        │    │ │ │ ┌dataCounterEnd'
    --        │    │ │ │ │ ┌subcarrierCounterEnd
    --        │    │ │ │ │ │ ┌subcarrierWrite
    --        │    │ │ │ │ │ │ ┌spreadCounter
    --        │    │ │ │ │ │ │ │ ┌spreadCounterEnd
    --        │    │ │ │ │ │ │ │ │ ┌cp_enable
    --        │    │ │ │ │ │ │ │ │ │ ┌slaveWrite
    -- Init   │    │ │ │ │ │ │ │ │ │ │
    nextState Init _ _ _ _ _ _ _ _ _ _ = WPlt -- Write the pilots
    -- Idle
    -- nextState Idle 1 _ _ _ _ _ _ _ _ _ = WPlt -- Write the pilots
    -- nextState Idle _ _ 1 _ _ _ _ _ _ _ = WDat
    -- nextState Idle _ _ _ _ _ _ _ _ _ _ = Idle
    -- Write pilot                             
    nextState WPlt _ 1 _ _ _ _ _ _ _ _ = Wait -- Go to Write data
    nextState WPlt _ 0 _ _ _ _ _ _ _ _ = WPlt
    -- Wait
    nextState Wait _ _ _ _ _ _ _ _ _ _ = WDat
    -- Write data                             
    nextState WDat _ _ _ 1 _ 1 _ 0 _ _ = WrSF
    nextState WDat _ _ _ 1 _ 1 0 _ 0 _ = Outp
    nextState WDat _ _ _ 1 _ 1 0 _ 1 _ = OuCP
    nextState WDat _ _ _ _ _ _ _ _ _ _ = WDat
    -- Write frequency spreading             
    nextState WrSF _ _ _ 1 _ _ _ 1 0 _ = Outp
    nextState WrSF _ _ _ 1 _ _ _ 1 1 _ = OuCP
    nextState WrSF _ _ _ _ _ _ _ _ _ _ = WrSF
    -- Output CP                             
    nextState OuCP _ _ _ _ 1 _ _ _ _ 1 = Outp
    nextState OuCP _ _ _ _ _ _ _ _ _ _ = OuCP
    -- Output                             
    nextState Outp _ _ _ _ 1 _ _ _ _ 1 = WPlt
    nextState Outp _ _ _ _ _ _ _ _ _ _ = Outp

    nextState' = nextState
      <$> state
      <*> pilotset_write_i
      <*> pilotCounterEnd
      <*> mod_valid_o
      <*> dataCounterEnd'
      <*> subcarrierCounterEnd'
      <*> subcarrierWrite'
      <*> spreadCounter
      <*> spreadCounterEnd
      <*> cp
      <*> slaveWrite

    frequencySpreadingEnable = boolToBit <$> (_frequencySpreading ./=. 1)

    state = register (Init :: State) nextState'

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
    nextIsLast WDat l _ = l
    nextIsLast _    _ x = x

    isLast = register (0 :: Bit) (nextIsLast <$> state <*> mod_last_o <*> isLast)

    -- ╔═════════════════╗
    -- ║ Data subcarrier ║
    -- ╚═════════════════╝
    -- Subcarrier that results from one or more data bits received

    k_mod :: Modulation -> Subcarrier -> Subcarrier
    k_mod BPSK  (a,b) = (a * kModBPSK, b * kModBPSK)
    k_mod QPSK  (a,b) = (a * kModQPSK, b * kModQPSK)
    k_mod QAM16 (a,b) = (a * kModQAM16, b * kModQAM16)

    dataSubcarrier = k_mod <$> _modulation <*> mod_data_o
    --dataSubcarrier = mod_data_o

    -- ╔═════════════════════╗
    -- ║ Frequency spreading ║
    -- ╚═════════════════════╝

    -- Frequency spread counter
    nextSpreadCounter :: State -> Bit -> Bit -> Bit -> Unsigned 2 -> Unsigned 2
    -- subcarrierCounterEnd dataCounterEnd' subcarrierWrite counter
    --                ┌state
    --                │    ┌subcarrierCounterEnd
    --                │    │ ┌dataCounterEnd
    --                │    │ │ ┌subcarrierWrite
    --                │    │ │ │ ┌counter
    --                │    │ │ │ │ 
    nextSpreadCounter WDat _ 1 1 x = 1
    nextSpreadCounter WDat _ _ _ x = x
    nextSpreadCounter WrSF _ 1 _ x = x + 1
    nextSpreadCounter WrSF _ _ _ x = x
    nextSpreadCounter _    _ _ _ _ = 0

    spreadCounter = register (0 :: Unsigned 2) (nextSpreadCounter <$> state <*> subcarrierCounterEnd' <*> dataCounterEnd' <*> subcarrierWrite' <*> spreadCounter)
    spreadCounterEnd = boolToBit <$> (spreadCounter .==. (resize <$> _frequencySpreading - 1))

    
    sfPhase :: Unsigned 3 -> Unsigned 2 -> Unsigned 7 -> Subcarrier -> Subcarrier
    -- frequencySpreading, spreadCounter, index, old carrier, new carrier
    -- 1x, no change
    sfPhase 1 _ _ x = x
    -- 2x, * np.exp(1j*2*np.pi*(2*k-1)/4)
    sfPhase 2 0 _ x = (fst x, snd x)
    sfPhase 2 1 d x
      -- *(-1j) (second, fourth, ...)
      | even k      = (snd x, -fst x)
      -- *(+1j) (first, third, ...)
      | otherwise   = (- snd x, fst x)
      where
        -- k is dataCounter + 1 because the tone index start at 1. See 18.2.3.6.1 of 802.15.4g-2012
        k = d + 1
    -- 4x
    sfPhase _ _ _ x = x

    -- ╔══════════════╗
    -- ║ Data counter ║
    -- ╚══════════════╝
    -- 
    nextDataCounter :: State -> Bit -> Bit -> Unsigned 7 -> Unsigned 7
    --              ┌state
    --              │    ┌subcarrierWrite'
    --              │    │ ┌dataCounterEnd'
    --              │    │ │ 
    nextDataCounter WPlt _ _ _ = 0
    nextDataCounter WDat 1 1 _ = 0
    nextDataCounter WDat 1 _ x = x + 1
    nextDataCounter WrSF 1 1 _ = 0
    nextDataCounter WrSF 1 _ x = x + 1
    nextDataCounter _    _ _ x = x

    dataCounter = register (0 :: Unsigned 7) $ nextDataCounter
      <$> state
      <*> subcarrierWrite'
      <*> dataCounterEnd'
      <*> dataCounter


    dataCounterEnd' = boolToBit <$> (dataCounter .==. (resize <$> _ndbps - 1))
    
    


  
    -- ╔══════════════════════════════╗
    -- ║ Subcarrier writing + counter ║
    -- ╚══════════════════════════════╝
    -- 

    subcarrierWrite :: State -> Bit -> Bit
    --              ┌state
    --              │     ┌mod_write
    -- Write pilot  │     │ 
    subcarrierWrite WPlt  _ = 1
    -- Write data
    subcarrierWrite WDat  x = x
    -- Write frequency spreading
    subcarrierWrite WrSF  _ = 1
    subcarrierWrite _     _ = 0

    subcarrierWrite' = subcarrierWrite <$> state <*> mod_write

    -- Counts the number of subcarrier written ("true message" and frequency spreading)
    -- Also counts the output index
    nextSubcarrierCounter :: State -> Bit -> Unsigned 8 -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Unsigned 7 -> Unsigned 7
    --                    ┌state
    --                    │    ┌cp
    --                    │    │ ┌_n_fft 
    --                    │    │ │ ┌subcarrierWrite
    --                    │    │ │ │ ┌subcarrierCounterEnd
    --                    │    │ │ │ │ ┌dataCounterEnd'
    --                    │    │ │ │ │ │ ┌slaveWrite
    --                    │    │ │ │ │ │ │ ┌spreadCounterEnd
    --                    │    │ │ │ │ │ │ │ ┌pilotNext 
    --                    │    │ │ │ │ │ │ │ │ ┌pilotAtStart
    --                    │    │ │ │ │ │ │ │ │ │ ┌counter
    -- Idle + Init        │    │ │ │ │ │ │ │ │ │ │
    nextSubcarrierCounter Init _ _ _ _ _ _ _ _ _ _ = 0
    -- Write pilots
    nextSubcarrierCounter Wait _ _ _ _ _ _ _ _ 0 _ = 0
    nextSubcarrierCounter Wait _ _ _ _ _ _ _ _ 1 _ = 1
    -- Writing
    nextSubcarrierCounter WDat _ _ 1 _ 0 _ _ 0 _ x = x + 1 -- Increment when data is written to the subcarriers
    nextSubcarrierCounter WDat _ _ 1 _ 0 _ _ 1 _ x = x + 2 -- Skip pilot
    nextSubcarrierCounter WDat 0 _ 1 _ 1 _ 1 _ _ _ = 0 -- Reset for output (no CP)
    nextSubcarrierCounter WDat 1 n 1 _ 1 _ 1 _ _ _ = resize $ n `div` 4 * 3 -- Go at 3/4
    nextSubcarrierCounter WDat _ _ 1 _ 1 _ _ _ 0 _ = 0 -- Reset for WrSF
    nextSubcarrierCounter WDat _ _ 1 _ 1 _ _ _ 1 _ = 1 -- Reset for WrSF (skip the first subcarrier, which is a pilot)
    -- Frequency spreading
    nextSubcarrierCounter WrSF 0 _ 1 _ 1 _ _ _ 0 _ = 0
    nextSubcarrierCounter WrSF 0 _ 1 _ 1 _ _ _ 1 _ = 1 -- skip the first pilot
    nextSubcarrierCounter WrSF 1 n 1 _ 1 _ _ _ _ _ = resize $ n `div` 4 * 3 -- Go at 3/4
    nextSubcarrierCounter WrSF _ _ _ _ _ _ _ 0 _ x = x + 1
    nextSubcarrierCounter WrSF _ _ _ _ _ _ _ 1 _ x = x + 2
    -- Reading
    nextSubcarrierCounter Outp _ _ _ 0 _ 1 _ _ _ x = x + 1
    nextSubcarrierCounter Outp _ _ _ 1 _ 1 _ _ _ x = 0
    -- Reading (CP)
    nextSubcarrierCounter OuCP _ _ _ 0 _ 1 _ _ _ x = x + 1
    nextSubcarrierCounter OuCP _ _ _ 1 _ 1 _ _ _ _ = 0 -- Reset for output (not CP this time)
    -- Default
    nextSubcarrierCounter _    _ _ _ _ _ _ _ _ _ x = x

    -- Output step, this is for getting a N/step sized FFT out of the 128FFT
    outputStep :: Unsigned 8 -> Unsigned 7
    outputStep a = resize $ 128 `div` a   

    subcarrierCounter      = register (0 :: Unsigned 7) $ nextSubcarrierCounter
      <$> state
      <*> cp
      <*>_n_fft
      <*> subcarrierWrite'
      <*> subcarrierCounterEnd'
      <*> dataCounterEnd'
      <*> slaveWrite
      <*> spreadCounterEnd
      <*> pilotNext
      <*> pilotAtStart
      <*> subcarrierCounter


    -- -- This counter is only used for frequency spreading
    nextSpreadSubcarrierCounter :: State -> Bit -> Unsigned 8 -> Bit -> Bit -> Bit -> Bit -> Unsigned 7 -> Unsigned 7
    --                          ┌state
    --                          │    ┌cp
    --                          │    │ ┌_n_fft 
    --                          │    │ │ ┌subcarrierWrite
    --                          │    │ │ │ ┌dataCounterEnd'
    --                          │    │ │ │ │ ┌spreadPilotNext 
    --                          │    │ │ │ │ │ ┌spreadPilotAtStart
    --                          │    │ │ │ │ │ │ ┌counter
    --                          │    │ │ │ │ │ │ │
    nextSpreadSubcarrierCounter WDat _ _ _ _ _ 0 _ = 0
    nextSpreadSubcarrierCounter WDat _ _ _ _ _ 1 _ = 1
    nextSpreadSubcarrierCounter WrSF 0 _ 1 1 _ 0 _ = 0
    nextSpreadSubcarrierCounter WrSF 0 _ 1 1 _ 1 _ = 1
    nextSpreadSubcarrierCounter WrSF 1 n 1 1 _ _ _ = resize $ n `div` 4 * 3 -- Go at 3/4
    nextSpreadSubcarrierCounter WrSF _ _ _ _ 0 _ x = x + 1
    nextSpreadSubcarrierCounter WrSF _ _ _ _ 1 _ x = x + 2
    nextSpreadSubcarrierCounter _    _ _ _ _ _ _ x = x

    spreadSubcarrierCounter = register (0 :: Unsigned 7) $ nextSpreadSubcarrierCounter
      <$> state
      <*> cp
      <*> _n_fft
      <*> subcarrierWrite'
      <*> dataCounterEnd'
      <*> spreadPilotNext
      <*> spreadPilotAtStart
      <*> spreadSubcarrierCounter
    

    stateIndex :: State -> Unsigned 7 -> Unsigned 7 -> Unsigned 7 -> Unsigned 7 -> Unsigned 7
    --         ┌state
    --         │    ┌subcarrierPilotIndex
    --         │    │ ┌subcarrierReadIndex
    --         │    │ │ ┌subcarrierDataWriteIndex
    --         │    │ │ │ ┌subcarrierSFWriteIndex
    --         │    │ │ │ │
    stateIndex WPlt x _ _ _ = x
    stateIndex Outp _ x _ _ = x
    stateIndex OuCP _ x _ _ = x
    stateIndex WDat _ _ x _ = x
    stateIndex WrSF _ _ _ x = x
    stateIndex _    _ _ _ _ = 0

    subcarrierDataWriteIndex = counterToSpreadIndex <$> _frequencySpreading <*> _n_fft <*> _activeTones <*> 0 <*> subcarrierCounter
    subcarrierSFWriteIndex   = counterToSpreadIndex <$> _frequencySpreading <*> _n_fft <*> _activeTones <*> spreadCounter <*> spreadSubcarrierCounter
    subcarrierReadIndex  = subcarrierCounter * (outputStep <$> _n_fft)
    subcarrierPilotIndex = pilotIndex

    subcarrierIndex = stateIndex
      <$> state
      <*> subcarrierPilotIndex
      <*> subcarrierReadIndex
      <*> subcarrierDataWriteIndex
      <*> subcarrierSFWriteIndex



    subcarrierCounterEnd :: State -> Unsigned 8 -> Unsigned 8 -> Unsigned 3 -> Unsigned 7 -> Bit
    -- state, nfft, dataTones, frequencySpreading
    -- WDat (Number of data bits in the OFDM spectrum)
    -- subcarrierCounterEnd WDat _ dt sf i = boolToBit $ i == (nData - 1)
      -- where
        -- nData = (resize dt) `div` (resize sf)
    -- WrSF (Write Frequency spreading)
    subcarrierCounterEnd WrSF _ dt sf i = boolToBit $ i == (nData - 1)
      where
        nData = (resize dt) `div` (resize sf)
    -- Cyclic prefix out
    subcarrierCounterEnd OuCP nfft _  _  i = boolToBit $ i == (resize $ nfft - 1)
    subcarrierCounterEnd Outp nfft _  _  i = boolToBit $ i == (resize $ nfft - 1)
    subcarrierCounterEnd _    _    _  _  _ = 0

    subcarrierCounterEnd' = subcarrierCounterEnd <$> state <*> _n_fft <*> _dataTones <*> _frequencySpreading <*> subcarrierCounter
    
    pilotNext = boolToBit <$> (testBit <$> pilotsFlags <*> (fromEnum <$> (counterToSpreadIndex <$> _frequencySpreading <*> _n_fft <*> _activeTones <*> 0 <*> (subcarrierCounter + 1))))
    pilotAtStart = boolToBit <$> (testBit <$> pilotsFlags <*> (fromEnum <$> (counterToSpreadIndex <$> _frequencySpreading <*> _n_fft <*> _activeTones <*> 0 <*> 0)))

    spreadPilotNext = boolToBit <$> (testBit <$> pilotsFlags <*> (fromEnum <$> (counterToSpreadIndex <$> _frequencySpreading <*> _n_fft <*> _activeTones <*> spreadCounter <*> (spreadSubcarrierCounter + 1))))
    spreadPilotAtStart = boolToBit <$> (testBit <$> pilotsFlags <*> (fromEnum <$> (counterToSpreadIndex <$> _frequencySpreading <*> _n_fft <*> _activeTones <*> spreadCounter <*> 0)))

    

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
    nextPilotCounter :: State -> Bit -> Bit -> Unsigned 3 -> Unsigned 3
    --               ┌state
    --               │    ┌pilotCounterEnd
    --               │    │ ┌pilotset_write_i
    --               │    │ │ ┌pilotCounter
    --               │    │ │ │
    nextPilotCounter WPlt 0 1 x = 0
    nextPilotCounter WPlt 0 0 x = x + 1
    nextPilotCounter _    _ _ _ = 0
    pilotCounter = register (0 :: Unsigned 3) $ nextPilotCounter <$> state <*> pilotCounterEnd <*> pilotset_write_i <*> pilotCounter
    pilotCounterEnd = boolToBit <$> (pilotCounter .==. (resize <$> (_pilotTones - 1)))

    

    -- Output the pilot position based on ofdmOption, pilotSetCounter and pilotCounter
    pilot :: OFDM_Option -> Unsigned 4 -> Unsigned 3 -> Unsigned 7
    pilot 1 s p = (pilots_indices_1 !! s) !! p
    pilot 2 s p = (pilots_indices_2 !! s) !! p
    pilot 3 s p = (pilots_indices_3 !! s) !! p
    pilot 4 s p = (pilots_indices_4 !! s) !! p

    pilotIndex = pilot <$> ofdmOption <*> pilotSetCounter <*> pilotCounter

    -- PN9 Sequence for pilot generation
    -- nextPn9_seed :: BitVector 9 -> BitVector 9
    -- nextPn9_seed x = x
    -- pn9_seed = register (0b1_1111_1111 :: BitVector 9) $ nextPn9_seed <$> pn9_seed


    pn9_next = boolToBit <$> (state .==. pure WPlt)
    (pilotValue, pn9_reg) = unbundle $ pn9 pn9seed_data_i pn9_next pn9seed_write_i



    -- ╔════════════════════╗
    -- ║ Subcarriers vector ║
    -- ╚════════════════════╝
    -- IFFT input buffer (subcarriers)
    nextSubcarriers :: State -> Bit -> Unsigned 7 -> Subcarrier -> Unsigned 8 -> Vec 128 Subcarrier -> Vec 128 Subcarrier
    --              ┌state
    --              │    ┌subcarrierWrite'
    --              │    │ ┌subcarrierIndex
    --              │    │ │ ┌subcarrierValue'
    --              │    │ │ │ ┌_n_fft
    --              │    │ │ │ │ ┌subcarriers
    --              │    │ │ │ │ │
    -- Idle         │    │ │ │ │ │
    nextSubcarriers Init _ _ _ _ _ = repeat (0.0,0.0) :: Vec 128 Subcarrier
    -- WPlt / WDat
    -- Apply conjugate and ifftshirt here to help the IFFT
    nextSubcarriers _    1 i v n x = replace (ifftshift n i) (iqConj v) x
    nextSubcarriers _    _ _ _ _ x = x

    subcarriers = register (repeat (0.0,0.0) :: Vec 128 Subcarrier) $ nextSubcarriers
      <$> state
      <*> subcarrierWrite'
      <*> subcarrierIndex
      <*> subcarrierValue'
      <*> _n_fft
      <*> subcarriers

    --pilotSkipIndex = indexWithPilotSkip <$> subcarrierIndex <*> pilotHere

    --pilotHere = boolToBit <$> (testBit <$> pilotsFlags <*> (fromEnum <$> subcarrierIndex))

    -- indexWithPilotSkip :: Unsigned 7 -> Bit -> Unsigned 7
    -- indexWithPilotSkip i 0 = i
    -- indexWithPilotSkip i 1 = i + 1

    subcarrierValue :: State -> Unsigned 3 -> Unsigned 2 -> Unsigned 7 -> Bit -> Subcarrier -> Subcarrier -> Subcarrier
    --              ┌state
    --              │    ┌frequencySpreading  
    --              │    │  ┌spreadCounter
    --              │    │  │  ┌dataCounter
    --              │    │  │  │ ┌pilot value 
    --              │    │  │  │ │ ┌dataSubcarrier 
    --              │    │  │  │ │ │ ┌dataSubcarrierRead 
    -- WPlt         │    │  │  │ │ │ │
    subcarrierValue WPlt _  _  _ 0 _ _ = (-1.0, 0.0)
    subcarrierValue WPlt _  _  _ 1 _ _ = (1.0, 0.0)
    -- WDat
    subcarrierValue WDat _  _  _ _ x _ = x
    subcarrierValue WrSF sf sc k _ _ x = sfPhase sf sc k x
    --subcarrierValue WrSF _  _  _ _ _ x = x
    subcarrierValue _    _  _  _ _ _ _ = (0.0, 0.0)

    dataSubcarrierReadIndex = counterToSpreadIndex <$> _frequencySpreading <*> _n_fft <*> _activeTones <*> 0 <*> subcarrierCounter
    dataSubcarrierRead :: Vec 128 Subcarrier -> Unsigned 7 -> Subcarrier
    dataSubcarrierRead sc i = sc !! i 
    dataSubcarrierRead' = iqConj <$> (dataSubcarrierRead <$> subcarriers <*> (ifftshift <$> _n_fft <*> dataSubcarrierReadIndex))


    -- spreadCounter, index, old carrier, new carrier
    -- Value to put in the subcarriers
    subcarrierValue' = subcarrierValue
      <$> state
      <*> _frequencySpreading
      <*> spreadCounter
      <*> dataCounter
      <*> pilotValue
      <*> dataSubcarrier
      <*> dataSubcarrierRead'

    -- ╔══════╗
    -- ║ IFFT ║
    -- ╚══════╝
    -- Conjugate of an IQ pair, this is to calculate the IFFT with an FFT (IFFT chapter)
    iqConj :: IQ -> IQ
    iqConj (a,b) = (a,-b)

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

    -- NOTE : Only the FFT is made by Adam Walker, how to do an IFFT then ?
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