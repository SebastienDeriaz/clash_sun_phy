module SunPhy.MR_OFDM.Constants where

import Clash.Prelude

-- SF as a function of MCS
frequencySpreading :: MCS -> Unsigned 3
frequencySpreading 0 = 4
frequencySpreading 1 = 2
frequencySpreading 2 = 2
frequencySpreading 3 = 1
frequencySpreading 4 = 1
frequencySpreading 5 = 1
frequencySpreading 6 = 1

--data OFDM_Option = 1 | 2 | 3 | 4
type OFDM_Option = (Unsigned 3)

--data MCS = 0 | 1 | 2 | 3 | 4 | 5 | 6
type MCS = (Unsigned 3)

-- N_bpsc as a function of MCS
nbpsc :: MCS -> Unsigned 3
nbpsc 0 = 1
nbpsc 1 = 1
nbpsc 2 = 2
nbpsc 3 = 2
nbpsc 4 = 2
nbpsc 5 = 4
nbpsc 6 = 4

-- N_FFT as a function of OFDM option
n_fft :: OFDM_Option -> Unsigned 8
n_fft 1 = 128
n_fft 2 = 64
n_fft 3 = 32
n_fft 4 = 16

type MFixed = SFixed 16 16
type IQ = (MFixed, MFixed)
type Subcarrier = (MFixed, MFixed)


data Modulation = BPSK
                | QPSK
                | QAM16
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX

-- Number of coded bits per symbol (as a function of modulation)
nbpsc_mod :: Modulation -> Unsigned 2
nbpsc_mod BPSK  = 1 
nbpsc_mod QPSK  = 2
nbpsc_mod QAM16 = 4

mcsModulation :: MCS -> Modulation
mcsModulation 0 = BPSK
mcsModulation 1 = BPSK
mcsModulation 2 = QPSK
mcsModulation 3 = QPSK
mcsModulation 4 = QPSK
mcsModulation 5 = QAM16
mcsModulation 6 = QAM16


pilotTones :: OFDM_Option -> Unsigned 8
pilotTones 1 = 8
pilotTones 2 = 4
pilotTones 3 = 2
pilotTones 4 = 2

pilotSets :: OFDM_Option -> Unsigned 4
pilotSets 1 = 13
pilotSets 2 = 7
pilotSets 3 = 7
pilotSets 4 = 4

dataTones :: OFDM_Option -> Unsigned 8
dataTones 1 = 96
dataTones 2 = 48
dataTones 3 = 24
dataTones 4 = 12

kModBPSK :: MFixed
kModBPSK = 1.0

kModQPSK :: MFixed
kModQPSK = $$(fLit (1 / sqrt 2))

kModQAM16 :: MFixed
kModQAM16 = $$(fLit (1 / sqrt 10))