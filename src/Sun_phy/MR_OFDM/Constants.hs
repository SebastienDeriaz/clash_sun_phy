module Sun_phy.MR_OFDM.Constants where

import Clash.Prelude

-- SF as a function of MCS
frequencySpreading :: Unsigned 3 -> Unsigned 3
frequencySpreading 0 = 4
frequencySpreading 1 = 2
frequencySpreading 2 = 2
frequencySpreading 3 = 1
frequencySpreading 4 = 1
frequencySpreading 5 = 1
frequencySpreading 6 = 1

-- N_bpsc as a function of MCS
nbpsc :: Unsigned 3 -> Unsigned 3
nbpsc 0 = 1
nbpsc 1 = 1
nbpsc 2 = 2
nbpsc 3 = 2
nbpsc 4 = 2
nbpsc 5 = 4
nbpsc 6 = 4

-- N_FFT as a function of OFDM option
nfft :: Unsigned 3 -> Unsigned 8
nfft 1 = 128
nfft 2 = 64
nfft 3 = 32
nfft _ = 16

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

data CP = CP_NONE
        | CP_QUARTER
        | CP_HALF
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass NFDataX