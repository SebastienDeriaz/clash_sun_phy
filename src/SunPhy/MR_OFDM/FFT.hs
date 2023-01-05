module SunPhy.MR_OFDM.FFT where


import Clash.Prelude
import SunPhy.MR_OFDM.Parallel
import SunPhy.MR_OFDM.Complex
import SunPhy.MR_OFDM.Constants




--sctest = register (repeat (0.0,0.0) :: Vec 16 Subcarrier) sctest


twiddles = $(listToVecTH (twiddleFactors 8))

--vec = 0.5:>1.0:>1.0:>1.0:>1.0:>1.0:>1.0:>1.0:>1.0:>1.0:>1.0:>1.0:>1.0:>1.0:>1.0:>1.0:>Nil
vec = (0.5,0.0):>(0.5,0.0):>(0.5,0.0):>(0.5,0.0):>(0.5,0.0):>(0.5,0.0):>(0.5,0.0):>(0.5,0.0):>(0.5,0.0):>(0.5,0.0):>(0.5,0.0):>(0.5,0.0):>(0.5,0.0):>(0.5,0.0):>(0.5,0.0):>(0.5,0.0):>Nil

--ifft = map toComplex ((fftDITRec twiddles (map fromComplex vec)))

toCplx :: Subcarrier -> Complex (SFixed 2 14)
toCplx (a,b) = (a) :+ (b)

fromCplx :: Complex (SFixed 2 14) -> Subcarrier
fromCplx (a :+ b) = (a,b)

--complexVec = toComplex <$> sctest

--ifft v = map fromCplx (fftDITRec twiddles (toCplx <$> v))
--ifft v = fftDITRec twiddles (toCplx <$> v)

vecCplx = toCplx <$> vec

fft = fftDITRec twiddles vecCplx

--ifftVec :: Signal dom (Vec 16 Subcarrier)
--ifftVec = ifft <$> sctest

--test = ifft vec

--ifft = toComplex <$> ((fftDITRec twiddles <$> (toComplex <$> sctest)))