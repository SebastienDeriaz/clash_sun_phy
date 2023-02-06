module SunPhy.MR_OFDM.FFT where

import Clash.DSP.Complex
import Clash.DSP.FFT.Parallel
import Clash.Prelude

-- | Compute a 16 element FFT, using the 'decimation in time' algorithm.
fftDITIter16 ::
  forall a.
  (Num a) =>
  -- | Precomputed twiddle factors
  Vec 8 (Complex a) ->
  -- | Input samples
  Vec 16 (Complex a) ->
  -- | Output samples
  Vec 16 (Complex a)
fftDITIter16 cexp8 inp = fft16
 where
  cexp1 :: Vec 1 (Complex a)
  cexp1 = halveTwiddles cexp2
  fft2 = concatMap (butterfly . twiddle cexp1) $ unconcatI $ reorder inp

  cexp2 :: Vec 2 (Complex a)
  cexp2 = halveTwiddles cexp4
  fft4 = concatMap (butterfly . twiddle cexp2) $ unconcatI fft2

  cexp4 :: Vec 4 (Complex a)
  cexp4 = halveTwiddles cexp8
  fft8 = concatMap (butterfly . twiddle cexp4) $ unconcatI fft4

  fft16 = concatMap (butterfly . twiddle cexp8) $ unconcatI fft8

-- | Compute a 32 element FFT, using the 'decimation in time' algorithm.
fftDITIter32 ::
  forall a.
  (Num a) =>
  -- | Precomputed twiddle factors
  Vec 16 (Complex a) ->
  -- | Input samples
  Vec 32 (Complex a) ->
  -- | Output samples
  Vec 32 (Complex a)
fftDITIter32 cexp16 inp = fft32
 where
  cexp1 :: Vec 1 (Complex a)
  cexp1 = halveTwiddles cexp2
  fft2 = concatMap (butterfly . twiddle cexp1) $ unconcatI $ reorder inp

  cexp2 :: Vec 2 (Complex a)
  cexp2 = halveTwiddles cexp4
  fft4 = concatMap (butterfly . twiddle cexp2) $ unconcatI fft2

  cexp4 :: Vec 4 (Complex a)
  cexp4 = halveTwiddles cexp8
  fft8 = concatMap (butterfly . twiddle cexp4) $ unconcatI fft4

  cexp8 :: Vec 8 (Complex a)
  cexp8 = halveTwiddles cexp16
  fft16 = concatMap (butterfly . twiddle cexp8) $ unconcatI fft8

  fft32 = concatMap (butterfly . twiddle cexp16) $ unconcatI fft16

-- | Compute a 64 element FFT, using the 'decimation in time' algorithm.
fftDITIter64 ::
  forall a.
  (Num a) =>
  -- | Precomputed twiddle factors
  Vec 32 (Complex a) ->
  -- | Input samples
  Vec 64 (Complex a) ->
  -- | Output samples
  Vec 64 (Complex a)
fftDITIter64 cexp32 inp = fft64
 where
  cexp1 :: Vec 1 (Complex a)
  cexp1 = halveTwiddles cexp2
  fft2 = concatMap (butterfly . twiddle cexp1) $ unconcatI $ reorder inp

  cexp2 :: Vec 2 (Complex a)
  cexp2 = halveTwiddles cexp4
  fft4 = concatMap (butterfly . twiddle cexp2) $ unconcatI fft2

  cexp4 :: Vec 4 (Complex a)
  cexp4 = halveTwiddles cexp8
  fft8 = concatMap (butterfly . twiddle cexp4) $ unconcatI fft4

  cexp8 :: Vec 8 (Complex a)
  cexp8 = halveTwiddles cexp16
  fft16 = concatMap (butterfly . twiddle cexp8) $ unconcatI fft8

  cexp16 :: Vec 16 (Complex a)
  cexp16 = halveTwiddles cexp32
  fft32 = concatMap (butterfly . twiddle cexp16) $ unconcatI fft16

  fft64 = concatMap (butterfly . twiddle cexp32) $ unconcatI fft32

-- | Compute a 64 element FFT, using the 'decimation in time' algorithm.
fftDITIter128 ::
  forall a.
  (Num a) =>
  -- | Precomputed twiddle factors
  Vec 64 (Complex a) ->
  -- | Input samples
  Vec 128 (Complex a) ->
  -- | Output samples
  Vec 128 (Complex a)
fftDITIter128 cexp64 inp = fft128
 where
  cexp1 :: Vec 1 (Complex a)
  cexp1 = halveTwiddles cexp2
  fft2 = concatMap (butterfly . twiddle cexp1) $ unconcatI $ reorder inp

  cexp2 :: Vec 2 (Complex a)
  cexp2 = halveTwiddles cexp4
  fft4 = concatMap (butterfly . twiddle cexp2) $ unconcatI fft2

  cexp4 :: Vec 4 (Complex a)
  cexp4 = halveTwiddles cexp8
  fft8 = concatMap (butterfly . twiddle cexp4) $ unconcatI fft4

  cexp8 :: Vec 8 (Complex a)
  cexp8 = halveTwiddles cexp16
  fft16 = concatMap (butterfly . twiddle cexp8) $ unconcatI fft8

  cexp16 :: Vec 16 (Complex a)
  cexp16 = halveTwiddles cexp32
  fft32 = concatMap (butterfly . twiddle cexp16) $ unconcatI fft16

  cexp32 :: Vec 32 (Complex a)
  cexp32 = halveTwiddles cexp64
  fft64 = concatMap (butterfly . twiddle cexp32) $ unconcatI fft32

  fft128 = concatMap (butterfly . twiddle cexp64) $ unconcatI fft64
