-- Copyright (c) 2016, Adam Walker

-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:

--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.

--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.

--     * Neither the name of Adam Walker nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-| Radix 2 complex-to-complex Cooley-Tukey FFTs. https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm.
    The FFTs in this module are fully parallel, which means they use a large number of multipliers and routing resources and are only practical for smaller FFTs.

    TODO: use the dependently typed fold to define FFTs of any length.
-}
module Sun_phy.MR_OFDM.Parallel (
    twiddleFactors,
    halveTwiddles,
    reorder,
    fftStepDITRec,
    fftStepDIFRec,
    fftDITRec,
    fftDIFRec,
    butterfly,
    twiddle,
    fftDITIter,
    fftDIFIter
    ) where

import Clash.Prelude

import Sun_phy.MR_OFDM.Complex
import Sun_phy.MR_OFDM.Twiddle

-- | Reorder FFT imput or output samples.
reorder 
    :: forall n a. KnownNat n 
    => Vec (2 ^ n) a -- ^ Input samples
    -> Vec (2 ^ n) a -- ^ Reordered output samples
reorder inp = map (inp !!) indices
    where
    indices :: Vec (2 ^ n) (BitVector n)
    indices = map revBits $ iterateI (+1) 0
    revBits :: forall n. KnownNat n => BitVector n -> BitVector n
    revBits x = pack $ reverse (unpack x :: Vec n Bit)

-- | A step in the recursive decimation in time FFT
fftStepDITRec 
    :: forall n a . (Num a, KnownNat n)
    => Vec n (Complex a)
    -> (Vec n (Complex a) -> Vec n (Complex a))
    -> Vec (2 * n) (Complex a)
    -> Vec (2 * n) (Complex a)
fftStepDITRec twiddleFactors recurse input = zipWith (+) fft1 twiddled ++ zipWith (-) fft1 twiddled
    where
    grouped     :: Vec n (Vec 2 (Complex a))
    grouped     =  unconcatI input
    partitioned :: Vec 2 (Vec n (Complex a))
    partitioned =  transpose grouped

    fft1        =  recurse $ partitioned !! 0
    fft2        =  recurse $ partitioned !! 1
    twiddled    =  zipWith (*) twiddleFactors fft2

-- | A step in the recursive decimation in frequency FFT
fftStepDIFRec 
    :: forall n a . (Num a, KnownNat n)
    => Vec n (Complex a)
    -> (Vec n (Complex a) -> Vec n (Complex a))
    -> Vec (2 * n) (Complex a)
    -> Vec (2 * n) (Complex a)
fftStepDIFRec twiddleFactors recurse input = fft1 ++ fft2
    where
    partitioned :: Vec 2 (Vec n (Complex a))
    partitioned =  unconcatI input

    fft1        = recurse $ zipWith (+) (partitioned !! 0) (partitioned !! 1)
    fft2        = recurse $ zipWith (*) twiddleFactors $ zipWith (-) (partitioned !! 0) (partitioned !! 1)

-- | Compute a 16 element FFT, using the 'decimation in time' algorithm recursively. You probably want to use `fftDITIter` as it can be more easily pipelined.
fftDITRec
    :: forall a. (Num a, Floating a)
    => Vec 8 (Complex a)  -- ^ Precomputed twiddle factors
    -> Vec 16 (Complex a) -- ^ Input samples
    -> Vec 16 (Complex a) -- ^ Output samples
fftDITRec twiddles = fft16
    where

    cexp1 :: Vec 1 (Complex a)
    cexp1 =  halveTwiddles cexp2
    fft2  :: Vec 2 (Complex a) -> Vec 2 (Complex a)
    fft2  =  fftStepDITRec cexp1 id

    cexp2 :: Vec 2 (Complex a)
    cexp2 =  halveTwiddles cexp4
    fft4  :: Vec 4 (Complex a) -> Vec 4 (Complex a)
    fft4  =  fftStepDITRec cexp2 fft2

    cexp4 :: Vec 4 (Complex a)
    cexp4 =  halveTwiddles twiddles
    fft8  :: Vec 8 (Complex a) -> Vec 8 (Complex a)
    fft8  =  fftStepDITRec cexp4 fft4

    fft16 :: Vec 16 (Complex a) -> Vec 16 (Complex a)
    fft16 =  fftStepDITRec twiddles fft8

-- | Compute a 16 element FFT, using the 'decimation in frequency' algorithm recursively. You probably want to use `fftDIFIter` as it can be more easily pipelined.
fftDIFRec 
    :: forall a. (Num a, Floating a)
    => Vec 8 (Complex a)  -- ^ Precomputed twiddle factors
    -> Vec 16 (Complex a) -- ^ Input samples
    -> Vec 16 (Complex a) -- ^ Output samples
fftDIFRec twiddles input = reorder $ fft16 input
    where

    cexp1 :: Vec 1 (Complex a)
    cexp1 =  halveTwiddles cexp2
    fft2  :: Vec 2 (Complex a) -> Vec 2 (Complex a)
    fft2  =  fftStepDIFRec cexp1 id

    cexp2 :: Vec 2 (Complex a)
    cexp2 =  halveTwiddles cexp4
    fft4  :: Vec 4 (Complex a) -> Vec 4 (Complex a)
    fft4  =  fftStepDIFRec cexp2 fft2

    cexp4 :: Vec 4 (Complex a)
    cexp4 =  halveTwiddles twiddles
    fft8  :: Vec 8 (Complex a) -> Vec 8 (Complex a)
    fft8  =  fftStepDIFRec cexp4 fft4

    fft16 :: Vec 16 (Complex a) -> Vec 16 (Complex a)
    fft16 =  fftStepDIFRec twiddles fft8

-- | The FFT butterfly calculation
butterfly 
    :: forall n a. (Num a, KnownNat n)
    => Vec (2 * n) (Complex a)
    -> Vec (2 * n) (Complex a)
butterfly input = butterflied1 ++ butterflied2
    where
    partitioned  :: Vec 2 (Vec n (Complex a))
    partitioned  =  unconcatI input
    butterflied1 =  zipWith (+) (partitioned !! 0) (partitioned !! 1)
    butterflied2 =  zipWith (-) (partitioned !! 0) (partitioned !! 1)

-- | The FFT twiddle step
twiddle 
    :: forall n a. (Num a, KnownNat n)
    => Vec n (Complex a)
    -> Vec (2 * n) (Complex a)
    -> Vec (2 * n) (Complex a)
twiddle twiddleFactors input = (partitioned !! 0) ++ twiddled
    where
    partitioned :: Vec 2 (Vec n (Complex a))
    partitioned =  unconcatI input
    twiddled    =  zipWith (*) twiddleFactors (partitioned !! 1)

-- | Compute a 16 element FFT, using the 'decimation in time' algorithm.
fftDITIter 
    :: forall a. (Num a)
    => Vec 8 (Complex a)  -- ^ Precomputed twiddle factors
    -> Vec 16 (Complex a) -- ^ Input samples
    -> Vec 16 (Complex a) -- ^ Output samples
fftDITIter twiddles inp = fft16
    where

    cexp1 :: Vec 1 (Complex a)
    cexp1 =  halveTwiddles cexp2
    fft2  =  concat $ map (butterfly . twiddle cexp1) $ unconcatI $ reorder inp

    cexp2 :: Vec 2 (Complex a)
    cexp2 =  halveTwiddles cexp4
    fft4  =  concat $ map (butterfly . twiddle cexp2) $ unconcatI fft2

    cexp4 :: Vec 4 (Complex a)
    cexp4 =  halveTwiddles twiddles
    fft8  =  concat $ map (butterfly . twiddle cexp4) $ unconcatI fft4

    fft16 =  concat $ map (butterfly . twiddle twiddles) $ unconcatI fft8

-- | Compute a 16 element FFT, using the 'decimation in frequency' algorithm.
fftDIFIter 
    :: forall a. (Num a)
    => Vec 8 (Complex a)  -- ^ Presomputed twiddle factors
    -> Vec 16 (Complex a) -- ^ Input samples
    -> Vec 16 (Complex a) -- ^ Output samples
fftDIFIter twiddles input = reorder fft2
    where

    fft16 =  concat $ map (twiddle twiddles . butterfly) $ unconcatI input
    
    cexp4 :: Vec 4 (Complex a)
    cexp4 =  halveTwiddles twiddles
    fft8  =  concat $ map (twiddle cexp4 . butterfly) $ unconcatI fft16

    cexp2 :: Vec 2 (Complex a)
    cexp2 =  halveTwiddles cexp4
    fft4  =  concat $ map (twiddle cexp2 . butterfly) $ unconcatI fft8

    cexp1 :: Vec 1 (Complex a)
    cexp1 =  halveTwiddles cexp2
    fft2  =  concat $ map (twiddle cexp1 . butterfly) $ unconcatI fft4

