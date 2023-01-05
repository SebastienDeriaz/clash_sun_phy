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

module SunPhy.MR_OFDM.Twiddle
    ( twiddleFactors
    , halveTwiddles
    ) where

import Clash.Prelude
import qualified Data.Complex as C
import SunPhy.MR_OFDM.Complex
import qualified Prelude as P

-- | Calculate FFT `twiddle` factors. You probably want to do this with template Haskell to ensure they are calculated at compile time.
twiddleFactors
    :: Int
    -- ^ Twiddle factor vector length
    -> [Complex Double]
    -- ^ Twiddle factors
twiddleFactors num = P.take num $ [fromComplex $ C.cis $ (-1) * P.pi * fromIntegral i / fromIntegral num | i <- [0 ..]]

-- | Take every second element of a vector of twiddle factors.
halveTwiddles
    :: KnownNat n
    => Vec (2 (*) n) a
    -- ^ Twiddle factors
    -> Vec n a
    -- ^ Halved output twiddle factors
halveTwiddles vec = transpose (unconcat (SNat @2) vec) !! 0
