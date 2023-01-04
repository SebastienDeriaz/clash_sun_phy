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

{-# LANGUAGE StandaloneDeriving, UndecidableInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable #-} --For the BitPack instance
module Sun_phy.MR_OFDM.Complex where

import Clash.Prelude

import GHC.Generics
import Test.QuickCheck

import qualified Data.Complex as C

{-| I defined my own complex type so that I can write a Num instance without the RealFloat constraint. TODO: think about whether this is really a good idea. -}
data Complex a = a :+ a deriving (Show, Lift, Generic, ShowX, NFDataX, Functor, Foldable, Traversable, Eq, Ord)

deriving instance (BitPack a, KnownNat (BitSize a)) => BitPack (Complex a)

instance Num a => Num (Complex a) where
    (a :+ b) + (c :+ d) = (a + c) :+ (b + d)
    (a :+ b) - (c :+ d) = (a - c) :+ (b - d)
    (a :+ b) * (c :+ d) = (a * c - b * d) :+ (a * d + b * c)
    fromInteger x       = (fromInteger x :+ 0)

instance Applicative Complex where
    pure a = a :+ a
    f :+ g <*> a :+ b = f a :+ g b

instance Arbitrary a => Arbitrary (Complex a)
    where
    arbitrary = liftA2 (:+) arbitrary arbitrary

instance Default a => Default (Complex a)
    where
    def = def :+ def

realPart :: Complex a -> a
realPart (x :+ _) = x

imagPart :: Complex a -> a
imagPart (_ :+ y) = y

fromComplex (a C.:+ b) = a :+ b
toComplex   (a :+ b)   = a C.:+ b

conjugate :: Num a => Complex a -> Complex a
conjugate (x :+ y) = x :+ (-y)

cMul 
    :: ExtendingNum a b 
    => ExtendingNum (MResult a b) (MResult a b)
    => Complex a
    -> Complex b
    -> Complex (AResult (MResult a b) (MResult a b))
cMul (xr :+ xi) (yr :+ yi) 
    =  ((xr `mul` yr) `sub` (xi `mul` yi)) 
    :+ ((xr `mul` yi) `add` (xi `mul` yr))

cMul3
    :: ExtendingNum b b
    => ExtendingNum a a
    => ExtendingNum a (AResult b b)
    => ExtendingNum b (AResult a a)
    => ExtendingNum (MResult b (AResult a a)) (MResult a (AResult b b))
    => Complex a
    -> Complex b
    -> Complex (AResult (MResult b (AResult a a)) (MResult a (AResult b b)))
cMul3 (xr :+ xi) (yr :+ yi) 
    =  (k1 `sub` k3)
    :+ (k1 `add` k2)
    where
    k1 = yr `mul` (xr `add` xi)
    k2 = xr `mul` (yi `sub` yr)
    k3 = xi `mul` (yr `add` yi)

cMulPipe
    :: HiddenClockResetEnable dom
    => ExtendingNum a b 
    => ExtendingNum (MResult a b) (MResult a b)
    => NFDataX (MResult a b)
    => NFDataX (AResult (MResult a b) (MResult a b))
    => Signal dom Bool
    -> Signal dom (Complex a)
    -> Signal dom (Complex b)
    -> Signal dom (Complex (AResult (MResult a b) (MResult a b)))
cMulPipe en x y 
    = liftA2 (:+) rr ri
    where
    --Products
    xryr = delayEn (errorX "initial xryr") en $ liftA2 mul (realPart <$> x) (realPart <$> y)
    xiyi = delayEn (errorX "initial xiyi") en $ liftA2 mul (imagPart <$> x) (imagPart <$> y)
    xryi = delayEn (errorX "initial xryi") en $ liftA2 mul (realPart <$> x) (imagPart <$> y)
    xiyr = delayEn (errorX "initial xiyr") en $ liftA2 mul (imagPart <$> x) (realPart <$> y)
    --Sums
    rr   = delayEn (errorX "initial rr")   en $ liftA2 sub xryr xiyi
    ri   = delayEn (errorX "initial ri")   en $ liftA2 add xryi xiyr

cMul3Pipe
    :: HiddenClockResetEnable dom
    => ExtendingNum b b
    => ExtendingNum a a
    => ExtendingNum a (AResult b b)
    => ExtendingNum b (AResult a a)
    => ExtendingNum (MResult b (AResult a a)) (MResult a (AResult b b))
    => NFDataX (AResult a a) 
    => NFDataX (AResult b b) 
    => NFDataX (MResult b (AResult a a))
    => NFDataX (MResult a (AResult b b))
    => NFDataX (AResult (MResult b (AResult a a)) (MResult a (AResult b b)))
    => NFDataX a
    => NFDataX b
    => Signal dom Bool
    -> Signal dom (Complex a)
    -> Signal dom (Complex b)
    -> Signal dom (Complex (AResult (MResult b (AResult a a)) (MResult a (AResult b b))))
cMul3Pipe en x y
    =  liftA2 (:+) rr ri
    where
    --Sums
    xSum    = delayEn (errorX "initial xSum")    en $ liftA2 add (realPart <$> x) (imagPart <$> x)
    yDiff   = delayEn (errorX "initial yDiff")   en $ liftA2 sub (imagPart <$> y) (realPart <$> y)
    ySum    = delayEn (errorX "initial ySum")    en $ liftA2 add (realPart <$> y) (imagPart <$> y)
    --Delays
    xD      = delayEn (errorX "initial xD") en x
    yD      = delayEn (errorX "initial yD") en y
    --Products
    yrXSum  = delayEn (errorX "initial yrXSum")  en $ liftA2 mul (realPart <$> yD) xSum
    xrYDiff = delayEn (errorX "initial xrYDiff") en $ liftA2 mul (realPart <$> xD) yDiff
    xiYSum  = delayEn (errorX "initial xiYSum")  en $ liftA2 mul (imagPart <$> xD) ySum
    --Sums
    rr      = delayEn (errorX "initial rr")      en $ liftA2 sub yrXSum xiYSum
    ri      = delayEn (errorX "initial ri")      en $ liftA2 add yrXSum xrYDiff

