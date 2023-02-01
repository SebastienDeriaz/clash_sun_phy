{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ImportQualifiedPost #-}

module VCD where

import Clash.Prelude
import "vcd" Data.VCD
import GHC.TypeLits
import Data.Proxy
import "bytestring" Data.ByteString.Char8 qualified as BS8
import Text.Printf


instance Variable Bit where
    var = variable "bit" 1 show

instance forall n . (KnownNat n) => Variable (Unsigned n) where
    var = variable "bit" n vcdBin
        where
            vcdBin = printf ("b%0" <> show n <> "b ")
            n = (fromIntegral $ natVal (Proxy :: Proxy n))



-- instance ToField Bit where
--     toField b = BS8.pack $ show b

-- instance ToNamedRecord Output where
--     toNamedRecord o =
--         namedRecord
--             [ "valid" .= o.valid
--             , "ready" .= o.ready
--             ]