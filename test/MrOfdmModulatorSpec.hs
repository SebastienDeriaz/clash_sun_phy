{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module MrOfdmModulatorSpec where

import Clash.Prelude hiding (concatMap, head, take, zip3)
import Prelude (concatMap, head, take, zip3)

import Clash.Explicit.Prelude (Int, d0)
import Clash.Sized.Vector (lengthS)
import Control.Monad (forM_)
import CsvData
import "bytestring" Data.ByteString (ByteString)
import "bytestring" Data.ByteString qualified as BS
import "bytestring" Data.ByteString.Char8 qualified as BS8
import "bytestring" Data.ByteString.Lazy qualified as BSL
import "cassava" Data.Csv
import "cassava" Data.Csv.Incremental
import Data.Foldable (find)
import Data.Functor ((<&>))
import "vcd" Data.VCD
import Debug.Trace
import GHC.Generics
import MrOfdmModulatorTester
import Parsers
import SunPhy.MR_OFDM.Constants
import System.Exit (exitFailure)
import System.IO
import System.IO qualified as IO
import Test.Hspec
import Test.QuickCheck
import GHC.TypeLits
import Data.Proxy
import Numeric
import Text.Printf

type OutputSize = 200
type InputSize = 608 -- (lengthS l1Data) * 8

type Input = MrOfdmModulatorTesterInput InputSize
type Output = MrOfdmModulatorTesterOutput OutputSize

inputs :: [Input]
inputs = i <$> [0 ..]
    where
        i tick =
            MrOfdmModulatorTesterInput
                { ofdmOption = 2
                , mcs = 3
                , phyOFDMInterleaving = 0
                , _data = $(listToVecTH $ concatMap hexOctetToBits l1Data)
                , start = boolToBit $ tick == 5
                , ready = 1
                }

expectedOutput :: Output
expectedOutput =
    MrOfdmModulatorTesterOutput
        { _data = repeat 0
        , valid = 1
        , ready = 0
        -- debug
        , phr_ofdm_ready_i = 0
        , phr_ofdm_valid_o = 0
        , psdu_ofdm_ready_i = 0
        , psdu_ofdm_valid_o = 0
        , phr_interleaver_ready_i = 0
        , phr_interleaver_valid_o = 0
        , ofdmMasterWriteCounter = 0
        }

writeCsv :: ToNamedRecord a => FilePath -> [a] -> IO ()
writeCsv file records = pure ()

instance Variable Bit where
    var = variable "bit" 1 show

instance forall n . (KnownNat n) => Variable (Unsigned n) where
    var = variable "bit" n vcdBin
        where
            vcdBin = printf ("b%0" <> show n <> "b ")
            n = (fromIntegral $ natVal (Proxy :: Proxy n))

instance Variable Input where
    var handle path a = do
        start <- var handle (path <> ["start_i"]) a.start
        ready <- var handle (path <> ["ready_i"]) a.ready
        pure $ \a -> do
            start a.start
            ready a.ready

instance Variable Output where
    var handle path a = do
        valid <- var handle (path <> ["valid_o"]) a.valid
        ready <- var handle (path <> ["ready_o"]) a.ready
        -- debug
        phr_ofdm_ready_i <- var handle (path <> ["debug", "phr_ofdm_ready_i"]) a.phr_ofdm_ready_i
        phr_ofdm_valid_o <- var handle (path <> ["debug", "phr_ofdm_valid_o"]) a.phr_ofdm_valid_o
        psdu_ofdm_ready_i <- var handle (path <> ["debug", "psdu_ofdm_ready_i"]) a.psdu_ofdm_ready_i
        psdu_ofdm_valid_o <- var handle (path <> ["debug", "psdu_ofdm_valid_o"]) a.psdu_ofdm_valid_o
        phr_interleaver_ready_i <- var handle (path <> ["debug", "phr_interleaver_ready_i"]) a.phr_interleaver_ready_i
        phr_interleaver_valid_o <- var handle (path <> ["debug", "phr_interleaver_valid_o"]) a.phr_interleaver_valid_o
        ofdmMasterWriteCounter <- var handle (path <> ["debug", "ofdmMasterWriteCounter"]) a.ofdmMasterWriteCounter
        pure $ \a -> do
            valid a.valid
            ready a.ready
            -- debug
            phr_ofdm_ready_i a.phr_ofdm_ready_i
            phr_ofdm_valid_o a.phr_ofdm_valid_o
            psdu_ofdm_ready_i a.psdu_ofdm_ready_i
            psdu_ofdm_valid_o a.psdu_ofdm_valid_o
            phr_interleaver_ready_i a.phr_interleaver_ready_i
            phr_interleaver_valid_o a.phr_interleaver_valid_o
            ofdmMasterWriteCounter a.ofdmMasterWriteCounter

writeVcd :: FilePath -> [Input] -> [Output] -> IO ()
writeVcd file inputs outputs = do
    handle <- IO.openFile file IO.WriteMode
    let initialInput = head inputs
    let initialOutput = expectedOutput
    vcd <- newVCD handle US
    input <- var vcd ["Input"] initialInput
    output <- var vcd ["Output"] initialOutput
    forM_ (zip3 [0 ..] inputs outputs) $ \(tick, i, o) -> do
        step' vcd 1
        input i
        output o

    putStrLn $ "File " <> file <> " written!"

instance ToField Bit where
    toField b = BS8.pack $ show b

instance ToNamedRecord Output where
    toNamedRecord o =
        namedRecord
            [ "valid" .= o.valid
            , "ready" .= o.ready
            ]

firstValidShouldBe :: [Output] -> Output -> Expectation
firstValidShouldBe actual expected =
    maybe
        (expectationFailure "No valid output found")
        (`shouldBe` expected)
        firstValid
    where
        firstValid :: Maybe Output
        firstValid = find (\o -> bitToBool o.valid) actual

nSamples :: Int
nSamples = 10000

mrOfdmModulatorSpec :: Spec
mrOfdmModulatorSpec = do
    describe "IEEE 802.15.4g-2012 Annex L" $ do
        runIO $ writeVcd "test/MrOfdmModulatorSpec.vcd" inputs (take nSamples actual)
        it "consumes message L.1 and produces IQ signal L.14" $
            do
                take nSamples actual `firstValidShouldBe` expectedOutput
    where
        actual = simulate @System mrOfdmModulatorTester inputs
