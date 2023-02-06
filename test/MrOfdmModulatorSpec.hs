{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module MrOfdmModulatorSpec where

import Clash.Prelude hiding (concatMap, head, take, zip)
import Prelude (concatMap, head, take, zip)

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
import Numeric
import Parsers
import SunPhy.AXI
import SunPhy.Concat4
import SunPhy.Concat4 qualified
import SunPhy.MR_OFDM.Constants
import SunPhy.MR_OFDM.MR_OFDM_Modulator
import System.Exit (exitFailure)
import System.IO
import System.IO qualified as IO
import Test.Hspec
import Test.QuickCheck
import VCD

type OutputSize = 5000
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
                , scramblerSeed = 0
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
        , -- debug
          -- ready signals
          stfReady = 0
        , ltfReady = 0
        , phrReady = 0
        , psduReady = 0
        , -- streams (PHR)
          phr_ready_i = 0
        , phr_valid_o = 0
        , phr_last_o = 0
        , phr_encoder_ready_i = 0
        , phr_encoder_valid_o = 0
        , phr_encoder_last_o = 0
        , phr_interleaver_ready_i = 0
        , phr_interleaver_valid_o = 0
        , phr_interleaver_last_o = 0
        , phr_ofdm_ready_i = 0
        , phr_ofdm_valid_o = 0
        , phr_ofdm_last_o = 0
        , -- streams (PSDU)
          psdu_ready_i = 0
        , psdu_valid_o = 0
        , psdu_last_o = 0
        , psdu_padder_ready_i = 0
        , psdu_padder_valid_o = 0
        , psdu_padder_last_o = 0
        , psdu_scrambler_ready_i = 0
        , psdu_scrambler_valid_o = 0
        , psdu_scrambler_last_o = 0
        , psdu_encoder_ready_i = 0
        , psdu_encoder_valid_o = 0
        , psdu_encoder_last_o = 0
        , psdu_puncturer_ready_i = 0
        , psdu_puncturer_valid_o = 0
        , psdu_puncturer_last_o = 0
        , psdu_interleaver_ready_i = 0
        , psdu_interleaver_valid_o = 0
        , psdu_interleaver_last_o = 0
        , psdu_ofdm_ready_i = 0
        , psdu_ofdm_valid_o = 0
        , psdu_ofdm_last_o = 0
        , -- other
          last = 0
        , phrInterleaverMasterCounter = 0
        , phrInterleaverSlaveCounter = 0
        , phrEncoderSlaveCounter = 0
        , phrEncoderMasterCounter = 0
        , psduInterleaverMasterCounter = 0
        , psduInterleaverSlaveCounter = 0
        , psduPadderCounter = 0
        , psduPadderState = 0
        }

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
        -- ready signals
        stfReady <- var handle (path <> ["debug", "stf_ready"]) a.stfReady
        ltfReady <- var handle (path <> ["debug", "ltf_ready"]) a.ltfReady
        phrReady <- var handle (path <> ["debug", "phr_ready"]) a.phrReady
        psduReady <- var handle (path <> ["debug", "psdu_ready"]) a.psduReady
        -- streams (PHR)
        phr_ready_i <- var handle (path <> ["debug", "phr_ready_i"]) a.phr_ready_i
        phr_valid_o <- var handle (path <> ["debug", "phr_valid_o"]) a.phr_valid_o
        phr_last_o <- var handle (path <> ["debug", "phr_last_o"]) a.phr_last_o
        phr_encoder_ready_i <- var handle (path <> ["debug", "phr_encoder_ready_i"]) a.phr_encoder_ready_i
        phr_encoder_valid_o <- var handle (path <> ["debug", "phr_encoder_valid_o"]) a.phr_encoder_valid_o
        phr_encoder_last_o <- var handle (path <> ["debug", "phr_encoder_last_o"]) a.phr_encoder_last_o
        phr_interleaver_ready_i <- var handle (path <> ["debug", "phr_interleaver_ready_i"]) a.phr_interleaver_ready_i
        phr_interleaver_valid_o <- var handle (path <> ["debug", "phr_interleaver_valid_o"]) a.phr_interleaver_valid_o
        phr_interleaver_last_o <- var handle (path <> ["debug", "phr_interleaver_last_o"]) a.phr_interleaver_last_o
        phr_ofdm_ready_i <- var handle (path <> ["debug", "phr_ofdm_ready_i"]) a.phr_ofdm_ready_i
        phr_ofdm_valid_o <- var handle (path <> ["debug", "phr_ofdm_valid_o"]) a.phr_ofdm_valid_o
        phr_ofdm_last_o <- var handle (path <> ["debug", "phr_ofdm_last_o"]) a.phr_ofdm_last_o
        -- streams (PSDU)
        psdu_ready_i <- var handle (path <> ["debug", "psdu_ready_i"]) a.psdu_ready_i
        psdu_valid_o <- var handle (path <> ["debug", "psdu_valid_o"]) a.psdu_valid_o
        psdu_last_o <- var handle (path <> ["debug", "psdu_last_o"]) a.psdu_last_o
        psdu_padder_ready_i <- var handle (path <> ["debug", "psdu_padder_ready_i"]) a.psdu_padder_ready_i
        psdu_padder_valid_o <- var handle (path <> ["debug", "psdu_padder_valid_o"]) a.psdu_padder_valid_o
        psdu_padder_last_o <- var handle (path <> ["debug", "psdu_padder_last_o"]) a.psdu_padder_last_o
        psdu_scrambler_ready_i <- var handle (path <> ["debug", "psdu_scrambler_ready_i"]) a.psdu_scrambler_ready_i
        psdu_scrambler_valid_o <- var handle (path <> ["debug", "psdu_scrambler_valid_o"]) a.psdu_scrambler_valid_o
        psdu_scrambler_last_o <- var handle (path <> ["debug", "psdu_scrambler_last_o"]) a.psdu_scrambler_last_o
        psdu_encoder_ready_i <- var handle (path <> ["debug", "psdu_encoder_ready_i"]) a.psdu_encoder_ready_i
        psdu_encoder_valid_o <- var handle (path <> ["debug", "psdu_encoder_valid_o"]) a.psdu_encoder_valid_o
        psdu_encoder_last_o <- var handle (path <> ["debug", "psdu_encoder_last_o"]) a.psdu_encoder_last_o
        psdu_puncturer_ready_i <- var handle (path <> ["debug", "psdu_puncturer_ready_i"]) a.psdu_puncturer_ready_i
        psdu_puncturer_valid_o <- var handle (path <> ["debug", "psdu_puncturer_valid_o"]) a.psdu_puncturer_valid_o
        psdu_puncturer_last_o <- var handle (path <> ["debug", "psdu_puncturer_last_o"]) a.psdu_puncturer_last_o
        psdu_interleaver_ready_i <- var handle (path <> ["debug", "psdu_interleaver_ready_i"]) a.psdu_interleaver_ready_i
        psdu_interleaver_valid_o <- var handle (path <> ["debug", "psdu_interleaver_valid_o"]) a.psdu_interleaver_valid_o
        psdu_interleaver_last_o <- var handle (path <> ["debug", "psdu_interleaver_last_o"]) a.psdu_interleaver_last_o
        psdu_ofdm_ready_i <- var handle (path <> ["debug", "psdu_ofdm_ready_i"]) a.psdu_ofdm_ready_i
        psdu_ofdm_valid_o <- var handle (path <> ["debug", "psdu_ofdm_valid_o"]) a.psdu_ofdm_valid_o
        psdu_ofdm_last_o <- var handle (path <> ["debug", "psdu_ofdm_last_o"]) a.psdu_ofdm_last_o
        -- other
        last <- var handle (path <> ["debug", "last"]) a.last
        phrInterleaverMasterCounter <- var handle (path <> ["debug", "phrInterleaverMasterCounter"]) a.phrInterleaverMasterCounter
        phrInterleaverSlaveCounter <- var handle (path <> ["debug", "phrInterleaverSlaveCounter"]) a.phrInterleaverSlaveCounter
        phrEncoderSlaveCounter <- var handle (path <> ["debug", "phrEncoderSlaveCounter"]) a.phrEncoderSlaveCounter
        phrEncoderMasterCounter <- var handle (path <> ["debug", "phrEncoderMasterCounter"]) a.phrEncoderMasterCounter
        psduInterleaverMasterCounter <- var handle (path <> ["debug", "psduInterleaverMasterCounter"]) a.psduInterleaverMasterCounter
        psduInterleaverSlaveCounter <- var handle (path <> ["debug", "psduInterleaverSlaveCounter"]) a.psduInterleaverSlaveCounter
        psduPadderCounter <- var handle (path <> ["debug", "psduPadderCounter"]) a.psduPadderCounter
        psduPadderState <- var handle (path <> ["debug", "psduPadderState"]) a.psduPadderState
        pure $ \a -> do
            valid a.valid
            ready a.ready
            -- ready signals
            stfReady a.stfReady
            ltfReady a.ltfReady
            phrReady a.phrReady
            psduReady a.psduReady
            -- streams (PHR)
            phr_ready_i a.phr_ready_i
            phr_valid_o a.phr_valid_o
            phr_last_o a.phr_last_o
            phr_encoder_ready_i a.phr_encoder_ready_i
            phr_encoder_valid_o a.phr_encoder_valid_o
            phr_encoder_last_o a.phr_encoder_last_o
            phr_interleaver_ready_i a.phr_interleaver_ready_i
            phr_interleaver_valid_o a.phr_interleaver_valid_o
            phr_interleaver_last_o a.phr_interleaver_last_o
            phr_ofdm_ready_i a.phr_ofdm_ready_i
            phr_ofdm_valid_o a.phr_ofdm_valid_o
            phr_ofdm_last_o a.phr_ofdm_last_o
            -- streams (PSDU)
            psdu_ready_i a.psdu_ready_i
            psdu_valid_o a.psdu_valid_o
            psdu_last_o a.psdu_last_o
            psdu_padder_ready_i a.psdu_padder_ready_i
            psdu_padder_valid_o a.psdu_padder_valid_o
            psdu_padder_last_o a.psdu_padder_last_o
            psdu_scrambler_ready_i a.psdu_scrambler_ready_i
            psdu_scrambler_valid_o a.psdu_scrambler_valid_o
            psdu_scrambler_last_o a.psdu_scrambler_last_o
            psdu_encoder_ready_i a.psdu_encoder_ready_i
            psdu_encoder_valid_o a.psdu_encoder_valid_o
            psdu_encoder_last_o a.psdu_encoder_last_o
            psdu_puncturer_ready_i a.psdu_puncturer_ready_i
            psdu_puncturer_valid_o a.psdu_puncturer_valid_o
            psdu_puncturer_last_o a.psdu_puncturer_last_o
            psdu_interleaver_ready_i a.psdu_interleaver_ready_i
            psdu_interleaver_valid_o a.psdu_interleaver_valid_o
            psdu_interleaver_last_o a.psdu_interleaver_last_o
            psdu_ofdm_ready_i a.psdu_ofdm_ready_i
            psdu_ofdm_valid_o a.psdu_ofdm_valid_o
            psdu_ofdm_last_o a.psdu_ofdm_last_o
            -- other
            last a.last
            phrInterleaverMasterCounter a.phrInterleaverMasterCounter
            phrInterleaverSlaveCounter a.phrInterleaverSlaveCounter
            phrEncoderSlaveCounter a.phrEncoderSlaveCounter
            phrEncoderMasterCounter a.phrEncoderMasterCounter
            psduInterleaverMasterCounter a.psduInterleaverMasterCounter
            psduInterleaverSlaveCounter a.psduInterleaverSlaveCounter
            psduPadderCounter a.psduPadderCounter
            psduPadderState a.psduPadderState

writeVcd :: FilePath -> [Input] -> [Output] -> IO ()
writeVcd file inputs outputs = do
    handle <- IO.openFile file IO.WriteMode
    let initialInput = head inputs
    let initialOutput = expectedOutput
    vcd <- newVCD handle US
    input <- var vcd ["Input"] initialInput
    output <- var vcd ["Output"] initialOutput
    forM_ (zip inputs outputs) $ \(i, o) -> do
        step vcd 1
        input i
        output o

    putStrLn $ "File " <> file <> " written!"

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
