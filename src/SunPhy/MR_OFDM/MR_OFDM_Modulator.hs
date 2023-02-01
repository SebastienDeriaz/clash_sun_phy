module SunPhy.MR_OFDM.MR_OFDM_Modulator where

import Clash.Prelude

import Data.Functor ((<&>))
import SunPhy.AXI
import SunPhy.Concat4
import SunPhy.MR_OFDM.Constants
import SunPhy.MR_OFDM.Encoder
import SunPhy.MR_OFDM.Interleaver
import SunPhy.MR_OFDM.LTF
import SunPhy.MR_OFDM.OFDM
import SunPhy.MR_OFDM.PHR
import SunPhy.MR_OFDM.Puncturer
import SunPhy.MR_OFDM.STF
import SunPhy.Scrambler

data MrOfdmModulatorInput = MrOfdmModulatorInput
    { ofdmOption :: OFDM_Option
    , mcs :: MCS
    , phyOFDMInterleaving :: Bit
    , scramblerSeed :: Unsigned 2
    , start :: Bit
    , psduLength :: Unsigned 11
    , psduAxiInput :: AxiForward Bit
    , axiOutputFeedback :: AxiBackward
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

data MrOfdmModulatorOutput = MrOfdmModulatorOutput
    { psduAxiInputFeedback :: AxiBackward
    , axiOutput :: AxiForward IQ
    , -- debug
      stfReady :: Bit
    , ltfReady :: Bit
    , phrReady :: Bit
    , psduReady :: Bit
    , phr_ready_i :: Bit
    , phr_valid_o :: Bit
    , phr_encoder_ready_i :: Bit
    , phr_encoder_valid_o :: Bit
    , phr_puncturer_ready_i :: Bit
    , phr_puncturer_valid_o :: Bit
    , phr_interleaver_ready_i :: Bit
    , phr_interleaver_valid_o :: Bit
    , phr_ofdm_ready_i :: Bit
    , phr_ofdm_valid_o :: Bit
    , psdu_interleaver_ready_i :: Bit
    , psdu_interleaver_valid_o :: Bit
    , psdu_interleaver_last_o :: Bit
    , psdu_ofdm_ready_i :: Bit
    , psdu_ofdm_valid_o :: Bit
    , psdu_ofdm_last_o :: Bit
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (NFDataX)

mrOfdmModulator
    :: forall dom
     . HiddenClockResetEnable dom
    => Signal dom MrOfdmModulatorInput
    -> Signal dom MrOfdmModulatorOutput
mrOfdmModulator input = do
    psduAxiInputFeedback <- psduScramblerOutput <&> (.axiInputFeedback)
    axiOutput <- concat4Output <&> (.axiOutput)
    stfReady <- concat4Output <&> (.axiInputFeedbackA) <&> (.ready)
    ltfReady <- concat4Output <&> (.axiInputFeedbackB) <&> (.ready)
    phrReady <- concat4Output <&> (.axiInputFeedbackC) <&> (.ready)
    psduReady <- concat4Output <&> (.axiInputFeedbackD) <&> (.ready)
    phr_ready_i <- phrPHRInput <&> (.axiOutputFeedback) <&> (.ready)
    phr_valid_o <- phrPHROutput <&> (.axiOutput) <&> (.valid)
    phr_encoder_ready_i <- phrPHRInput <&> (.axiOutputFeedback) <&> (.ready)
    phr_encoder_valid_o <- phrPHROutput <&> (.axiOutput) <&> (.valid)
    phr_puncturer_ready_i <- phrEncoderInput <&> (.axiOutputFeedback) <&> (.ready)
    phr_puncturer_valid_o <- phrEncoderOutput <&> (.axiOutput) <&> (.valid)
    phr_interleaver_ready_i <- phrInterleaverInput <&> (.axiOutputFeedback) <&> (.ready)
    phr_interleaver_valid_o <- phrInterleaverOutput <&> (.axiOutput) <&> (.valid)
    phr_ofdm_ready_i <- phrOfdmInput <&> (.axiOutputFeedback) <&> (.ready)
    phr_ofdm_valid_o <- phrOfdmOutput <&> (.axiOutput) <&> (.valid)
    psdu_interleaver_ready_i <- psduOfdmInput <&> (.axiOutputFeedback) <&> (.ready)
    psdu_interleaver_valid_o <- psduOfdmOutput <&> (.axiOutput) <&> (.valid)
    psdu_interleaver_last_o <- psduOfdmOutput <&> (.axiOutput) <&> (.last)
    psdu_ofdm_ready_i <- psduOfdmInput <&> (.axiOutputFeedback) <&> (.ready)
    psdu_ofdm_valid_o <- psduOfdmOutput <&> (.axiOutput) <&> (.valid)
    psdu_ofdm_last_o <- psduOfdmOutput <&> (.axiOutput) <&> (.last)
    pure MrOfdmModulatorOutput {..}
    where
        -- Set scrambler seeds (See table 158 802.15.4g)
        scrambler_pn9_seed = pure (0b1_1111_1111 :: BitVector 9)
        phyOFDMInterleaving = pure (0 :: Bit)
        phrLength =
            resize
                <$> ( mul
                        <$> ( n_dbps
                                <$> (input <&> (.ofdmOption))
                                <*> (lowestMCS <$> (input <&> (.ofdmOption)))
                                <*> phyOFDMInterleaving
                            )
                        <*> ( phrNSymbols
                                <$> (input <&> (.ofdmOption))
                                <*> phyOFDMInterleaving
                            )
                    )
        -- Interleaver MCS can be different for PHR and Payload

        -- PSDU
        -- 1) Scrambler
        psduScramblerInput :: Signal dom ScramblerInput
        psduScramblerInput =
            bundle (input, psduEncoderOutput, scrambler_pn9_seed)
                <&> \(input, psduEncoderOutput, scrambler_pn9_seed) ->
                    ScramblerInput
                        { axiInput = input.psduAxiInput
                        , axiOutputFeedback = psduEncoderOutput.axiInputFeedback
                        , pn9Seed = scrambler_pn9_seed
                        }

        psduScramblerOutput :: Signal dom ScramblerOutput
        psduScramblerOutput = scrambler psduScramblerInput

        -- 2) Encoder
        psduEncoderInput :: Signal dom EncoderInput
        psduEncoderInput =
            bundle (psduScramblerOutput, psduPuncturerOutput, input)
                <&> \(psduScramblerOutput, psduPuncturerOutput, input) ->
                    EncoderInput
                        { axiInput = psduScramblerOutput.axiOutput
                        , axiOutputFeedback = psduPuncturerOutput.axiInputFeedback
                        , reset = input.start
                        }
        psduEncoderOutput :: Signal dom EncoderOutput
        psduEncoderOutput = encoder psduEncoderInput

        -- 3) Puncturer
        psduPuncturerInput :: Signal dom PuncturerInput
        psduPuncturerInput =
            bundle (psduEncoderOutput, psduInterleaverOutput, input)
                <&> \(psduEncoderOutput, psduInterleaverOutput, input) ->
                    PuncturerInput
                        { enable = rate (input.mcs)
                        , axiInput = psduEncoderOutput.axiOutput
                        , axiOutputFeedback = psduInterleaverOutput.axiInputFeedback
                        , reset = input.start
                        }
        psduPuncturerOutput :: Signal dom PuncturerOutput
        psduPuncturerOutput = puncturer psduPuncturerInput

        -- 4) Interleaver
        psduInterleaverInput :: Signal dom InterleaverInput
        psduInterleaverInput =
            bundle (input, psduPuncturerOutput, psduOfdmOutput)
                <&> \(input, psduPuncturerOutput, psduOfdmOutput) ->
                    InterleaverInput
                        { ofdmOption = input.ofdmOption
                        , mcs = input.mcs
                        , phyOFDMInterleaving = input.phyOFDMInterleaving
                        , axiInput = psduPuncturerOutput.axiOutput
                        , axiOutputFeedback = psduOfdmOutput.axiInputFeedback
                        }
        psduInterleaverOutput :: Signal dom InterleaverOutput
        psduInterleaverOutput = interleaver psduInterleaverInput

        -- 5) OFDM
        psduOfdmInput :: Signal dom OfdmInput
        psduOfdmInput =
            bundle (input, psduInterleaverOutput, concat4Output, phrOfdmOutput)
                <&> \(input, psduInterleaverOutput, concat4Output, phrOfdmOutput) ->
                    OfdmInput
                        { ofdmOption = input.ofdmOption
                        , mcs = input.ofdmOption
                        , cpEnable = True
                        , axiInput = psduInterleaverOutput.axiOutput
                        , axiOutputFeedback = concat4Output.axiInputFeedbackD
                        , pilotsetIndex = phrOfdmOutput.pilotSetCounter
                        , pilotsetWrite = phrOfdmOutput.axiOutput.last
                        , pn9Seed = phrOfdmOutput.pn9Reg
                        , pn9SeedWrite = phrOfdmOutput.axiOutput.last
                        }
        psduOfdmOutput :: Signal dom OfdmOutput
        psduOfdmOutput = ofdm psduOfdmInput

        -- PHR
        -- 1) Generation
        phrPHRInput :: Signal dom PHRInput
        phrPHRInput =
            bundle (input, phrLength, phrEncoderOutput)
                <&> \(input, phrLength, phrEncoderOutput) ->
                    PHRInput
                        { mcs = input.mcs
                        , frameLength = input.psduLength
                        , scramblerSeed = input.scramblerSeed
                        , phrLength = phrLength
                        , start = input.start
                        , axiOutputFeedback = phrEncoderOutput.axiInputFeedback
                        }
        phrPHROutput :: Signal dom PHROutput
        phrPHROutput = phr phrPHRInput
        -- 2) Encoder (No need for a puncturer as the PHR will never be sent using a high MCS)
        phrEncoderInput :: Signal dom EncoderInput
        phrEncoderInput =
            bundle (phrPHROutput, phrInterleaverOutput, input)
                <&> \(phrPHROutput, phrInterleaverOutput, input) ->
                    EncoderInput
                        { axiInput = phrPHROutput.axiOutput
                        , axiOutputFeedback = phrInterleaverOutput.axiInputFeedback
                        , reset = input.start
                        }
        phrEncoderOutput = encoder phrEncoderInput
        -- 3) Interleaver
        phrInterleaverInput :: Signal dom InterleaverInput
        phrInterleaverInput =
            bundle (phrEncoderOutput, phrOfdmOutput, input)
                <&> \(phrEncoderOutput, phrOfdmOutput, input) ->
                    InterleaverInput
                        { ofdmOption = input.ofdmOption
                        , mcs = lowestMCS input.ofdmOption
                        , phyOFDMInterleaving = input.phyOFDMInterleaving
                        , axiInput = phrEncoderOutput.axiOutput
                        , axiOutputFeedback = phrOfdmOutput.axiInputFeedback
                        }
        phrInterleaverOutput :: Signal dom InterleaverOutput
        phrInterleaverOutput = interleaver phrInterleaverInput
        -- 4) OFDM
        phrOfdmInput :: Signal dom OfdmInput
        phrOfdmInput =
            bundle (phrInterleaverOutput, concat4Output, input)
                <&> \(phrInterleaverOutput, concat4Output, input) ->
                    OfdmInput
                        { ofdmOption = input.ofdmOption
                        , mcs = lowestMCS input.ofdmOption
                        , cpEnable = True
                        , axiInput = phrInterleaverOutput.axiOutput
                        , axiOutputFeedback = concat4Output.axiInputFeedbackC
                        , pilotsetIndex = 0
                        , pilotsetWrite = 0
                        , pn9Seed = 0
                        , pn9SeedWrite = 0
                        }
        phrOfdmOutput :: Signal dom OfdmOutput
        phrOfdmOutput = ofdm phrOfdmInput

        stfInput :: Signal dom STFInput
        stfInput =
            bundle (input, concat4Output)
                <&> \(input, concat4Output) ->
                    STFInput
                        { ofdmOption = input.ofdmOption
                        , axiOutputFeedback = concat4Output.axiInputFeedbackA
                        , start = input.start
                        }
        stfOutput :: Signal dom STFOutput
        stfOutput = stf stfInput

        ltfInput :: Signal dom LTFInput
        ltfInput =
            bundle (input, concat4Output)
                <&> \(input, concat4Output) ->
                    LTFInput
                        { ofdmOption = input.ofdmOption
                        , axiOutputFeedback = concat4Output.axiInputFeedbackB
                        , start = input.start
                        }
        ltfOutput :: Signal dom LTFOutput
        ltfOutput = ltf ltfInput

        -- Concatenate
        concat4Input :: Signal dom Concat4Input
        concat4Input =
            bundle (input, stfOutput, ltfOutput, phrOfdmOutput, psduOfdmOutput)
                <&> \(input, stfOutput, ltfOutput, phrOfdmOutput, psduOfdmOutput) ->
                    Concat4Input
                        { axiInputA = stfOutput.axiOutput
                        , axiInputB = ltfOutput.axiOutput
                        , axiInputC = phrOfdmOutput.axiOutput
                        , axiInputD = psduOfdmOutput.axiOutput
                        , axiOutputFeedback = input.axiOutputFeedback
                        }
        concat4Output :: Signal dom Concat4Output
        concat4Output = concat4 concat4Input