# MR-OFDM Modulator testbench
# 
# Sébastien Deriaz
# 17.01.2023

from clash_testbench import Chronogram, Testbench, Signal, Function, Level
from os.path import join, dirname

import numpy as np
import pytest
import matplotlib.pyplot as plt

from sun_phy import Mr_ofdm_modulator
from itertools import product

import pickle


#OFDM_options = list(range(1,5))
OFDM_options = [4]
#MCSs = list(range(7))
MCSs = [3]
#phyOFDMInterleaving = list(range(2))
phyOFDMInterleaving = [0]


@pytest.mark.parametrize("OFDM_Option, MCS, phyOFDMInterleaving", list(product(MCSs, OFDM_options, phyOFDMInterleaving)))
def test_MR_OFDM_Modulator(OFDM_Option, MCS, phyOFDMInterleaving):
    filepath = join(dirname(__file__), './Test_MR_OFDM_Modulator.hs')
    tb = Testbench(filepath, 'testMrOfdmModulator')
    
    mod = Mr_ofdm_modulator(
        MCS = MCS,
        OFDM_Option=OFDM_Option,
        phyOFDMInterleaving=phyOFDMInterleaving
    )

    print(f"Bits per symbol : {mod.bits_per_symbol()}")
    message = np.array([1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0])

    I, Q, _ = mod.message_to_IQ(message, binary=True)

    message_vec = f"{':>'.join([str(x) for x in message])}:>Nil"


    Nout = I.size * 5
    ofdm_option = Signal("ofdmOption", [OFDM_Option])
    mcs = Signal("mcs", [MCS])
    _phyOFDMInterleaving = Signal("phyOFDMInterleaving", [phyOFDMInterleaving])
    ready_i = Signal("ready_i", [0,0] + (Nout-2)*[1])
    start_i = Signal("start_i", [0,0,0,0] + [1] + (Nout-5)*[0])
    inputVec = Signal("inputVec", [message_vec])


    tb.setInputs([
        ofdm_option,
        mcs,
        _phyOFDMInterleaving,
        inputVec,
        start_i,
        ready_i,
    ])

    tb.setActualOutputsNames([
        "ready_o (actual)",
        "valid_o (actual)",
        "data_o (actual)",
        "last_o (actual)",
        "phr_encoder_ready_i",
        "phr_encoder_valid_o",
        "phr_interleaver_ready_i",
        "phr_interleaver_valid_o",
        "phr_ofdm_ready_i",
        "phr_ofdm_valid_o",
        "ofdm_state"
    ])

    tb.setExpectedOutputs([
        None,
        None,
        None,
        None
    ])

    tb.run()
    cg = Chronogram()
    cg.setSignals(tb.getAllSignals())

    cg.saveSVG(join(dirname(__file__), 'test_MR_OFDM_Modulator.svg'))

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)
    

    saveSignals = {}
    for k, signal in tb._actualOutputs.items():
        saveSignals[k] = [str(s.value()) for s in signal.samples]
    for signal in tb.inputSignals:
        saveSignals[signal.name] = [str(s.value()) for s in signal.samples]
    
    with open(join(dirname(__file__), 'data.pkl'), 'wb') as f:
        pickle.dump(saveSignals, f)
   


    readySignal = np.array([int(str(s.value())) for s in cg["ready_i"].samples])
    validSignal = np.array([int(str(s.value())) for s in tb._actualOutputs["valid_o (actual)"].samples])
    rawOutputSignal = np.array([complex(*[float(x) for x in str(s)[1:-1].split(',')]) for s in tb._actualOutputs["data_o (actual)"].samples])
    # Location of data values (when ready = 1)
    dataLocations = np.logical_and(readySignal, validSignal)
    outputSignal = np.asarray(rawOutputSignal[dataLocations])


    print(outputSignal)

    # plt.figure()
    # plt.subplot(211)
    # plt.plot(I, linewidth=2)
    # plt.plot(outputSignal.real, linewidth=1)
    # plt.subplot(212)
    # plt.plot(Q, linewidth=2)
    # plt.plot(outputSignal.imag, linewidth=1)
    # plt.show()


    # # Check the data
    # data = np.stack([(float(a), float(b)) for a,b in [str(x)[1:-1].split(',') for x in tb._actualOutputs["data_o (actual)"][3:(3+N)]]])
    # data_th = np.stack([ltf_th_i, ltf_th_q], axis=1)

    # assert np.var(data - data_th) < 0.001

def test_MR_OFDM_Modulator_single():
    filepath = join(dirname(__file__), '../../../SunPhy/MR_OFDM/MR_OFDM_Modulator.hs')
    tb = Testbench(filepath, 'mrOfdmModulator')
    
    mod = Mr_ofdm_modulator(
        MCS = 3,
        OFDM_Option=3
    )

    cg = Chronogram(join(dirname(__file__), 'test_MR_OFDM_Modulator.json'))

    tb.setInputs([
        cg["OFDM_Option"],
        cg["MCS"],
        cg["phyOFDMInterleaving"],
        cg["ready_i"],
        cg["valid_i"],
        cg["psdu_valid_i"],
        cg["psdu_data_i"],
        cg["psdu_last_i"],
        cg["psdu_length"]
    ])

    tb.setActualOutputsNames([
        "psdu_ready_o (actual)",
        "valid_o (actual)",
        "data_o (actual)",
        "last_o (actual)"
    ])

    tb.setExpectedOutputs([
        None,
        None,
        None,
        None
    ])

    tb.run()
    cg = Chronogram()
    cg.setSignals(tb.getAllSignals())
    
    cg.saveSVG(join(dirname(__file__), 'test_MR_OFDM_Modulator_single.svg'))

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)

if __name__ == '__main__':
    test_MR_OFDM_Modulator(4, 3, 0)