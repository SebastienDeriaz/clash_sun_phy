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

filepath = join(dirname(__file__), '../../../SunPhy/MR_OFDM/MR_OFDM_Modulator.hs')

#OFDM_options = list(range(1,5))
OFDM_options = [4]


@pytest.mark.parametrize("OFDM_Option", OFDM_options)
def test_MR_OFDM_Modulator(OFDM_Option):
    tb = Testbench(filepath, 'mrOfdmModulator')
    
    mod = Mr_ofdm_modulator(
        MCS = 2,
        OFDM_Option=OFDM_Option
    )

    N = 16

    ofdm_option = Signal("ofdmOption", [OFDM_Option])
    ready_i = Signal("ready_i", [1])
    valid_i = Signal("valid_i", [1])
    psdu_valid_i = Signal("psdu_valid_i", [1])
    psdu_data_i = Signal("psdu_data_i", [0])
    psdu_last_i = Signal("psdu_last_i", [0,0,0] + (N-1)*[0] + [1] + 50*[0])


    tb.setInputs([
        ofdm_option,
        ready_i,
        valid_i,
        psdu_valid_i,
        psdu_data_i,
        psdu_last_i
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
    

    # PAD = 2
    
    # valid_i = Signal('valid_i', [0,0] + [1] + (N + PAD) * [0])
    # ready_i = Signal('ready_i', [1])

    # valid_o = Signal('valid_o', [0,0,0] + N * [1] + (PAD) * [0])
    # ready_o = Signal('ready_o', [1])    
    # last_o = Signal('last_o', [0,0,0] + (N-1)*[0] + [1] + PAD*[0])

    # tb.setInputs([
    #     ofdm_option,
    #     ready_i,
    #     valid_i,
    # ])

    # tb.setExpectedOutputs([
    #     valid_o,
    #     None,
    #     last_o
    # ])

    # tb.setActualOutputsNames([
    #     "valid_o (actual)",
    #     "data_o (actual)",
    #     "last_o (actual)"
    # ])

    # tb.run()
    # cg = Chronogram()
    # cg.setSignals(tb.getAllSignals())
    # cg.saveSVG(join(dirname(__file__), 'test_LTF.svg'), 0.5)

    # for s in tb:
    #     if s.isChecked():
    #         s.print(True)
    #         assert s.isValid(), s.message()
    #     else:
    #         s.print(True)

    # # Check the data
    # data = np.stack([(float(a), float(b)) for a,b in [str(x)[1:-1].split(',') for x in tb._actualOutputs["data_o (actual)"][3:(3+N)]]])
    # data_th = np.stack([ltf_th_i, ltf_th_q], axis=1)

    # assert np.var(data - data_th) < 0.001