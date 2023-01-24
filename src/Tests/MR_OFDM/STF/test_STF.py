# STF testbench
# 
# Sébastien Deriaz
# 16.01.2023

from clash_testbench import Chronogram, Testbench, Signal, Function, Level
from os.path import join, dirname

import numpy as np
import pytest
import matplotlib.pyplot as plt

from sun_phy import Mr_ofdm_modulator

filepath = join(dirname(__file__), '../../../SunPhy/MR_OFDM/STF.hs')

OFDM_options = list(range(1,5))


@pytest.mark.parametrize("OFDM_Option", OFDM_options)
def test_HCS(OFDM_Option):
    tb = Testbench(filepath, 'stf')
    
    mod = Mr_ofdm_modulator(
        MCS = 2,
        OFDM_Option=OFDM_Option
    )
    stf_th_i, stf_th_q = mod._STF()
    N = stf_th_i.size

    PAD = 2
    ofdm_option = Signal("ofdmOption", [OFDM_Option])
    valid_i = Signal('valid_i', [0,0] + [1] + (N + PAD) * [0])
    ready_i = Signal('ready_i', [1])

    valid_o = Signal('valid_o', [0,0,0] + N * [1] + (PAD) * [0])
    last_o = Signal('last_o', [0,0,0] + (N-1)*[0] + [1] + PAD*[0])

    tb.setInputs([
        ofdm_option,
        ready_i,
        valid_i,
    ])

    tb.setExpectedOutputs([
        valid_o,
        None,
        last_o
    ])

    tb.setActualOutputsNames([
        "valid_o (actual)",
        "data_o (actual)",
        "last_o (actual)"
    ])

    tb.run()
    cg = Chronogram()
    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_STF.svg'), 1)

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)

    # Check the data
    data = np.stack([(float(a), float(b)) for a,b in [str(x)[1:-1].split(',') for x in tb._actualOutputs["data_o (actual)"][3:(3+N)]]])
    data_th = np.stack([stf_th_i, stf_th_q], axis=1)

    plt.figure()
    plt.plot(data.real)
    plt.show()

    assert np.var(data - data_th) < 0.001

if __name__ == '__main__':
    test_HCS(4)