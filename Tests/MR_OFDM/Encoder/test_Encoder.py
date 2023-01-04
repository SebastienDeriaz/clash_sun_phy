# Encoder testbench
# 
# Sébastien Deriaz
# 30.12.2022
#

from clash_testbench import Chronogram, Testbench, Signal, Function
from os.path import join, dirname
import numpy as np

import pytest

from itertools import product

from sun_phy import Mr_ofdm_modulator
from sun_phy.mr_ofdm.mr_ofdm_modulator import FREQUENCY_SPREADING, N_BPSC, FFT_SIZE
from sun_phy.tools.errors import UnsupportedError

filepath = join(dirname(__file__), '../../../Sun_phy/MR_OFDM/Encoder.hs')

def test_Encoder():
    tb = Testbench(filepath, 'encoder')
    cg = Chronogram(join(dirname(__file__), 'test_Encoder.json'))
    mod = Mr_ofdm_modulator(
        MCS = 0,
        OFDM_Option = 2,
        phyOFDMInterleaving = 0,
        scrambler = 0,
    )

    message = np.array([1, 0, 0, 1, 0, 1,])
    # MCS :
    # 0 -> rate 1/2
    # 4 -> rate 3/4
    for rate in ['1/2', '3/4']:
        encodedMessage = mod._encoder(message, rate)
        
        print(f"Rate {rate} : {message} -> {encodedMessage}")

    tb.setInputs([
        cg["rate"],
        cg["valid_i"],
        cg["data_i"],
        cg["last_i"],
        cg["ready_i"]
    ])

    tb.setExpectedOutputs([
        cg["ready_o"],
        cg["data_o"],
        cg["valid_o"],
        cg["last_o"]
    ])

    tb.setActualOutputsNames([
        "ready_o (actual)",
        "data_o (actual)",
        "valid_o (actual)",
        "last_o (actual)"
    ])

    cg.setTemplates({
        "data_o (actual)" : "data_o"
    })

    tb.run()
    

    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_Encoder.svg'))

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)

if __name__ == '__main__':
    test_Encoder()