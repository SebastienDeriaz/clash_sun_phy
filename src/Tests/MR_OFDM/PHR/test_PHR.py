# PHR testbench
# 
# Sébastien Deriaz
# 12.01.2023

from clash_testbench import Chronogram, Testbench, Signal, Function, Level
from os.path import join, dirname

import numpy as np
import pytest

from sun_phy import Mr_ofdm_modulator
from sun_phy.mr_ofdm.fields import HCS_calculation
# from sun_phy.mr_ofdm.mr_ofdm_modulator import FREQUENCY_SPREADING, N_BPSC, FFT_SIZE
# from sun_phy.tools.errors import UnsupportedError

filepath = join(dirname(__file__), '../../../SunPhy/MR_OFDM/PHR.hs')

def test_HCS():
    tb = Testbench(filepath, 'serialHCS')
    inp = np.random.randint(0, 2, 22)

    hcs_th = HCS_calculation(inp)
    print(f"{inp} -> {hcs_th}")
    hcs_th_str = f'0b{"".join([str(x) for x in hcs_th[:4]])}_{"".join([str(x) for x in hcs_th[4:]])}'

    N = inp.size
    PAD = 2
    valid_i = Signal('valid_i', [0,0] + N*[1] + PAD*[0])
    data_i = Signal('data_i', [0,0] + inp.tolist() + PAD*[0])
    hcs_out = Signal('hcs_out', [Level.UNKNOWN]*(N+2) + [hcs_th_str] + (PAD-1)*[Level.UNKNOWN])

    tb.setInputs([
        valid_i,
        data_i
    ])

    tb.setExpectedOutputs([
        hcs_out
    ])

    tb.setActualOutputsNames([
        "hcs_out (actual)"
    ])

    tb.run()
    cg = Chronogram()
    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_HCS.svg'), 3)

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)





def test_PHR():
    mod = Mr_ofdm_modulator(
        MCS=2,
        OFDM_Option=1,
        scrambler=1
    )

    _, _, _ = mod.message_to_IQ(b'abcdefg', binary=False)
    
    mod._PHR(message_length=13*8)
    phrTh = mod._PHY_header
    print(f"{phrTh = }")

    cg = Chronogram(join(dirname(__file__), './test_PHR.json'))
    tb = Testbench(filepath, 'phr', verbose=False)

    tb.setInputs([
        cg["Rate"],
        cg["FrameLength"],
        cg["Scrambler"],
        cg["phrLength"],
        cg["ready_i"],
        cg["valid_i"]
    ])

    tb.setExpectedOutputs([
        cg["valid_o"],
        cg["data_o"],
        cg["last_o"]
    ])

    tb.setActualOutputsNames([
        "valid_o (actual)",
        "data_o (actual)",
        "last_o (actual)"
    ])

    cg.setTemplates({
        "data_o (actual)" : "data_o"
    })

    tb.run()

    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_PHR.svg'))

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)


if __name__ == '__main__':
    test_PHR()