
# PHR_PSDU testbench
# 
# Sébastien Deriaz
# 16.12.2022


from clash_testbench import Chronogram, Testbench, Signal
from os.path import join, dirname
import numpy as np
import random

from sun_phy.mr_fsk.mr_fsk_modulator import Mr_fsk_modulator

filepath = join(dirname(__file__), 'Top.hs')

def test_PHR_PSDU():
    tb = Testbench(filepath, 'topEntity')

    cg = Chronogram(join(dirname(__file__), 'test_PHR_PSDU.json'))

    # input
    PSDU = [1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]

    mod = Mr_fsk_modulator(
        symbolRate=1e3,
        FSKModulationIndex=1,
        phyMRFSKSFD=0,
        modulation='2FSK',
        phyFSKFECEnabled=True,
        phyFSKFECScheme=1,
        macFCSType=0,
        phyFSKScramblePSDU=True,
        phyFSKFECInterleavingRSC=True)

    PHR = list(mod._PHR(len(PSDU) // 8))
    output_bitstream = PHR + PSDU

    print(f"PHR        = {PHR}")
    print(f"PSDU       = {PSDU}")
    print(f"PHR + PSDU = {output_bitstream}")

    tb.setInputs([
        cg["macFCSType"],
        cg["phyFSKScramblePSDU"],
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
    cg.saveSVG(join(dirname(__file__), 'test_PHR_PSDU.svg'))

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)