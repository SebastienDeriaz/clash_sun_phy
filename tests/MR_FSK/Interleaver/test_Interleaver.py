
# Interleaver testbench
# 
# Sébastien Deriaz
# 05.12.2022
#
# input sequence :
# 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1
# Output sequence
# 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0


from clash_testbench import Chronogram, Testbench, Signal
from os.path import join, dirname
import numpy as np

from sun_phy.mr_fsk.mr_fsk_modulator import Mr_fsk_modulator

filepath = join(dirname(__file__), '../../../Sun_phy/MR_FSK/Interleaver.hs')


def test_Interleaver():
    tb = Testbench(filepath, 'interleaver')

    cg = Chronogram(join(dirname(__file__), 'test_Interleaver.json'))
    
    mod = Mr_fsk_modulator(
        phyMRFSKSFD=0,
        modulation='2FSK',
        phyFSKFECEnabled=True,
        phyFSKFECScheme=1,
        macFCSType=0,
        phyFSKScramblePSDU=False,
        phyFSKFECInterleavingRSC=False)

    input_sequence = np.array([0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,1,1,1,0,1,1,0,0,0,1,1,1,1,0,1])
    output_sequence = mod._interleaver(input_sequence)

    print(f"{input_sequence} -> {output_sequence}")

    print(cg["data_o"])

    tb.setInputs([
        cg["valid_i"],
        cg["data_i"],
        cg["last_i"],
        cg["ready_i"]
    ])
    tb.setExpectedOutputs([
        cg["ready_o"],
        cg["data_o"],
        cg["valid_o"],
        cg["last_o"],
        cg["state"],
        None
    ])

    tb.setActualOutputsNames([
        "ready_o (actual)",
        "data_o (actual)",
        "valid_o (actual)",
        "last_o (actual)",
        "state (actual)"
    ])

    cg.setTemplates({
        "data_o (actual)" : "data_o"
    })


    tb.run()

    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_Interleaver.svg'))

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)