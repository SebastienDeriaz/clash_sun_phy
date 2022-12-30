
# MR_FSK_modulator testbench
# 
# Sébastien Deriaz
# 12.12.2022


from clash_testbench import Chronogram, Testbench, Signal
from os.path import join, dirname
import numpy as np
import random

from sun_phy.mr_fsk.mr_fsk_modulator import Mr_fsk_modulator

filepath = join(dirname(__file__), 'Top.hs')

def test_MR_FSK_Modulator():
    tb = Testbench(filepath, 'topEntity')

    cg = Chronogram(join(dirname(__file__), 'test_MR_FSK_modulator.json'))

    # input
    input_bitstream = [1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]

    mod = Mr_fsk_modulator(
        symbolRate=1e3,
        FSKModulationIndex=1,
        phyMRFSKSFD=0,
        modulation='2FSK',
        phyFSKFECEnabled=True,
        phyFSKFECScheme=1,
        macFCSType=0,
        phyFSKScramblePSDU=True,
        phyFSKFECInterleavingRSC=True,
        phyFSKPreambleLength=4
    )

    mod.message_to_IQ(input_bitstream, binary=True)
    output_bitstream = mod._binarySignal
    # [0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 0 1 1 1 1 0 1 0 0 1 1 1 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 1 1 0 1 0 0 0 0 0 0 0 1 1 0 0 1 1 1 1 0 1 1 0 1 1 0 0 1 1 0 0 0 0 0 1 0 1 0 1 1 0 1 0 1 0 1 1 1 1 1 1 0 0 0 0 0 0 1 1 0 1 1 1 0 0 1 0 0 1 1 1 1 1 0 1 1 0 0 0 0 1 0 1 1 0 0 0 0 1 1 1 0 0 0 0 1 0 1 1 0 0 0 0 1 1 0 1 0 0 0 0]
    #


    # PHR + PSDU (with scrambler)
    #                         PHR                                               PSDU
    # 1 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 1 1 0 1 0 0 0 0 0 0 0 1 1 0 0 | 1 1 0 0 0 0 1 1 1 0 1 1 1 1 0 0 1 1 1 1 0 1 0 0 1 0 1 0 0 0 0 0 1 0 0 0 1 1 1 1 0 1 0 1 0 1 0 0 0 0 0 0 1 1 0 0 0 1 1 0 0 0 1 0 0 1 0 1 1 0 0 0 0 1 1 1 0 0 1 1 1 0 1 1 0 0 0 0 1 1 0 1 0 0 0 1

    tb.setInputs([
        cg["macFCSType"],
        cg["phyFSKScramblePSDU"],
        cg["phyFSKFECScheme"],
        cg["phyFSKFECEnabled"],
        cg["phyFSKFECInterleavingRSC"],
        cg["modulation"],
        cg["phyFSKPreambleLength"],
        cg["phyMRFSKSFD"],
        cg["ready_i"],
        cg["valid_i"]
    ])
    tb.setExpectedOutputs([
        cg["ready_o"],
        cg["valid_o"],
        cg["data_o"],
        cg["last_o"],
        None,
        None,
        None,
        None
    ])

    tb.setActualOutputsNames([
        "ready_o (actual)",
        "valid_o (actual)",
        "data_o (actual)",
        "last_o (actual)",
        "test 0",
        "test 1",
        "test 2",
        "test 3"
    ])

    cg.setTemplates({
        "data_o (actual)" : "data_o"
    })


    tb.run()

    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_MR_FSK_modulator.svg'))

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)