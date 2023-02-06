
# Bypass testbench
# 
# Sébastien Deriaz
# 06.12.2022


from clash_testbench import Chronogram, Testbench, Signal
from os.path import join, dirname
import numpy as np

from sun_phy.mr_fsk.mr_fsk_modulator import Mr_fsk_modulator

filepath = join(dirname(__file__), './test_Bypass.hs')


def test_Bypass():
    tb = Testbench(filepath, 'testBypass')

    cg = Chronogram(join(dirname(__file__), 'test_Bypass.json'))

    tb.setInputs([
        cg["valid_i"],
        cg["data_i"],
        cg["last_i"],
        cg["ready_i"]
    ])
    tb.setExpectedOutputs([
        cg["valid_o"],
        cg["data_o"],
        cg["last_o"],
        cg["ready_o"]
    ])

    tb.setActualOutputsNames([
        "valid_o (actual)",
        "data_o (actual)",
        "last_o (actual)",
        "ready_o (actual)"
    ])

    cg.setTemplates({
        "ready_o (actual)" : "ready_o",
        "valid_o (actual)" : "valid_o",
        "data_o (actual)" : "data_o",
        "last_o (actual)" : "last_o"
    })


    tb.run()

    cg.loadTestbench(tb)
    cg.saveSVG(join(dirname(__file__), 'test_Bypass.svg'))

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)