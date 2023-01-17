
# Switch2 testbench
# 
# Sébastien Deriaz
# 05.12.2022


from clash_testbench import Chronogram, Testbench, Signal
from os.path import join, dirname
import numpy as np

from sun_phy.mr_fsk.mr_fsk_modulator import Mr_fsk_modulator

filepath = join(dirname(__file__), '../../SunPhy/Switch2.hs')


def test_Switch2():
    tb = Testbench(filepath, 'switch2')

    cg = Chronogram(join(dirname(__file__), 'test_Switch2.json'))

    tb.setInputs([
        cg["A_valid_i"],
        cg["A_data_i"],
        cg["A_last_i"],
        cg["B_valid_i"],
        cg["B_data_i"],
        cg["B_last_i"],
        cg["ready_i"],
        cg["sel"],
    ])
    tb.setExpectedOutputs([
        cg["A_ready_o"],
        cg["B_ready_o"],
        cg["valid_o"],
        cg["data_o"],
        cg["last_o"]
    ])

    tb.setActualOutputsNames([
        "A_ready_o (actual)",
        "B_ready_o (actual)",
        "valid_o (actual)",
        "data_o (actual)",
        "last_o (actual)"
    ])

    cg.setTemplates({
        "A_ready_o (actual)" : "A_ready_o",
        "B_ready_o (actual)" : "B_ready_o",
        "valid_o (actual)" : "valid_o",
        "data_o (actual)" : "data_o",
        "last_o (actual)" : "last_o"
    })


    tb.run()

    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_Switch2.svg'))

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)