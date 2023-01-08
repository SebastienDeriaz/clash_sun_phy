
# Splitter2 testbench
# 
# Sébastien Deriaz
# 05.12.2022


from clash_testbench import Chronogram, Testbench, Signal
from os.path import join, dirname
import numpy as np

from sun_phy.mr_fsk.mr_fsk_modulator import Mr_fsk_modulator

filepath = join(dirname(__file__), '../../Sun_phy/Splitter2.hs')


def test_Splitter2():
    tb = Testbench(filepath, 'splitter2')

    cg = Chronogram(join(dirname(__file__), 'test_Splitter2.json'))

    tb.setInputs([
        cg["valid_i"],
        cg["data_i"],
        cg["last_i"],
        cg["a_ready_i"],
        cg["b_ready_i"],
        cg["a_length"]
    ])
    tb.setExpectedOutputs([
        cg["ready_o"],
        cg["a_valid_o"],
        cg["a_data_o"],
        cg["a_last_o"],
        cg["b_valid_o"],
        cg["b_data_o"],
        cg["b_last_o"]
        
    ])

    tb.setActualOutputsNames([
        "ready_o (actual)",
        "a_valid_o (actual)",
        "a_data_o (actual)",
        "a_last_o (actual)",
        "b_valid_o (actual)",
        "b_data_o (actual)",
        "b_last_o (actual)"
    ])

    cg.setTemplates({
        "ready_o (actual)" : "ready_o",
        "a_valid_o (actual)" : "a_valid_o",
        "a_data_o (actual)" : "a_data_o",
        "a_last_o (actual)" : "a_last_o",
        "b_valid_o (actual)" : "b_valid_o",
        "b_data_o (actual)" : "b_data_o",
        "b_last_o (actual)" : "b_last_o"
    })


    tb.run()

    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_Splitter2.svg'))

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)