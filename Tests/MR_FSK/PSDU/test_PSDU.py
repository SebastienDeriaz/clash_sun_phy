# PSDU testbench
#

from clash_testbench import Testbench, Chronogram, Signal
from os.path import join, dirname

def test_PSDU():
    filepath = join(dirname(__file__), '../../../Sun_phy/MR_FSK/PSDU.hs')

    tb = Testbench(filepath, 'psdu')
    cg = Chronogram(join(dirname(__file__), 'test_PSDU.json'))
    tb.setInputs([
        cg["ready_i"]
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

    tb.run()

    cg.loadTestbench(tb)
    cg.saveSVG(join(dirname(__file__), 'test_PSDU.svg'))

    for s in tb:
        s.print(True)
        if s.isChecked():
            assert s.isValid(), s.message()

    


if __name__ == '__main__':
    test_PSDU()