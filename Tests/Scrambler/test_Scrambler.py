# Scrambler testbench
#
# Sébastien Deriaz
# 07.12.2022

from clash_testbench import Testbench, Chronogram, Signal
from os.path import join, dirname

def test_Scrambler():
    filepath = join(dirname(__file__), '../../Sun_phy/Scrambler.hs')

    tb = Testbench(filepath, 'scrambler')
    cg = Chronogram(join(dirname(__file__), 'test_Scrambler.json'))

    tb.setInputs([
        cg["bypass"],
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
    tb.run()

    cg.loadTestbench(tb)
    cg.saveSVG(join(dirname(__file__), 'test_Scrambler.svg'))

    for s in tb:
        s.print(True)
        if s.isChecked():
            assert s.isValid(), s.message()


if __name__ == '__main__':
    test_Scrambler()