# Scrambler testbench
#
# Sébastien Deriaz
# 07.12.2022

from clash_testbench import Testbench, Chronogram, Signal
from os.path import join, dirname

def test_Scrambler():
    filepath = join(dirname(__file__), '../../SunPhy/Scrambler.hs')

    tb = Testbench(filepath, 'scrambler')
    cg = Chronogram(join(dirname(__file__), 'test_Scrambler.json'))

    tb.setInputs([
        cg["bypass"],
        cg["ready_i"],
        cg["valid_i"],
        cg["data_i"],
        cg["last_i"],
        cg["seed"]
    ])

    tb.setExpectedOutputs([
        cg["ready_o"],
        cg["valid_o"],
        cg["data_o"],
        cg["last_o"]
    ])

    tb.setActualOutputsNames([
        "ready_o (actual)",
        "valid_o (actual)",
        "data_o (actual)",
        "last_o (actual)"
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