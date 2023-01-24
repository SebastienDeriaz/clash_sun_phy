# PN9 testbench
#
# Sébastien Deriaz
# 07.12.2022

from clash_testbench import Testbench, Chronogram, Signal
from os.path import join, dirname

def test_PN9():
    filepath = join(dirname(__file__), '../../SunPhy/PN9.hs')

    tb = Testbench(filepath, 'pn9')
    cg = Chronogram(join(dirname(__file__), 'test_PN9.json'))

    tb.setInputs([
        cg["seed"],
        cg["next_i"],
        cg["reset_i"]
    ])

    tb.setExpectedOutputs([
        cg["data_o"],
        None
        ])
    tb.setActualOutputsNames([
        "data_o (actual)",
        "reg"
        ])
    tb.run()

    cg.loadTestbench(tb)
    cg.saveSVG(join(dirname(__file__), 'test_PN9.svg'))

    for s in tb:
        s.print(True)
        if s.isChecked():
            assert s.isValid(), s.message()


if __name__ == '__main__':
    test_PN9()