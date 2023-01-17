# PHR testbench
#

from clash_testbench import Testbench, Chronogram
from clash_testbench.signals import *
from os.path import join, dirname

def test_concat2():
    filepath = join(dirname(__file__), '../../SunPhy/Concat3.hs')

    tb = Testbench(filepath, 'concat3')

    cg = Chronogram(join(dirname(__file__), 'test_Concat3.json'))


    tb.setInputs([
        cg["a_valid_i"],
        cg["a_data_i"],
        cg["a_last_i"],
        cg["b_valid_i"],
        cg["b_data_i"],
        cg["b_last_i"],
        cg["c_valid_i"],
        cg["c_data_i"],
        cg["c_last_i"],
        cg["ready_i"]
    ])

    tb.setExpectedOutputs([
        cg["a_ready_o"],
        cg["b_ready_o"],
        cg["c_ready_o"],
        cg["valid_o"],
        cg["data_o"],
        cg["last_o"]
    ])

    tb.setActualOutputsNames([
        "a_ready_o (actual)",
        "b_ready_o (actual)",
        "c_ready_o (actual)",
        "valid_o (actual)",
        "data_o (actual)",
        "last_o (actual)"
    ])

    tb.run()

    cg.loadTestbench(tb)
    
    cg.saveSVG(join(dirname(__file__), 'test_Concat3.svg'))

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)


if __name__ == '__main__':
    test_concat2()