# PHR testbench
#

from clash_testbench.entity import Entity
from clash_testbench.signals import *
from clash_testbench.chronogram import Chronogram
from os.path import join, dirname

def test_PSDU():
    filepath = join(dirname(__file__), 'PSDU.hs')

    entity = Entity(filepath, 'psdu')

    PSDU_DATA = [1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]

    inputs = {
        "start" : Bit([0,1] + (len(PSDU_DATA) + 2) * [0]),
        "busy" : Bit(0)
    }

    outputs = {
        "end" : Bit([1,1] + (len(PSDU_DATA)) * [0] + 2* [1]),
        "valid" : Bit([0,0] + len(PSDU_DATA) * [1] + 2*[0]),
        "data" : Bit([PSDU_DATA[0]]*2 + PSDU_DATA + 2*[PSDU_DATA[0]])
    }


    report = entity.test(inputs, outputs)

    cg = Chronogram(report)
    cg.saveSVG(join(dirname(__file__), 'test_PSDU.svg'))

    for s in report:
        s.print()
        assert s.valid, s.message()

    


if __name__ == '__main__':
    test_PSDU()