# PHR testbench
#

from clash_testbench import Entity, Chronogram
from clash_testbench.signals import *
from os.path import join, dirname

def test_concat2():
    filepath = join(dirname(__file__), 'concat3.hs')

    entity = Entity(filepath, 'concat3')

    cg = Chronogram(join(dirname(__file__), 'test_concat3.json'))

    report = entity.test(cg.inputs, cg.expectedOutputs)

    cg.loadReport(report)
    
    cg.saveSVG(join(dirname(__file__), 'test_concat3.svg'))

    for s in report:
        s.print()
        assert s.valid, s.message()

if __name__ == '__main__':
    test_concat2()