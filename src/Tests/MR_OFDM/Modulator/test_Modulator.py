# Modulator testbench
# 
# Sébastien Deriaz
# 25.01.2023
#

from clash_testbench import Chronogram, Testbench, Signal, Function
from os.path import join, dirname
import numpy as np

import pytest

from itertools import product

from sun_phy.tools.modulations import BPSK, QPSK, QAM16

filepath = join(dirname(__file__), '../../../SunPhy/MR_OFDM/Modulator.hs')

def test_BPSK():
    tb = Testbench(filepath, 'modulator')
    cg = Chronogram(join(dirname(__file__), 'test_Modulator_BPSK.json'))
    mod = BPSK()
    np.random.seed(25012023)
    message = np.random.randint(0, 2, mod.bits_per_symbol() * 10)
    th = mod.convert(message)
    
    print(f"message           : {message}")
    print(f"modulated message : {th}")


    tb.setInputs([
        cg["MCS"],
        cg["valid_i"],
        cg["data_i"],
        cg["last_i"],
        cg["ready_i"]
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
        "last_o (actual)",
        "dataCounter",
    ])

    cg.setTemplates({
        'data_o (actual)' : 'data_o'
    })

    tb.run()
    

    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_Modulator_BPSK.svg'), ticks=True, scale=1.5)

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)

def test_QPSK():
    tb = Testbench(filepath, 'modulator')
    cg = Chronogram(join(dirname(__file__), 'test_Modulator_QPSK.json'))
    mod = QPSK()
    np.random.seed(25012023)
    message = np.random.randint(0, 2, mod.bits_per_symbol() * 5)
    th = mod.convert(message)
    
    print(f"message           : {message}")
    print(f"modulated message : {th}")


    tb.setInputs([
        cg["MCS"],
        cg["valid_i"],
        cg["data_i"],
        cg["last_i"],
        cg["ready_i"]
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
        "last_o (actual)",
        "dataCounter",
    ])

    cg.setTemplates({
        'data_o (actual)' : 'data_o'
    })

    tb.run()
    

    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_Modulator_QPSK.svg'), ticks=True, scale=1.5)

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)

def test_QAM16():
    tb = Testbench(filepath, 'modulator')
    cg = Chronogram(join(dirname(__file__), 'test_Modulator_QAM16.json'))
    mod = QAM16()
    np.random.seed(25012023)
    message = np.random.randint(0, 2, mod.bits_per_symbol() * 5)
    th = mod.convert(message)
    
    print(f"message           : {message}")
    print(f"modulated message : {th}")


    tb.setInputs([
        cg["MCS"],
        cg["valid_i"],
        cg["data_i"],
        cg["last_i"],
        cg["ready_i"]
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
        "last_o (actual)",
        "dataCounter",
    ])

    cg.setTemplates({
        'data_o (actual)' : 'data_o'
    })

    tb.run()
    

    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_Modulator_QAM16.svg'), ticks=True, scale=1.5)

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)