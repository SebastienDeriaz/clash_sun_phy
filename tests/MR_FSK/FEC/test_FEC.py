# PHR testbench
#

from clash_testbench import Chronogram, Testbench, Signal
from os.path import join, dirname
import numpy as np

from sun_phy.mr_fsk.mr_fsk_modulator import Mr_fsk_modulator

filepath = join(dirname(__file__), 'FEC.hs')


def test_RSC():
    tb = Testbench(filepath, 'rscEncoder')

    cg = Chronogram()
    
    mod = Mr_fsk_modulator(
        phyMRFSKSFD=0,
        modulation='2FSK',
        phyFSKFECEnabled=True,
        phyFSKFECScheme=1,
        macFCSType=0,
        phyFSKScramblePSDU=False,
        phyFSKFECInterleavingRSC=False)

    rsc_input = np.array([0,0,1,1,0,1,1,1])
    rsc_output = mod._FEC(rsc_input, False, False)

    enable = Signal("enable", [0,0] + [1] * len(rsc_input) + [0])
    input  = Signal("input", [0,0] + list(rsc_input) + [0])
    ui0    = Signal("ui0", [0,0,0] + list(rsc_output)[1::2])
    ui1    = Signal("ui1", [0,0,0] + list(rsc_output)[::2])

    tb.setInputs([
        enable,
        input
    ])
    tb.setExpectedOutputs([
        None,
        ui0,
        ui1
    ])

    tb.setActualOutputsNames([
        None,
        "ui0 (actual)",
        "ui1 (actual)"
    ])

    
    tb.run()

    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_RSC.svg'))

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)

def test_NRNSC():
    tb = Testbench(filepath, 'nrnscEncoder')

    cg = Chronogram()
    
    mod = Mr_fsk_modulator(
        phyMRFSKSFD=0,
        modulation='2FSK',
        phyFSKFECEnabled=True,
        phyFSKFECScheme=0,
        macFCSType=0,
        phyFSKScramblePSDU=False,
        phyFSKFECInterleavingRSC=False)

    nrnsc_input = np.array([0,0,1,1,0,1,1,1])
    nrnsc_output = mod._FEC(nrnsc_input, False, False)

    print(f"{nrnsc_input} -> {nrnsc_output}")

    enable = Signal("enable", [0,0] + [1] * len(nrnsc_input) + [0])
    input  = Signal("input", [0,0] + list(nrnsc_input) + [0])
    ui0    = Signal("ui0", [0,0,0] + list(nrnsc_output)[1::2])
    ui1    = Signal("ui1", [0,0,0] + list(nrnsc_output)[::2])

    tb.setInputs([
        enable,
        input
    ])
    tb.setExpectedOutputs([
        None,
        ui0,
        ui1
    ])

    tb.setActualOutputsNames([
        None,
        "ui0 (actual)",
        "ui1 (actual)"
    ])

    
    tb.run()

    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_NRNSC.svg'))

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)

def test_FEC():
    
    tb = Testbench(filepath, 'fec')

    for phyFSKFECScheme in [0, 1]:
        mod = Mr_fsk_modulator(
            phyMRFSKSFD=0,
            modulation='2FSK',
            phyFSKFECEnabled=True,
            phyFSKFECScheme=phyFSKFECScheme,
            macFCSType=0,
            phyFSKScramblePSDU=False,
            phyFSKFECInterleavingRSC=False)

        data = np.array([0,0,1,1,0,1,1,1])
        fec_data = mod._FEC(data)

        
        cg = Chronogram(join(dirname(__file__), 'test_FEC.json'))

        data_o = cg["data_o"]
        data_o[4:4+len(fec_data)] = list(fec_data)
        cg["data_o"] = data_o

        cg["phyFSKFECScheme"] = Signal("phyFSKFECScheme", [phyFSKFECScheme] * len(cg["data_o"]))

        tb.setInputs([
            cg["phyFSKFECScheme"],
            cg["valid_i"],
            cg["data_i"],
            cg["last_i"],
            cg["ready_i"]
        ])

        tb.setExpectedOutputs([
            cg["ready_o"],
            cg["valid_o"],
            cg["data_o"],
            cg["last_o"],
            None,#cg["state"],
            None
        ])

        tb.setActualOutputsNames([
            "ready_o (actual)",
            "valid_o (actual)",
            "data_o (actual)",
            "last_o (actual)",
            "state (actual)",
            "ui0",
            "ui1",
            "enable",
            "input",
            "test"
        ])

        cg.setTemplates({
            #"ready_o (actual)" : "ready_o"
            "data_o (actual)" : "data_o"
        })

        tb.run()

        cg.setSignals(tb.actualOutputs())

        cg.saveSVG(join(dirname(__file__), f'test_FEC_{phyFSKFECScheme}.svg'))

        for s in tb:
            if s.isChecked():
                s.print(True)
                assert s.isValid(), s.message()
            else:
                s.print(True)

if __name__ == '__main__':
    test_FEC()