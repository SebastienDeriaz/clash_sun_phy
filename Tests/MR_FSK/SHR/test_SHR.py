# SHR testbench
#

from clash_testbench import Testbench, Chronogram, Signal
from os.path import join, dirname
from glob import glob

from sun_phy.mr_fsk.mr_fsk_modulator import Mr_fsk_modulator


def test_SHR():
    filepath = join(dirname(__file__), '../../../Sun_phy/MR_FSK/SHR.hs')

    tb = Testbench(filepath, 'shr')
    for cg_file in glob(join(dirname(__file__), 'test_SHR*.json')):
        cg = Chronogram(cg_file)

        tb.setInputs([
            cg["ready_i"],
            cg["modulation"],
            cg["phyMRFSKSFD"],
            cg["phyFSKFECEnabled"],
            cg["phyFSKPreambleLength"],
            cg["valid_i"]
        ])

        tb.setExpectedOutputs([
            cg["valid_o"],
            cg["data_o"],
            cg["last_o"],
            None
        ])

        tb.setActualOutputsNames([
            "valid_o (actual)",
            "data_o (actual)",
            "last_o (actual)"
        ])

        tb.run()

        cg.setTemplates({
            "data_o (actual)" : "data_o"
        })

        cg.loadTestbench(tb)
        cg.saveSVG()
        #cg.saveSVG(join(dirname(__file__), 'test_SHR.svg'))

        for s in tb:
            s.print(True)
            if s.isChecked():
                assert s.isValid(), s.message()


if __name__ == '__main__':
    test_SHR()