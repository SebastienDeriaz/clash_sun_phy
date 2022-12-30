# PHR testbench
#

from clash_testbench import Testbench, Chronogram, Signal
from os.path import join, dirname

from sun_phy.mr_fsk.mr_fsk_modulator import Mr_fsk_modulator, PHR_LENGTH

def test_PHR():
    filepath = join(dirname(__file__), '../../../Sun_phy/MR_FSK/PHR.hs')

    tb = Testbench(filepath, 'phr')
    # inputs
    phrModeSwitch = Signal("phrModeSwitch", [0])
    phrFCS = Signal("phrFCS", [0,0])
    phrDataWhitening = Signal("phrDataWhitening", [0,0])
    phrFrameLength = Signal("phrFrameLength", [0,0]) ## 11 bits
    start = Signal("start", [0,0])
    ready_i = Signal("ready_i", [0,0])
    valid_i = Signal("valid_i", [0,0])
    # outputs
    valid_o = Signal("valid_o", [0,0,0])
    data_o = Signal("data_o", [0,0])
    last_o = Signal("last_o", [0,0])


    # Number of samples added by each test
    N_PHR = PHR_LENGTH * 8
    N = N_PHR + 4
    
    for _phrFCS in range(2):
        for _phrDataWhitening in range(2):
            for _phrFrameLength in [0, 100, 2047]: # 2047 is the max
                mod = Mr_fsk_modulator(
                    symbolRate=1e3,
                    FSKModulationIndex=1,
                    phyMRFSKSFD=0,
                    modulation='2FSK',
                    phyFSKFECEnabled=False,
                    phyFSKFECScheme=0,
                    macFCSType=_phrFCS,
                    phyFSKScramblePSDU=_phrDataWhitening,
                    phyFSKFECInterleavingRSC=False)
                PHR_th = mod._PHR(_phrFrameLength)
                # Inputs
                ready_i += [0] + N_PHR // 2 * [1] + [0] + N_PHR // 2 * [1] + [0, 0]
                phrFCS += (N * [_phrFCS])
                phrDataWhitening += N * [_phrDataWhitening]
                phrFrameLength += N * [_phrFrameLength]
                start += [1] + (N-1) * [0]
                valid_i += [1] * N
                # Outputs
                last_o += (N_PHR+1) * [0] + [1] + [0,0]
                PHR_th_list = list(PHR_th)
                data_o += [0] + \
                    PHR_th_list[:len(PHR_th_list) // 2] + \
                    [PHR_th_list[len(PHR_th_list) // 2]] + \
                    PHR_th_list[len(PHR_th_list) // 2:] + 2 * [0]

    valid_o += [1] * (len(data_o) - len(valid_o))

    tb.setInputs([
        phrModeSwitch,
        phrFCS,
        phrDataWhitening,
        phrFrameLength,
        ready_i,
        valid_i
    ])

    tb.setExpectedOutputs([
        valid_o,
        data_o,
        last_o
    ])

    tb.setActualOutputsNames([
        "valid_o (actual)",
        "data_o (actual)",
        "last_o (actual)"
    ])

    cg = Chronogram()

    tb.run()

    cg.loadTestbench(tb)
    cg.saveSVG(join(dirname(__file__), 'test_PHR.svg'))

    for s in tb:
        s.print(True)
        if s.isChecked():
            assert s.isValid(), s.message()
        

    


if __name__ == '__main__':
    test_PHR()