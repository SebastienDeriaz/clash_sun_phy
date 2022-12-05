# PHR testbench
#

from clash_testbench.entity import Entity
from clash_testbench.signals import *
from clash_testbench.chronogram import Chronogram
from os.path import join, dirname

from sun_phy.mr_fsk.mr_fsk_modulator import Mr_fsk_modulator, PHR_LENGTH

def test_PHR():
    filepath = join(dirname(__file__), 'PHR.hs')

    entity = Entity(filepath, 'phr')

    inputs = {
        "phrModeSwitch" : Bit(0),
        "phrFCS" : Bit([0,0]),
        "phrDataWhitening" : Bit([0,0]),
        "phrFrameLength" : Unsigned(11, [0,0]),
        "start" : Bit([0,0]),
        "busy" : Bit([0,0])
    }

    outputs = {
        "end" : Bit([1,1]),
        "valid" : Bit([0,0]),
        "data" : Bit([0,0])
    }

    # Number of samples added by each test
    N_PHR = PHR_LENGTH * 8
    N = N_PHR + 4
    
    for phrFCS in range(2):
        for phrDataWhitening in range(2):
            for phrFrameLength in [0, 100, 2047]: # 2047 is the max
                mod = Mr_fsk_modulator(
                    phyMRFSKSFD=0,
                    modulation='2FSK',
                    phyFSKFECEnabled=False,
                    phyFSKFECScheme=0,
                    macFCSType=phrFCS,
                    phyFSKScramblePSDU=phrDataWhitening,
                    phyFSKFECInterleavingRSC=False)
                PHR_th = mod._PHR(phrFrameLength)
                # Inputs
                
                inputs["busy"] += [0,0] + N_PHR // 2 * [0] + [1] + N_PHR // 2 * [0] + [0]
                inputs["phrFCS"] += (N * [phrFCS])
                inputs["phrDataWhitening"] += N * [phrDataWhitening]
                inputs["phrFrameLength"] += N * [phrFrameLength]
                inputs["start"] += [1] + (N-1) * [0]
                # Outputs
                outputs["end"] += [1] + (N_PHR+1) * [0] + 2 * [1]
                outputs["valid"] += [0] + (N_PHR+1) * [1] + 2 * [0]
                PHR_th_list = list(PHR_th)
                outputs["data"] += [0] + \
                    PHR_th_list[:len(PHR_th_list) // 2+1] + \
                    [PHR_th_list[len(PHR_th_list) // 2+1]] + \
                    PHR_th_list[len(PHR_th_list) // 2+1:] + 2 * [0]

    report = entity.test(inputs, outputs)

    cg = Chronogram(report)
    cg.saveSVG(join(dirname(__file__), 'test_PHR.svg'))

    for s in report:
        s.print()
        assert s.valid, s.message()

    


if __name__ == '__main__':
    test_PHR()