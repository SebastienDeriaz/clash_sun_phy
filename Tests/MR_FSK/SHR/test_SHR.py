# SHR testbench
#

from clash_testbench import Testbench, Chronogram, Signal
from os.path import join, dirname

from sun_phy.mr_fsk.mr_fsk_modulator import Mr_fsk_modulator


def test_SHR():
    filepath = join(dirname(__file__), '../../../Sun_phy/MR_FSK/SHR.hs')

    tb = Testbench(filepath, 'shr')
    cg = Chronogram(join(dirname(__file__), 'test_SHR.json'))

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

# def test_SHR_all():
#     filepath = join(dirname(__file__), '../../../Sun_phy/MR_FSK/PHR.hs')

#     tb = Testbench(filepath, 'phr')
#     # inputs
#     phrModeSwitch = Signal("phrModeSwitch", [0])
#     phrFCS = Signal("phrFCS", [0,0])
#     phrDataWhitening = Signal("phrDataWhitening", [0,0])
#     phrFrameLength = Signal("phrFrameLength", [0,0]) ## 11 bits
#     start = Signal("start", [0,0])
#     busy = Signal("busy", [0,0])
#     # outputs
#     end = Signal("end", [1,1])
#     valid = Signal("valid", [0,0])
#     data = Signal("data", [0,0])
    
#     for _modulation in range(2):
#         for _phrDataWhitening in range(2):
#             for _phrFrameLength in [0, 100, 2047]: # 2047 is the max
#                 mod = Mr_fsk_modulator(
#                     phyMRFSKSFD=0,
#                     modulation='2FSK' if _modulation else '4FSK'
#                     phyFSKFECEnabled=False,
#                     phyFSKFECScheme=0,
#                     macFCSType=_phrFCS,
#                     phyFSKScramblePSDU=_phrDataWhitening,
#                     phyFSKFECInterleavingRSC=False)
                
#                 SHR_th = mod._SHR()
#                 N_SHR = len(SHR_th)
#                 # Inputs
#                 start = Signal("start", [0,0] + [1] + N_SHR * [0])
#                 ready_i = Signal("ready_i", [1,1] + N_SHR * [1])
#                 modulation = Signal("modulation", [_modulation])
#                 phyMRFSKSFD = 
#                 phyFSKFECEnabled = 
#                 phyFSKPreambleLength = 

#                 busy += [0,0] + N_PHR // 2 * [0] + [1] + N_PHR // 2 * [0] + [0]
#                 phrFCS += (N * [_phrFCS])
#                 phrDataWhitening += N * [_phrDataWhitening]
#                 phrFrameLength += N * [_phrFrameLength]
#                 start += [1] + (N-1) * [0]
#                 # Outputs
#                 end += [1] + (N_PHR+1) * [0] + 2 * [1]
#                 valid += [0] + (N_PHR+1) * [1] + 2 * [0]
#                 PHR_th_list = list(PHR_th)
#                 data += [0] + \
#                     PHR_th_list[:len(PHR_th_list) // 2+1] + \
#                     [PHR_th_list[len(PHR_th_list) // 2+1]] + \
#                     PHR_th_list[len(PHR_th_list) // 2+1:] + 2 * [0]

#     tb.setInputs([
#         phrModeSwitch,
#         phrFCS,
#         phrDataWhitening,
#         phrFrameLength,
#         start,
#         busy
#     ])

#     tb.setExpectedOutputs([
#         end,
#         valid,
#         data
#     ])

#     tb.setActualOutputsNames([
#         "end",
#         "valid",
#         "data"
#     ])

#     cg = Chronogram()

#     tb.run()

#     cg.loadTestbench(tb)
#     cg.saveSVG(join(dirname(__file__), 'test_PHR.svg'))

#     for s in tb:
#         s.print(True)
#         if s.isChecked():
#             assert s.isValid(), s.message()
        

    


if __name__ == '__main__':
    test_PHR()