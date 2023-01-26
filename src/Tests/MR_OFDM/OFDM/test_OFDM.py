# OFDM testbench
# 
# Sébastien Deriaz
# 01.01.2023
#
from clash_testbench import Chronogram, Testbench, Signal, Function
from os.path import join, dirname

import numpy as np
import pytest

import matplotlib.pyplot as plt
from itertools import product

from sun_phy import Ofdm_modulator, Mr_ofdm_modulator
# from sun_phy.mr_ofdm.mr_ofdm_modulator import FREQUENCY_SPREADING, N_BPSC, FFT_SIZE
# from sun_phy.tools.errors import UnsupportedError

filepath = join(dirname(__file__), '../../../SunPhy/MR_OFDM/OFDM.hs')


# def test_subcarrierCounterToIndex():
#     f = Function(filepath, 'subcarrierCounterToIndex')
    
#     for N_FFT, active_tones in zip([128, 64, 32, 16], [104, 52, 26, 14]):
#         for i in range(active_tones):
#             offset = (N_FFT - active_tones) // 2
#             expected = offset + i + (1 if i >= active_tones // 2 else 0)
#             out = int(f.test([N_FFT, active_tones, i]))

#             assert expected == out

# def test_counterToSpreadIndex():
#     f = Function(filepath, 'counterToSpreadIndex')
    
    
#     print([f.test([4, 64, 52, i]) for i in range(52)])
#     # for N_FFT, active_tones in zip([128, 64, 32, 16], [104, 52, 26, 14]):
#     #     for SF in [1, 2, 4]:
#     #         if N_FFT <= 32 and SF == 4:
#     #             # Skip these because they aren't in the norm (and not possible)
#     #             continue

#     #     for i in range(active_tones):
#     #         offset = (N_FFT - active_tones) // 2
#     #         expected = offset + i + (1 if i > active_tones // 2 else 0)
#     #         out = int(f.test([N_FFT, active_tones, i]))

#     #         assert expected == out

# #OFDM_Option = list(range(1,5))
OFDM_Option = [4]
MCS = [2]

test_options = list(product(OFDM_Option, MCS))


@pytest.mark.parametrize("OFDM_Option, MCS", test_options)
def test_OFDM(OFDM_Option, MCS):
    mod = Mr_ofdm_modulator(
        OFDM_Option=OFDM_Option,
        MCS=MCS,
        verbose=False
    )
    
    psdu = np.array([0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1])
    
    mod.message_to_IQ(psdu, binary=True)

    message = mod._PHY_header_interleaved
    print(f"message : {message}")
    I, Q = mod._PHR_I, mod._PHR_Q
    subcarrier_vec = mod._phr_subcarriers.reshape(-1, order='F')
    np.save(join(dirname(__file__), 'subcarriers_th'), mod._phr_subcarriers)
    np.save(join(dirname(__file__), 'ofdm_th'), I + 1j*Q)
    #I, Q = subcarrier_vec.real, subcarrier_vec.imag

    cg = Chronogram(join(dirname(__file__), './test_OFDM.json'))
    tb = Testbench(filepath, 'ofdm', verbose=False)

    tb.setInputs([
        cg["OFDM_option"],
        cg["MCS"],
        cg["CP"],
        cg["data_valid_i"],
        cg["data_i"],
        cg["data_last_i"],
        cg["pilotset_index_i"],
        cg["pilotset_write_i"],
        cg["pn9seed_data_i"],
        cg["pn9seed_write_i"],
        cg["ready_i"],
    ])

    tb.setExpectedOutputs([
        cg["data_ready_o"],
        cg["valid_o"],
        None,
        cg["last_o"],
        None,
        None
    ])

    tb.setActualOutputsNames([
        "data_ready_o (actual)",
        "valid_o (actual)",
        "data_o (actual)",
        "last_o (actual)",
        "pilotSetCounter",
        "pn9 register",
        "State"
    ])

    tb.run()

    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_OFDM.svg'))

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)

    # Check IQ signal
    outputTh = I + 1j*Q
    readySignal = np.array([int(str(s.value())) for s in cg["ready_i"].samples])
    validSignal = np.array([int(str(s.value())) for s in tb._actualOutputs["valid_o (actual)"].samples])
    rawOutputSignal = np.array([complex(*[float(x) for x in str(s)[1:-1].split(',')]) for s in tb._actualOutputs["data_o (actual)"].samples])
    # Location of data values (when ready = 1)
    dataLocations = np.logical_and(readySignal, validSignal)
    outputSignal = np.asarray(rawOutputSignal[dataLocations])
    np.save(join(dirname(__file__), 'raw_ofdm'), outputSignal)

    print(outputSignal)

    plt.figure()
    plt.subplot(211)
    plt.plot(outputTh.real)
    plt.plot(outputSignal.real)
    plt.vlines(np.arange(7)*16, -2, 2)
    plt.grid()
    plt.subplot(212)
    plt.plot(outputTh.imag)
    plt.plot(outputSignal.imag)
    plt.vlines(np.arange(7)*16, -2, 2)
    plt.grid()
    plt.show()
    
    assert np.var(outputSignal - outputTh) < 0.001


if __name__ == '__main__':
    test_OFDM(4, 2)