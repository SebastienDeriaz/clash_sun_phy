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

from sun_phy import Ofdm_modulator
# from sun_phy.mr_ofdm.mr_ofdm_modulator import FREQUENCY_SPREADING, N_BPSC, FFT_SIZE
# from sun_phy.tools.errors import UnsupportedError

filepath = join(dirname(__file__), '../../../Sun_phy/MR_OFDM/OFDM.hs')


def test_subcarrierCounterToIndex():
    f = Function(filepath, 'subcarrierCounterToIndex')
    
    for N_FFT, active_tones in zip([128, 64, 32, 16], [104, 52, 26, 14]):
        for i in range(active_tones):
            offset = (N_FFT - active_tones) // 2
            expected = offset + i + (1 if i >= active_tones // 2 else 0)
            out = int(f.test([N_FFT, active_tones, i]))

            assert expected == out

def test_counterToSpreadIndex():
    f = Function(filepath, 'counterToSpreadIndex')
    
    
    print([f.test([4, 64, 52, i]) for i in range(52)])
    # for N_FFT, active_tones in zip([128, 64, 32, 16], [104, 52, 26, 14]):
    #     for SF in [1, 2, 4]:
    #         if N_FFT <= 32 and SF == 4:
    #             # Skip these because they aren't in the norm (and not possible)
    #             continue

    #     for i in range(active_tones):
    #         offset = (N_FFT - active_tones) // 2
    #         expected = offset + i + (1 if i > active_tones // 2 else 0)
    #         out = int(f.test([N_FFT, active_tones, i]))

    #         assert expected == out

def test_OFDM():
    mod = Ofdm_modulator(
        N_FFT=16,
        BW=8e3,
        modulation='QPSK',
        CP=1/4,
        padding_left=1,
        padding_right=0,
        pilots_indices=np.array([-3, 5]),
        pilots_values=np.array([1, 1]),
        frequency_spreading=2,
        verbose=False
    )


    message = np.array([0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1])

    I, Q, _ = mod.messageToIQ(message)


    print(f"{message} - {I,Q}")
    print(f"subcarriers : {mod._subcarriers}")

    cg = Chronogram(join(dirname(__file__), './test_OFDM.json'))
    tb = Testbench(filepath, 'ofdm', verbose=False)

    tb.setInputs([
        cg["N_FFT"],
        cg["Bandwidth"],
        cg["Modulation"],
        cg["CP"],
        cg["dataTones"],
        cg["pilotTones"],
        cg["frequency_spreading"],
        cg["pilot_valid_i"],
        cg["pilot_index_i"],
        cg["pilot_value_i"],
        cg["pilot_last_i"],
        cg["data_valid_i"],
        cg["data_i"],
        cg["data_last_i"],
        cg["ready_i"]
    ])

    tb.setExpectedOutputs([
        cg["pilot_ready_o"],
        cg["data_ready_o"],
        cg["data_o"],
        cg["valid_o"],
        cg["last_o"],
        None
    ])

    tb.setActualOutputsNames([
        "pilot_ready_o (actual)",
        "data_ready_o (actual)",
        "data_o (actual)",
        "valid_o (actual)",
        "last_o (actual)",
        "state",
        "subcarrierCounter",
        "subcarrierIndex'"
    ])

    cg.setTemplates({
        "data_o (actual)" : "data_o"
    })

    tb.run()

    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_OFDM.svg'))

    # for s in tb:
    #     if s.isChecked():
    #         s.print(True)
    #         assert s.isValid(), s.message()
    #     else:
    #         s.print(True)

    # Check IQ signal
    outputTh = I + 1j*Q
    outputSignal = np.array([complex(*[float(x) for x in str(s)[1:-1].split(',')]) for s in tb._actualOutputs["data_o (actual)"].samples][25:25+outputTh.size])
    print(outputSignal)

    # plt.figure()
    # plt.subplot(211)
    # plt.plot(outputTh.real, linewidth=3)
    # plt.plot(outputSignal.real)
    # plt.subplot(212)
    # plt.plot(outputTh.imag, linewidth=3)
    # plt.plot(outputSignal.imag)
    # plt.show()

    assert np.var(outputSignal - outputTh) < 0.1


if __name__ == '__main__':
    test_OFDM()