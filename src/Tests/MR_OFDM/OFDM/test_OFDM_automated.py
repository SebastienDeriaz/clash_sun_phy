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

from sun_phy import Ofdm_modulator, Mr_ofdm_modulator
from sun_phy.mr_ofdm.mr_ofdm_modulator import DATA_TONES, MODULATION_AND_CODING_SCHEME, FREQUENCY_SPREADING
from sun_phy.tools.errors import UnsupportedError
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



def test_OFDM_automated():
    cp = 1/4
    ofdmOption = 3
    MCS = 2

    try:
        mrmod = Mr_ofdm_modulator(
            MCS=MCS,
            OFDM_Option=ofdmOption,
        )
    except UnsupportedError:
        return
    
    N_FFT = mrmod._N_FFT
    modulation = MODULATION_AND_CODING_SCHEME[MCS]
    frequencySpreading = FREQUENCY_SPREADING[MCS]

    mod = Ofdm_modulator(
        N_FFT=N_FFT,
        BW=8e3,
        modulation=modulation,
        CP=cp,
        padding_left=1,
        padding_right=0,
        pilots_indices=np.array([-3, 5]),
        pilots_values=np.array([1, 1]),
        frequency_spreading=frequencySpreading,
        verbose=False
    )

    message = np.random.randint(0, 2, mod._message_split_length)
    #message = np.array([0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1])

    I, Q, _ = mod.messageToIQ(message)


    print(f"{message} - {I,Q}")
    print(f"subcarriers : {mod._subcarriers}")

    cg = Chronogram(join(dirname(__file__), './test_OFDM.json'))
    tb = Testbench(filepath, 'ofdm', verbose=False)
    
    message_iter = iter(message)
    data_i_samples = [next(message_iter)]
    for r in cg["data_ready_o"].samples:
        if int(str(r.value())) > 0:
            try:
                data_i_samples.append(next(message_iter))
            except StopIteration:
                data_i_samples.append(0)
        else:
            data_i_samples.append(data_i_samples[-1])



    data_i = Signal("data_i", data_i_samples[:-1])
    n_fft = Signal("N_FFT", [N_FFT])
    mod = Signal("Modulation", modulation)
    data_tones = Signal("dataTones", [DATA_TONES[ofdmOption]])
    pilot_tones = Signal("pilotTones", mod._N_pilots)
    frequency_spreading = Signal("frequency_spreading", [frequencySpreading])
    pilot_valid = Signal("pilot_valid_i", 3*[0] + mod._N_pilots*[1] + )

    if cp == 1/2:
        cp_str = 'CP_HALF'
    elif cp == 1/4:
        cp_str = 'CP_QUARTER'
    else:
        cp_str = 'CP_NONE'
    cp = Signal("CP", cp_str)

    tb.setInputs([
        n_fft,
        mod,
        cp,
        data_tones,
        pilot_tones,
        frequency_spreading,
        cg["pilot_valid_i"],
        cg["pilot_index_i"],
        cg["pilot_value_i"],
        cg["pilot_last_i"],
        cg["data_valid_i"],
        data_i,
        cg["data_last_i"],
        cg["ready_i"]
    ])

    tb.setExpectedOutputs([
        cg["pilot_ready_o"],
        cg["data_ready_o"],
        None,
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
        "subcarrierReadEnd",
        "isLast"
    ])

    cg.setTemplates({
        "data_o (actual)" : "data_o"
    })

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
    outputSignal = rawOutputSignal[dataLocations]
    print(outputSignal)

    assert np.var(outputSignal - outputTh) < 0.001


if __name__ == '__main__':
    test_OFDM()