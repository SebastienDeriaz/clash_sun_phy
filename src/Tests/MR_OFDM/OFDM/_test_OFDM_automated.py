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
from sun_phy.mr_ofdm.mr_ofdm_modulator import DATA_TONES, MODULATION_AND_CODING_SCHEME, FREQUENCY_SPREADING, PILOTS_INDICES, N_BPSC, ACTIVE_TONES
from sun_phy.tools.errors import UnsupportedError

filepath = join(dirname(__file__), '../../../SunPhy/MR_OFDM/OFDM.hs')

def _test_OFDM_automated():
    cp = 1/4
    ofdmOption = 4
    MCS = 2
    pilot_sets = PILOTS_INDICES[ofdmOption]
    pilot_set = pilot_sets[:,np.random.randint(0, pilot_sets.shape[1])]
    pilot_values = np.random.randint(0, 2, pilot_set.size)


    try:
        mrmod = Mr_ofdm_modulator(
            MCS=MCS,
            OFDM_Option=ofdmOption
        )
    except UnsupportedError:
        return
    
    N_FFT = mrmod._N_FFT
    print(f"N_FFT = {N_FFT}")
    modulation = MODULATION_AND_CODING_SCHEME[MCS]
    frequencySpreading = FREQUENCY_SPREADING[MCS]
    print(f"{frequencySpreading = }")
    print(f"{modulation = }")

    padding_left = (N_FFT - ACTIVE_TONES[ofdmOption]) // 2

    mod = Ofdm_modulator(
        N_FFT=N_FFT,
        BW=8e3,
        modulation=modulation,
        CP=cp,
        padding_left=padding_left,
        padding_right=padding_left-1,
        pilots_indices=pilot_set,
        pilots_values=pilot_values,
        frequency_spreading=frequencySpreading,
        verbose=False
    )
    
    print(f"message length : {mod._message_split_length}")

    cg = Chronogram(join(dirname(__file__), './test_OFDM.json'))
    tb = Testbench(filepath, 'ofdm', verbose=False)

    
    N_pilot = mod._N_pilots
    N_message = int(mod._message_split_length / 2 * 3 + N_pilot + 2)
    N_IQ = int(N_FFT * (1 + cp))
    PAD = 2
    
    data_i_samples = np.random.randint(0, 2, 3 + N_pilot + N_message + N_IQ + PAD)
    data_i = Signal("data_i", data_i_samples.tolist())

    n_fft = Signal("N_FFT", [N_FFT])
    _mod = Signal("Modulation", [modulation])
    data_tones = Signal("dataTones", [DATA_TONES[ofdmOption]])
    pilot_tones = Signal("pilotTones", N_pilot)
    frequency_spreading = Signal("frequency_spreading", [frequencySpreading])
    pilot_valid = Signal("pilot_valid_i", 3*[0] + mod._N_pilots*[1] + N_message*[0] + N_IQ*[0] + PAD * [0])
    pilot_index = Signal("pilot_index_i", 3*[0] + pilot_set.tolist() + N_message * [0] + N_IQ * [0] + PAD * [0])
    pilot_value = Signal("pilot_value_i", 3*[0] + pilot_values.tolist() + N_message * [0] + N_IQ * [0] + PAD * [0])
    pilot_last = Signal("pilot_value_i", (2 + N_pilot)*[0] + [1] + N_message * [0] + N_IQ * [0] + PAD * [0])
    data_valid = Signal("data_valid_i", (3 + N_pilot)*[0] + N_message * [1] + (N_IQ + PAD) * [0])
    data_last = Signal("data_valid_i", (3 + N_pilot)*[0] + N_message * [1] + (N_IQ + PAD) * [0])
    ready = Signal("ready_i", [1])
    pilot_ready = Signal("pilot_ready_o", 2*[0] + (N_pilot+1) * [1] + (N_message + N_IQ + PAD) * [0])
    valid = Signal("valid_o", 3*[0] + N_pilot * [0] + N_message * [0] + N_IQ * [1] + PAD * [0])
    last = Signal("last_o", 3*[0] + N_pilot * [0] + N_message * [0] + (N_IQ-1) * [0] + [1] + PAD * [0])



    if cp == 1/2:
        cp_str = 'CP_HALF'
    elif cp == 1/4:
        cp_str = 'CP_QUARTER'
    else:
        cp_str = 'CP_NONE'
    cp = Signal("CP", [cp_str])

    tb.setInputs([
        n_fft,
        _mod,
        cp,
        data_tones,
        pilot_tones,
        frequency_spreading,
        pilot_valid,
        pilot_index,
        pilot_value,
        pilot_last,
        data_valid,
        data_i,
        data_last,
        ready
    ])

    tb.setExpectedOutputs([
        pilot_ready,
        None,
        None,
        valid,
        last,
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
    
    tb.run()

    data_ready_o = np.array([int(str(s.value())) for s in tb._actualOutputs["data_ready_o (actual)"].samples])
    message = data_i_samples[data_ready_o]
    message = np.random.randint(0, 2, mod._message_split_length)

    I, Q, _ = mod.messageToIQ(message)

    print(f"{message} - {I,Q}")
    print(f"subcarriers : {mod._subcarriers}")


    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_OFDM_automated.svg'))

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
    test_OFDM_automated()