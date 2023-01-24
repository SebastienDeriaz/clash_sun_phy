# Interleaver testbench
# 
# Sébastien Deriaz
# 28.12.2022
#


from clash_testbench import Chronogram, Testbench, Signal, Function
from os.path import join, dirname
import numpy as np

import pytest

from itertools import product

from sun_phy import Mr_ofdm_modulator
from sun_phy.mr_ofdm.mr_ofdm_modulator import FREQUENCY_SPREADING, N_BPSC, FFT_SIZE
from sun_phy.tools.errors import UnsupportedError

filepath = join(dirname(__file__), '../../../SunPhy/MR_OFDM/Interleaver.hs')

MCS = list(range(7))
#MCS = [6]
OFDM_Option = list(range(1, 5, 1))
#OFDM_Option = [4]
phyOFDMInterleaver = list(range(2))
#phyOFDMInterleaver = [0]

test_options = list(product(MCS, OFDM_Option, phyOFDMInterleaver))


@pytest.mark.parametrize("_MCS, _OFDM_Option, _phyOFDMInterleaving", test_options)
def test_I(_MCS, _OFDM_Option, _phyOFDMInterleaving):
    I = Function(filepath, 'i')


    try:
        mod = Mr_ofdm_modulator(
            MCS = _MCS,
            OFDM_Option = _OFDM_Option,
            phyOFDMInterleaving = _phyOFDMInterleaving,
            scrambler = 0,
        )
    except UnsupportedError as e:
        return

    mod.message_to_IQ(b'test', binary=False)

    N_bpsc = N_BPSC[_MCS]
    SF = FREQUENCY_SPREADING[_MCS]

    # Interleaver for Payload only
    if _phyOFDMInterleaving == 0:
        # interleaving depth of 1
        N_cbps = int(np.round(mod._N_FFT * N_bpsc / SF * (3/4)))
    else:
        # interleaving depth of SF
        # See table
        N_cbps = int(np.round(mod._N_FFT * N_bpsc * (3/4)))
    N_row = 12 // SF

    k = np.arange(N_cbps, dtype=int)
    i = ((N_cbps / N_row) * (np.mod(k, N_row)) +
        np.floor(k / N_row)).astype(int)

    s = int(np.max([1, N_bpsc/2])) # s would be an integer anyway
    
    # Then check if the I and J are correct
    for ki, ii in zip(k, i):
        it = int(I.test([N_cbps, N_row, ki, s]))
        assert ii == it

@pytest.mark.parametrize("_MCS, _OFDM_Option, _phyOFDMInterleaving", test_options)
def test_J(_MCS, _OFDM_Option, _phyOFDMInterleaving):
    J = Function(filepath, 'j')

    try:
        mod = Mr_ofdm_modulator(
            MCS = _MCS,
            OFDM_Option = _OFDM_Option,
            phyOFDMInterleaving = _phyOFDMInterleaving,
            scrambler = 0,
        )
    except UnsupportedError as e:
        return
    
    N_FFT = FFT_SIZE[_OFDM_Option]

    N_bpsc = N_BPSC[_MCS]
    SF = FREQUENCY_SPREADING[_MCS]

    # Interleaver for Payload only
    if _phyOFDMInterleaving == 0:
        # interleaving depth of 1
        N_cbps = int(np.round(N_FFT * N_bpsc / SF * (3/4)))
    else:
        # interleaving depth of SF
        # See table
        N_cbps = int(np.round(N_FFT * N_bpsc * (3/4)))

    N_row = 12 // SF

    k = np.arange(N_cbps, dtype=int)
    s = int(np.max([1, N_bpsc/2])) # s would be an integer anyway
    k = np.arange(N_cbps, dtype=int)
    j = (s * np.floor(k / s) + np.mod(k + N_cbps -
        np.floor(N_row * k / N_cbps), s)).astype(int)

    for ki, ji in zip(k, j):
        jt = int(J.test([N_cbps, N_row, ki, s]))
        if ji != jt:
            print(N_row, ki, N_cbps)
        assert ji == jt


@pytest.mark.parametrize("_MCS, _OFDM_Option, _phyOFDMInterleaving", test_options)
def test_Interleaver(_MCS, _OFDM_Option, _phyOFDMInterleaving):
    tb = Testbench(filepath, 'interleaver')
    N_FFT = FFT_SIZE[_OFDM_Option]
    SF = FREQUENCY_SPREADING[_MCS]
    N_bpsc = N_BPSC[_MCS]
    if _phyOFDMInterleaving == 0:
        # interleaving depth of 1
        N_cbps = int(np.round(N_FFT * N_bpsc / SF * (3/4)))
    else:
        # interleaving depth of SF
        # See table
        N_cbps = int(np.round(N_FFT * N_bpsc * (3/4)))

    np.random.seed(0)
    message = np.random.randint(0,1+1,N_cbps)

    print(f"{N_FFT = }")
    print(f"{N_cbps = }")
    print(f"{N_bpsc = }")
    print(f"{SF = }")
    

    try:
        mod = Mr_ofdm_modulator(
            MCS = _MCS,
            OFDM_Option = _OFDM_Option,
            phyOFDMInterleaving = _phyOFDMInterleaving,
            scrambler = 0,
        )
    except UnsupportedError as e:
        return
    
    interleavedMessage = mod._interleaver(message, _MCS)

    print(f"j = {mod._ij}")

    print(f"{message}")
    print("             |")
    print("             V")
    print(f"{interleavedMessage}")

    tb.setInputs([
        Signal("bypass", 0),
        Signal("MCS", _MCS),
        Signal("OFDM_Option", _OFDM_Option),
        Signal("phyOFDMInterleaving", _phyOFDMInterleaving),
        Signal("valid_i", ['0','0'] + ['1']*N_cbps + (N_cbps + 2) * ['0']),
        Signal("data_i", [0,0] + message.tolist() + (N_cbps + 2) * [0]),
        Signal("last_i", ['0','0'] + (N_cbps - 1) * ['0'] + ['1'] + (N_cbps + 2) * ['0']),
        Signal("ready_i", 1)
    ])

    tb.setExpectedOutputs([
        Signal("ready_o", ['1'] * (N_cbps + 2) + (N_cbps) * ['0'] + ['1', '1']),
        Signal("data_o", [0]*(N_cbps+2) + interleavedMessage.tolist() +[0,0]),
        Signal("valid_o", ['0'] * (N_cbps + 2) + (N_cbps) * ['1'] + ['0', '0']),
        Signal("last_o", ['0'] * (2*N_cbps+1) + ['1'] + ['0','0']),
        None
    ])

    tb.setActualOutputsNames([
        "ready_o (actual)",
        "data_o (actual)",
        "valid_o (actual)",
        "last_o (actual)"
    ])


    tb.run()

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)

def test_Interleaver_single():
    _MCS = 3
    _OFDM_Option = 4
    _phyOFDMInterleaving = 0

    tb = Testbench(filepath, 'interleaver')
    cg = Chronogram(join(dirname(__file__), 'test_Interleaver.json'))

    N_FFT = FFT_SIZE[_OFDM_Option]
    SF = FREQUENCY_SPREADING[_MCS]
    N_bpsc = N_BPSC[_MCS]

    if _phyOFDMInterleaving == 0:
        # interleaving depth of 1
        N_cbps = int(np.round(N_FFT * N_bpsc / SF * (3/4)))
    else:
        # interleaving depth of SF
        # See table
        N_cbps = int(np.round(N_FFT * N_bpsc * (3/4)))

    np.random.seed(0)
    message = np.random.randint(0,1+1,N_cbps)

    print(f"{N_FFT = }")
    print(f"{N_cbps = }")
    print(f"{N_bpsc = }")
    print(f"{SF = }")
    

    try:
        mod = Mr_ofdm_modulator(
            MCS = _MCS,
            OFDM_Option = _OFDM_Option,
            phyOFDMInterleaving = _phyOFDMInterleaving,
            scrambler = 0,
        )
    except UnsupportedError as e:
        return
    
    interleavedMessage = mod._interleaver(message, _MCS)

    print(f"{message}")
    print("             |")
    print("             V")
    print(f"{interleavedMessage}")

    tb.setInputs([
        cg["bypass"],
        cg["MCS"],
        cg["OFDM_Option"],
        cg["phyOFDMInterleaving"],
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
        "data_o (actual)",
        "valid_o (actual)",
        "last_o (actual)"
    ])

    cg.setTemplates({
        "data_o (actual)" : "data_o"
    })


    tb.run()

    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_Interleaver.svg'))

    for s in tb:
        if s.isChecked():
            s.print(True)
            assert s.isValid(), s.message()
        else:
            s.print(True)

if __name__ == '__main__':
    test_Interleaver_single()