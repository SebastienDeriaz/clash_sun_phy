# OFDM testbench
# 
# Sébastien Deriaz
# 01.01.2023
#
from clash_testbench import Chronogram, Testbench, Signal, Function
from os.path import join, dirname

import numpy as np
import pytest
from itertools import product

import matplotlib.pyplot as plt

from sun_phy import Ofdm_modulator, Mr_ofdm_modulator
from sun_phy.mr_ofdm.mr_ofdm_modulator import FREQUENCY_SPREADING
from sun_phy.tools.errors import UnsupportedError

filepath = join(dirname(__file__), './test_OFDM_automated.hs')

#OFDM_Options = list(range(1,5))
OFDM_Options = [4]
#MCS = list(range(7))
MCS = [3]
test_options = list(product(OFDM_Options, MCS))

@pytest.mark.parametrize("OFDM_Option, MCS", test_options)
def test_OFDM_automated(OFDM_Option, MCS):
    phyOFDMInterleaving = 0
    try:
        mrmod = Mr_ofdm_modulator(
            MCS=MCS,
            OFDM_Option=OFDM_Option,
            phyOFDMInterleaving=phyOFDMInterleaving,
            scrambler=0,
            verbose=True
        )
    except UnsupportedError:
        return
    
    print(f"Bits per symbol : {mrmod.bits_per_symbol()}")
    np.random.seed(0)
    message_raw = np.random.randint(0, 2, mrmod.bits_per_symbol(), dtype=int)

    I, Q, _ = mrmod.message_to_IQ(message_raw, binary=True)
    
    pilot_set = mrmod._mod_phy.get_pilot_set_index()
    pn9_seed = mrmod._mod_phy.get_pn9_value()
    print(f"pn9 seed = {np.binary_repr(pn9_seed, 9)}")
    
    message = mrmod._payload_interleaved
    print(f"message ({message.size}) = {message}")

    signal_th = mrmod._PAYLOAD_I + mrmod._PAYLOAD_Q * 1j

    tb = Testbench(filepath, 'testOFDMautomated', verbose=False)

    message_vec = f"{':>'.join([str(int(x)) for x in message])}:>Nil"

    N = int(message.size * 2.2 * FREQUENCY_SPREADING[MCS])  

    ofdm_option = Signal("OFDM_Option", [OFDM_Option])
    mcs = Signal("MCS", [MCS])
    _phyOFDMInterleaving = Signal("phyOFDMInterleaving", [phyOFDMInterleaving])
    inputVec = Signal("inputVec", [message_vec])
    start_i = Signal("start_i", [0,0,0,0] + [1] + (N-5)*[0])
    ready_i = Signal("ready_i", [1])
    pilotSet = Signal("pilot_set", [pilot_set])
    pn9Seed = Signal("pn9_seed", [pn9_seed])
    pn9PilotWrite = Signal("pn9 + pilot write", [0,0] + [1] + (N-3)*[0])

    tb.setInputs([
        ofdm_option,
        mcs,
        _phyOFDMInterleaving,
        inputVec,
        start_i,
        ready_i,
        pilotSet,
        pn9Seed,
        pn9PilotWrite
    ])

    tb.setActualOutputsNames([
        "ready_o (actual)",
        "valid_o (actual)",
        "data_o (actual)",
        "last_o (actual)",
        "state",
        "data read",
        "subcarrierCounter",
        "subcarrierCounterEnd",
        "pilot next"
    ])

    tb.setExpectedOutputs([
        None,
        None,
        None,
        None
    ])

    tb.run()
    cg = Chronogram()
    cg.setSignals(tb.getAllSignals())
    cg.saveSVG(join(dirname(__file__), 'test_OFDM_automated.svg'), ticks = True)

    validSignal = np.array([int(str(s.value())) for s in tb._actualOutputs["valid_o (actual)"].samples])
    rawOutputSignal = np.array([complex(*[float(x) for x in str(s)[1:-1].split(',')]) for s in tb._actualOutputs["data_o (actual)"].samples])
    # Location of data values (when valid = 1)
    outputSignal = np.asarray(rawOutputSignal[validSignal > 0])

    print(outputSignal.shape)
    print(signal_th.shape)

    np.save(join(dirname(__file__), 'signal.npy'), outputSignal)
    np.save(join(dirname(__file__), 'signal_th.npy'), signal_th)

    assert np.var(outputSignal - signal_th) < 0.001


if __name__ == '__main__':
    test_OFDM_automated()