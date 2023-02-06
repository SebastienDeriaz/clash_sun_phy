#!/usr/bin/env/python
from os.path import join, dirname



FOLDER = join(dirname(__file__), '../src/SunPhy/MR_OFDM')
STF_FILE = join(FOLDER, 'STF_constants.hs')
LTF_FILE = join(FOLDER, 'LTF_constants.hs')

from sun_phy.mr_ofdm.mr_ofdm_modulator import Mr_ofdm_modulator
import numpy as np


def vec(name, data):
    N = data.size
    v = ' :> '.join([f"(({x.real:.16f}) :+ ({x.imag:.16f}))" for x in data])
    return f"{name} :: Vec {N} (Subcarrier)\n{name} = {v} :> Nil"


def main():
    stf_str = "module SunPhy.MR_OFDM.STF_constants where\n\n"
    stf_str += "import SunPhy.MR_OFDM.Constants\n"
    stf_str += "import Clash.DSP.Complex\n"
    stf_str += "import Clash.Prelude\n\n"
    ltf_str = "module SunPhy.MR_OFDM.LTF_constants where\n\n"
    ltf_str += "import SunPhy.MR_OFDM.Constants\n"
    ltf_str += "import Clash.DSP.Complex\n"
    ltf_str += "import Clash.Prelude\n\n"

    for OFDM_Option in range(1, 4+1):
        
        print(f"OFDM Option {OFDM_Option}")
        mod = Mr_ofdm_modulator(
            MCS = 2,
            OFDM_Option=OFDM_Option
        )
        
        stf_ofdm_i, stf_ofdm_q = mod._STF()
        N_STF = stf_ofdm_i.size
        stf_ofdm_i, stf_ofdm_q = stf_ofdm_i[:N_STF//4], stf_ofdm_q[:N_STF//4]
        stf_str += vec(f"stf_{OFDM_Option}", (stf_ofdm_i + stf_ofdm_q * 1j).squeeze()) + "\n\n"
        print(f"  STF symbol length : {stf_ofdm_i.size}")

        ltf_ofdm_i, ltf_ofdm_q = mod._LTF()
        N_LTF = ltf_ofdm_i.size
        ltf_ofdm_i, ltf_ofdm_q = ltf_ofdm_i[N_LTF//5:3*N_LTF//5], ltf_ofdm_q[N_LTF//5:3*N_LTF//5]
        ltf_str += vec(f"ltf_{OFDM_Option}", (ltf_ofdm_i + ltf_ofdm_q * 1j).squeeze()) + "\n\n"
        print(f"  LTF symbol length : {ltf_ofdm_i.size}")

    with open(STF_FILE, 'w', encoding='utf-8') as f:
        f.write(stf_str)
    with open(LTF_FILE, 'w', encoding='utf-8') as f:
        f.write(ltf_str)





if __name__ == '__main__':
    main()