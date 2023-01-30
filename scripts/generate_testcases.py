from sun_phy import Mr_ofdm_modulator
from sun_phy.tools.errors import UnsupportedError
import numpy as np
from os.path import join
from itertools import product

OFDM_OPTIONS = list(range(1,5))
MCS = list(range(7))
PHYOFDMINTERLEAVING = list(range(2))
SCRAMBLER = list(range(4))

DESTINATION = 'test/sun_phy_testcases'

def message_file_format(mcs, ofdm_option, phyOFDMInterleaving, scrambler, extension):
    return join(DESTINATION, f"message_{mcs}_{ofdm_option}_{phyOFDMInterleaving}_{scrambler}{extension}")

def iq_file_format(mcs, ofdm_option, phyOFDMInterleaving, scrambler, extension):
    return join(DESTINATION, f"iq_{mcs}_{ofdm_option}_{phyOFDMInterleaving}_{scrambler}{extension}")

def main():
    for ofdm_option, mcs, phyOFDMInterleaving, scrambler in product(OFDM_OPTIONS, MCS, PHYOFDMINTERLEAVING, SCRAMBLER):
        print(f"{ofdm_option = }, {mcs = }, {phyOFDMInterleaving = }, {scrambler = }")
        try: 
            mod = Mr_ofdm_modulator(
                MCS = mcs,
                OFDM_Option=ofdm_option,
                phyOFDMInterleaving=phyOFDMInterleaving,
                scrambler=scrambler
            )
        except UnsupportedError as e:
            continue
        message = np.random.randint(0, 2, mod.bits_per_symbol() * 2)
        np.savetxt(
            message_file_format(mcs, ofdm_option, phyOFDMInterleaving, scrambler, '.csv'),
            message,
            fmt='%d',
            header='Bitstream',
            comments=''
            )

        I, Q, _ = mod.message_to_IQ(message, binary=True)
        np.savetxt(
            iq_file_format(mcs, ofdm_option, phyOFDMInterleaving, scrambler, '.csv'),
            np.stack([I, Q], axis=1),
            fmt='%f',
            header='Real,Imaginary',
            comments=''
        )

if __name__ == '__main__':
    main()

