# clash_sun_phy

Clash implementation of the WiSun PHY layer modulations (MR-FSK, MR-OFDM, MR-O-QPSK)

The modulations are built in accordance to the 802.15.4g norm.


## Structure

Each modulation "block" is implemented in its own file and used inside a AXI4-Stream-like pipeline with the following signals

```
Master       Slave
ready_i  <-  ready_o
valid_o  ->  valid_i
data_o   ->  data_i
last_o   ->  last_i
```

This pipeline provides both forward and back pressure

