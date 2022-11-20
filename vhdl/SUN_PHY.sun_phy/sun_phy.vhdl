-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.sun_phy_types.all;

entity sun_phy is
  port(-- clock
       CLOCK_50 : in sun_phy_types.clk_System;
       -- reset
       reset    : in sun_phy_types.rst_System;
       -- enable
       enable   : in sun_phy_types.en_System;
       input    : in std_logic_vector(3 downto 0);
       A        : out std_logic_vector(3 downto 0);
       B        : out std_logic_vector(6 downto 0));
end;

architecture structural of sun_phy is
  signal result : sun_phy_types.Tup2;

begin
  result <= ( Tup2_sel0_std_logic_vector_0 => input
            , Tup2_sel1_std_logic_vector_1 => std_logic_vector'("0000000") );

  A <= result.Tup2_sel0_std_logic_vector_0;

  B <= result.Tup2_sel1_std_logic_vector_1;


end;

