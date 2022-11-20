-- helper function of Clash.Explicit.Testbench.assert
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

package testBench_slv2string_2F58399B7F4E729C is
  function slv2string (slv : std_logic_vector) return STRING;
end;

package body testBench_slv2string_2F58399B7F4E729C is
  function slv2string (slv : std_logic_vector) return STRING is
     variable result : string (1 to slv'length);
     variable res_l : string (1 to 3);
     variable r : integer;
   begin
     r := 1;
     for i in slv'range loop
        res_l := std_logic'image(slv(i));
        result(r) := res_l(2);
        r := r + 1;
     end loop;
     return result;
  end slv2string;
end;

