-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.PN9_testBench_types.all;
library topEntity;
use testBench_slv2string_2F58399B7F4E729C.all;

entity testBench is
  port(result : out boolean);
end;

architecture structural of testBench is
  signal \c$ds_app_arg\               : PN9_testBench_types.index_8;
  signal \c$ds_app_arg_0\             : std_logic;
  signal s                            : PN9_testBench_types.index_8 := to_unsigned(0,3);
  -- PN9.hs:31:5-7
  signal \PN9.testBench_clk\          : PN9_testBench_types.clk_System;
  signal z                            : PN9_testBench_types.index_17;
  signal result_1                     : PN9_testBench_types.index_9;
  signal \c$ds_app_arg_1\             : std_logic;
  signal \c$result_rec\               : boolean;
  signal s_0                          : PN9_testBench_types.index_9 := to_unsigned(0,4);
  signal f1                           : boolean;
  signal \f'\                         : boolean := false;
  -- PN9.hs:29:1-9
  signal \c$PN9.testBench_app_arg\    : std_logic;
  -- PN9.hs:29:1-9
  signal \c$PN9.testBench_app_arg_0\  : PN9_testBench_types.rst_System;
  signal \c$ds_app_arg_selection_res\ : boolean;
  signal \c$vec\                      : PN9_testBench_types.array_of_std_logic(0 to 7);
  signal result_selection_res         : boolean;
  signal \c$vec_0\                    : PN9_testBench_types.array_of_std_logic(0 to 8);
  signal arg                          : PN9_testBench_types.product;
  signal arg_0_0                      : PN9_testBench_types.clk_System;
  signal arg_1_1                      : PN9_testBench_types.rst_System;
  signal arg_2_2                      : PN9_testBench_types.en_System;
  signal arg_0                        : std_logic;
  signal result_2                     : std_logic;

begin
  \c$ds_app_arg_selection_res\ <= s < to_unsigned(7,3);

  \c$ds_app_arg\ <= s + to_unsigned(1,3) when \c$ds_app_arg_selection_res\ else
                    s;

  \c$vec\ <= PN9_testBench_types.array_of_std_logic'( '1'
                                                    , '0'
                                                    , '0'
                                                    , '0'
                                                    , '0'
                                                    , '0'
                                                    , '0'
                                                    , '0' );

  -- index begin
  indexVec : block
    signal vec_index : integer range 0 to 8-1;
  begin
    vec_index <= to_integer((signed(std_logic_vector(resize(s,64)))))
    -- pragma translate_off
                 mod 8
    -- pragma translate_on
                 ;
    \c$ds_app_arg_0\ <= \c$vec\(vec_index);
  end block;
  -- index end

  -- register begin
  s_register : process(\PN9.testBench_clk\,\c$PN9.testBench_app_arg_0\)
  begin
    if \c$PN9.testBench_app_arg_0\ =  '1'  then
      s <= to_unsigned(0,3);
    elsif rising_edge(\PN9.testBench_clk\) then
      s <= \c$ds_app_arg\;
    end if;
  end process;
  -- register end

  -- tbClockGen begin
  -- pragma translate_off
  clkGen : process is
    constant half_periodH : time := 10000000 fs / 2;
    constant half_periodL : time := 10000000 fs - half_periodH;
  begin
    \PN9.testBench_clk\ <= '0';
    wait for 10000 ps;
    while (not \c$result_rec\) loop
      \PN9.testBench_clk\ <= not \PN9.testBench_clk\;
      wait for half_periodH;
      \PN9.testBench_clk\ <= not \PN9.testBench_clk\;
      wait for half_periodL;
    end loop;
    wait;
  end process;
  -- pragma translate_on
  -- tbClockGen end

  z <= resize(s_0,5) + resize(to_unsigned(1,4),5);

  result_selection_res <= z > to_unsigned(8,5);

  result_1 <= to_unsigned(8,4) when result_selection_res else
              resize(z,4);

  \c$vec_0\ <= PN9_testBench_types.array_of_std_logic'( '0'
                                                      , '0'
                                                      , '0'
                                                      , '0'
                                                      , '0'
                                                      , '0'
                                                      , '0'
                                                      , '0'
                                                      , '1' );

  -- index begin
  indexVec_0 : block
    signal vec_index_0 : integer range 0 to 9-1;
  begin
    vec_index_0 <= to_integer((signed(std_logic_vector(resize(s_0,64)))))
    -- pragma translate_off
                 mod 9
    -- pragma translate_on
                 ;
    \c$ds_app_arg_1\ <= \c$vec_0\(vec_index_0);
  end block;
  -- index end

  \c$result_rec\ <= \f'\ when \f'\ else
                    f1;

  -- register begin
  s_0_register : process(\PN9.testBench_clk\,\c$PN9.testBench_app_arg_0\)
  begin
    if \c$PN9.testBench_app_arg_0\ =  '1'  then
      s_0 <= to_unsigned(0,4);
    elsif rising_edge(\PN9.testBench_clk\) then
      s_0 <= result_1;
    end if;
  end process;
  -- register end

  -- assert begin
  r_assert : block
    -- pragma translate_off
    signal actual : std_logic;
    signal expected : std_logic;
    -- pragma translate_on
  begin
    -- pragma translate_off
    actual <= \c$PN9.testBench_app_arg\;
    expected <= \c$ds_app_arg_1\;
    process(\PN9.testBench_clk\) is
    begin
      if (rising_edge(\PN9.testBench_clk\)) then
        assert (toSLV(actual) = toSLV(expected)) report (("outputVerifier") & ", expected: " & testBench_slv2string_2F58399B7F4E729C.slv2string(toSLV(expected)) & ", actual: " & testBench_slv2string_2F58399B7F4E729C.slv2string(toSLV(actual))) severity error;
      end if;
    end process;
    -- pragma translate_on
    f1 <= \f'\;
  end block;
  -- assert end

  -- register begin
  f_register : process(\PN9.testBench_clk\,\c$PN9.testBench_app_arg_0\)
  begin
    if \c$PN9.testBench_app_arg_0\ =  '1'  then
      \f'\ <= false;
    elsif rising_edge(\PN9.testBench_clk\) then
      \f'\ <= (s_0 = to_unsigned(8,4));
    end if;
  end process;
  -- register end

  arg <= ( product_sel0_clk_System => \PN9.testBench_clk\
         , product_sel1_rst_System => \c$PN9.testBench_app_arg_0\
         , product_sel2_en_System => true );

  arg_0 <= \c$ds_app_arg_0\;

  arg_0_0 <= arg.product_sel0_clk_System;

  arg_1_1 <= arg.product_sel1_rst_System;

  arg_2_2 <= arg.product_sel2_en_System;

  topEntity_cPN9testBench_app_arg : entity topEntity.topEntity
    port map
      (arg_0_0, arg_1_1, arg_2_2, arg_0, result_2);

  \c$PN9.testBench_app_arg\ <= result_2;

  -- resetGen begin
  resetGen : block
    constant reset_delay : time := 10000 ps - 1 ps + (integer'(1) * 10000 ps);
  begin
  -- pragma translate_off
  \c$PN9.testBench_app_arg_0\
    <= '1',
       '0' after reset_delay;
  -- pragma translate_on
  end block;
  -- resetGen end

  result <= \c$result_rec\;


end;

