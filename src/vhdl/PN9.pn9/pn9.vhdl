-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.PN9_pn9_types.all;

entity pn9 is
  port(-- clock
       d_0_0  : in PN9_pn9_types.clk_System;
       -- reset
       d_1_0  : in PN9_pn9_types.rst_System;
       -- enable
       d_2_0  : in PN9_pn9_types.en_System;
       eta    : in std_logic;
       result : out std_logic);
end;

architecture structural of pn9 is
  -- PN9.hs:24:1-3
  signal \c$reg_app_arg\                : std_logic_vector(8 downto 0);
  -- PN9.hs:24:1-3
  signal \c$reg_app_arg_0\              : std_logic_vector(8 downto 0);
  -- PN9.hs:24:1-3
  signal \c$reg_app_arg_1\              : std_logic_vector(8 downto 0);
  -- PN9.hs:24:1-3
  signal \c$reg_case_alt\               : std_logic_vector(8 downto 0);
  -- PN9.hs:24:1-3
  signal \c$reg_app_arg_2\              : std_logic_vector(8 downto 0);
  signal result_1                       : std_logic_vector(8 downto 0) := std_logic_vector'("111111111");
  signal d                              : PN9_pn9_types.product;
  signal \c$shI\                        : signed(63 downto 0);
  signal \c$shI_0\                      : signed(63 downto 0);
  signal \c$shI_1\                      : signed(63 downto 0);
  signal \c$reg_case_alt_selection_res\ : boolean;
  signal \c$reg_app_arg_selection_res\  : boolean;
  signal \c$bv\                         : std_logic_vector(8 downto 0);

begin
  d <= ( product_sel0_clk_System => d_0_0
       , product_sel1_rst_System => d_1_0
       , product_sel2_en_System => d_2_0 );

  \c$shI\ <= to_signed(1,64);

  creg_app_arg_shiftR : block
    signal sh : natural;
  begin
    sh <=
        -- pragma translate_off
        natural'high when (\c$shI\(64-1 downto 31) /= 0) else
        -- pragma translate_on
        to_integer(\c$shI\);
    \c$reg_app_arg\ <= std_logic_vector(shift_right(unsigned(result_1),sh))
        -- pragma translate_off
        when (to_signed(1,64) >= 0) else (others => 'X')
        -- pragma translate_on
        ;
  end block;

  \c$shI_0\ <= to_signed(3,64);

  creg_app_arg_0_shiftL : block
    signal sh_0 : natural;
  begin
    sh_0 <=
        -- pragma translate_off
        natural'high when (\c$shI_0\(64-1 downto 31) /= 0) else
        -- pragma translate_on
        to_integer(\c$shI_0\);
    \c$reg_app_arg_0\ <= std_logic_vector(shift_left(unsigned(result_1),sh_0))
        -- pragma translate_off
        when (to_signed(3,64) >= 0) else (others => 'X')
        -- pragma translate_on
        ;
  end block;

  \c$shI_1\ <= to_signed(8,64);

  creg_app_arg_1_shiftL : block
    signal sh_1 : natural;
  begin
    sh_1 <=
        -- pragma translate_off
        natural'high when (\c$shI_1\(64-1 downto 31) /= 0) else
        -- pragma translate_on
        to_integer(\c$shI_1\);
    \c$reg_app_arg_1\ <= std_logic_vector(shift_left(unsigned(result_1),sh_1))
        -- pragma translate_off
        when (to_signed(8,64) >= 0) else (others => 'X')
        -- pragma translate_on
        ;
  end block;

  \c$reg_case_alt_selection_res\ <= eta = '1';

  \c$reg_case_alt\ <= \c$reg_app_arg\ or ((\c$reg_app_arg_0\ xor \c$reg_app_arg_1\) and std_logic_vector'("100000000")) when \c$reg_case_alt_selection_res\ else
                      std_logic_vector'(0 to 8 => '-');

  \c$reg_app_arg_selection_res\ <= eta = '0';

  \c$reg_app_arg_2\ <= result_1 when \c$reg_app_arg_selection_res\ else
                       \c$reg_case_alt\;

  -- register begin
  result_1_register : process(d.product_sel0_clk_System,d.product_sel1_rst_System)
  begin
    if d.product_sel1_rst_System =  '1'  then
      result_1 <= std_logic_vector'("111111111");
    elsif rising_edge(d.product_sel0_clk_System) then
      if d.product_sel2_en_System then
        result_1 <= \c$reg_app_arg_2\;
      end if;
    end if;
  end process;
  -- register end

  \c$bv\ <= (result_1);

  result <=  \c$bv\(\c$bv\'high) ;


end;

