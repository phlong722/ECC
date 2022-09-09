----------------------------------------------------------------------------
-- Scalar Multiplicatione 163 bit (scalar_mult.vhd)
-- Computes the polynomial multiplication c = a*b mod f in GF(2**m)
-- Implements a sequential cincuit
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity montgomery_ladder_w_coordinate is
  port (
    clk    : in std_logic;
    rst    : in std_logic;
    enable : in std_logic;
    d      : in std_logic_vector(162 downto 0);
    w1, z1 : in std_logic_vector(162 downto 0);
    w2, z2 : in std_logic_vector(162 downto 0);
    inv_w0 : in std_logic_vector(162 downto 0);

    done : out std_logic;

    wA, zA : out std_logic_vector(162 downto 0);
    wD, zD : out std_logic_vector(162 downto 0)
    );
end montgomery_ladder_w_coordinate;

architecture Behavioral of montgomery_ladder_w_coordinate is
  -- Enumurated type declaration and state signal declaration
  type st is (idle, st1, st2, st3, st4, st5, st6, st_done);

  signal current_state, next_state : st;
  -- Control signal 
  signal iter_enable               : std_logic;
  signal regA, regB, regC, regD    : std_logic_vector (162 downto 0);

  -- -- Temporary Register for the Component
  signal inMultA, inMultB, inSqr     : std_logic_vector(162 downto 0);
  signal outMult, outSqr             : std_logic_vector(162 downto 0);
  signal doneMult, doneSqr, done_tmp : std_logic;

  component montgomery_multiplier is
    port (
      a, b            : in  std_logic_vector (162 downto 0);
      clk, rst, start : in  std_logic;
      z               : out std_logic_vector (162 downto 0);
      done            : out std_logic
      );
  end component;

  component montgomery_squarer is
    port (
      a               : in  std_logic_vector (162 downto 0);
      clk, rst, start : in  std_logic;
      z               : out std_logic_vector (162 downto 0);
      done            : out std_logic
      );
  end component;
begin
  iter_enable <= not(doneMult) when (current_state /= idle and rst = '1' and done_tmp /= '1') else '0';
  -----------------------------------------
  -- MAPPING COMPONENT IN USED
  -----------------------------------------
  U1 : montgomery_squarer
    port map (
      clk   => clk,
      rst   => rst,
      a     => inSqr,
      start => iter_enable,
      done  => doneSqr,
      z     => outSqr
      );

  U2 : montgomery_multiplier
    port map (
      clk   => clk,
      rst   => rst,
      a     => inMultA,
      b     => inMultB,
      start => iter_enable,
      done  => doneMult,
      z     => outMult
      );
  -----------------------------------------
  -- PROCESSES OF DESIGN
  -----------------------------------------               
  -- state register 
  next_proc : process(clk, rst)
  begin
    if (rst = '0') then
      current_state <= idle;
    elsif (rising_edge (clk)) then
      current_state <= next_state;
    end if;
  end process next_proc;

  -- control 
  assign_proc : process (clk, rst)
  begin
    if rst = '0' then
      done          <= '0';
      wA            <= (others => '0');
      zA            <= (others => '0');
      wD            <= (others => '0');
      zD            <= (others => '0');
      regA          <= (others => '0');
      regB          <= (others => '0');
      regC          <= (others => '0');
      regD          <= (others => '0');
    elsif rising_edge(clk) then
      if (doneMult = '1') then
        if (current_state = st1) then
          regA <= outMult;
          regB <= outSqr;
        end if;

        if (current_state = st2) then
          regC <= outMult;
          regD <= outSqr;
        end if;

        if (current_state = st3) then
          regB <= outMult;
          regA <= outSqr;
        end if;

        if (current_state = st4) then
          regD <= outSqr;
          regC <= outMult;
        end if;

        if (current_state = st5) then
          regA <= regA xor regC;
          regB <= regB xor regC;
          regC <= outMult;
        end if;

        if (current_state = st6) then
          regD <= outSqr xor outMult;
          regC <= outSqr;
        end if;
      end if;

      if (done_tmp = '1') then
        wD   <= regC;
        zD   <= regD;
        wA   <= regA;
        zA   <= regB;
        done <= '1';
      else
        done <= '0';
      end if;
    end if;
  end process assign_proc;

  -- next state logic
  fsm_proc : process(current_state, enable, doneMult, doneSqr)
  begin
    case current_state is
      when idle =>
        if enable = '1' then
          next_state <= st1;
        else
          next_state <= idle;
        end if;
      when st1 =>
        if doneSqr = '1' then
          next_state <= st2;
        else
          next_state <= st1;
        end if;
      when st2 =>
        if doneSqr = '1' then
          next_state <= st3;
        else
          next_state <= st2;
        end if;
      when st3 =>
        if doneSqr = '1' then
          next_state <= st4;
        else
          next_state <= st3;
        end if;
      when st4 =>
        if doneMult = '1' then
          next_state <= st5;
        else
          next_state <= st4;
        end if;
      when st5 =>
        if doneSqr = '1' then
          next_state <= st6;
        else
          next_state <= st5;
        end if;
      when st6 =>
        if doneSqr = '1' then
          next_state <= st_done;
        else
          next_state <= st6;
        end if;
      when st_done =>
        next_state <= idle;
      when others =>
        next_state <= idle;
    end case;
  end process fsm_proc;

  -- output logic 
  state_unit : process(current_state)
  begin
    case current_state is
      when idle =>
        inMultA <= (others => '0');
        inMultB <= (others => '0');
        inSqr   <= (others => '0');
        done_tmp <= '0';

      when st1 =>
        inMultA  <= w2;
        inMultB  <= z1;
        inSqr    <= z2;
        done_tmp <= '0';

      when st2 =>
        inMultA  <= w1;
        inMultB  <= z2;
        inSqr    <= z1;
        done_tmp <= '0';

      when st3 =>
        inMultA  <= regB;
        inMultB  <= regD;
        inSqr    <= regA xor regC;
        done_tmp <= '0';

      when st4 =>
        inMultA  <= inv_w0;
        inMultB  <= regA;
        inSqr    <= regD;
        done_tmp <= '0';

      when st5 =>
        inMultA  <= w1;
        inMultB  <= z1 xor w1;
        inSqr    <= (others => '0');
        done_tmp <= '0';

      when st6 =>
        inMultA  <= d;
        inMultB  <= regD;
        inSqr    <= regC;
        done_tmp <= '0';

      when st_done =>
        inMultA  <= (others => '0');
        inMultB  <= (others => '0');
        inSqr    <= (others => '0');
        done_tmp <= '1';

      when others =>
    end case;
  end process state_unit;

end Behavioral;