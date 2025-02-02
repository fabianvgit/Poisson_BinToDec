
-- Library Clause(s) (optional)
library IEEE;
-- Use Clause(s) (optional)
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity test_clk is
		Port ( 
		     clk_20ns      : in STD_LOGIC;
           reset         : in STD_LOGIC;  --The risign edge corresponds to the end of count (EOC) of accumulators Additionally for test purpose 
           clk_out_01us  : out STD_LOGIC;  --clk signal generator for cd4040
			  clk_out_02us 	 : out STD_LOGIC
  			  );
end test_clk;
	

architecture bhl of test_clk is
	
	signal counter_01us  : unsigned(12 downto 0) := (others => '0');--25
	signal clk_in_01us   : STD_LOGIC ;
	--constant period_01us : unsigned(25 downto 0) := to_unsigned(12500000, 26); --15x20ns semiper => 300x2 ns periodo 1.67MHz 
	constant period_01us : unsigned(12 downto 0) := to_unsigned(8000, 13);  --4000 para T=150us

   signal counter_02us  : unsigned(9 downto 0) := (others => '0');--25
	signal clk_in_02us   : STD_LOGIC ;
	--constant period_01us : unsigned(25 downto 0) := to_unsigned(12500000, 26); --15x20ns semiper => 300x2 ns periodo 1.67MHz 
	constant period_02us : unsigned(9 downto 0) := to_unsigned(1000, 10);   --430 para 17,2useg	

begin
    -- Clock generation for 1 second  connect to poisson gen
    --process(clk_20ns, reset)
    process(clk_20ns,reset)
	 begin
        if reset ='0' then
		  if rising_edge(clk_20ns) then
			  
            if counter_01us = period_01us - 1 then
               counter_01us <= (others => '0');
               clk_in_01us <= not clk_in_01us;
            else
               counter_01us <= counter_01us + 1;
            end if;
			
		  end if;	
		end if;  
    end process;
	 
	 process(clk_20ns,reset)
	 begin
        if reset ='0' then
		  if rising_edge(clk_20ns) then
			  
            if counter_02us = period_02us - 1 then
               counter_02us <= (others => '0');
               clk_in_02us <= not clk_in_02us;
            else
               counter_02us <= counter_02us + 1;
            end if;
			
		  end if;	
		end if;  
    end process;
	
     clk_out_01us <= clk_in_01us;
	  clk_out_02us <= clk_in_02us;
	  
end bhl;


