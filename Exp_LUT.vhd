library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity Exp_LUT is
    Port ( index : in  unsigned(6 downto 0);  -- Input index (0 to 100)
           exp_out : out integer range 0 to 32768--unsigned(15 downto 0)  -- Output value in fixed-point format
         );
end Exp_LUT;

architecture Behavioral of Exp_LUT is   --calculados con 2^15  requiere un LSFR de 15 bits
    -- LUT to store pre-calculated values of exp(-lambda)
    type lut_array is array (0 to 100) of integer range 0 to 32768; --unsigned(15 downto 0);
    constant exp_lut : lut_array := (
	 32768,	29650,	26828,	24275,	21965,	19875,	17983,	16272,	14724,	
	 13322,	12055,	10908,	9870,	8930,	8080,	7312,	6616,	5986,	
	 5417,	4901,	4435,	4013,	3631,	3285,	2973,	2690,	2434,	2202,	
	 1993,	1803,	1631,	1476,	1336,	1209,	1094,	990,	895,	810,	
	 733,	663,	600,	543,	491,	445,	402,	364,	329,	298,	
	 270,	244,	221,	200,	181,	164,	148,	134,	121,	110,	
	 99,	90,	81,	73,	67,	60,	54,	49,	45,	40,	36	,
	 33,	30,	27,	24,	22,	20,	18,	16,	15,	13,	12,
	 11,	10,	9,	8,	7,	7,	6,	5,	5,	4,
	 4,	4,	3,	3,	3,	2,	2,	2,	2,	2,	1
	 

    );
begin
    -- Lookup the value in the LUT based on the index
    process(index)
    begin
        exp_out <= exp_lut(10*to_integer(index));
    end process;
end Behavioral;
