library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package data_types5 is
    constant input_size : integer := 4;
    constant output_size : integer := 8;
	 constant cifras 	 			: integer := 8;    --8
	 constant channel 				: integer := 12;--12;--12--input_size+output_size
    constant bit_size : integer := 23; --numero de bits del  vector count 
    type input_signal is array (0 to input_size - 1) of std_logic;
	-- type input_signal2 : std_logic_vector (input_size - 1 downto 0);
    type output_signal is array (0 to output_size - 1) of std_logic;
    type corr_matrix is array (0 to output_size - 1) of input_signal;
    type delays_array is array (0 to input_size - 1) of std_logic_vector(7 downto 0);
 --   type state_prob is array (0 to 19) of unsigned(31 downto 0);
    type counts_array is array(0 to input_size + output_size) of unsigned(23 downto 0);
 --   type bs_input_signal is array(0 to 2) of std_logic;
 --   function measure_state(quantum_state : state_prob; random_number : unsigned(31 downto 0)) return bs_input_signal;
 --   function sum_array(quantum_state : state_prob; start_int : integer; end_int : integer) return unsigned;
    type tx_array is array(0 to input_size + output_size - 1) of std_logic;

	 type tx_array1 is array( 0 to cifras-1) of integer;
	 type tx_array2 is array(0 to channel - 1) of integer;
    type tx_array3 is array(1 to channel) of integer;
	 type tx_array4 is array(1 to cifras*(channel +1)+1) of integer;
	 type tx_array5 is array(1 to channel*(cifras+1)+1) of integer; --de 1 a 109
	 type vector_dec_matrix is array(((cifras+1)*channel) downto 0) of std_logic_vector(7 downto 0); 
	 type pines_array is array(0 to input_size + output_size-1) of std_logic;
	 type pines_shortener_array is array(0 to input_size - 1) of std_logic;
    type vector_integer_array is array (cifras-1 downto 0) of integer;
 
end data_types5;