library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
--use IEEE.STD_LOGIC_ARITH.ALL;
--use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.numeric_std.all;
use IEEE.MATH_REAL.ALL;
use work.data1_types.all;


entity Poisson_BinToDec is
    Port ( clk_in        : in STD_LOGIC;
           reset         : in STD_LOGIC;
           --enable      : in STD_LOGIC;
           lambda        : in unsigned(6 downto 0);  -- Lambda parameter for Poisson distribution (escalado por un factor, p. ej. 1000)
           --poisson_number : out integer range 0 to 255;
           --capture     : in STD_LOGIC; -- Signal to capture generated numbers
           --eoc           : out STD_LOGIC;
			  o_start	    : out STD_LOGIC;
			  o_short_start : out std_logic;
			  o_short_eoc   : out std_logic;
			  o_tx_ena      : out std_logic;--test habilitacion UART  (enable)
			  counter_out   : out  STD_LOGIC_VECTOR (7 downto 0 ); --test contador
			  pulse_out     : out STD_LOGIC;
			  slow_clk1     : out std_logic;
			  slow_clk2     : out std_logic;
			  ready_internal: out std_logic; --control de paso por los estados de tx
			  o_tx_busy     : out std_logic;  --monitor tx_busy 
			  tx_out	  		 : out std_logic
			 -- tst_buffer_empty : OUT STD_logic;
			 -- tst_buffer_full  : OUT STD_logic
			  --captured_numbers : out integer_vector(0 to 15)  -- Buffer to store captured numbers
         );
end Poisson_BinToDec;

architecture Behavioral of Poisson_BinToDec is
    
	 constant nbits : integer:= 15;
	 signal w_clk : std_logic;
	 signal v_clk : std_logic;
	 signal flag : std_logic:= '0';
    signal lfsr_reg : STD_LOGIC_VECTOR(nbits downto 0);
    signal feedback : STD_LOGIC;
    signal uniform_random : integer range 0 to 65535;  --255-- Usamos enteros para representar números aleatorios uniformes escalados
    signal Mfactor:integer:=32768;--factor basado en el número de bits  del LSFR
	 signal k : integer range 0 to 65535;
    signal p : integer:= 1;  -- Escalar el valor de p (1.0 * 1000000)
	 signal counter1 : integer range 0 to 5 :=0;  
    signal poisson_internal : unsigned (15 downto 0); --integer range 0 to 255 :=0;  -- Señal interna para almacenar el valor de poisson_number
    signal capture_buffer : integer_vector(0 to 15);
  --  signal write_index : integer := 0;
    --signal buffer_full : STD_LOGIC := '0';
	 signal exp_lamb  :integer; --unsigned(15 downto 0);--conector 
    signal exp_lambda : integer range 0 to 32768 ;   --:= 1000;  -- Exp(-lambda) aproximado y escalado
	 signal captured_numbers : integer_vector(0 to 15);
	-- signal w_eoc    : STD_LOGIC := '0';
	 signal w_poisson_out : integer;
	 signal w_start	: STD_LOGIC := '0';
	 signal w_short_start : std_logic ;
	-- signal w_short_eoc  : std_logic;
	 signal start_delayed : std_logic;
	-- signal eoc_delayed : std_logic;
	signal w_tx_ena   : std_logic;     --conexion entre BinToDecimalDiv y UART
	signal w_tx_busy  : std_logic;    --conexion entre BinToDecimalDiv y UART
	signal b_ascii_in : std_logic_vector(7 downto 0);  --conexion entre BinToDecimalDiv y UART

	 component  Exp_LUT is
    Port ( 
	        index   : in  unsigned(6 downto 0);  -- Input index (0 to 100)
           exp_out : out integer range 0 to 32768 --unsigned(15 downto 0)  -- Output value in fixed-point format
         );
    end component Exp_LUT;
	 
--	 component CircularBuffer is
--    generic (
--        n : integer := 64 -- Tamaño del buffer
--    );
--    port (
--        clk           : in  std_logic;            -- Señal de reloj
--        reset         : in  std_logic;            -- Señal de reinicio
--        data_in       : in  integer;              -- Datos a escribir en el buffer
--        write_enable  : in  std_logic;            -- Habilitación de escritura
--        read_enable   : in  std_logic;            -- Habilitación de lectura
--        data_out      : out integer;              -- Datos leídos del buffer
--        buffer_full   : out std_logic;            -- Señal que indica que el buffer está lleno
--        buffer_empty  : out std_logic             -- Señal que indica que el buffer está vacío
--    );
--  end component CircularBuffer;
  
--	 component Pulse_Generator is
--    Port (
--        clk               : in  STD_LOGIC;            -- Reloj del sistema
--        reset             : in  STD_LOGIC;            -- Señal de reset
--        enable            : in  STD_LOGIC;            -- Habilitación del generador
--        poisson_in        : in  integer; --            _vector(0 to 15); -- Buffer de números Poisson
--        eoc 				  : out STD_LOGIC;            -- va a read enable
--		  counter_out       : out STD_LOGIC_VECTOR (7 downto 0 ); --test contador
--        pulse_out         : out STD_LOGIC             -- Señal de salida del pulso
--    );
--   end component;
	
	 component  test_clk is
		Port ( 
		     clk_20ns         : in STD_LOGIC;
           reset            : in STD_LOGIC;  --The risign edge corresponds to the end of count (EOC) of accumulators Additionally for test purpose 
           clk_out_01us     : out STD_LOGIC;  --clk signal generator for cd4040
  			  clk_out_02us 	 : out STD_LOGIC
			  );
  end component test_clk;
  
  component  BinToDecimalDiv is
		Port ( 
		binary_in   : in  unsigned(15 downto 0);               -- Número binario de entrada (0 a 255)
        --hundreds  : out std_logic_vector(7 downto 0);       -- ASCII del dígito de centenas
        --tens      : out std_logic_vector(7 downto 0);       -- ASCII del dígito de decenas
        --unit      : out std_logic_vector(7 downto 0);        -- ASCII del dígito de unidades
        short_start       : in  std_logic;                          -- Señal de inicio para la conversión
        i_tx_busy     : in  std_logic;          -- Señal de ocupado para transmisión
        o_ready_internal: out  std_logic;  		 -- test de  generacion de k completada 
		  --o_short_start : out  std_logic;          -- test de inicio de la transmision 
		  
		  tx_enable   : out std_logic;                          -- Señal de habilitación de transmisión
        tx_data     : out std_logic_vector(7 downto 0);       -- Salida del dato ASCII a transmitir
		  --clk_slow	  : in  std_logic; 
		  clk         : in  std_logic;                          --reloj 50 MHz 
		  reset       : in  std_logic
       -- tx          : out std_logic                           -- Señal de a salida a RS232
     
       
	 );
end component BinToDecimalDiv;

component uart_module5 is
    generic (
        clk_freq  : integer := 50000000; --frequency of system clock in Hertz
        baud_rate : integer := 115200; --data link baud rate in bits/second
        os_rate   : integer := 16; --oversampling rate to find center of receive bits (in samples per baud period)
        d_width   : integer := 8 --data bus width
    );
    port (
        clk       : in std_logic; --system clock
        reset_n   : in std_logic; --ascynchronous reset
        tx_ena    : in std_logic; --initiate transmission
        tx_data   : in std_logic_vector(d_width - 1 downto 0); --data to transmit
        --rx      : in std_logic; --receive pin
        --rx_busy : out std_logic; --data reception in progress
        --rx_data : out std_logic_vector(d_width - 1 downto 0); --data received
        tx_busy   : out std_logic; --transmission in progress
        tx        : out std_logic --transmit pin
		  );
		  
end component uart_module5;
	 
begin

	 process (v_clk)  --clk_in? o v_clk !!!!!!!!!!!!
	 begin
	 if rising_edge (v_clk) then   
	 --if counter1 = 0 then
	 start_delayed <= w_start;
	 w_short_start <= w_start and not start_delayed;
	 --else
	 --counter1 <= (counter1 + 1 ) mod 3;
	 
	 end if;
	-- end if;
	 end process;
	 	 

	 
	 
    process(w_clk, reset)
    --variable poisson : integer range 0 to 255;
	 begin
	 
        if reset = '1' then
            lfsr_reg <= "0000000000000001"; -- Inicialización del LFSR
            k <= 0;
            p <= Mfactor; --000000;  -- Reiniciar p al valor escalado de 1.0
            -- write_index <= 0;
            --buffer_full <= '0';
            flag  <=  '0';
            -- Inicializar el buffer a ceros explícitamente
           -- for i in 0 to 15 loop
                --capture_buffer(i) <= 0;  -- Inicialización correcta de enteros
          --  end loop;
        elsif rising_edge(w_clk) then
            --if enable = '1' then
                -- LFSR para generar número pseudoaleatorio
                --feedback <= lfsr_reg(7) xor lfsr_reg(5) xor lfsr_reg(4) xor lfsr_reg(3);
                --lfsr_reg <= lfsr_reg(6 downto 0) & feedback;
                -- Polinomio de retroalimentación: x^16 + x^14 + x^13 + x^11 + 1
                feedback <= lfsr_reg(15) xor lfsr_reg(13) xor lfsr_reg(12) xor lfsr_reg(10);
                lfsr_reg <= lfsr_reg(14 downto 0) & feedback; -- Desplazamiento a la derecha con retroalimentación
                -- Convertir la salida de LFSR a número aleatorio uniforme (0, 1) escalado (0 a 1000000)
                uniform_random <= to_integer(unsigned(lfsr_reg)) ; -- 3906 es el factor de escala para obtener valores entre 0 y 1000000
                -- Generación de Poisson usando el método de Knuth exp_lambda está almacenado en la LUT
                if p > exp_lambda then  -- Comparar p escalado con exp(-lambda) escalado
                    p <= p * uniform_random/Mfactor; -- / 1000000;  -- Escalar uniform_random y multiplicar
                    k <= k + 1;
						  w_start <= '0';
					  --poisson_internal <= k;
                 --elsif flag = '0' and p < exp_lambda then
					  else 
                    poisson_internal <= to_unsigned(k - 1,16);  --"10010001";--145 decimal -- Asignación a la señal interna
						  w_start <='1';
					  --else
						  --poisson_internal <= poisson;
                    k <= 0;
                    p <= Mfactor ; --000000;  -- Reiniciar p a Mfactor escalado
                    --flag <= '0';
					 end if;
            --end if;

            -- Capturar el número Poisson generado cuando capture está en alto
--            if capture = '1' and buffer_full = '0' then
--                capture_buffer(write_index) <= poisson_internal;  -- Usar la señal interna
--                if write_index = 15 then
--                    write_index <= 0;
--                    buffer_full <= '1';--el buffer se llenó con 16 enteros 
--                else
--                    write_index <= write_index + 1;
--                end if;
--            end if;
        end if;
    end process;
	 
	 
    -- Asignar el valor de la señal interna a la salida poisson_number
    --poisson_number <= k;--carga de numeros k de P(X=k)
    
	 --exp_lambda <= to_integer(exp_lamb); 
    -- Asignar el buffer capturado a la salida
	 --*************************************************
    --**** Conexiones al módulo TOP**********************
	 --**************************************************
	 captured_numbers <= capture_buffer;--
	 -- eoc <= w_eoc;
	 o_tx_ena      <= w_tx_ena;
	 o_start       <= w_start;
	 o_short_start <= w_short_start;
	 --o_short_eoc <= w_short_eoc;
	 o_tx_busy <= w_tx_busy;--monitor tx_busy 
	 slow_clk1 <= w_clk;
	 slow_clk2 <= v_clk;
	 --tst_buffer_empty <= buffer_empty;
	 --tst_buffer_full <= buffer_full;
	 --**************************************************
	 
	 
	 connect1 : Exp_LUT 
	 port map(
	        index   => lambda, --to_unsigned(lambda,7),    --carga de la media de la distribucion de Poisson
           exp_out => exp_lambda
          );
	 
--	 connect2 : CircularBuffer
--	 
--    port map(
--        clk           => v_clk, --clk_in, --w_clk,                    -- Señal de reloj del sistema o lento
--        reset         => reset ,                   -- Señal de reinicio
--        data_in       => poisson_internal,         -- Datos a escribir en el buffer
--        write_enable  => w_short_start, --'1',                      -- capture,Habilitación de escritura
--        read_enable   => w_short_eoc, --w_eoc,                    -- Habilitación de lectura
--        data_out      => w_poisson_out,            -- Datos leídos del buffer
--        buffer_full   => tst_buffer_full,          -- Señal que indica que el buffer está lleno
--        buffer_empty  => tst_buffer_empty          -- Señal que indica que el buffer está vacío
--    );
--
--	 
--	 connect3 : Pulse_Generator 
--    port map(
--        clk                 => w_clk,                 -- Reloj del sistema o lento
--        reset               => reset,                 -- Señal de reset
--        enable              => '1',                   -- enable,Habilitación del generador
--        poisson_in          => w_poisson_out,         -- Buffer de números Poisson
--        eoc                 => w_eoc,                 -- señal de fin de cuenta
--		  counter_out         => counter_out,            --test contador 
--        pulse_out           => pulse_out              -- Señal de salida del pulso
--    );
	  
	 connect4: test_clk
	 port map( 
		     clk_20ns      => clk_in,
           reset         => reset,       --The risign edge corresponds to the end of count (EOC) of accumulators Additionally for test purpose 
           clk_out_01us  => w_clk,        --clk signal generator for cd4040
			  clk_out_02us  => v_clk
	 ); 
	 
	 connect5: BinToDecimalDiv
	 port map( 
				short_start        => w_short_start,
				clk                => clk_in,
				binary_in          => poisson_internal, --"00110101"--entrada de números
				i_tx_busy          =>  w_tx_busy,
				o_ready_internal   =>  ready_internal,  --control de paso por los estados de tx 
				reset              =>   reset,
				tx_data            =>   b_ascii_in,
				tx_enable          =>   w_tx_ena
	);
	
	 
	connect6 : uart_module5
	port map (
        clk             =>   clk_in,           -- reloj de 50MHz
        reset_n         =>   not reset,     -- conexion a switch
        tx_ena          =>   w_tx_ena,      -- conexion con BinToDecimalDiv
        tx_data         =>   b_ascii_in,    -- entrada de numeros aleatorios 
        --rx      
        --rx_busy 
        --rx_data 
        tx_busy         =>   w_tx_busy,   --conexion con BinToDecimalDiv
        tx              =>   tx_out       --salida de datos en BinToDec
		  );
	  

end Behavioral;
