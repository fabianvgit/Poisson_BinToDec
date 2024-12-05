library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity BinToDecimalDiv is
    port (
        binary_in   : in  unsigned(15 downto 0);               -- Número binario de entrada (0 a 255)
        --hundreds  : out std_logic_vector(7 downto 0);       -- ASCII del dígito de centenas
        --tens      : out std_logic_vector(7 downto 0);       -- ASCII del dígito de decenas
        --unit      : out std_logic_vector(7 downto 0);        -- ASCII del dígito de unidades
        short_start       : in  std_logic;                          -- Señal de inicio para la conversión
        i_tx_busy         : in  std_logic;          -- Señal de ocupado para transmisión
        o_ready_internal  : out  std_logic;  		 -- test de  generacion de k completada 
		  -- o_short_start : out  std_logic;          -- test de inicio de la transmision 
		 
		  tx_enable   : out std_logic;                          -- Señal de habilitación de transmisión
        tx_data     : out std_logic_vector(7 downto 0);       -- Salida del dato ASCII a transmitir
		  --clk_slow	  : in  std_logic; 
		  clk         : in  std_logic;                          --reloj 50 MHz 
		  reset       : in  std_logic
       -- tx          : out std_logic                           -- Señal de a salida a RS232
     
       
	 );
end entity;

architecture Behavioral of BinToDecimalDiv is
	 type state_type is (IDLE, CALC_H, CALC_T, CALC_U, SEND_H, SEND_T, SEND_U); -- Estados de la FSM
    signal state : state_type := IDLE;               -- Estado actual
    signal digit : integer;                          -- Valor de entrada en entero

    signal h_ascii, t_ascii, u_ascii : std_logic_vector(7 downto 0);  -- Almacenamiento de valores ASCII
    signal ready_internal : std_logic := '0';                          -- Señal interna de listo
   -- type state_type is (IDLE, SEND_H, SEND_T, SEND_U);      -- Estados de la máquina de estados
   -- signal state : state_type := IDLE;                      -- Estado actual
   -- signal short_start :std_logic :='0';                  --señal de start acortada
	-- signal start_delayed :std_logic :='0';                 --auxiliar señal acortada 
    --signal tx_data: std_logic_vector(7 downto 0); -- Almacenamiento de valores ASCII
   -- signal ready_internal,tx_busy,ready : std_logic;  --                               -- Señal interna para la preparación de transmisión
    --signal tx_enable :STD_LOGIC :='0';
	 signal tx_busy :std_logic :='0';
	 signal start_active : std_logic := '0';
	 signal pulse_active : std_logic := '0';
	 signal tx_active : std_logic := '0';
	 signal counter : integer := 0;  


		 
	 
	 
begin
   




    -- Proceso de conversión de binario a ASCII usando una FSM
    process(clk,i_tx_busy)
        variable h, t, u : integer;
    begin
        if rising_edge(clk) then
		     if i_tx_busy = '0' and tx_active = '1'then
		       tx_enable <= '1';
				 tx_active <= '0';
		     elsif i_tx_busy = '1' then
			      tx_enable <= '0';
			  end if;
		 --if tx_enable = '1' then
		  --if tx_active = '1' then          --tx active se genera en la tx de H,T y U
                --tx_enable <= '1';       --generacion del tx enable 
					 --tx_active <= '0';
			 
			--tx_enable <= '0';
         --end if; --DEBERIA SER UN ELSE!!!!!
			
            case state is
                when IDLE =>
					 ready_internal <= '0';
					 --tx_enable <= '0';
                    if short_start  = '1' then
                        digit <= to_integer(binary_in);  -- Convertimos a entero		
                        state <= CALC_H;
                    end if;

                when CALC_H =>
                    h := digit / 100;                  -- Centenas
                    digit <= digit mod 100;            -- Resto para siguiente cálculo
                    h_ascii <= std_logic_vector(to_unsigned(h + 48, 8)); -- ASCII de centenas
                    state <= CALC_T;

                when CALC_T =>
                    t := digit / 10;                   -- Decenas
                    digit <= digit mod 10;             -- Resto para unidades
                    t_ascii <= std_logic_vector(to_unsigned(t + 48, 8)); -- ASCII de decenas
                    state <= CALC_U;

                when CALC_U =>
                    u := digit;                        -- Unidades
                    u_ascii <= std_logic_vector(to_unsigned(u + 48, 8)); -- ASCII de unidades
                    ready_internal <= '1';      --  test fin de calculo inicio de tx
                    state <= SEND_H;

                -- Estados de transmisión secuencial
                when SEND_H =>
                    if i_tx_busy = '0' then
                        tx_data <= h_ascii;            -- Transmitir centenas
                        tx_active <= '1';         
                        ready_internal <= '1';  
								state <= SEND_T;
								
                    end if;

                when SEND_T =>
                    if i_tx_busy = '0' then
                       tx_data   <= t_ascii;            -- Transmitir decenas
                       tx_active <= '1';
							  ready_internal <= '0';  
                       state     <= SEND_U;
								
                    end if;

                when SEND_U =>
                    if i_tx_busy = '0' then
                        tx_data <= u_ascii;            -- Transmitir unidades
                        tx_active <= '1';
                        ready_internal <= '1';        -- test fin de transmision 
                        state          <= IDLE;                 -- Vuelve al estado IDLE
								
                    end if;

                when others =>
                    state <= IDLE;
            end case;
        end if;
		  --end if;
    end process;
    --tx_enable <= tx_active; 
    o_ready_internal <= ready_internal;  -- Señal de inicio y fin de transmision 
	 --o_short_start <= short_start;
	 --end architecture;

	 

	 
	-- o_ready_internal  <=  ready_internal;--test conversion completa
	 

end architecture;
