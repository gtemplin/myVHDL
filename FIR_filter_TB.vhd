library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use std.textio.all;
use IEEE.std_logic_textio.all; 

entity FIR_filter_TB is
end FIR_filter_TB;

architecture Behavioral of FIR_filter_TB is

    FILE fwrite: TEXT OPEN WRITE_MODE IS "d:\Users\gmtempli\OneDrive - Indiana University\Documents\sobel_out_iupui.txt";
    FILE fread: TEXT OPEN READ_MODE IS "d:\Users\gmtempli\OneDrive - Indiana University\Documents\data_in.txt";
    
    -- Component Declaration for the Unit Under Test (UUT)
    COMPONENT FIR_filter
        GENERIC (
            n : INTEGER := 3;
            m : INTEGER := 9;
            p : INTEGER := 64
        );
        PORT (
            clk : IN  std_logic;
            rst : IN  std_logic;
            load : IN  std_logic;
            run : IN  std_logic;
            x_input : IN  signed(m-1 DOWNTO 0);
            h_coef_input : IN  signed(m-1 DOWNTO 0);
            v_coef_input : IN  signed(m-1 DOWNTO 0);
            y : OUT  signed(2*m-1 DOWNTO 0);
            overflow : OUT  std_logic
        );
    END COMPONENT;
   
   --Inputs
    signal clk : std_logic := '0';
    signal Reset_n : std_logic := '0';
    signal int_load : std_logic := '0';
    signal int_run : std_logic := '0';
    signal int_x_input : signed(8 downto 0) := (others => '0');
    signal int_h_coef_input : signed(8 downto 0) := (others => '0');
    signal int_v_coef_input : signed(8 downto 0) := (others => '0');

    --Outputs
    signal y : signed(17 downto 0);
    signal overflow : std_logic;
    
    --Signals to coordinate simulation 
    signal EndOfSim : boolean := false;
    --signal input : signed(8 downto 0) := (others => '0');  
    
    --Image data (64x64 with 8-bit black/white pixels)
    --TYPE input_pix_array IS ARRAY (1 TO 4096) OF UNSIGNED(7 DOWNTO 0); 

   
begin
    -- Instantiate the Unit Under Test (UUT)
    uut: COMPONENT FIR_filter
        PORT MAP (
            clk => clk,
            rst => Reset_n,
            load => int_load,
            run => int_run,
            x_input => int_x_input,
            h_coef_input => int_h_coef_input,
            v_coef_input => int_v_coef_input,
            y => y,
            overflow => overflow
        );
main: process
    begin
        Reset_n<='0';
        int_run<='0';
        wait for 2000 ns;
        Reset_n <= '1';
        wait for 200 ns;
        int_run<='1';
        --wait for 41050 ns; --4096+8 clocks
        wait for 43610 ns; 
        EndOfSim <= true;
end process main;

-- process for clock 10ns period --
genclock: process
begin
    loop
        exit when EndOfSim;
        clk <= '0';
        wait for 5 ns;
        exit when EndOfSim;
        clk <= '1';
        wait for 5 ns;
    end loop;
    -- wait;
end process genclock;

sobel_xy_coefs: process(clk)
    VARIABLE i: INTEGER RANGE 0 to 3;
    VARIABLE coef_var : INTEGER RANGE 0 to 500;
begin
    if rising_edge(clk) then
        if Reset_n ='0' then
            int_h_coef_input <= (others => '0');
            int_v_coef_input <= (others => '0');
            i:= 0;
            int_load <='0';
        else
            int_load <= '1';
            if i=0 then
                int_h_coef_input <= "111111111";
                int_v_coef_input <= "000000001";
            elsif i=1 then
                int_h_coef_input <= "000000000";
                int_v_coef_input <= "000000010";
            elsif i=2 then
                int_h_coef_input <= "000000001";
                int_v_coef_input <= "000000001";
            end if;
            if (i < 3) then 
                i:= i + 1; 
            else 
                int_load <= '0';
            end if;
        end if;
    end if;
end process sobel_xy_coefs;

-- Load in the pixels 
take_pixel_inputs: process(clk)
    variable pixel_count: INTEGER RANGE 0 to 4096 := 0; 
    variable lread: LINE; -- read from some file 
    variable wasRead: STD_LOGIC_VECTOR(7 DOWNTO 0):= (others => '0'); 
begin
    if rising_edge(clk) AND int_run='1' THEN
        if pixel_count <= 4096 THEN
            pixel_count := pixel_count + 1;
        end if;
        if(int_run='1') AND pixel_count < 4097 then -- Read input from file (4096 pixels)
            IF NOT ENDFILE(fread) THEN
                READLINE(fread, lread);
                READ(lread, wasRead);
                int_x_input <= SIGNED('0' & wasRead);
            END IF;             
        end if;
    end if;
end process take_pixel_inputs;

-- Write pixels to the output 
writeOut: process(clk)
    variable lwrite: LINE; -- write to some file
    variable toWrite: SIGNED(7 DOWNTO 0) := (others => '0');
    variable counter: INTEGER RANGE 0 to 4500 := 0;
    variable linesWritten: INTEGER RANGE 0 to 4097 := 0;
BEGIN
    if rising_edge(clk) then 
        if int_run='1' THEN 
            counter := counter + 1;
            -- Write output to file 
            toWrite := y(7 DOWNTO 0);
            WRITE(lwrite, STD_LOGIC_VECTOR(toWRITE));
            WRITELINE(fwrite,lwrite);
            linesWritten := linesWritten + 1; 
        end if; 
    end if; -- end write edge-triggered process 
end process writeOut;
end Behavioral;
