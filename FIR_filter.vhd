-----------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.all;
use IEEE.NUMERIC_STD.ALL; --package needed for SIGNED
-----------------------------------------------------------------
ENTITY FIR IS -- FIR = finite impulse response 
    GENERIC (n: INTEGER := 3; --number of coefficients (eg: -1 0 1)
        m: INTEGER := 9; --number of bits per coefficient and x_input data
        p: INTEGER := 64); --number of elements in the line buffer (pixels)
    PORT (clk, rst: IN STD_LOGIC;
        load: STD_LOGIC; --to enter new coefficient values
        run: STD_LOGIC; --to compute the output
        x_input, h_coef_input, v_coef_input: IN SIGNED(m-1 DOWNTO 0);
        y: OUT SIGNED(2*m-1 DOWNTO 0);
        overflow: OUT STD_LOGIC);
END FIR;
-----------------------------------------------------------------
ARCHITECTURE Behavioral OF FIR IS
    TYPE internal_array IS ARRAY (1 TO n) OF SIGNED(m-1 DOWNTO 0);
    SIGNAL h_c, v_c: internal_array; --stored coefficients for horizontal and vertical filters 
    SIGNAL h_x, v_x: internal_array; --stored input values 

    SIGNAL h_output: SIGNED(2*m-1 DOWNTO 0); -- store the horizontal filter's output 

    TYPE line_buffer IS ARRAY (1 TO p) of SIGNED(m-1 DOWNTO 0); -- an array large enough to hold all of the pixels (64 here)
    TYPE vertical_buffers IS ARRAY (1 to n) of line_buffer; -- multiple arrays connected to each other 
    SIGNAL v_buffer: vertical_buffers; -- will hold the line information 
    --SIGNAL buffers_loaded: STD_LOGIC; -- determines if the vertical buffers are loaded and ready to begin vertical output 
    --SIGNAL v_overflow: STD_LOGIC;

    SIGNAL inputCounter: INTEGER RANGE 0 TO 4096; -- how many pixels have been input 
BEGIN  
    horizontal: PROCESS (clk, rst) -- horizontal filter
        VARIABLE prod, acc: SIGNED(2*m-1 DOWNTO 0) := (OTHERS=>'0');
        VARIABLE sign_prod, sign_acc: STD_LOGIC;
    BEGIN
        IF(rst='0') THEN
            h_output <= (others => '0'); -- reset the output
            overflow <= '0'; --fixthis neglected overflow for simplification
            FOR i IN 1 TO n LOOP
                h_x(i) <= (others => '0'); -- reset the old input values  
                h_c(i) <= (others => '0'); -- reset the coefficients used for convolution
            END LOOP; 
        ELSIF rising_edge(clk) THEN -- load registers 
            IF(load='1') THEN
                h_c <= (h_coef_input & h_c(1 TO (n-1))); -- load in the n desired coefficients
            ELSIF(run='1') THEN
                h_x <= x_input & h_x(1 to (n-1)); -- once coefficients loaded, load in pixel inputs
            END IF;
        END IF; 
        -- Calculate the output 
        acc := (others => '0');
        FOR i IN 1 TO n LOOP -- multiply each input by each coefficient, and add results up 
            prod := h_x(i) * h_c(i); 
            sign_prod := prod(2*m-1); 
            acc := prod + acc;
        END LOOP;
        IF rising_edge(clk) THEN -- on the rising edge, set the output
            h_output <= acc; -- an 18-bit output 
        END IF; 
    END PROCESS horizontal;

    vertical: PROCESS (clk, rst)-- vertical
        VARIABLE prod, acc: SIGNED(2*m-1 DOWNTO 0) := (OTHERS=>'0');
        VARIABLE sign_prod, sign_acc: STD_LOGIC;
        VARIABLE input_count: INTEGER RANGE 0 TO 4096 := 0;
    BEGIN
        IF(rst='0') THEN
            y <= (others => '0'); -- reset the output
            FOR i IN 1 TO n LOOP
                v_x(i) <= (others => '0'); -- reset the old input values  
                v_c(i) <= (others => '0'); -- reset the coefficients used for convolution
                FOR j IN 1 TO p LOOP
                    v_buffer(i)(j) <= (others => '0'); 
                END LOOP; 
            END LOOP; 
        ELSIF rising_edge(clk) THEN -- load the registers 
            IF(load='1') THEN
                v_c <= (v_coef_input & v_c(1 TO (n-1))); -- load in the n desired coefficients
            ELSIF(run='1') THEN
                FOR i IN n DOWNTO 1 LOOP
                    IF(i>1) THEN -- shift the last pixel (index = p) of the previous line to the current line
                        v_buffer(i) <= (v_buffer(i-1)(p) & v_buffer(i)(1 TO (p-1))); 
                    ELSE -- shift the horizontal output in 
                        v_buffer(i) <= (h_output(m-1 DOWNTO 0) & v_buffer(i)(1 TO (p-1))); -- ignore the upper m bits 
                    END IF;
                END LOOP; -- buffers are now set
                input_count := input_count + 1; -- additional input has been set 
                IF(input_count > 256) THEN
                    acc := (others => '0'); -- initially nothing accumulated 
                    FOR i IN 1 TO n LOOP
                        prod := h_c(i) * v_buffer(i)(p); -- multiply the coefficient with the last element in one of the buffers 
                        acc := prod + acc;
                    END LOOP;
                END IF; -- added

            END IF; --end run/load
        END IF; --end rst/clk
        IF rising_edge(clk) THEN
            y <= acc; -- set output to the accumulated value 
        END IF; 
    END PROCESS vertical;
END Behavioral;
-----------------------------------------------------------------


