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

TYPE line_buffer IS ARRAY (1 TO p) of SIGNED(m-1 DOWNTO 0); -- an array large enough to hold all of the pixels (64 here)
TYPE vertical_buffers IS ARRAY (1 to n) of line_buffer; -- multiple arrays connected to each other 
SIGNAL v_buffer: vertical_buffers; -- will hold the line information 
SIGNAL v_output: SIGNED(m-1 DOWNTO 0); -- store the vertical filter's output
SIGNAL buffers_loaded: STD_LOGIC; -- determines if the vertical buffers are loaded and ready to begin vertical output 
SIGNAL v_overflow: STD_LOGIC;

BEGIN  
    PROCESS (clk, rst) --vertical filter
        VARIABLE inputCounter: INTEGER RANGE 0 TO 4096; -- how many pixels have been input (once equal to 256, all buffers loaded)
        VARIABLE prod, acc: SIGNED(2*m-1 DOWNTO 0) := (OTHERS=>'0');
        VARIABLE sign_prod, sign_acc: STD_LOGIC;
        BEGIN
            IF(rst='0') THEN
                inputCounter := 0; -- no inputs yet
                buffers_loaded <= '0'; -- buffers are empty 
                v_overflow <= '0'; -- assume no overflow 
                FOR i IN 1 TO n LOOP -- for each buffer 
                    FOR j IN 1 TO p LOOP -- for each of the 64 pixels 
                        FOR k IN m-1 DOWNTO 0 LOOP -- for each of the bits in every pixel 
                            v_buffer(i)(j)(k) <= '0'; 
                        END LOOP; 
                    END LOOP;
                END LOOP;
            ELSIF rising_edge(clk) THEN
                --Shift registers:-----------------------
                IF(load='1') THEN -- load in the coefficients 
                    v_c <= (h_coef_input & v_c(1 TO n-1)); -- load in the coefficients 
                ELSIF(run='1') THEN -- load the buffers 
                    inputCounter := inputCounter + 1; -- there has been another input 
                    FOR i IN n DOWNTO 1 LOOP -- for each buffer
                        IF(i>1) THEN
                            v_buffer(i) <= (v_buffer(i-1)(64) & v_buffer(i)(1 TO (p-1))); -- get the last element of the previous buffer, and use the rest of the old values 
                        ELSE
                            v_buffer(i) <= (x_input & v_buffer(i)(1 TO (p-1))); -- the first buffer gets the input 
                        END IF; 
                    END LOOP; 
                    IF(inputCounter>=256) THEN -- the buffers are now full, load in vertical inputs from the buffer (64th element)
                    buffers_loaded <= '1';
                        FOR i IN 1 TO n LOOP
                            prod := v_buffer(i)(64)*v_c(i); -- last element of buffer holds the vertical pixel to be multiplied 
                            sign_prod := prod(2*m-1);
                            sign_acc := acc(2*m-1);
                            acc := prod + acc;
                            IF (sign_prod=sign_acc AND acc(2*m-1)/=sign_acc) THEN 
                                v_overflow <= '1';
                            ELSE
                                v_overflow <= '0';
                            END IF;
                        END LOOP;
                    ELSE
                        buffers_loaded <= '0'; -- if there aren't at least 256 inputs buffered, don't have enough to process 4 vertical pixels 
                    END IF; -- end calculate inputs 
                END IF; -- end shift registers 
            END IF; -- end process if statements 
            IF rising_edge(clk) THEN   
                v_output <= acc; -- the serial output of the vertical filter 
            END IF; -- end output to horizontal if statement 
    END PROCESS;

    PROCESS (clk, rst)-- horizontal filter
        VARIABLE prod, acc: SIGNED(2*m-1 DOWNTO 0) := (OTHERS=>'0');
        VARIABLE sign_prod, sign_acc: STD_LOGIC;
        BEGIN --Reset:---------------------------------    
            IF (rst='0') THEN
                FOR i IN 1 TO n LOOP
                    FOR j IN m-1 DOWNTO 0 LOOP
                        h_x(i)(j) <= '0';
                    END LOOP;
                END LOOP;
            --Shift registers:-----------------------
            ELSIF rising_edge(clk) THEN
                IF (load='1') THEN   
                    h_c <= (h_coef_input & h_c(1 TO n-1)); -- load in the coefficients 
                ELSIF (run='1' AND buffers_loaded = '1') THEN  -- only start caring about the horizontal filter when the vertical filters buffers are filled
                    h_x <= (v_output & h_x(1 TO n-1));
                END IF;
            END IF;
            --MACs and output (w/ overflow check):---
            acc := (OTHERS=>'0');
            FOR i IN 1 TO n LOOP
                prod := h_x(i)*h_c(i);
                sign_prod := prod(2*m-1);
                sign_acc := acc(2*m-1);
                acc := prod + acc;
                IF (sign_prod=sign_acc AND acc(2*m-1)/=sign_acc) THEN overflow <= '1';
                ELSE
                overflow <= '0';
                END IF;
            END LOOP;
        IF rising_edge(clk) THEN   y <= acc;   END IF;
    END PROCESS;
END Behavioral;
-----------------------------------------------------------------


