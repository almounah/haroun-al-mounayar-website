library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.PKG.all;

entity CPU_CND is
    generic (
        mutant      : integer := 0
    );
    port (
        rs1         : in w32;
        alu_y       : in w32;
        IR          : in w32;
        slt         : out std_logic;
        jcond       : out std_logic
    );
end entity;

architecture RTL of CPU_CND is
    signal extention_signe : std_logic;
    signal result, rs1_etendu, alu_y_entendu : unsigned(32 downto 0);
    signal z : std_logic;
    signal s : std_logic;

begin
    extention_signe <= ( (not IR(12)) and (not IR(6)) ) or ( IR(6) and (not IR(13)));
    
    rs1_etendu <= ('0' & rs1) when extention_signe = '0' else (rs1(31) & rs1);
    alu_y_entendu <= ('0' & alu_y) when extention_signe = '0' else (alu_y(31) & alu_y);
    result <=  rs1_etendu - alu_y_entendu;

    z <= '1' when alu_y_entendu = rs1_etendu else '0';
    
    s <= '1' when rs1_etendu(32) > alu_y_entendu(32) else 
         result(32) when rs1_etendu(32) = alu_y_entendu(32) else '0'; 
   
    jcond <= ((not IR(14)) and ((IR(12)) xor z)) or ((IR(12) xor s) and (IR(14)));
    slt <= s;
end architecture;
