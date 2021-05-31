library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.PKG.all;

entity CPU_CSR is
    generic (
        INTERRUPT_VECTOR : waddr   := w32_zero;
        mutant           : integer := 0
    );
    port (
        clk         : in  std_logic;
        rst         : in  std_logic;

        -- Interface de et vers la PO
        cmd         : in  PO_cs_cmd;
        it          : out std_logic;
        pc          : in  w32;
        rs1         : in  w32;
        imm         : in  W32;
        csr         : out w32;
        mtvec       : out w32;
        mepc        : out w32;

        -- Interface de et vers les IP d'interruption
        irq         : in  std_logic;
        meip        : in  std_logic;
        mtip        : in  std_logic;
        mie         : out w32;
        mip         : out w32;
        mcause      : in  w32
    );
end entity;

architecture RTL of CPU_CSR is
    -- Fonction retournant la valeur à écrire dans un csr en fonction
    -- du « mode » d'écriture, qui dépend de l'instruction
    function CSR_write (CSR        : w32;
                         CSR_reg    : w32;
                         WRITE_mode : CSR_WRITE_mode_type)
        return w32 is
        variable res : w32;
    begin
        case WRITE_mode is
            when WRITE_mode_simple =>
                res := CSR;
            when WRITE_mode_set =>
                res := CSR_reg or CSR;
            when WRITE_mode_clear =>
                res := CSR_reg and (not CSR);
            when others => null;
        end case;
        return res;
    end CSR_write;

signal q_mcause, q_mie, q_mstatus, q_mip, q_mtvec, q_mepc : w32;
signal d_mcause, d_mie, d_mstatus, d_mip, d_mtvec, d_mepc : w32;
signal TO_CSR : w32;
signal q_irq, d_irq : std_logic;
begin
    TO_CSR <= rs1 when cmd.TO_CSR_sel = TO_CSR_from_rs1 else 
              imm when cmd.TO_CSR_sel = TO_CSR_from_imm;
    d_mcause <= mcause;
    d_mip <= X"00000" & meip & "000" & mtip & X"0" & "000";

    d_mie <= TO_CSR;
    d_mstatus <= TO_CSR;
    d_mtvec <= TO_CSR;
    d_mepc <= TO_CSR when cmd.MEPC_sel = MEPC_from_csr else
               pc when cmd.MEPC_sel = MEPC_from_pc;
    d_irq <= irq;
    bascule : process (clk)
    begin
        if clk'event and clk='1' then
            
            q_irq <= d_irq;
            q_mip <= d_mip;
            if rst = '1' then
                q_mstatus <= (others => '0');
                q_mie <= (others => '0');
                q_mtvec <= (others => '0');
                q_mepc <= (others => '0');
                q_mcause <= (others => '0');
            else
                if q_irq = '1' then
                    q_mcause <= d_mcause;
                end if;
                

                if cmd.CSR_we = CSR_mie then
                    q_mie <= CSR_write(d_mie, q_mie, cmd.CSR_WRITE_mode);
                end if;

                if cmd.MSTATUS_mie_set = '1' then
                    q_mstatus <= q_mstatus(31 downto 4) & '1' & q_mstatus(2 downto 0);
                elsif cmd.MSTATUS_mie_reset = '1' then
                    q_mstatus <= q_mstatus(31 downto 4) & '0' & q_mstatus(2 downto 0);
                elsif cmd.CSR_we = CSR_mstatus then
                    q_mstatus <= CSR_write(d_mstatus, q_mstatus, cmd.CSR_WRITE_mode);
                end if;


                if cmd.CSR_we = CSR_mtvec then
                    q_mtvec <= CSR_write(d_mtvec, q_mtvec, cmd.CSR_WRITE_mode)(31 downto 2) & "00";
                end if;

                if cmd.CSR_we = CSR_mepc then
                    q_mepc <= CSR_write(d_mepc, q_mepc, cmd.CSR_WRITE_mode)(31 downto 2) & "00";
                end if;
            end if;
        end if;
    end process bascule;

    mip <= q_mip;
    mie <= q_mie;
    csr <= q_mcause when cmd.CSR_sel = CSR_from_mcause else
           q_mip when cmd.CSR_sel = CSR_from_mip else
           q_mie when cmd.CSR_sel = CSR_from_mie else
           q_mstatus when cmd.CSR_sel = CSR_from_mstatus else
           q_mtvec when cmd.CSR_sel = CSR_from_mtvec else
           q_mepc when cmd.CSR_sel = CSR_from_mepc;
    it <= q_mstatus(3) and q_irq;
    mtvec <= q_mtvec;
    mepc <= q_mepc;

end architecture;
