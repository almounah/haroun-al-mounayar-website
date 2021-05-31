library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.PKG.all;


entity CPU_PC is
    generic(
        mutant: integer := 0
    );
    Port (
        -- Clock/Reset
        clk    : in  std_logic ;
        rst    : in  std_logic ;

        -- Interface PC to PO
        cmd    : out PO_cmd ;
        status : in  PO_status
    );
end entity;

architecture RTL of CPU_PC is
    type State_type is (
        S_Error,
        S_Init,
        S_Pre_Fetch,
        S_Fetch,
        S_Decode,
        S_LUI,
        S_ADDI,
        S_ADD,
        S_SLL,
        S_AUIPC,
        S_AND,
        S_OR,
        S_ORI,
        S_ANDI,
        S_XOR,
        S_XORI,
        S_SUB,
        S_SRL,
        S_SRA,
        S_SRAI,
        S_SLLI,
        S_SRLI,
        S_SLT,
        S_BEQ,
        S_BNE,
        S_BLT,
        S_BGE,
        S_BLTU,
        S_BGEU,
        S_SLTI,
        S_SLTU, 
        S_SLTIU,
        S_LW, S_LW_calc, S_LW_mem,
        S_LB, S_LB_calc, S_LB_mem,
        S_LH, S_LH_calc, S_LH_mem,
        S_LBU, S_LBU_calc, S_LBU_mem,
        S_LHU, S_LHU_calc, S_LHU_mem,
        S_SW, S_SW_calc,
        S_SH, S_SH_calc,
        S_SB, S_SB_calc,
        S_JAL, S_JALR,
        S_CSRRW,
        S_CSRRWI,
        S_CSRRS,
        S_CSRRSI,
        S_CSRRC,
        S_CSRRCI,
        S_start_interupt, S_mret
    );

    signal state_d, state_q : State_type;


begin

    FSM_synchrone : process(clk)
    begin
        if clk'event and clk='1' then
            if rst='1' then
                state_q <= S_Init;
            else
                state_q <= state_d;
            end if;
        end if;
    end process FSM_synchrone;

    FSM_comb : process (state_q, status)
    begin

        -- Valeurs par défaut de cmd à définir selon les préférences de chacun
        cmd.ALU_op            <= ALU_plus;
        cmd.LOGICAL_op        <= LOGICAL_and;
        cmd.ALU_Y_sel         <= ALU_Y_rf_rs2;

        cmd.SHIFTER_op        <= SHIFT_rl;
        cmd.SHIFTER_Y_sel     <= SHIFTER_Y_rs2;

        cmd.RF_we             <= '0';
        cmd.RF_SIZE_sel       <= RF_SIZE_word;
        cmd.RF_SIGN_enable    <= '0';
        cmd.DATA_sel          <= DATA_from_alu;

        cmd.PC_we             <= '0';
        cmd.PC_sel            <= PC_from_pc;

        cmd.PC_X_sel          <= PC_X_pc;
        cmd.PC_Y_sel          <= PC_Y_cst_x04;

        cmd.TO_PC_Y_sel       <= TO_PC_Y_cst_x04;

        cmd.AD_we             <= '0';
        cmd.AD_Y_sel          <= AD_Y_immI;

        cmd.IR_we             <= '0';

        cmd.ADDR_sel          <= ADDR_from_ad;
        cmd.mem_we            <= '0';
        cmd.mem_ce            <= '0';

        cmd.cs.CSR_we            <= CSR_none;

        cmd.cs.TO_CSR_sel        <= TO_CSR_from_rs1;
        cmd.cs.CSR_sel           <= CSR_from_mepc;
        cmd.cs.MEPC_sel          <= MEPC_from_csr;

        cmd.cs.MSTATUS_mie_set   <= '0';
        cmd.cs.MSTATUS_mie_reset <= '0';

        cmd.cs.CSR_WRITE_mode    <= WRITE_mode_simple;

        state_d <= state_q;

        case state_q is
            when S_Error =>
                -- Etat transitoire en cas d'instruction non reconnue 
                -- Aucune action
                state_d <= S_Init;

            when S_Init =>
                -- PC <- RESET_VECTOR
                cmd.PC_we <= '1';
                cmd.PC_sel <= PC_rstvec;
                state_d <= S_Pre_Fetch;

            when S_Pre_Fetch =>

                -- mem[PC]
                cmd.mem_we   <= '0';
                cmd.mem_ce   <= '1';
                cmd.ADDR_sel <= ADDR_from_pc;
                state_d      <= S_Fetch;

            when S_Fetch =>
                if status.IT then
                    state_d <= S_start_interupt;
                else
                    -- IR <- mem_datain
                    cmd.IR_we <= '1';
                    state_d <= S_Decode;
                end if;

            when S_Decode =>

                if status.IR(6 downto 0) = "0110111" then
                    cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                    cmd.PC_sel <= PC_from_pc;
                    cmd.PC_we <= '1';
                    state_d <= S_LUI;
                elsif status.IR(6 downto 0) = "0010011" then
                    cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                    cmd.PC_sel <= PC_from_pc;
                    cmd.PC_we <= '1';
                    if status.IR(14 downto 12) = "000" then
                        state_d <= S_ADDI;
                    elsif status.IR(14 downto 12) = "111" then
                        state_d <= S_ANDI;
                    elsif status.IR(14 downto 12) = "110" then
                        state_d <= S_ORI;
                    elsif status.IR(14 downto 12) = "100" then
                        state_d <= S_XORI;
                    elsif status.IR(14 downto 12) = "001" then
                        state_d <= S_SLLI;
                    elsif status.IR(14 downto 12) = "101" then
                        if status.IR(31 downto 25) = "0000000" then
                            state_d <= S_SRLI;
                        elsif status.IR(31 downto 25) = "0100000" then
                            state_d <= S_SRAI;
                        end if;         
                    elsif status.IR(14 downto 12) = "010" then
                        state_d <= S_SLTI;
                    elsif status.IR(14 downto 12) = "011" then
                        state_d <= S_SLTIU;
                    end if;
                elsif status.IR(6 downto 0) = "0110011" then
                    cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                    cmd.PC_sel <= PC_from_pc;
                    cmd.PC_we <= '1';
                    if status.IR(14 downto 12) = "000" then
                        if status.IR(31 downto 25) = "0000000" then
                            state_d <= S_ADD;
                        elsif status.IR(31 downto 25) = "0100000" then
                            state_d <= S_SUB;
                        end if;
                    elsif status.IR(14 downto 12) = "001" then
                        state_d <= S_SLL;
                    elsif status.IR(14 downto 12) = "101" then
                        if status.IR(31 downto 25) = "0100000" then
                            state_d <= S_SRA;
                        elsif status.IR(31 downto 25) = "0000000" then
                            state_d <= S_SRL;
                        end if;
                    elsif status.IR(14 downto 12) = "111" then
                        state_d <= S_AND;
                    elsif status.IR(14 downto 12) = "110" then
                        state_d <= S_OR;
                    elsif status.IR(14 downto 12) = "100" then
                        state_d <= S_XOR;
                    elsif status.IR(14 downto 12) = "010" then
                        state_d <= S_SLT;
                    elsif status.IR(14 downto 12) = "011" then
                        state_d <= S_SLTU;
                    end if;
                elsif status.IR(6 downto 0) = "0010111" then
                    state_d <= S_AUIPC;
                elsif status.IR(6 downto 0) = "1100011" then
                    if status.IR(14 downto 12) = "000" then
                        state_d <= S_BEQ;
                    elsif status.IR(14 downto 12) = "001" then
                        state_d <= S_BNE;
                    elsif status.IR(14 downto 12) = "100" then
                        state_d <= S_BLT;
                    elsif status.IR(14 downto 12) = "101" then
                        state_d <= S_BGE;
                    elsif status.IR(14 downto 12) = "111" then
                        state_d <= S_BGEU;
                    elsif status.IR(14 downto 12) = "110" then
                        state_d <= S_BLTU;
                    end if;
                elsif status.IR(6 downto 0) = "0000011" then 
                    if status.IR(14 downto 12) = "010" then
                        state_d <= S_LW_calc;
                    elsif status.IR(14 downto 12) = "000" then
                        state_d <= S_LB_calc;
                    elsif status.IR(14 downto 12) = "100" then
                        state_d <= S_LBU_calc;
                    elsif status.IR(14 downto 12) = "001" then
                        state_d <= S_LH_calc;
                    elsif status.IR(14 downto 12) = "101" then
                        state_d <= S_LHU_calc;
                    end if;
                elsif status.IR(6 downto 0) = "0100011" then
                    if status.IR(14 downto 12) = "010" then
                        state_d <= S_SW_calc;
                    elsif status.IR(14 downto 12) = "000" then
                        state_d <= S_SB_calc;
                    elsif status.IR(14 downto 12) = "001" then
                        state_d <= S_SH_calc;
                    end if;
                elsif status.IR(6 downto 0) = "1101111" then
                    state_d <= S_JAL;
                elsif status.IR(6 downto 0) = "1100111" then
                    state_d <= S_JALR;
                elsif status.IR(6 downto 0) = "1110011" then
                    if status.IR(14 downto 12) = "001" then
                        state_d <= S_CSRRW;         
                    elsif status.IR(14 downto 12) = "101" then
                        state_d <= S_CSRRWI;
                    elsif status.IR(14 downto 12) = "010" then
                        state_d <= S_CSRRS;
                    elsif status.IR(14 downto 12) = "110" then
                        state_d <= S_CSRRSI;
                    elsif status.IR(14 downto 12) = "011" then
                        state_d <= S_CSRRC;
                    elsif status.IR(14 downto 12) = "111" then
                        state_d <= S_CSRRCI;
                    elsif status.IR(14 downto 12) = "000" then
                        state_d <= S_mret;
                    end if; 
                else
                    state_d <= S_Error;
                end if;

---------- Instructions avec immediat de type U ----------
            when S_LUI =>
                -- rd <- ImmU + 0
                cmd.PC_X_sel <= PC_X_cst_x00;
                cmd.PC_Y_sel <= PC_Y_immU;
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_pc;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;

           when S_AUIPC =>
              -- rd <- pc + ImmU
              cmd.PC_X_sel <= PC_X_pc;
              cmd.PC_Y_sel <= PC_Y_immU;
              cmd.RF_we <= '1';
              cmd.DATA_sel <= DATA_from_pc;
              -- ajout pc = pc + 4
              cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
              cmd.PC_sel <= PC_from_pc;
              cmd.PC_we <= '1';
              -- next state
              state_d <= S_Pre_Fetch;

---------- Instructions avec immediat de type I ----------
                
            when S_ADDI =>
                -- rd <- rs1 + ImmI
                cmd.ALU_Y_sel <= ALU_Y_immI;
                cmd.DATA_sel <= DATA_from_alu;
                cmd.RF_we <= '1';
                cmd.ALU_op <= ALU_plus;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;
            
            when S_ORI =>
                -- rd <- rs1 OR ImmI
                cmd.ALU_Y_sel <= ALU_Y_immI;
                cmd.DATA_sel <= DATA_from_logical;
                cmd.RF_we <= '1';
                cmd.LOGICAL_op <= LOGICAL_or;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;

            when S_XORI =>
                -- rd <- rs1 XOR ImmI
                cmd.ALU_Y_sel <= ALU_Y_immI;
                cmd.DATA_sel <= DATA_from_logical;
                cmd.RF_we <= '1';
                cmd.LOGICAL_op <= LOGICAL_xor;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;

            when S_ANDI =>
                -- rd <- rs1 AND ImmI
                cmd.ALU_Y_sel <= ALU_Y_immI;
                cmd.DATA_sel <= DATA_from_logical;
                cmd.RF_we <= '1';
                cmd.LOGICAL_op <= LOGICAL_and;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;

            when S_SLTI =>
                -- compare registes signee
                cmd.ALU_Y_sel <= ALU_Y_immI;
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_slt;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;

            when S_SLTIU =>
                -- compare registes signee
                cmd.ALU_Y_sel <= ALU_Y_immI;
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_slt;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;
---------- Instructions arithmétiques et logiques ----------
                
            when S_ADD =>
                -- rd <- rs1 + rs2
                cmd.ALU_Y_sel <= ALU_Y_rf_rs2;
                cmd.DATA_sel <= DATA_from_alu;
                cmd.RF_we <= '1';
                cmd.ALU_op <= ALU_plus;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;
           
            when S_SUB =>
                -- rd <- rs1 + rs2
                cmd.ALU_Y_sel <= ALU_Y_rf_rs2;
                cmd.DATA_sel <= DATA_from_alu;
                cmd.RF_we <= '1';
                cmd.ALU_op <= ALU_minus;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;
            

            when S_AND =>
                -- rd <- rs1 and rs2
                cmd.ALU_Y_sel <= ALU_Y_rf_rs2;
                cmd.DATA_sel <= DATA_from_logical;
                cmd.RF_we <= '1';
                cmd.LOGICAL_op <= LOGICAL_and;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;  

            when S_OR =>
                -- rd <- rs1 or rs2
                cmd.ALU_Y_sel <= ALU_Y_rf_rs2;
                cmd.DATA_sel <= DATA_from_logical;
                cmd.RF_we <= '1';
                cmd.LOGICAL_op <= LOGICAL_or;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;    

            when S_XOR =>
                -- rd <- rs1 xor rs2
                cmd.ALU_Y_sel <= ALU_Y_rf_rs2;
                cmd.DATA_sel <= DATA_from_logical;
                cmd.RF_we <= '1';
                cmd.LOGICAL_op <= LOGICAL_xor;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;     

            when S_SLL =>
                -- rd <- rs1 << rs2
                cmd.SHIFTER_Y_sel <= SHIFTER_Y_rs2;
                cmd.SHIFTER_op <= SHIFT_ll;
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_shifter;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;
             
            when S_SRL =>
                -- rd <- rs1 >> rs2
                cmd.SHIFTER_Y_sel <= SHIFTER_Y_rs2;
                cmd.SHIFTER_op <= SHIFT_rl;
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_shifter;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;

            when S_SRA =>
                -- rd <- rs1 >>> rs2
                cmd.SHIFTER_Y_sel <= SHIFTER_Y_rs2;
                cmd.SHIFTER_op <= SHIFT_ra;
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_shifter;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;

            when S_SRAI =>
                -- rd <- rs1 >>> shamt
                cmd.SHIFTER_Y_sel <= SHIFTER_Y_ir_sh;
                cmd.SHIFTER_op <= SHIFT_ra;
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_shifter;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;

            when S_SLLI => 
                -- rd <- rs1 << shamt
                cmd.SHIFTER_Y_sel <= SHIFTER_Y_ir_sh;
                cmd.SHIFTER_op <= SHIFT_ll;
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_shifter;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;
             
            when S_SRLI =>
                -- rd <- rs1 >> shamt
                cmd.SHIFTER_Y_sel <= SHIFTER_Y_ir_sh;
                cmd.SHIFTER_op <= SHIFT_rl;
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_shifter;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;
---------- Instructions de saut ----------

            when S_SLT =>
                -- compare registes signee
                cmd.ALU_Y_sel <= ALU_Y_rf_rs2;
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_slt;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;

            when S_SLTU =>
                -- compare registes signee
                cmd.ALU_Y_sel <= ALU_Y_rf_rs2;
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_slt;
                -- lecture mem[PC]
                cmd.ADDR_sel <= ADDR_from_pc;
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                -- next state
                state_d <= S_Fetch;


           when S_BEQ =>
                -- Calcul de Jcond validation de saut
                cmd.ALU_Y_sel <= ALU_Y_rf_rs2;
                if status.JCOND then
                    -- pc <- pc + ImmB
                    cmd.TO_PC_Y_sel <= TO_PC_Y_immB;
                    cmd.PC_sel <= PC_from_pc; 
                    cmd.PC_we <= '1';
                else
                    -- ajout pc = pc + 4
                    cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                    cmd.PC_sel <= PC_from_pc;
                    cmd.PC_we <= '1';
                end if;
                -- next state
                state_d <= S_Pre_Fetch;

           when S_BNE =>
                -- Calcul de Jcond validation de saut
                cmd.ALU_Y_sel <= ALU_Y_rf_rs2;
                if status.JCOND then
                    -- pc <- pc + ImmB
                    cmd.TO_PC_Y_sel <= TO_PC_Y_immB;
                    cmd.PC_sel <= PC_from_pc; 
                    cmd.PC_we <= '1';
                else
                    -- ajout pc = pc + 4
                    cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                    cmd.PC_sel <= PC_from_pc;
                    cmd.PC_we <= '1';
                end if;
                -- next state
                state_d <= S_Pre_Fetch;


           when S_BLT =>
                -- Calcul de Jcond validation de saut
                cmd.ALU_Y_sel <= ALU_Y_rf_rs2;
                if status.JCOND then
                    -- pc <- pc + ImmB
                    cmd.TO_PC_Y_sel <= TO_PC_Y_immB;
                    cmd.PC_sel <= PC_from_pc; 
                    cmd.PC_we <= '1';
                else
                    -- ajout pc = pc + 4
                    cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                    cmd.PC_sel <= PC_from_pc;
                    cmd.PC_we <= '1';
                end if;
                -- next state
                state_d <= S_Pre_Fetch;

           when S_BGE =>
                -- Calcul de Jcond validation de saut
                cmd.ALU_Y_sel <= ALU_Y_rf_rs2;
                if status.JCOND then
                    -- pc <- pc + ImmB
                    cmd.TO_PC_Y_sel <= TO_PC_Y_immB;
                    cmd.PC_sel <= PC_from_pc; 
                    cmd.PC_we <= '1';
                else
                    -- ajout pc = pc + 4
                    cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                    cmd.PC_sel <= PC_from_pc;
                    cmd.PC_we <= '1';
                end if;
                -- next state
                state_d <= S_Pre_Fetch;

           when S_BGEU =>
                -- Calcul de Jcond validation de saut
                cmd.ALU_Y_sel <= ALU_Y_rf_rs2;
                if status.JCOND then
                    -- pc <- pc + ImmB
                    cmd.TO_PC_Y_sel <= TO_PC_Y_immB;
                    cmd.PC_sel <= PC_from_pc; 
                    cmd.PC_we <= '1';
                else
                    -- ajout pc = pc + 4
                    cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                    cmd.PC_sel <= PC_from_pc;
                    cmd.PC_we <= '1';
                end if;
                -- next state
                state_d <= S_Pre_Fetch;


           when S_BLTU =>
                -- Calcul de Jcond validation de saut
                cmd.ALU_Y_sel <= ALU_Y_rf_rs2;
                if status.JCOND then
                    -- pc <- pc + ImmB
                    cmd.TO_PC_Y_sel <= TO_PC_Y_immB;
                    cmd.PC_sel <= PC_from_pc; 
                    cmd.PC_we <= '1';
                else
                    -- ajout pc = pc + 4
                    cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                    cmd.PC_sel <= PC_from_pc;
                    cmd.PC_we <= '1';
                end if;
                -- next state
                state_d <= S_Pre_Fetch;

            when S_JAL =>
                -- calcul de rd = pc + 4
                cmd.PC_X_sel <= PC_X_pc;
                cmd.PC_Y_sel <= PC_Y_cst_x04;
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_pc;
                -- changement de pc = pc + cst
                cmd.TO_PC_Y_sel <= TO_PC_Y_immJ;
                cmd.PC_we <= '1';
                cmd.PC_sel <= PC_from_pc;
                -- next state
                state_d <= S_PRE_Fetch; 

             when S_JALR =>
                -- calcul de rd = pc + 4
                cmd.PC_X_sel <= PC_X_pc;
                cmd.PC_Y_sel <= PC_Y_cst_x04;
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_pc;
                -- changement de pc = rs1 + immI
                cmd.ALU_op <= ALU_plus;
                cmd.ALU_Y_sel <= ALU_Y_immI;
                cmd.PC_we <= '1';
                cmd.PC_sel <= PC_from_alu;
                -- next state
                state_d <= S_PRE_Fetch; 



---------- Instructions de chargement à partir de la mémoire ----------
            when S_LW_calc => 
                -- On calcul l'adresse et on la met dans le registre Ad
                cmd.AD_Y_sel <= AD_Y_immI;
                cmd.AD_we <= '1';
                state_d <= S_LW_mem;

            when S_LW_mem =>
                -- On cherche de la memoire
                -- lecture mem[rs1+ImmI]
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                cmd.ADDR_sel <= ADDR_from_ad;
                state_d <= S_LW;


            when S_LW =>
                 -- On ajuste la taille et extension de signe 
                 -- On met ensuite dans rd 
                 cmd.RF_SIGN_enable <= '1';
                 cmd.RF_SIZE_sel <= RF_SIZE_word; 
                 cmd.RF_we <= '1';
                 cmd.DATA_sel <= DATA_from_mem;

                 -- ajout pc = pc + 4
                 cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                 cmd.PC_sel <= PC_from_pc;
                 cmd.PC_we <= '1';
                 -- next state
                 state_d <= S_PRE_Fetch;

            when S_LH_calc => 
                -- On calcul l'adresse et on la met dans le registre Ad
                cmd.AD_Y_sel <= AD_Y_immI;
                cmd.AD_we <= '1';
                state_d <= S_LH_mem;

            when S_LH_mem =>
                -- On cherche de la memoire
                -- lecture mem[rs1+ImmI]
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                cmd.ADDR_sel <= ADDR_from_ad;
                state_d <= S_LH;


            when S_LH =>
                 -- On ajuste la taille et extension de signe 
                 -- On met ensuite dans rd 
                 cmd.RF_SIGN_enable <= '1';
                 cmd.RF_SIZE_sel <= RF_SIZE_half; 
                 cmd.RF_we <= '1';
                 cmd.DATA_sel <= DATA_from_mem;

                 -- ajout pc = pc + 4
                 cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                 cmd.PC_sel <= PC_from_pc;
                 cmd.PC_we <= '1';
                 -- next state
                 state_d <= S_PRE_Fetch;

            when S_LB_calc => 
                -- On calcul l'adresse et on la met dans le registre Ad
                cmd.AD_Y_sel <= AD_Y_immI;
                cmd.AD_we <= '1';
                state_d <= S_LB_mem;

            when S_LB_mem =>
                -- On cherche de la memoire
                -- lecture mem[rs1+ImmI]
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                cmd.ADDR_sel <= ADDR_from_ad;
                state_d <= S_LB;


            when S_LB =>
                 -- On ajuste la taille et extension de signe 
                 -- On met ensuite dans rd 
                 cmd.RF_SIGN_enable <= '1';
                 cmd.RF_SIZE_sel <= RF_SIZE_byte; 
                 cmd.RF_we <= '1';
                 cmd.DATA_sel <= DATA_from_mem;

                 -- ajout pc = pc + 4
                 cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                 cmd.PC_sel <= PC_from_pc;
                 cmd.PC_we <= '1';
                 -- next state
                 state_d <= S_PRE_Fetch;


             when S_LBU_calc => 
                -- On calcul l'adresse et on la met dans le registre Ad
                cmd.AD_Y_sel <= AD_Y_immI;
                cmd.AD_we <= '1';
                state_d <= S_LBU_mem;

            when S_LBU_mem =>
                -- On cherche de la memoire
                -- lecture mem[rs1+ImmI]
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                cmd.ADDR_sel <= ADDR_from_ad;
                state_d <= S_LBU;


            when S_LBU =>
                 -- On ajuste la taille et extension de signe 
                 -- On met ensuite dans rd
                 cmd.RF_SIGN_enable <= '0';
                 cmd.RF_SIZE_sel <= RF_SIZE_byte; 
                 cmd.RF_we <= '1';
                 cmd.DATA_sel <= DATA_from_mem;

                 -- ajout pc = pc + 4
                 cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                 cmd.PC_sel <= PC_from_pc;
                 cmd.PC_we <= '1';
                 -- next state
                 state_d <= S_PRE_Fetch;              


             when S_LHU_calc => 
                -- On calcul l'adresse et on la met dans le registre Ad
                cmd.AD_Y_sel <= AD_Y_immI;
                cmd.AD_we <= '1';
                state_d <= S_LHU_mem;

            when S_LHU_mem =>
                -- On cherche de la memoire
                -- lecture mem[rs1+ImmI]
                cmd.mem_ce <= '1';
                cmd.mem_we <= '0';
                cmd.ADDR_sel <= ADDR_from_ad;
                state_d <= S_LHU;


            when S_LHU =>
                 -- On ajuste la taille et extension de signe 
                 -- On met ensuite dans rd
                 cmd.RF_SIGN_enable <= '0';
                 cmd.RF_SIZE_sel <= RF_SIZE_half; 
                 cmd.RF_we <= '1';
                 cmd.DATA_sel <= DATA_from_mem;

                 -- ajout pc = pc + 4
                 cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                 cmd.PC_sel <= PC_from_pc;
                 cmd.PC_we <= '1';
                 -- next state
                 state_d <= S_PRE_Fetch;              


---------- Instructions de sauvegarde en mémoire ----------
            when S_SW_calc =>
                -- On calcul l'adresse et on la met dans le registre Ad
                cmd.AD_Y_sel <= AD_Y_immS;
                cmd.AD_we <= '1';
                state_d <= S_SW;

            when S_SW  =>
                -- On cherche de la memoire
                -- ecriture mem[rs1+ImmI]
                cmd.mem_ce <= '1';
                cmd.mem_we <= '1';
                cmd.ADDR_sel <= ADDR_from_ad;
                cmd.RF_SIZE_sel <= RF_SIZE_word;
                -- ajout pc = pc + 4
                cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                cmd.PC_sel <= PC_from_pc;
                cmd.PC_we <= '1';
                -- next state
                state_d <= S_PRE_Fetch;

            when S_SB_calc =>
                -- On calcul l'adresse et on la met dans le registre Ad
                cmd.AD_Y_sel <= AD_Y_immS;
                cmd.AD_we <= '1';
                state_d <= S_SB;

            when S_SB  =>
                -- On cherche de la memoire
                -- ecriture mem[rs1+ImmI]
                cmd.mem_ce <= '1';
                cmd.mem_we <= '1';
                cmd.RF_SIZE_sel <= RF_SIZE_byte;
                cmd.ADDR_sel <= ADDR_from_ad;
                -- ajout pc = pc + 4
                cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                cmd.PC_sel <= PC_from_pc;
                cmd.PC_we <= '1';
                -- next state
                state_d <= S_PRE_Fetch;

            when S_SH_calc =>
                -- On calcul l'adresse et on la met dans le registre Ad
                cmd.AD_Y_sel <= AD_Y_immS;
                cmd.AD_we <= '1';
                state_d <= S_SH;

            when S_SH  =>
                -- On cherche de la memoire
                -- ecriture mem[rs1+ImmI]
                cmd.mem_ce <= '1';
                cmd.mem_we <= '1';
                cmd.ADDR_sel <= ADDR_from_ad;
                cmd.RF_SIZE_sel <= RF_SIZE_half;
                -- ajout pc = pc + 4
                cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                cmd.PC_sel <= PC_from_pc;
                cmd.PC_we <= '1';
                -- next state
                state_d <= S_PRE_Fetch;
---------- Instructions d'accès aux CSR ----------

            when S_start_interupt => 
                -- sauvegarde pc
                cmd.cs.CSR_we <= CSR_mepc;
                cmd.cs.MEPC_sel <= MEPC_from_pc;
                cmd.cs.CSR_WRITE_mode <= WRITE_mode_simple;
                -- mstatus(3) = 0
                cmd.cs.MSTATUS_mie_reset <= '1';
                -- saute a l'adresse dans mtvec
                cmd.PC_we <= '1';
                cmd.PC_sel <= PC_mtvec;
                cmd.cs.CSR_sel <= CSR_from_mtvec;
                -- next state
                state_d <= S_PRE_Fetch;

            when S_mret =>
                 -- revient a l'adresse dans mepc
                cmd.PC_we <= '1';
                cmd.PC_sel <= PC_from_mepc;
                cmd.cs.CSR_sel <= CSR_from_mepc;
                -- mstatus(3) = 1
                cmd.cs.MSTATUS_mie_set <= '1';
                -- next state
                state_d <= S_Pre_Fetch;

            when S_CSRRW => 
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_csr;
                if status.IR(31 downto 20) = X"300" then
                    cmd.cs.CSR_sel <= CSR_from_mstatus;
                elsif status.IR(31 downto 20) = X"304" then 
                    cmd.cs.CSR_sel <= CSR_from_mie;
                elsif status.IR(31 downto 20) = X"305" then
                    cmd.cs.CSR_sel <= CSR_from_mtvec;
                elsif status.IR(31 downto 20) = X"341" then
                    cmd.cs.CSR_sel <= CSR_from_mepc;
                elsif status.IR(31 downto 20) = X"342" then
                    cmd.cs.CSR_sel <= CSR_from_mcause;
                elsif status.IR(31 downto 20) = X"344" then
                    cmd.cs.CSR_sel <= CSR_from_mip;
                end if;


                if status.IR(31 downto 20) = X"300" then
                    cmd.cs.CSR_we <= CSR_mstatus;
                elsif status.IR(31 downto 20) = X"304" then 
                    cmd.cs.CSR_we <= CSR_mie;
                elsif status.IR(31 downto 20) = X"305" then
                    cmd.cs.CSR_we <= CSR_mtvec;
                elsif status.IR(31 downto 20) = X"341" then
                    cmd.cs.CSR_we <= CSR_mepc;
                end if;
                cmd.cs.TO_CSR_sel <= TO_CSR_from_rs1;
                cmd.cs.CSR_WRITE_mode <= WRITE_mode_simple;
                cmd.cs.MEPC_sel <= MEPC_from_csr;
                cmd.cs.MSTATUS_mie_set <= '0';
                cmd.cs.MSTATUS_mie_reset <= '0';
                -- ajout pc = pc + 4
                cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                cmd.PC_sel <= PC_from_pc;
                cmd.PC_we <= '1';
                -- next state
                state_d <= S_Pre_Fetch;
            
            when S_CSRRWI => 
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_csr;
                if status.IR(31 downto 20) = X"300" then
                    cmd.cs.CSR_sel <= CSR_from_mstatus;
                elsif status.IR(31 downto 20) = X"304" then 
                    cmd.cs.CSR_sel <= CSR_from_mie;
                elsif status.IR(31 downto 20) = X"305" then
                    cmd.cs.CSR_sel <= CSR_from_mtvec;
                elsif status.IR(31 downto 20) = X"341" then
                    cmd.cs.CSR_sel <= CSR_from_mepc;
                elsif status.IR(31 downto 20) = X"342" then
                    cmd.cs.CSR_sel <= CSR_from_mcause;
                elsif status.IR(31 downto 20) = X"344" then
                    cmd.cs.CSR_sel <= CSR_from_mip;
                end if;


                if status.IR(31 downto 20) = X"300" then
                    cmd.cs.CSR_we <= CSR_mstatus;
                elsif status.IR(31 downto 20) = X"304" then 
                    cmd.cs.CSR_we <= CSR_mie;
                elsif status.IR(31 downto 20) = X"305" then
                    cmd.cs.CSR_we <= CSR_mtvec;
                elsif status.IR(31 downto 20) = X"341" then
                    cmd.cs.CSR_we <= CSR_mepc;
                end if;
                cmd.cs.TO_CSR_sel <= TO_CSR_from_imm;
                cmd.cs.CSR_WRITE_mode <= WRITE_mode_simple;
                cmd.cs.MEPC_sel <= MEPC_from_csr;
                cmd.cs.MSTATUS_mie_set <= '0';
                cmd.cs.MSTATUS_mie_reset <= '0';
                -- ajout pc = pc + 4
                cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                cmd.PC_sel <= PC_from_pc;
                cmd.PC_we <= '1';
                -- next state
                state_d <= S_Pre_Fetch;

            when S_CSRRS => 
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_csr;
                if status.IR(31 downto 20) = X"300" then
                    cmd.cs.CSR_sel <= CSR_from_mstatus;
                elsif status.IR(31 downto 20) = X"304" then 
                    cmd.cs.CSR_sel <= CSR_from_mie;
                elsif status.IR(31 downto 20) = X"305" then
                    cmd.cs.CSR_sel <= CSR_from_mtvec;
                elsif status.IR(31 downto 20) = X"341" then
                    cmd.cs.CSR_sel <= CSR_from_mepc;
                elsif status.IR(31 downto 20) = X"342" then
                    cmd.cs.CSR_sel <= CSR_from_mcause;
                elsif status.IR(31 downto 20) = X"344" then
                    cmd.cs.CSR_sel <= CSR_from_mip;
                end if;


                if status.IR(31 downto 20) = X"300" then
                    cmd.cs.CSR_we <= CSR_mstatus;
                elsif status.IR(31 downto 20) = X"304" then 
                    cmd.cs.CSR_we <= CSR_mie;
                elsif status.IR(31 downto 20) = X"305" then
                    cmd.cs.CSR_we <= CSR_mtvec;
                elsif status.IR(31 downto 20) = X"341" then
                    cmd.cs.CSR_we <= CSR_mepc;
                end if;
                cmd.cs.TO_CSR_sel <= TO_CSR_from_rs1;
                cmd.cs.CSR_WRITE_mode <= WRITE_mode_set;
                cmd.cs.MEPC_sel <= MEPC_from_csr;
                cmd.cs.MSTATUS_mie_set <= '0';
                cmd.cs.MSTATUS_mie_reset <= '0';
                -- ajout pc = pc + 4
                cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                cmd.PC_sel <= PC_from_pc;
                cmd.PC_we <= '1';
                -- next state
                state_d <= S_Pre_Fetch;

            when S_CSRRSI => 
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_csr;
                if status.IR(31 downto 20) = X"300" then
                    cmd.cs.CSR_sel <= CSR_from_mstatus;
                elsif status.IR(31 downto 20) = X"304" then 
                    cmd.cs.CSR_sel <= CSR_from_mie;
                elsif status.IR(31 downto 20) = X"305" then
                    cmd.cs.CSR_sel <= CSR_from_mtvec;
                elsif status.IR(31 downto 20) = X"341" then
                    cmd.cs.CSR_sel <= CSR_from_mepc;
                elsif status.IR(31 downto 20) = X"342" then
                    cmd.cs.CSR_sel <= CSR_from_mcause;
                elsif status.IR(31 downto 20) = X"344" then
                    cmd.cs.CSR_sel <= CSR_from_mip;
                end if;


                if status.IR(31 downto 20) = X"300" then
                    cmd.cs.CSR_we <= CSR_mstatus;
                elsif status.IR(31 downto 20) = X"304" then 
                    cmd.cs.CSR_we <= CSR_mie;
                elsif status.IR(31 downto 20) = X"305" then
                    cmd.cs.CSR_we <= CSR_mtvec;
                elsif status.IR(31 downto 20) = X"341" then
                    cmd.cs.CSR_we <= CSR_mepc;
                end if;
                cmd.cs.TO_CSR_sel <= TO_CSR_from_imm;
                cmd.cs.CSR_WRITE_mode <= WRITE_mode_set;
                cmd.cs.MEPC_sel <= MEPC_from_csr;
                cmd.cs.MSTATUS_mie_set <= '0';
                cmd.cs.MSTATUS_mie_reset <= '0';
                -- ajout pc = pc + 4
                cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                cmd.PC_sel <= PC_from_pc;
                cmd.PC_we <= '1';
                -- next state
                state_d <= S_Pre_Fetch;

            when S_CSRRC => 
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_csr;
                if status.IR(31 downto 20) = X"300" then
                    cmd.cs.CSR_sel <= CSR_from_mstatus;
                elsif status.IR(31 downto 20) = X"304" then 
                    cmd.cs.CSR_sel <= CSR_from_mie;
                elsif status.IR(31 downto 20) = X"305" then
                    cmd.cs.CSR_sel <= CSR_from_mtvec;
                elsif status.IR(31 downto 20) = X"341" then
                    cmd.cs.CSR_sel <= CSR_from_mepc;
                elsif status.IR(31 downto 20) = X"342" then
                    cmd.cs.CSR_sel <= CSR_from_mcause;
                elsif status.IR(31 downto 20) = X"344" then
                    cmd.cs.CSR_sel <= CSR_from_mip;
                end if;


                if status.IR(31 downto 20) = X"300" then
                    cmd.cs.CSR_we <= CSR_mstatus;
                elsif status.IR(31 downto 20) = X"304" then 
                    cmd.cs.CSR_we <= CSR_mie;
                elsif status.IR(31 downto 20) = X"305" then
                    cmd.cs.CSR_we <= CSR_mtvec;
                elsif status.IR(31 downto 20) = X"341" then
                    cmd.cs.CSR_we <= CSR_mepc;
                end if;
                cmd.cs.TO_CSR_sel <= TO_CSR_from_rs1;
                cmd.cs.CSR_WRITE_mode <= WRITE_mode_clear;
                cmd.cs.MEPC_sel <= MEPC_from_csr;
                cmd.cs.MSTATUS_mie_set <= '0';
                cmd.cs.MSTATUS_mie_reset <= '0';
                -- ajout pc = pc + 4
                cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                cmd.PC_sel <= PC_from_pc;
                cmd.PC_we <= '1';
                -- next state
                state_d <= S_Pre_Fetch;

            when S_CSRRCI => 
                cmd.RF_we <= '1';
                cmd.DATA_sel <= DATA_from_csr;
                if status.IR(31 downto 20) = X"300" then
                    cmd.cs.CSR_sel <= CSR_from_mstatus;
                elsif status.IR(31 downto 20) = X"304" then 
                    cmd.cs.CSR_sel <= CSR_from_mie;
                elsif status.IR(31 downto 20) = X"305" then
                    cmd.cs.CSR_sel <= CSR_from_mtvec;
                elsif status.IR(31 downto 20) = X"341" then
                    cmd.cs.CSR_sel <= CSR_from_mepc;
                elsif status.IR(31 downto 20) = X"342" then
                    cmd.cs.CSR_sel <= CSR_from_mcause;
                elsif status.IR(31 downto 20) = X"344" then
                    cmd.cs.CSR_sel <= CSR_from_mip;
                end if;


                if status.IR(31 downto 20) = X"300" then
                    cmd.cs.CSR_we <= CSR_mstatus;
                elsif status.IR(31 downto 20) = X"304" then 
                    cmd.cs.CSR_we <= CSR_mie;
                elsif status.IR(31 downto 20) = X"305" then
                    cmd.cs.CSR_we <= CSR_mtvec;
                elsif status.IR(31 downto 20) = X"341" then
                    cmd.cs.CSR_we <= CSR_mepc;
                end if;
                cmd.cs.TO_CSR_sel <= TO_CSR_from_imm;
                cmd.cs.CSR_WRITE_mode <= WRITE_mode_clear;
                cmd.cs.MEPC_sel <= MEPC_from_csr;
                cmd.cs.MSTATUS_mie_set <= '0';
                cmd.cs.MSTATUS_mie_reset <= '0';
                -- ajout pc = pc + 4
                cmd.TO_PC_Y_sel <= TO_PC_Y_cst_x04;
                cmd.PC_sel <= PC_from_pc;
                cmd.PC_we <= '1';
                -- next state
                state_d <= S_Pre_Fetch;
        end case;

    end process FSM_comb;

end architecture;
