module Control_Unit (
    input wire clk,
    input wire rst_n,
    input wire [6:0] instruction_opcode,
    output reg pc_write,
    output reg ir_write,
    output reg pc_source,
    output reg reg_write,
    output reg memory_read,
    output reg is_immediate,
    output reg memory_write,
    output reg pc_write_cond,
    output reg lorD,
    output reg memory_to_reg,
    output reg [1:0] aluop,
    output reg [1:0] alu_src_a,
    output reg [1:0] alu_src_b
);

// machine states
localparam FETCH        = 4'b0000;
localparam DECODE       = 4'b0001;
localparam MEMADR       = 4'b0010;
localparam MEMREAD      = 4'b0011;
localparam MEMWB        = 4'b0100;
localparam MEMWRITE     = 4'b0101;
localparam EXECUTER     = 4'b0110;
localparam ALUWB        = 4'b0111;
localparam EXECUTEI     = 4'b1000;
localparam BRANCH       = 4'b1001;
localparam JAL_CALC     = 4'b1010;
localparam JAL_WB       = 4'b1011;
localparam JALR_CALC    = 4'b1100;
localparam JALR_WB      = 4'b1101;
localparam AUIPC_CALC   = 4'b1110;
localparam AUIPC_WB     = 4'b1111;
localparam LUI          = 5'b10000;
localparam LUI_WB       = 5'b10001;
localparam JALR_WAIT    = 5'b10010;

// Instruction Opcodes
localparam LW      = 7'b0000011;
localparam SW      = 7'b0100011;
localparam RTYPE   = 7'b0110011;
localparam ITYPE   = 7'b0010011;
localparam JALI    = 7'b1101111;
localparam JALRI   = 7'b1100111;
localparam BRANCHI = 7'b1100011;
localparam AUIPCI  = 7'b0010111;
localparam LUII    = 7'b0110111;

reg [4:0] current_state, following_state;

always @(posedge clk or negedge rst_n) begin
    if (!rst_n)
        current_state <= FETCH;
    else
        current_state <= following_state;
end

always @(*) begin
    case (current_state)
        FETCH:      following_state = DECODE;
        DECODE: begin
            case (instruction_opcode)
                LW:       following_state = MEMADR;
                SW:       following_state = MEMADR;
                RTYPE:    following_state = EXECUTER;
                ITYPE:    following_state = EXECUTEI;
                JALI:     following_state = JAL_CALC;
                JALRI:    following_state = JALR_WAIT;
                BRANCHI:  following_state = BRANCH;
                AUIPCI:   following_state = AUIPC_CALC;
                LUII:     following_state = LUI;
                default:  following_state = FETCH;
            endcase
        end
        MEMADR:       following_state = (instruction_opcode == LW) ? MEMREAD : MEMWRITE;
        MEMREAD:      following_state = MEMWB;
        MEMWRITE:     following_state = FETCH;
        MEMWB:        following_state = FETCH;
        EXECUTEI:     following_state = ALUWB;
        EXECUTER:     following_state = ALUWB;
        ALUWB:        following_state = FETCH;
        JAL_CALC:     following_state = JAL_WB;
        JAL_WB:       following_state = FETCH;
        JALR_WAIT:    following_state = JALR_CALC;
        JALR_CALC:    following_state = JALR_WB;
        JALR_WB:      following_state = FETCH;
        BRANCH:       following_state = FETCH;
        AUIPC_CALC:   following_state = AUIPC_WB;
        AUIPC_WB:     following_state = FETCH;
        LUI:          following_state = LUI_WB;
        LUI_WB:       following_state = FETCH;
        default:      following_state = FETCH;
    endcase
end

always @(*) begin
    pc_write      = 0;
    ir_write      = 0;
    pc_source     = 0;
    reg_write     = 0;
    memory_read   = 0;
    is_immediate  = 0;
    memory_write  = 0;
    pc_write_cond = 0;
    lorD          = 0;
    memory_to_reg = 0;
    aluop         = 2'b00;
    alu_src_a     = 2'b00;
    alu_src_b     = 2'b00;

    case (current_state)
        FETCH: begin
            alu_src_a   = 2'b00;
            alu_src_b   = 2'b01;
            memory_read = 1;
            ir_write    = 1;
            pc_write    = 1;
        end

        DECODE: begin
            alu_src_a = 2'b10;
            alu_src_b = 2'b10;
        end

        MEMADR: begin
            alu_src_a = 2'b01;
            alu_src_b = 2'b10;
        end

        MEMREAD: begin
            memory_read = 1;
            lorD        = 1;
        end

        MEMWRITE: begin
            memory_write = 1;
            lorD         = 1;
        end

        MEMWB: begin
            memory_to_reg = 1;
            reg_write     = 1;
        end

        EXECUTER: begin
            alu_src_a = 2'b01;
            alu_src_b = 2'b00;
            aluop     = 2'b10;
        end

        EXECUTEI: begin
            alu_src_a    = 2'b01;
            alu_src_b    = 2'b10;
            aluop        = 2'b10;
            is_immediate = 1;
        end

        ALUWB: begin
            reg_write     = 1;
            memory_to_reg = 0;
        end

        JAL_CALC: begin
            alu_src_a = 2'b10;
            alu_src_b = 2'b01;
            pc_source = 1;
            pc_write  = 1;
        end

        JAL_WB: begin
            memory_to_reg = 0;
            reg_write     = 1;
        end

        JALR_WAIT: begin
            alu_src_a = 2'b01;
            alu_src_b = 2'b10;
            aluop     = 2'b00;
        end

        JALR_CALC: begin
            alu_src_a    = 2'b10;
            alu_src_b    = 2'b01;
            aluop        = 2'b00;
            pc_source    = 1;
            pc_write     = 1;
            is_immediate = 1;
        end

        JALR_WB: begin
            memory_to_reg = 0;
            reg_write     = 1;
        end

        BRANCH: begin
            alu_src_a     = 2'b01;
            alu_src_b     = 2'b00;
            aluop         = 2'b01;
            pc_write_cond = 1;
            pc_source     = 1;
        end

        AUIPC_CALC: begin
            alu_src_b = 2'b10;
            alu_src_a = 2'b10;
        end

        AUIPC_WB: begin
            memory_to_reg = 0;
            reg_write     = 1;
        end

        LUI: begin
            alu_src_b = 2'b10;
            alu_src_a = 2'b11;
        end

        LUI_WB: begin
            memory_to_reg = 0;
            reg_write     = 1;
        end
    endcase
end

endmodule