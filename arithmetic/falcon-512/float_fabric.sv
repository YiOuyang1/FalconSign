`include "falconsoar_pkg.sv"
`include "float_pkg.sv"

module float_fabric
    import falconsoar_pkg::*;
    import float_pkg::*;
(
    input                   clk               ,
    input                   rst_n             ,
    exec_operator_if.slave  task_itf          ,
    exec_operator_if.master task_itf_inter [5],
    fpu_if.master           fpu_itf           ,
    output logic            fpu_en            ,
    fpu_if.slave            fpu_itf_inter  [4],

    mem_inst_if.slave_rd    buff_rd_fft    [6],
    mem_inst_if.slave_wr    buff_wr_fft    [4],

    mem_inst_if.slave_rd    buff_rd_spl    [6],
    mem_inst_if.slave_wr    buff_wr_spl    [4],

    mem_inst_if.slave_rd    buff_rd_fpr    [4],
    mem_inst_if.slave_wr    buff_wr_fpr    [4],

    mem_inst_if.slave_rd    buff_rd_fpc    [4],
    mem_inst_if.slave_wr    buff_wr_fpc    [4],

    mem_inst_if.slave_rd    buff_rd_fmt    [4],
    mem_inst_if.slave_wr    buff_wr_fmt    [4],
    
    mem_inst_if.master_rd   mem_rd        [4],
    mem_inst_if.master_wr   mem_wr        [4],

    mem_inst_if.master_rd   bank_rd        [2]
);

    logic [2:0] task_type;

    always_comb begin
        task_type = 'x;
        case(task_itf.input_task[10:6])
        5'b000_00,
        5'b001_00: task_type = 3'd0; // FFT
        5'b010_00,
        5'b011_00,
        5'b100_00,
        5'b101_00: task_type = 3'd1; // SPLIT
        5'b000_01,
        5'b001_01,
        5'b010_01,
        5'b011_01,
        5'b100_01: task_type = 3'd2; // FPR
        5'b000_10,
        5'b001_10,
        5'b010_10,
        5'b011_10,
        5'b100_10: task_type = 3'd3; // FPC
        5'b000_11,
        5'b001_11: task_type = 3'd4; // FORMAT   
        endcase
    end

    always_ff @(posedge clk, negedge rst_n) begin
        if     (!rst_n)           fpu_en <= 1'b0;
        else if(task_itf.start)   fpu_en <= 1'b1;
        else if(task_itf.op_done) fpu_en <= 1'b0;
    end

    for(genvar i=0;i<5;i++) begin:gen_task_itf_inter
        assign task_itf_inter[i].start = task_itf.start & (task_type == i);
        assign task_itf_inter[i].input_task = task_itf.input_task;
    end

    logic [4:0] task_op_done ;

    for(genvar i=0; i<5; i++) begin:gen_task_output
        assign task_op_done [i] = task_itf_inter[i].op_done ;
    end

    assign task_itf.op_done           = |task_op_done ;

    for(genvar i=0; i<4; i++) begin:gen_buff_rd_fabric_0
        always_comb begin
            unique case(1'b1)
            buff_rd_fft[i].en:begin mem_rd[i].en = 1'b1; mem_rd[i].addr = buff_rd_fft[i].addr; end
            buff_rd_spl[i].en:begin mem_rd[i].en = 1'b1; mem_rd[i].addr = buff_rd_spl[i].addr; end
            buff_rd_fpr[i].en:begin mem_rd[i].en = 1'b1; mem_rd[i].addr = buff_rd_fpr[i].addr; end
            buff_rd_fpc[i].en:begin mem_rd[i].en = 1'b1; mem_rd[i].addr = buff_rd_fpc[i].addr; end
            buff_rd_fmt[i].en:begin mem_rd[i].en = 1'b1; mem_rd[i].addr = buff_rd_fmt[i].addr; end
            default          :begin mem_rd[i].en = 1'b0; mem_rd[i].addr = 'x;                  end
            endcase
        end

        assign buff_rd_fft[i].data = mem_rd[i].data;
        assign buff_rd_spl[i].data = mem_rd[i].data;
        assign buff_rd_fpr[i].data = mem_rd[i].data;
        assign buff_rd_fpc[i].data = mem_rd[i].data;
        assign buff_rd_fmt[i].data = mem_rd[i].data;
    end

    for(genvar i=4; i<6; i++) begin:gen_buff_rd_fabric
        always_comb begin
            unique case(1'b1)
            buff_rd_fft[i].en:begin bank_rd[i-4].en = 1'b1; bank_rd[i-4].addr = mem_addr_t'(buff_rd_fft[i].addr); end
            buff_rd_spl[i].en:begin bank_rd[i-4].en = 1'b1; bank_rd[i-4].addr = mem_addr_t'(buff_rd_spl[i].addr); end
            default          :begin bank_rd[i-4].en = 1'b0; bank_rd[i-4].addr = 'x;                               end
            endcase
        end

        assign buff_rd_fft[i].data = bank_rd[i-4].data;
        assign buff_rd_spl[i].data = bank_rd[i-4].data;
    end

    for(genvar i=0; i<4; i++) begin:gen_buff_wr_fabric
        always_comb begin
            unique case(1'b1)
            buff_wr_fft[i].en:begin mem_wr[i].en = 1'b1; mem_wr[i].addr = buff_wr_fft[i].addr; mem_wr[i].data = buff_wr_fft[i].data; end
            buff_wr_spl[i].en:begin mem_wr[i].en = 1'b1; mem_wr[i].addr = buff_wr_spl[i].addr; mem_wr[i].data = buff_wr_spl[i].data; end
            buff_wr_fpr[i].en:begin mem_wr[i].en = 1'b1; mem_wr[i].addr = buff_wr_fpr[i].addr; mem_wr[i].data = buff_wr_fpr[i].data; end
            buff_wr_fpc[i].en:begin mem_wr[i].en = 1'b1; mem_wr[i].addr = buff_wr_fpc[i].addr; mem_wr[i].data = buff_wr_fpc[i].data; end
            buff_wr_fmt[i].en:begin mem_wr[i].en = 1'b1; mem_wr[i].addr = buff_wr_fmt[i].addr; mem_wr[i].data = buff_wr_fmt[i].data; end
            default          :begin mem_wr[i].en = 1'b0; mem_wr[i].addr = 'x;                  mem_wr[i].data = 'x;                  end
            endcase
        end
    end

    always_comb begin
        case(task_type)
        2'd0: begin fpu_itf.mode = fpu_itf_inter[0].mode; fpu_itf.d_i = fpu_itf_inter[0].d_i; end
        2'd1: begin fpu_itf.mode = fpu_itf_inter[1].mode; fpu_itf.d_i = fpu_itf_inter[1].d_i; end
        2'd2: begin fpu_itf.mode = fpu_itf_inter[2].mode; fpu_itf.d_i = fpu_itf_inter[2].d_i; end
        2'd3: begin fpu_itf.mode = fpu_itf_inter[3].mode; fpu_itf.d_i = fpu_itf_inter[3].d_i; end
        endcase
    end

    for(genvar i=0; i<4; i++) begin:gen_fpu_itf_data
        assign fpu_itf_inter[i].d_o = fpu_itf.d_o;
    end

endmodule
