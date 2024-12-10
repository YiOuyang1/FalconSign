`include "falconsoar_pkg.sv"

module fft_format_transfer_top
    import falconsoar_pkg::*;
#(
    parameter DW = 128  // the data size of a complex number
)
(
    input                  clk        ,
    input                  rst_n      ,
    exec_operator_if.slave task_itf   ,

    mem_inst_if.master_rd  buff_rd [4], 
    mem_inst_if.master_wr  buff_wr [4]
);
   
    localparam FORMAT_TRANS_DELAY = 5;

    localparam bit [4:0] TRANS_TO_FFT     = 5'b000_11;
    localparam bit [4:0] TRANS_TO_NORMAL  = 5'b001_11;
    
    //configure control signal

    wire  input_position_set = task_itf.input_task[4]; //0:positive; 1:negetive
    wire output_position_set = task_itf.input_task[5]; //0:positive; 1:negetive

    mem_addr_t src0, src1, dst0, len;

    wire rd_addr_vld, wr_addr_vld;
    mem_addr_t rd_addr0, wr_addr0;
    
    assign src0 = task_itf.input_task[TASK_REDUCE_BW - 0*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 1*MEM_ADDR_BITS]; 
    assign src1 = task_itf.input_task[TASK_REDUCE_BW - 1*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 2*MEM_ADDR_BITS]; 
    assign dst0 = task_itf.input_task[TASK_REDUCE_BW - 2*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 3*MEM_ADDR_BITS]; 
    assign len  = task_itf.input_task[TASK_REDUCE_BW - 3*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 4*MEM_ADDR_BITS]; 

    wire task_type = (task_itf.input_task[10:6] ==  TRANS_TO_NORMAL) ? 1: 0; //0 : trans to fft; 1: trans to normal

    //instance
    logic [FORMAT_TRANS_DELAY:1] start_d;  

    pipline_delay_bit #(.R(1),.P(1),.D(FORMAT_TRANS_DELAY)) u_dly_0 (.c(clk),.r(rst_n),.i(task_itf.start),.o(start_d)); // write depend on fpu pipeline stages

    wire rd_start = start_d[1];
    wire wr_start = start_d[FORMAT_TRANS_DELAY];

    //read pyhsical addr and enable generation block 
    wire rd_swap = (^rd_addr0) ^ input_position_set ;
    wire wr_swap = (^wr_addr0) ^ output_position_set;
    mem_addr_t rd_addr_mode_a[2] , rd_addr_mode_b[2];  //a:order, b:non conflict
    mem_addr_t wr_addr_mode_a[2] , wr_addr_mode_b[2];  //a:order, b:non conflict

    for(genvar i=0; i<2; i++) begin:gen_buff_rd_addr
        assign rd_addr_mode_a[i] = src0 + rd_addr0 + i*BANK_DEPTH ;
        assign rd_addr_mode_b[i] = src0 + (rd_addr0/2) + i*BANK_DEPTH + rd_swap*BANK_DEPTH*2;

        assign buff_rd[i].en = rd_addr_vld;
        assign buff_rd[i].addr = task_type ? rd_addr_mode_b[i] : rd_addr_mode_a[i];
    end

    //write pyhsical addr and enable generation block 
    for(genvar j=0; j<2; j++) begin:gen_buff_wr_addr
        assign wr_addr_mode_a[j] = dst0 + wr_addr0 + j*BANK_DEPTH ;
        assign wr_addr_mode_b[j] = dst0 + (wr_addr0/2) + j*BANK_DEPTH + wr_swap*BANK_DEPTH*2;

        assign buff_wr[j].en = wr_addr_vld;
        assign buff_wr[j].addr = task_type ? wr_addr_mode_a[j] : wr_addr_mode_b[j];
    end

    //instance
    addr_gen u_rd_fsm
    (
        .clk,
        .rst_n,
        .start       (rd_start   ),
        .max_len     (len        ),    
        .done        (           ),
        .rd_addr_vld (rd_addr_vld),
        .rd_addr0    (rd_addr0   ),
        .wr_addr_vld (           ),
        .wr_addr0    (           )
    );

    wire wr_done;

    assign task_itf.op_done = wr_done;

    addr_gen u_wr_fsm
    (
        .clk,
        .rst_n,
        .start       (wr_start   ),
        .max_len     (len        ),    
        .done        (wr_done    ),
        .rd_addr_vld (           ),
        .rd_addr0    (           ),
        .wr_addr_vld (wr_addr_vld),
        .wr_addr0    (wr_addr0   )
    );

    format_transfer u_format_transfer
    (
        .task_type ( task_type       ),
        .data_in_0 ( buff_rd[0].data ),
        .data_in_1 ( buff_rd[1].data ),
        .data_out_0( buff_wr[0].data ),
        .data_out_1( buff_wr[1].data )
    );

endmodule

module addr_gen
    import falconsoar_pkg::*;
(
    input         clk,
    input         rst_n,
    input         start,
    input  [MEM_ADDR_BITS - 1:0] max_len,      // max value: 512
    output        done,
    output        rd_addr_vld,
    output [MEM_ADDR_BITS - 1:0] rd_addr0,
    output        wr_addr_vld,
    output [MEM_ADDR_BITS - 1:0] wr_addr0
);

    wire cnt_start = start;
    wire [MEM_ADDR_BITS - 1:0] end_num = max_len;
    wire cnt_run = 1'b1;
    wire cnt_en;
    wire cnt_done;
    wire [MEM_ADDR_BITS - 1:0] cnt;

    assign rd_addr_vld = cnt_en;
    assign wr_addr_vld = cnt_en;
    assign rd_addr0 = cnt;
    assign wr_addr0 = cnt;
    assign done = cnt_done;

    counter_ce #(
        .BW(MEM_ADDR_BITS)
    ) u_counter_ce  // counter with configurable end number
    (
        .clk   ,
        .rst_n ,
        .start (cnt_start),
        .num   (end_num),
        .run   (cnt_run),
        .cnt   (cnt),
        .cnt_en(cnt_en),
        .last  (),
        .done  (cnt_done)
    );



endmodule

module format_transfer
    import falconsoar_pkg::*;
#(
    parameter BW = 64  // the data size of a float number
) (
    input                task_type, //0 : trans to fft; 1: trans to normal
    input  [4*BW - 1: 0] data_in_0,
    input  [4*BW - 1: 0] data_in_1,
    output [4*BW - 1: 0] data_out_0,
    output [4*BW - 1: 0] data_out_1
);

    logic [4*BW - 1: 0] data_out_0_tmp[2];
    logic [4*BW - 1: 0] data_out_1_tmp[2];

    assign data_out_0_tmp[0] = {data_in_1[BW*2-1 : BW*1], data_in_0[BW*2-1 : BW*1], data_in_1[BW*1-1 : BW*0], data_in_0[BW*1-1 : BW*0]};
    assign data_out_1_tmp[0] = {data_in_1[BW*4-1 : BW*3], data_in_0[BW*4-1 : BW*3], data_in_1[BW*3-1 : BW*2], data_in_0[BW*3-1 : BW*2]};

    assign data_out_0_tmp[1] = {data_in_1[BW*3-1 : BW*2], data_in_1[BW*1-1 : BW*0], data_in_0[BW*3-1 : BW*2], data_in_0[BW*1-1 : BW*0]};
    assign data_out_1_tmp[1] = {data_in_1[BW*4-1 : BW*3], data_in_1[BW*2-1 : BW*1], data_in_0[BW*4-1 : BW*3], data_in_0[BW*2-1 : BW*1]};  

    assign data_out_0 = (task_type == 'd1)? data_out_0_tmp[1] : data_out_0_tmp[0];
    assign data_out_1 = (task_type == 'd1)? data_out_1_tmp[1] : data_out_1_tmp[0];

endmodule

