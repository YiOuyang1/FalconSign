`include "falconsoar_pkg.sv"

module fpc_top
    import falconsoar_pkg::*;
#(
    parameter DW = 128  // the data size of a complex number
)
(
    input                  clk        ,
    input                  rst_n      ,
    exec_operator_if.slave task_itf   ,
    fpu_if.master          fpu_itf    ,
   
    mem_inst_if.master_rd  buff_rd [4], 
    mem_inst_if.master_wr  buff_wr [4]
);

    localparam bit [4:0] ADD_FOUR   = 5'b10_000;
    localparam bit [4:0] ADD_DOUBLE = 5'b01_000;
    localparam bit [4:0] ADD_SINGLE = 5'b00_000;
    localparam bit [4:0] SUB_FOUR   = 5'b10_001;
    localparam bit [4:0] SUB_DOUBLE = 5'b01_001;
    localparam bit [4:0] SUB_SINGLE = 5'b00_001;
    localparam bit [4:0] MUL_FOUR   = 5'b10_010;
    localparam bit [4:0] MUL_DOUBLE = 5'b01_010;
    localparam bit [4:0] MUL_SINGLE = 5'b00_010;
    localparam bit [4:0] ADJ_FOUR   = 5'b10_011;
    localparam bit [4:0] ADJ_DOUBLE = 5'b01_011;
    localparam bit [4:0] ADJ_SINGLE = 5'b00_011;
    localparam bit [4:0] SQR_FOUR   = 5'b10_100;
    localparam bit [4:0] SQR_DOUBLE = 5'b01_100;
    localparam bit [4:0] SQR_SINGLE = 5'b00_100;

    localparam bit [3:0] FPC_ADD = 4'd5;
    localparam bit [3:0] FPC_SUB = 4'd6;
    localparam bit [3:0] FPC_MUL = 4'd7;
    localparam bit [3:0] FPC_ADJ = 4'd8;
    localparam bit [3:0] FPC_SQR = 4'd9;

    //configure control signal
    mem_addr_t src0; assign src0 = task_itf.input_task[TASK_REDUCE_BW - 0*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 1*MEM_ADDR_BITS]; // src0
    mem_addr_t src1; assign src1 = task_itf.input_task[TASK_REDUCE_BW - 1*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 2*MEM_ADDR_BITS]; // src1
    mem_addr_t dst0; assign dst0 = task_itf.input_task[TASK_REDUCE_BW - 2*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 3*MEM_ADDR_BITS]; // dst0
    mem_addr_t len ; assign len  = task_itf.input_task[TASK_REDUCE_BW - 3*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 4*MEM_ADDR_BITS]; // the actul size of every bank used in task

    wire [2:0] task_type = task_itf.input_task[10:8];  // 000:ADD; 001:SUB; 010:MUL; 011:ADJ; 100:SQR , fixed by oyy
    wire  input_position_set = task_itf.input_task[4]; //0:positive; 1:negetive
    wire output_position_set = task_itf.input_task[5]; //0:positive; 1:negetive
    wire [1:0] bank_type = task_itf.input_task[3:2];    //00:single; 01:double; 10:four

    // read addr and enable signal generation
    logic addr_vld [2];
    mem_addr_t addr [3];

    assign buff_rd[0].en = addr_vld[0];
    assign buff_rd[1].en = addr_vld[0] & (bank_type != 2'b00); // NOT work in single mode
    assign buff_rd[2].en = addr_vld[0] & ((task_type == 3'b000)|(task_type == 3'b001)|(task_type == 3'b010)); // Only work in ADD, SUB, MUL, fixed by oyy
    assign buff_rd[3].en = addr_vld[0] & (bank_type != 2'b00) & ((task_type == 3'b000)|(task_type == 3'b001)|(task_type == 3'b010)); // NOT work in single mode

    assign buff_rd[0].addr = addr[0];
    assign buff_rd[1].addr = addr[0] + BANK_DEPTH;
    assign buff_rd[2].addr = addr[1];
    assign buff_rd[3].addr = addr[1] + BANK_DEPTH;

    assign buff_wr[0].en = addr_vld[1] &  (task_type == 3'd0);  //  only in ADD mode
    assign buff_wr[1].en = addr_vld[1] &  (task_type == 3'd0) & ((bank_type == 2'b01)|(bank_type == 2'b10));  // work in ADD double , four mode
    assign buff_wr[2].en = addr_vld[1] & ((task_type == 3'd1)|(task_type == 3'd2)|(task_type == 3'd3)|(task_type == 3'd4));  // work in SUB MUL ADJ, SQR mode
    assign buff_wr[3].en = addr_vld[1] & ((task_type == 3'd1)|(task_type == 3'd2)|(task_type == 3'd3)|(task_type == 3'd4))
                                & ((bank_type == 2'b01)|(bank_type == 2'b10));  // work in SUB MUL ADJ, SQR and DOUBLE FOUR mode

    assign buff_wr[0].addr = addr[2];
    assign buff_wr[1].addr = addr[2] + BANK_DEPTH;
    assign buff_wr[2].addr = addr[2];
    assign buff_wr[3].addr = addr[2] + BANK_DEPTH;

    //net to external fpu for compute

    always_comb begin
        fpu_itf.mode = 'x;
        case (task_type)
        3'b000: fpu_itf.mode = FPC_ADD;
        3'b001: fpu_itf.mode = FPC_SUB;
        3'b010: fpu_itf.mode = FPC_MUL;
        3'b011: fpu_itf.mode = FPC_ADJ;
        3'b100: fpu_itf.mode = FPC_SQR;
        endcase
    end

    assign fpu_itf.d_i[0] = {buff_rd[1].data,buff_rd[0].data};
    assign fpu_itf.d_i[1] = ((task_type == 3'b100)|(task_type == 3'b011))? {buff_rd[1].data,buff_rd[0].data} :{buff_rd[3].data,buff_rd[2].data}; //ADJ,SQR means two same input
    assign fpu_itf.d_i[2] = {buff_rd[1].data,buff_rd[0].data};

    assign {buff_wr[1].data,buff_wr[0].data} = fpu_itf.d_o[0];
    assign {buff_wr[3].data,buff_wr[2].data} = fpu_itf.d_o[1];

    //instance
    wire [WR_DELAY:1] start_d;

    pipline_delay_bit #(.R(1),.P(1),.D(WR_DELAY)) u_dly_0 (.c(clk),.r(rst_n),.i(task_itf.start),.o(start_d)); // write depend on fpu pipeline stages

    wire rd_start = start_d[1];
    wire wr_start = start_d[WR_DELAY];

    fpc_addr_gen u_rd_fsm (
        .clk                ,
        .rst_n              ,
        .start              (   rd_start    ), // i
        .input_position_set (input_position_set),  // i
        .output_position_set(output_position_set), // i 
        .bank_type          (   bank_type   ), // i 00:single; 01:double; 10:four
        .max_len            (   len         ), // i [MEM_ADDR_BITS - 1:0] max value: 128
        .src0               (   src0        ), // i [MEM_ADDR_BITS - 1:0] base addr of first oprands
        .src1               (   src1        ), // i [MEM_ADDR_BITS - 1:0] base addr of second oprands
        .dst0               (   dst0        ), // i [MEM_ADDR_BITS - 1:0] base addr of result
        .done               (               ), // o
        .addr_vld_01        (   addr_vld[0] ), // o
        .addr0              (   addr[0]     ), // o [MEM_ADDR_BITS - 1:0]
        .addr1              (   addr[1]     ), // o [MEM_ADDR_BITS - 1:0]
        .addr_vld_2         (               ), // o
        .addr2              (               )  // o [MEM_ADDR_BITS - 1:0]
    );

    fpc_addr_gen u_wr_fsm (
        .clk                 ,
        .rst_n               ,
        .start               (   wr_start            ), // i
        .input_position_set (input_position_set),  // i
        .output_position_set(output_position_set), // i 
        .bank_type          (   bank_type           ), // i 00:single; 01:double; 10:four
        .max_len            (   len                 ), // i [MEM_ADDR_BITS - 1:0] max value: 128
        .src0               (   src0                ), // i [MEM_ADDR_BITS - 1:0] base addr of first oprands
        .src1               (   src1                ), // i [MEM_ADDR_BITS - 1:0] base addr of second oprands
        .dst0               (   dst0                ), // i [MEM_ADDR_BITS - 1:0] base addr of result
        .done               (   task_itf.op_done    ), // o
        .addr_vld_01        (                       ), // o
        .addr0              (                       ), // o [MEM_ADDR_BITS - 1:0]
        .addr1              (                       ), // o [MEM_ADDR_BITS - 1:0]
        .addr_vld_2         (   addr_vld[1]         ), // o
        .addr2              (   addr[2]             )  // o [MEM_ADDR_BITS - 1:0]
    );

endmodule

module fpc_addr_gen 
    import falconsoar_pkg::*;
(
    input             clk,
    input             rst_n,
    input             start,
    input             input_position_set,
    input             output_position_set,
    input      [1:0]  bank_type,    // 00:single; 01:double; 10:four
    input      [MEM_ADDR_BITS - 1:0] max_len,      // max value: 128
    input      [MEM_ADDR_BITS - 1:0] src0,         // base addr of first oprands
    input      [MEM_ADDR_BITS - 1:0] src1,         // base addr of second oprands
    input      [MEM_ADDR_BITS - 1:0] dst0,         // base addr of result
    output            done,
    output            addr_vld_01,
    output reg [MEM_ADDR_BITS - 1:0] addr0,
    output reg [MEM_ADDR_BITS - 1:0] addr1,
    output            addr_vld_2,
    output reg [MEM_ADDR_BITS - 1:0] addr2
);

    localparam ADD_FOUR = 5'b10_000;
    localparam SUB_FOUR = 5'b10_001;
    localparam MUL_FOUR = 5'b10_010;
    localparam ADD_DOUBLE = 5'b01_000;
    localparam SUB_DOUBLE = 5'b01_001;
    localparam MUL_DOUBLE = 5'b01_010;
    localparam ADD_SINGLE = 5'b00_000;
    localparam SUB_SINGLE = 5'b00_001;
    localparam MUL_SINGLE = 5'b00_010;

    wire start_four = (bank_type == 2'b10) & start;
    wire start_single_double = ((bank_type == 2'b01) | (bank_type == 2'b00)) & start;


    wire done_four, done_single_double;
    wire rd_addr_vld_four, rd_addr_vld_single_double;
    wire wr_addr_vld_four, wr_addr_vld_single_double;

    wire [MEM_ADDR_BITS - 1:0] rd_addr0_four, rd_addr0_single_double;
    wire [MEM_ADDR_BITS - 1:0] rd_addr1_four, rd_addr1_single_double;
    wire [MEM_ADDR_BITS - 1:0] wr_addr0_four, wr_addr0_single_double;


    assign done = done_four | done_single_double;
    assign addr_vld_01 = rd_addr_vld_four | rd_addr_vld_single_double;
    assign addr_vld_2 = wr_addr_vld_four | wr_addr_vld_single_double;

    always_comb begin
        addr0 = 'x;
        addr1 = 'x;
        addr2 = 'x;
        case(bank_type)
        2'b10: begin
            addr0 = rd_addr0_four;
            addr1 = rd_addr1_four;
            addr2 = wr_addr0_four;
        end
        2'b01, 2'b00: begin
            addr0 = rd_addr0_single_double;
            addr1 = rd_addr1_single_double;
            addr2 = wr_addr0_single_double;
        end
        endcase
    end

    ///////////////////////////////////////
    // instance
    fpc_addr_gen_four_bank u_fpc_addr_gen_four_bank (
        .clk                ,
        .rst_n              ,
        .start              (start_four),         // input
        .input_position_set (input_position_set), // input
        .output_position_set(output_position_set),// input 
        .max_len            (max_len),           // input  [MEM_ADDR_BITS - 1:0]  // max value: 128
        .src0               (src0),              // input  [MEM_ADDR_BITS - 1:0]  // base addr of first oprands
        .src1               (src1),              // input  [MEM_ADDR_BITS - 1:0]  // base addr of second oprands
        .dst0               (dst0),              // input  [MEM_ADDR_BITS - 1:0]  // base addr of result
        .done               (done_four),         // output
        .rd_addr_vld        (rd_addr_vld_four),  // output
        .rd_addr0           (rd_addr0_four),     // output [MEM_ADDR_BITS - 1:0]
        .rd_addr1           (rd_addr1_four),     // output [MEM_ADDR_BITS - 1:0]
        .wr_addr_vld        (wr_addr_vld_four),  // output
        .wr_addr0           (wr_addr0_four)      // output [MEM_ADDR_BITS - 1:0]
    );

    fpc_addr_gen_single_double_bank u_fpc_addr_gen_single_double_bank (
        .clk                ,
        .rst_n              ,
        .start              (start_single_double),        // input 
        .max_len            (max_len),                    // input  [MEM_ADDR_BITS - 1:0]  // max value: 128
        .src0               (src0),                       // input  [MEM_ADDR_BITS - 1:0]  // base addr of first oprands
        .src1               (src1),                       // input  [MEM_ADDR_BITS - 1:0]  // base addr of second oprands
        .dst0               (dst0),                       // input  [MEM_ADDR_BITS - 1:0]  // base addr of result
        .done               (done_single_double),         // output
        .rd_addr_vld        (rd_addr_vld_single_double),  // output
        .rd_addr0           (rd_addr0_single_double),     // output [MEM_ADDR_BITS - 1:0]
        .rd_addr1           (rd_addr1_single_double),     // output [MEM_ADDR_BITS - 1:0]
        .wr_addr_vld        (wr_addr_vld_single_double),  // output
        .wr_addr0           (wr_addr0_single_double)      // output [MEM_ADDR_BITS - 1:0]
    );
endmodule

module fpc_addr_gen_four_bank 
    import falconsoar_pkg::*;
(
    input         clk,
    input         rst_n,
    input         start,
    input         input_position_set,
    input         output_position_set,
    input  [MEM_ADDR_BITS - 1:0] max_len,      // max value: 512
    input  [MEM_ADDR_BITS - 1:0] src0,         // base addr of first oprands
    input  [MEM_ADDR_BITS - 1:0] src1,         // base addr of second oprands
    input  [MEM_ADDR_BITS - 1:0] dst0,         // base addr of result
    output        done,
    output        rd_addr_vld,
    output [MEM_ADDR_BITS - 1:0] rd_addr0,
    output [MEM_ADDR_BITS - 1:0] rd_addr1,
    output        wr_addr_vld,
    output [MEM_ADDR_BITS - 1:0] wr_addr0
);

    wire cnt_start = start;
    wire [MEM_ADDR_BITS - 1:0] end_num = max_len * 2;
    wire cnt_run = 1'b1;
    wire cnt_en;
    wire cnt_done;
    wire [MEM_ADDR_BITS - 1:0] cnt;

    assign rd_addr_vld = cnt_en;
    assign wr_addr_vld = cnt_en;
    assign rd_addr0 = cnt[0] ^ input_position_set  ? (src0 + {1'b0, cnt[MEM_ADDR_BITS - 1:1]} + BANK_DEPTH*2) : (src0 + {1'b0, cnt[MEM_ADDR_BITS - 1:1]});
    assign rd_addr1 = cnt[0] ^ input_position_set  ? (src1 + {1'b0, cnt[MEM_ADDR_BITS - 1:1]})                : (src1 + {1'b0, cnt[MEM_ADDR_BITS - 1:1]} + BANK_DEPTH*2);
    assign wr_addr0 = cnt[0] ^ output_position_set ? (dst0 + {1'b0, cnt[MEM_ADDR_BITS - 1:1]} + BANK_DEPTH*2) : (dst0 + {1'b0, cnt[MEM_ADDR_BITS - 1:1]});
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

module fpc_addr_gen_single_double_bank 
    import falconsoar_pkg::*;
(
    input         clk,
    input         rst_n,
    input         start,
    input  [MEM_ADDR_BITS - 1:0] max_len,      // max value: 512
    input  [MEM_ADDR_BITS - 1:0] src0,         // base addr of first oprands
    input  [MEM_ADDR_BITS - 1:0] src1,         // base addr of second oprands
    input  [MEM_ADDR_BITS - 1:0] dst0,         // base addr of result
    output        done,
    output        rd_addr_vld,
    output [MEM_ADDR_BITS - 1:0] rd_addr0,
    output [MEM_ADDR_BITS - 1:0] rd_addr1,
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
    assign rd_addr0 =  (src0 + cnt) ;
    assign rd_addr1 =  (src1 + cnt) ;
    assign wr_addr0 =  (dst0 + cnt) ;
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

