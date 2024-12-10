`include "falconsoar_pkg.sv"

module fpr_top
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
    localparam bit [4:0] SUB_FOUR   = 5'b10_001;
    localparam bit [4:0] MUL_FOUR   = 5'b10_010;
    localparam bit [4:0] ADD_DOUBLE = 5'b01_000;
    localparam bit [4:0] SUB_DOUBLE = 5'b01_001;
    localparam bit [4:0] MUL_DOUBLE = 5'b01_010;
    localparam bit [4:0] ADD_SINGLE = 5'b00_000;
    localparam bit [4:0] SUB_SINGLE = 5'b00_001;
    localparam bit [4:0] MUL_SINGLE = 5'b00_010;

    localparam bit [3:0] FPR_ADD = 4'd2;
    localparam bit [3:0] FPR_SUB = 4'd3;
    localparam bit [3:0] FPR_MUL = 4'd4;

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
    wire        addr_vld_01;
    wire [MEM_ADDR_BITS - 1:0] addr0;
    wire [MEM_ADDR_BITS - 1:0] addr1;

    assign buff_rd[0].en = addr_vld_01;
    assign buff_rd[1].en = addr_vld_01 & (~((bank_type == 2'b00)|(task_type == 3'b010)));  // NOT work in single mode and all FPR MUL mode
    assign buff_rd[2].en = addr_vld_01;
    assign buff_rd[3].en = addr_vld_01 & (~((bank_type == 2'b00)|(task_type == 3'b010)));  // NOT work in single mode and all FPR MUL mode

    assign buff_rd[0].addr = addr0;
    assign buff_rd[1].addr = addr0 + BANK_DEPTH;
    assign buff_rd[2].addr = addr1;
    assign buff_rd[3].addr = addr1 + BANK_DEPTH;

    //write sddr and enable signal generation
    wire        addr_vld_2;
    wire [MEM_ADDR_BITS - 1:0] addr2;

    assign buff_wr[0].en = addr_vld_2 & (task_type == 3'b000);                                                  //  only in ADD mode
    assign buff_wr[1].en = addr_vld_2 & (task_type == 3'b000) & ((bank_type == 2'b01)|(bank_type == 2'b10));  //  only in ADD double and four mode
    assign buff_wr[2].en = addr_vld_2 & ((task_type == 3'b001) | (task_type == 3'b010));                       // only in SUB, MUL mode
    assign buff_wr[3].en = addr_vld_2 & (task_type == 3'b001) & ((bank_type == 2'b01)|(bank_type == 2'b10)); // only in SUB double and four mode


    assign buff_wr[0].addr = addr2;
    assign buff_wr[1].addr = addr2 + BANK_DEPTH;
    assign buff_wr[2].addr = addr2;
    assign buff_wr[3].addr = addr2 + BANK_DEPTH;

    //net to external fpu for compute
    wire [DW*4-1:0] fpu_mul_op0, fpu_mul_op1; // only work in fpr mul task
    wire [DW*2-1:0] fpu_fpr_mul_result;       //

    assign fpu_itf.mode = (task_type == 3'b000)? FPR_ADD: ((task_type == 3'b001)? FPR_SUB:FPR_MUL);

    for(genvar i=0; i<4; i++) begin:mul_net
        assign fpu_mul_op0[128*i+:128] = {64'b0,buff_rd[0].data[64*i+:64]};
        assign fpu_mul_op1[128*i+:128] = {64'b0,buff_rd[2].data[64*i+:64]};
        assign fpu_fpr_mul_result[64*i+:64] = fpu_itf.d_o[1][128*i+:64];
    end

    assign fpu_itf.d_i[0] = {buff_rd[1].data,buff_rd[0].data};
    assign fpu_itf.d_i[1] = (task_type == 3'b010)? fpu_mul_op0: {buff_rd[3].data,buff_rd[2].data};
    assign fpu_itf.d_i[2] = fpu_mul_op1;

    assign {buff_wr[1].data,buff_wr[0].data} = fpu_itf.d_o[0];
    assign buff_wr[2].data = (task_type == 3'b010)? fpu_fpr_mul_result: fpu_itf.d_o[1][0+:DW*2]; // only choose fpu_fpr_mul_result in fpr mul task
    assign buff_wr[3].data = fpu_itf.d_o[1][DW*2+:DW*2];

    //instance
    wire [WR_DELAY:1] start_d;

    pipline_delay_bit #(.R(1),.P(1),.D(WR_DELAY)) u_start_dly (.c(clk),.r(rst_n),.i(task_itf.start),.o(start_d)); // write depend on fpu pipeline stages

    wire rd_start = start_d[1];
    wire wr_start = start_d[WR_DELAY];
    wire wr_done;

    assign task_itf.op_done = wr_done ;

    fpr_addr_gen u_rd_fsm
    (
        .clk                ,
        .rst_n              ,
        .start              ( rd_start    ) ,// input
        .input_position_set (input_position_set), // input
        .output_position_set(output_position_set),// input 
        .bank_type          ( bank_type   ) ,// input      [1:0]  // 00:single; 01:double; 10:four
        .task_type          ( task_type   ) ,// input      [2:0]  // 000:ADD; 001:SUB; 010:MUL
        .max_len            ( len         ) ,// input      [MEM_ADDR_BITS - 1:0]  // max value: 128
        .src0               ( src0        ) ,// input      [MEM_ADDR_BITS - 1:0]  // base addr of first oprands
        .src1               ( src1        ) ,// input      [MEM_ADDR_BITS - 1:0]  // base addr of second oprands
        .dst0               ( dst0        ) ,// input      [MEM_ADDR_BITS - 1:0]  // base addr of result
        .done               (             ) ,// output
        .addr_vld_01        ( addr_vld_01 ) ,// output
        .addr0              ( addr0       ) ,// output reg [MEM_ADDR_BITS - 1:0]
        .addr1              ( addr1       ) ,// output reg [MEM_ADDR_BITS - 1:0]
        .addr_vld_2         (             ) ,// output
        .addr2              (             )  // output reg [MEM_ADDR_BITS - 1:0]
    );

    fpr_addr_gen u_wr_fsm
    (
        .clk                ,
        .rst_n              ,
        .start              (   wr_start            ) ,// i
        .input_position_set (input_position_set     ), // input
        .output_position_set(output_position_set    ),// input 
        .bank_type          (   bank_type           ) ,// i [1:0]  // 00:single; 01:double; 10:four
        .task_type          (   task_type           ) ,// i [2:0]  // 000:ADD; 001:SUB; 010:MUL
        .max_len            (   len                 ) ,// i [MEM_ADDR_BITS - 1:0]  // max value: 128
        .src0               (   src0                ) ,// i [MEM_ADDR_BITS - 1:0]  // base addr of first oprands
        .src1               (   src1                ) ,// i [MEM_ADDR_BITS - 1:0]  // base addr of second oprands
        .dst0               (   dst0                ) ,// i [MEM_ADDR_BITS - 1:0]  // base addr of result
        .done               (   wr_done             ) ,// o
        .addr_vld_01        (                       ) ,// o
        .addr0              (                       ) ,// o [MEM_ADDR_BITS - 1:0]
        .addr1              (                       ) ,// o [MEM_ADDR_BITS - 1:0]
        .addr_vld_2         (   addr_vld_2          ) ,// o
        .addr2              (   addr2               )  // o [MEM_ADDR_BITS - 1:0]
    );

endmodule

module fpr_addr_gen
    import falconsoar_pkg::*;
(
    input             clk                ,
    input             rst_n              ,
    input             start              ,
    input             input_position_set ,
    input             output_position_set,
    input      [1:0]  bank_type          , // 00:single; 01:double; 10:four
    input      [2:0]  task_type          , // 000:ADD; 001:SUB; 010:MUL
    input      [MEM_ADDR_BITS - 1:0] max_len            , // max value: 128
    input      [MEM_ADDR_BITS - 1:0] src0               , // base addr of first oprands
    input      [MEM_ADDR_BITS - 1:0] src1               , // base addr of second oprands
    input      [MEM_ADDR_BITS - 1:0] dst0               , // base addr of result
    output            done               ,
    output            addr_vld_01        ,
    output reg [MEM_ADDR_BITS - 1:0] addr0              ,
    output reg [MEM_ADDR_BITS - 1:0] addr1              ,
    output            addr_vld_2         ,
    output reg [MEM_ADDR_BITS - 1:0] addr2
);

    localparam ADD_FOUR   = 5'b10_000;
    localparam SUB_FOUR   = 5'b10_001;
    localparam MUL_FOUR   = 5'b10_010;
    localparam ADD_DOUBLE = 5'b01_000;
    localparam SUB_DOUBLE = 5'b01_001;
    localparam MUL_DOUBLE = 5'b01_010;
    localparam ADD_SINGLE = 5'b00_000;
    localparam SUB_SINGLE = 5'b00_001;
    localparam MUL_SINGLE = 5'b00_010;

    wire start_fpr_mul_four = ({bank_type, task_type} == MUL_FOUR) & start; //only MUL four bank mode
    wire start_fpr_mul_double = ({bank_type, task_type} == MUL_DOUBLE) & start;  //only MUL double bank mode
    wire start_four = (({bank_type, task_type} == ADD_FOUR) | ({bank_type, task_type} == SUB_FOUR)) & start; //only ADD SUB four bank mode
    wire start_single_double = (({bank_type, task_type} == ADD_DOUBLE) | ({bank_type, task_type} == SUB_DOUBLE) |
                               (({bank_type, task_type} == ADD_SINGLE) | ({bank_type, task_type} == SUB_SINGLE) | ({bank_type, task_type} == MUL_SINGLE)))
                               & start; //only ADD SUB  singlr and double bank mode , and MUL single bank

    wire        done_fpr_mul_four,        done_fpr_mul_double,        done_four,        done_single_double;
    wire rd_addr_vld_fpr_mul_four, rd_addr_vld_fpr_mul_double, rd_addr_vld_four, rd_addr_vld_single_double;
    wire wr_addr_vld_fpr_mul_four, wr_addr_vld_fpr_mul_double, wr_addr_vld_four, wr_addr_vld_single_double;

    wire [MEM_ADDR_BITS - 1:0] rd_addr0_fpr_mul_four, rd_addr0_fpr_mul_double, rd_addr0_four, rd_addr0_single_double;
    wire [MEM_ADDR_BITS - 1:0] rd_addr1_fpr_mul_four, rd_addr1_fpr_mul_double, rd_addr1_four, rd_addr1_single_double;
    wire [MEM_ADDR_BITS - 1:0] wr_addr0_fpr_mul_four, wr_addr0_fpr_mul_double, wr_addr0_four, wr_addr0_single_double;

    assign done =        |{       done_fpr_mul_four,       done_fpr_mul_double,       done_four,       done_single_double};
    assign addr_vld_01 = |{rd_addr_vld_fpr_mul_four,rd_addr_vld_fpr_mul_double,rd_addr_vld_four,rd_addr_vld_single_double};
    assign addr_vld_2 =  |{wr_addr_vld_fpr_mul_four,wr_addr_vld_fpr_mul_double,wr_addr_vld_four,wr_addr_vld_single_double};

    always_comb begin
        case({bank_type,task_type})
        MUL_DOUBLE: begin addr0 = rd_addr0_fpr_mul_double; addr1 = rd_addr1_fpr_mul_double; addr2 = wr_addr0_fpr_mul_double; end
        MUL_FOUR:   begin addr0 = rd_addr0_fpr_mul_four;   addr1 = rd_addr1_fpr_mul_four;   addr2 = wr_addr0_fpr_mul_four;   end
        ADD_FOUR,
        SUB_FOUR:   begin addr0 = rd_addr0_four;           addr1 = rd_addr1_four;           addr2 = wr_addr0_four;           end
        ADD_DOUBLE,
        SUB_DOUBLE,
        ADD_SINGLE,
        SUB_SINGLE,
        MUL_SINGLE: begin addr0 = rd_addr0_single_double;  addr1 = rd_addr1_single_double;  addr2 = wr_addr0_single_double;  end
        default:    begin addr0 = 'x;                      addr1 = 'x;                      addr2 = 'x;                      end
        endcase
    end

    ///////////////////////////////////////
    // instance
    addr_gen_fpr_mul_four_bank u_addr_gen_fpr_mul_four_bank
    (
        .clk                ,
        .rst_n              ,
        .start              ( start_fpr_mul_four  ),      // input
        .input_position_set (input_position_set),         // input
        .output_position_set(output_position_set),        // input 
        .max_len            ( max_len     ),              // input  [MEM_ADDR_BITS - 1:0]  // max value: 128
        .src0               ( src0        ),              // input  [MEM_ADDR_BITS - 1:0]  // base addr of first oprands
        .src1               ( src1        ),              // input  [MEM_ADDR_BITS - 1:0]  // base addr of second oprands
        .dst0               ( dst0        ),              // input  [MEM_ADDR_BITS - 1:0]  // base addr of result
        .done               ( done_fpr_mul_four   ),      // output
        .rd_addr_vld        ( rd_addr_vld_fpr_mul_four ), // output
        .rd_addr0           ( rd_addr0_fpr_mul_four    ), // output [MEM_ADDR_BITS - 1:0]
        .rd_addr1           ( rd_addr1_fpr_mul_four    ), // output [MEM_ADDR_BITS - 1:0]
        .wr_addr_vld        ( wr_addr_vld_fpr_mul_four ), // output
        .wr_addr0           ( wr_addr0_fpr_mul_four    )  // output [MEM_ADDR_BITS - 1:0]
    );

    addr_gen_fpr_mul_double_bank u_addr_gen_fpr_mul_double_bank
    (
        .clk         ,
        .rst_n       ,
        .start       ( start_fpr_mul_double  ), // input
        .max_len     ( max_len     ), // input  [MEM_ADDR_BITS - 1:0]  // max value: 128
        .src0        ( src0        ), // input  [MEM_ADDR_BITS - 1:0]  // base addr of first oprands
        .src1        ( src1        ), // input  [MEM_ADDR_BITS - 1:0]  // base addr of second oprands
        .dst0        ( dst0        ), // input  [MEM_ADDR_BITS - 1:0]  // base addr of result
        .done        ( done_fpr_mul_double   ), // output
        .rd_addr_vld ( rd_addr_vld_fpr_mul_double ), // output
        .rd_addr0    ( rd_addr0_fpr_mul_double    ), // output [MEM_ADDR_BITS - 1:0]
        .rd_addr1    ( rd_addr1_fpr_mul_double    ), // output [MEM_ADDR_BITS - 1:0]
        .wr_addr_vld ( wr_addr_vld_fpr_mul_double ), // output
        .wr_addr0    ( wr_addr0_fpr_mul_double    )  // output [MEM_ADDR_BITS - 1:0]
    );

    addr_gen_four_bank u_addr_gen_four_bank
    (
        .clk                ,
        .rst_n              ,
        .start              ( start_four  ),      // input
        .input_position_set (input_position_set), // input
        .output_position_set(output_position_set),// input 
        .max_len            ( max_len     ), // input  [8:0]  // max value: 128
        .src0               ( src0        ), // input  [MEM_ADDR_BITS - 1:0]  // base addr of first oprands
        .src1               ( src1        ), // input  [MEM_ADDR_BITS - 1:0]  // base addr of second oprands
        .dst0               ( dst0        ), // input  [MEM_ADDR_BITS - 1:0]  // base addr of result
        .done               ( done_four   ), // output
        .rd_addr_vld        ( rd_addr_vld_four ), // output
        .rd_addr0           ( rd_addr0_four    ), // output [MEM_ADDR_BITS - 1:0]
        .rd_addr1           ( rd_addr1_four    ), // output [MEM_ADDR_BITS - 1:0]
        .wr_addr_vld        ( wr_addr_vld_four ), // output
        .wr_addr0           ( wr_addr0_four    )  // output [MEM_ADDR_BITS - 1:0]
    );

    addr_gen_single_double_bank u_addr_gen_single_double_bank
    (
        .clk         ,
        .rst_n       ,
        .start       ( start_single_double  ), // input
        .max_len     ( max_len     ), // input  [MEM_ADDR_BITS - 1:0]  // max value: 128
        .src0        ( src0        ), // input  [MEM_ADDR_BITS - 1:0]  // base addr of first oprands
        .src1        ( src1        ), // input  [MEM_ADDR_BITS - 1:0]  // base addr of second oprands
        .dst0        ( dst0        ), // input  [MEM_ADDR_BITS - 1:0]  // base addr of result
        .done        ( done_single_double   ), // output
        .rd_addr_vld ( rd_addr_vld_single_double ), // output
        .rd_addr0    ( rd_addr0_single_double    ), // output [MEM_ADDR_BITS - 1:0]
        .rd_addr1    ( rd_addr1_single_double    ), // output [MEM_ADDR_BITS - 1:0]
        .wr_addr_vld ( wr_addr_vld_single_double ), // output
        .wr_addr0    ( wr_addr0_single_double    )  // output [MEM_ADDR_BITS - 1:0]
    );
endmodule

module addr_gen_fpr_mul_four_bank
    import falconsoar_pkg::*;
(
    input             clk        ,
    input             rst_n      ,
    input             start      ,
    input             input_position_set,
    input             output_position_set,
    input      [MEM_ADDR_BITS - 1:0] max_len    , // max value: 512
    input      [MEM_ADDR_BITS - 1:0] src0       , // base addr of first oprands
    input      [MEM_ADDR_BITS - 1:0] src1       , // base addr of second oprands
    input      [MEM_ADDR_BITS - 1:0] dst0       , // base addr of result
    output            done       ,
    output            rd_addr_vld,
    output reg [MEM_ADDR_BITS - 1:0] rd_addr0   ,
    output reg [MEM_ADDR_BITS - 1:0] rd_addr1   ,
    output reg        wr_addr_vld,
    output reg [MEM_ADDR_BITS - 1:0] wr_addr0
);
    wire cnt_start = start;
    wire [MEM_ADDR_BITS - 1:0] end_num = max_len * 4;
    wire cnt_run = 1'b1;
    wire cnt_en;
    wire cnt_done;
    wire [MEM_ADDR_BITS - 1:0] cnt;

    assign rd_addr0 = input_position_set ? (src0 + {2'b0, cnt[MEM_ADDR_BITS - 1:2]} + (~cnt[1]) * BANK_DEPTH*2) : (src0 + {2'b0, cnt[MEM_ADDR_BITS - 1:2]} + (cnt[1]) * BANK_DEPTH*2);
    assign rd_addr1 = input_position_set ? (src1 + {2'b0, cnt[MEM_ADDR_BITS - 1:2]} + (cnt[1]) * BANK_DEPTH*2) : (src1 + {2'b0, cnt[MEM_ADDR_BITS - 1:2]} + (~cnt[1]) * BANK_DEPTH*2);
    assign wr_addr0 = output_position_set ? (dst0 + {2'b0, cnt[MEM_ADDR_BITS - 1:2]} + (~cnt[1]) * BANK_DEPTH*2) : (dst0 + {2'b0, cnt[MEM_ADDR_BITS - 1:2]} + (cnt[1]) * BANK_DEPTH*2);

    assign rd_addr_vld = cnt_en;
    assign wr_addr_vld = cnt_en;
    assign done = cnt_done;

    counter_ce
    #(
        .BW         (  MEM_ADDR_BITS     )
    )
    u_counter_ce // counter with configurable end number
    (
        .clk        (   clk              ),
        .rst_n      (   rst_n            ),
        .start      (   cnt_start        ),
        .num        (   end_num          ),
        .run        (   cnt_run          ),
        .cnt        (   cnt              ),
        .cnt_en     (   cnt_en           ),
        .last       (                    ),
        .done       (   cnt_done         )
    );

endmodule

module addr_gen_fpr_mul_double_bank
    import falconsoar_pkg::*;
(
    input         clk        ,
    input         rst_n      ,
    input         start      ,
    input  [MEM_ADDR_BITS - 1:0] max_len    , // max value: 512
    input  [MEM_ADDR_BITS - 1:0] src0       , // base addr of first oprands
    input  [MEM_ADDR_BITS - 1:0] src1       , // base addr of second oprands
    input  [MEM_ADDR_BITS - 1:0] dst0       , // base addr of result
    output        done       ,
    output        rd_addr_vld,
    output [MEM_ADDR_BITS - 1:0] rd_addr0   ,
    output [MEM_ADDR_BITS - 1:0] rd_addr1   ,
    output        wr_addr_vld,
    output [MEM_ADDR_BITS - 1:0] wr_addr0
);
    wire cnt_start = start;
    wire [MEM_ADDR_BITS - 1:0] end_num = max_len * 2;
    wire cnt_run = 1'b1;
    wire cnt_en;
    wire cnt_done;
    wire [MEM_ADDR_BITS - 1:0] cnt;

    assign rd_addr0 = src0 + (cnt / 2) + ((cnt % 2) * BANK_DEPTH);
    assign rd_addr1 = src1 + (cnt / 2) + ((cnt % 2) * BANK_DEPTH);
    assign wr_addr0 = dst0 + (cnt / 2) + ((cnt % 2) * BANK_DEPTH);

    assign rd_addr_vld = cnt_en;
    assign wr_addr_vld = cnt_en;
    assign done = cnt_done;

    counter_ce
    #(
        .BW         (  10     )
    )
    u_counter_ce // counter with configurable end number
    (
        .clk        (   clk              ),
        .rst_n      (   rst_n            ),
        .start      (   cnt_start        ),
        .num        (   end_num          ),
        .run        (   cnt_run          ),
        .cnt        (   cnt              ),
        .cnt_en     (   cnt_en           ),
        .last       (                    ),
        .done       (   cnt_done         )
    );

endmodule

module addr_gen_four_bank
    import falconsoar_pkg::*;
(
    input         clk        ,
    input         rst_n      ,
    input         start      ,
    input         input_position_set,
    input         output_position_set,
    input  [MEM_ADDR_BITS - 1:0] max_len    , // max value: 512
    input  [MEM_ADDR_BITS - 1:0] src0       , // base addr of first oprands
    input  [MEM_ADDR_BITS - 1:0] src1       , // base addr of second oprands
    input  [MEM_ADDR_BITS - 1:0] dst0       , // base addr of result
    output        done       ,
    output        rd_addr_vld,
    output [MEM_ADDR_BITS - 1:0] rd_addr0   ,
    output [MEM_ADDR_BITS - 1:0] rd_addr1   ,
    output        wr_addr_vld,
    output [MEM_ADDR_BITS - 1:0] wr_addr0
);

    wire cnt_start = start;
    wire [MEM_ADDR_BITS - 1:0] end_num = max_len * 2;
    wire cnt_run = 1'b1;
    wire cnt_en;
    wire cnt_done;
    wire [MEM_ADDR_BITS - 1:0] cnt;

    assign rd_addr0 = cnt[0] ^ input_position_set  ? (src0 + {1'b0, cnt[MEM_ADDR_BITS - 1:1]} + BANK_DEPTH*2) : (src0 + {1'b0, cnt[MEM_ADDR_BITS - 1:1]});
    assign rd_addr1 = cnt[0] ^ input_position_set  ? (src1 + {1'b0, cnt[MEM_ADDR_BITS - 1:1]}) : (src1 + {1'b0, cnt[MEM_ADDR_BITS - 1:1]} + BANK_DEPTH*2);
    assign wr_addr0 = cnt[0] ^ output_position_set ? (dst0 + {1'b0, cnt[MEM_ADDR_BITS - 1:1]} + BANK_DEPTH*2) : (dst0 + {1'b0, cnt[MEM_ADDR_BITS - 1:1]});

    assign rd_addr_vld = cnt_en;
    assign wr_addr_vld = cnt_en;
    assign done = cnt_done;

    counter_ce
    #(
        .BW         (  MEM_ADDR_BITS     )
    )
    u_counter_ce // counter with configurable end number
    (
        .clk        (   clk              ),
        .rst_n      (   rst_n            ),
        .start      (   cnt_start        ),
        .num        (   end_num          ),
        .run        (   cnt_run          ),
        .cnt        (   cnt              ),
        .cnt_en     (   cnt_en           ),
        .last       (                    ),
        .done       (   cnt_done         )
    );

endmodule

module addr_gen_single_double_bank
    import falconsoar_pkg::*;
(
    input         clk        ,
    input         rst_n      ,
    input         start      ,
    input  [MEM_ADDR_BITS - 1:0] max_len    , // max value: 128
    input  [MEM_ADDR_BITS - 1:0] src0       , // base addr of first oprands
    input  [MEM_ADDR_BITS - 1:0] src1       , // base addr of second oprands
    input  [MEM_ADDR_BITS - 1:0] dst0       , // base addr of result
    output        done       ,
    output        rd_addr_vld,
    output [MEM_ADDR_BITS - 1:0] rd_addr0   ,
    output [MEM_ADDR_BITS - 1:0] rd_addr1   ,
    output        wr_addr_vld,
    output [MEM_ADDR_BITS - 1:0] wr_addr0
);

    wire cnt_start = start;
    wire [MEM_ADDR_BITS - 1:0] end_num = max_len;
    wire cnt_run = 1'b1;
    wire cnt_en;
    wire cnt_done;
    wire [MEM_ADDR_BITS - 1:0] cnt;

    assign rd_addr0 = src0 + cnt;
    assign rd_addr1 = src1 + cnt;
    assign wr_addr0 = dst0 + cnt;

    assign rd_addr_vld = cnt_en;
    assign wr_addr_vld = cnt_en;
    assign done = cnt_done;
    counter_ce
    #(
        .BW         (  MEM_ADDR_BITS     )
    )
    u_counter_ce // counter with configurable end number
    (
        .clk        (   clk              ),
        .rst_n      (   rst_n            ),
        .start      (   cnt_start        ),
        .num        (   end_num          ),
        .run        (   cnt_run          ),
        .cnt        (   cnt              ),
        .cnt_en     (   cnt_en           ),
        .last       (                    ),
        .done       (   cnt_done         )
    );

endmodule
