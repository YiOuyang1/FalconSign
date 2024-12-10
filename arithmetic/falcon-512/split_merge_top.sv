`include "falconsoar_pkg.sv"

module split_merge_top
    import falconsoar_pkg::*;
#(
    parameter DW = 128  // the data size of a complex number
) (
    input                  clk         ,
    input                  rst_n       ,
    exec_operator_if.slave task_itf    ,
    fpu_if.master          fpu_itf     ,
                                 
    mem_inst_if.master_rd  buff_rd [6] , // buff_rd [4],[5]is for read twiddle factor
    mem_inst_if.master_wr  buff_wr [4]    
);
    localparam FFT_MODE = 2'd0;

    localparam bit [2:0] SPLIT_256 = 3'd2;
    localparam bit [2:0] MERGE_256 = 3'd3;
    localparam bit [2:0] SPLIT_512 = 3'd4;
    localparam bit [2:0] MERGE_512 = 3'd5;

    localparam FPU_FFT = 4'd0;
    localparam FPU_IFFT = 4'd1;

    //configure control signal
    wire  input_position_set = task_itf.input_task[4]; //0:positive; 1:negetive
    wire output_position_set = task_itf.input_task[5]; //0:positive; 1:negetive

    mem_addr_t base_addr [3];

    assign base_addr[0] = task_itf.input_task[TASK_REDUCE_BW - 0*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 1*MEM_ADDR_BITS]; // mode: 0:split_src, 1:merge_src
    assign base_addr[1] = task_itf.input_task[TASK_REDUCE_BW - 1*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 2*MEM_ADDR_BITS]; // mode: 0:split_dst, 1:merge_src
    assign base_addr[2] = task_itf.input_task[TASK_REDUCE_BW - 2*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 3*MEM_ADDR_BITS]; // mode: 0:split_dst, 1:merge_dst

    wire [1:0] task_mode = task_itf.input_task[7:6]; // 0:fft mode
    wire [2:0] task_type = task_itf.input_task[10:8]; // 2: split_256; 3:merge_256; 4: split_512; 5:merge_512;
    wire [4:0] fft_stage = task_itf.input_task[15:11]; // stage_num, remember check the value of 'addrw_offset' and 'split_merge_addr_gen'

    wire mode = (task_mode == FFT_MODE) & ((task_type == SPLIT_256) | (task_type == SPLIT_512)); // 1"Split; 0:merge

    wire addr_gen_tpye = ((task_type == SPLIT_512) | (task_type == MERGE_512)); //0: node 156; 1: 512 node

    assign fpu_itf.mode = mode ? FPU_IFFT : FPU_FFT;  // split: fpu ifft mode; merge: fpu fft mode

    //instance
    logic [WR_DELAY:1] start_d;  

    pipline_delay_bit #(.R(1),.P(1),.D(WR_DELAY)) u_dly_0 (.c(clk),.r(rst_n),.i(task_itf.start),.o(start_d)); // write depend on fpu pipeline stages

    wire rd_start = start_d[1];
    wire wr_start = start_d[WR_DELAY];

    //read pyhsical addr and enable generation block
    logic rd_addr_vld;
    logic wr_addr_vld;

    mem_addr_t rd_logic_addr [3];
    mem_addr_t wr_logic_addr [3];

    wire rd_swap_split = (^rd_logic_addr[0]) ^ input_position_set;
    wire rd_swap_merge = (^rd_logic_addr[2]) ^ input_position_set;
    wire wr_swap_split = (^wr_logic_addr[2]) ^ output_position_set;
    wire wr_swap_merge = (^wr_logic_addr[0]) ^ output_position_set;

    mem_addr_t rd_base_addr_split [4];
    mem_addr_t rd_addr_split      [4];
    mem_addr_t rd_base_addr_merge [4];
    mem_addr_t rd_addr_merge      [4];

    mem_addr_t wr_base_addr_split [4];
    mem_addr_t wr_addr_split      [4];
    mem_addr_t wr_base_addr_merge [4];
    mem_addr_t wr_addr_merge      [4];

    for(genvar i=0; i<4; i++) begin:gen_buff_rd_addr
        assign rd_base_addr_split[i] = base_addr[0] + ((i + {rd_swap_split,1'b0}) % 4) * BANK_DEPTH;
        assign wr_base_addr_split[i] = base_addr[i[1] + 1] + { (wr_swap_split ^ i[1]) ,i[0]}  * BANK_DEPTH;
        assign rd_addr_split[i] = (rd_logic_addr[i[1]] / 2) + rd_base_addr_split[i];
        assign wr_addr_split[i] = (wr_logic_addr[2] / 2) + wr_base_addr_split[i];

        assign rd_base_addr_merge[i] = base_addr[i[1]] + { (rd_swap_merge ^ i[1]) ,i[0]} * BANK_DEPTH;
        assign wr_base_addr_merge[i] = base_addr[2] + ((i + {wr_swap_merge,1'b0}) % 4) * BANK_DEPTH;
        assign rd_addr_merge[i] = (rd_logic_addr[2] / 2) + rd_base_addr_merge[i];
        assign wr_addr_merge[i] = (wr_logic_addr[i[1]] / 2) + wr_base_addr_merge[i];

        assign buff_rd[i].en = rd_addr_vld;
        assign buff_wr[i].en = wr_addr_vld;
        assign buff_rd[i].addr = mode ? rd_addr_split[i] : rd_addr_merge[i];
        assign buff_wr[i].addr = mode ? wr_addr_split[i] : wr_addr_merge[i];
    end

    //twiddle factor addr and enbale generation blocks
    logic [$clog2(64)-1:0] rd_addr_w, rd_addr_w_d1, rd_addr_w_d2;
    logic [$clog2(64)-1:0] addrw_offset;

    always_comb begin
        addrw_offset = 'x;
        case (fft_stage)
            'd0: addrw_offset = 'd0;
            'd1: addrw_offset = 'd1;
            'd2: addrw_offset = 'd2;
            'd3: addrw_offset = 'd3;
            'd4: addrw_offset = 'd5;
            'd5: addrw_offset = 'd9;
            'd6: addrw_offset = 'd17;
            'd7: addrw_offset = 'd33;
            'd8: addrw_offset = 'd65;
        endcase
    end

    logic rd_en_w_d2;

    pipline_delay_bit #(.R(1),.P(0),.D(2)) u_dly_rd_w (.c(clk),.r(rst_n),.i(rd_addr_vld),.o(rd_en_w_d2));
    //pipeline_delay #(.DW($bits(rd_addr_w)),.DEPTH(2)) u_dly_addr_w (.clk(clk),.rst_n(rst_n),.en_i(rd_addr_vld),.d_i(rd_addr_w), .d_o(rd_addr_w_d2));
    always_ff @(posedge clk) rd_addr_w_d1 <= rd_addr_w;
    always_ff @(posedge clk) rd_addr_w_d2 <= rd_addr_w_d1;

    for(genvar i=4; i<6; i++) begin:gen_buff_en
        assign buff_rd[i].en = rd_en_w_d2;
    end

    assign buff_rd[4].addr = rd_addr_w_d2 + addrw_offset;
    assign buff_rd[5].addr = rd_addr_w_d2 + addrw_offset + BANK_DEPTH;

    //net to external fpu for compute
    wire permut_in_en = (task_type == 3'd2) | (task_type == 3'd4); // work only in split_n256 and split_n512

    wire [DW*4-1:0] permut_in_data_a = {buff_rd[1].data,buff_rd[0].data};
    wire [DW*4-1:0] permut_in_data_b = {buff_rd[3].data,buff_rd[2].data};

    wire permut_out_en = (task_type == 3'd3) | (task_type == 3'd5); // work only in merge_n256 and merge_n512
    wire [DW*4-1:0] permut_out_data_a;
    wire [DW*4-1:0] permut_out_data_b;

    assign fpu_itf.d_i[2] = {buff_rd[5].data,buff_rd[4].data};

    assign {buff_wr[1].data,buff_wr[0].data} = permut_out_data_a;
    assign {buff_wr[3].data,buff_wr[2].data} = permut_out_data_b;

    split_merge_addr_gen u_rd_fsm
    (
        .clk        ,
        .rst_n      ,
        .stage      (fft_stage),     // i [3:0]
        .task_type  (addr_gen_tpye), // i 0:n256, 1:n512
        .start      (rd_start),      // i
        .done       (),              // o
        .addr_vld   (rd_addr_vld),   // o
        .addr       (rd_logic_addr)  // o mem_addr_t [3]
    );

    fft_input_permutation u_input_permutation
    (
        .perm (permut_in_en),      // i
        .in_0 (permut_in_data_a),  // i [DW*4-1:0]
        .in_1 (permut_in_data_b),  // i [DW*4-1:0]
        .out_0(fpu_itf.d_i[0]),      // o [DW*4-1:0]
        .out_1(fpu_itf.d_i[1])       // o [DW*4-1:0]
    );

    wire wr_done;

    assign task_itf.op_done = wr_done;

    split_merge_addr_gen u_wr_fsm
    (
        .clk        ,
        .rst_n      ,
        .stage      (fft_stage),       // i [3:0]
        .task_type  (addr_gen_tpye),   // i          0:n256, 1:n512
        .start      (wr_start),        // i
        .done       (wr_done),         // o
        .addr_vld   (wr_addr_vld),     // o
        .addr       (wr_logic_addr)    // o mem_addr_t [3]
    );

    fft_output_permutation u_output_permutation (
        .perm (permut_out_en),      // i
        .in_0 (fpu_itf.d_o[0]),     // i [DW*4-1:0]
        .in_1 (fpu_itf.d_o[1]),     // i [DW*4-1:0]
        .out_0(permut_out_data_a),  // o [DW*4-1:0]
        .out_1(permut_out_data_b)   // o [DW*4-1:0]
    );

    counter_ce
    #(
        .BW($clog2(64))
    )
    u_counter_ce  // counter with configurable end number
    (
        .clk    ,
        .rst_n  ,
        .start  (rd_start),
        .num    ($clog2(64)'(64)),
        .run    (1'b1),
        .cnt    (rd_addr_w),
        .cnt_en (),
        .last   (),
        .done   ()
    );

endmodule

module split_merge_addr_gen
    import falconsoar_pkg::*;
(
    input              clk,
    input              rst_n,
    input        [3:0] stage,
    input              task_type, // 0: node 256; 1: node 512
    input              start,
    output             done,
    output             addr_vld,
    output mem_addr_t  addr [3]
);

    wire start_single = start & (stage < 'd3);

    logic [8:3] start_stage;

    always_comb for(int i=3; i<8; i++) start_stage[i] = start & (stage == i);

    assign start_stage[8] = start & (stage == 'd8) & task_type;

    logic       done_single, addr_vld_single;
    logic [8:3] done_stage , addr_vld_stage ;

    mem_addr_t addr_stage [3:8][3]; 

    mem_addr_t addr_single [3];

    assign done = |{done_single,done_stage};
    assign addr_vld = |{addr_vld_single,addr_vld_stage};

    always_comb foreach(addr[i]) begin
        if(stage > 'd2) addr[i] = addr_stage[stage][i];
        else            addr[i] = addr_single[i];
    end

    ///////////////////////////////////////
    // instance
    split_merge_addr_gen_stage012 u_split_merge_addr_gen_stage012
    (
        .clk        ,
        .rst_n      ,
        .start      (start_single),        // input
        .done       (done_single),         // output
        .addr_vld   (addr_vld_single),  // output
        .addr       (addr_single)        // output [9:0]
    );

    localparam int ROTATE_NUM [3:8][2] = '{{1,1},{2,2},{3,3},{0,0},{1,1},{1,1}};

    for(genvar i=3; i<9; i++) begin:gen_split_merge_addr_gen_stage
        split_merge_addr_gen_stage
        #(
            .STAGE(i),
            .ROTATE_NUM(ROTATE_NUM[i])
        )
        u_split_merge_addr_gen_stage
        (
            .clk        ,
            .rst_n      ,
            .task_type  ,
            .start      (start_stage[i]),        // input
            .done       (done_stage[i]),         // output
            .addr_vld   (addr_vld_stage[i]),  // output
            .addr       (addr_stage[i])        // output [9:0]
        );
    end

endmodule

module split_merge_addr_gen_stage012
    import falconsoar_pkg::*;
(
    input              clk,
    input              rst_n,
    input              start,
    output logic       done,
    output logic       addr_vld,
    output mem_addr_t  addr [3]
);

    assign addr[0] = mem_addr_t'(0);
    assign addr[1] = mem_addr_t'(1);
    assign addr[2] = mem_addr_t'(0);

    assign addr_vld = done;

    always_ff @(posedge clk, negedge rst_n) if(!rst_n) done <= 1'b0; else done <= start;

endmodule

module split_merge_addr_gen_stage
    import falconsoar_pkg::*;
#(
    parameter int STAGE = 4,
    parameter int ROTATE_NUM [2] = '{0,0}
)
(
    input              clk      ,
    input              rst_n    ,
    input              task_type, // 0: node 256; 1: node 512
    input              start    ,
    output             done     ,
    output logic       addr_vld ,
    output mem_addr_t addr [3]
);

    localparam int unsigned BW_ADDR  = STAGE - 1;
    localparam int unsigned SIZE_CNT = 2 ** (BW_ADDR - 1);

    logic [$clog2(BW_ADDR)-1:0] rotate_num;

    assign rotate_num = $clog2(BW_ADDR)'(ROTATE_NUM[task_type]);

    logic [BW_ADDR-1:0] addr_0, addr_1;
    logic [BW_ADDR-2:0] addr_2;

    assign addr[0] = mem_addr_t'(addr_0);
    assign addr[1] = mem_addr_t'(addr_1);
    assign addr[2] = mem_addr_t'(addr_2);

    logic [BW_ADDR-2:0] addr2_bit_rev, cnt;

    wire [BW_ADDR-1:0] addr0_bit_rev = {1'b0,addr2_bit_rev};
    wire [BW_ADDR-1:0] addr1_bit_rev = {1'b1,addr2_bit_rev};

    if(BW_ADDR != 2) begin
        always_comb foreach(addr2_bit_rev[i]) addr2_bit_rev[i] = cnt[BW_ADDR-2-i];

        rbsh #(
            .A_width (BW_ADDR-1),
            .SH_width($clog2(BW_ADDR-1))
        ) u_rotate_halfsize (
            .A    (addr2_bit_rev),
            .SH   ($clog2(BW_ADDR-1)'(rotate_num)),
            .SH_TC(1'b0),
            .B    (addr_2)
        );
    end else begin
        assign addr2_bit_rev = cnt;
        assign addr_2 = addr2_bit_rev;
    end

    counter_ce
    #(
        .BW         ($clog2(SIZE_CNT))
    )
    u_counter_ce // counter with configurable end number
    (
        .clk        (   clk         ),
        .rst_n      (   rst_n       ),
        .start      (   start       ),
        .num        (   $clog2(SIZE_CNT)'(SIZE_CNT) ),
        .run        (   1'b1        ),
        .cnt        (   cnt         ),
        .cnt_en     (   addr_vld    ),
        .done       (   done        )
    );

    rbsh #(
        .A_width (BW_ADDR),
        .SH_width($clog2(BW_ADDR))
    ) u_rotate_cnt (
        .A    (addr0_bit_rev),
        .SH   (rotate_num),
        .SH_TC(1'b0),
        .B    (addr_0)
    );

    rbsh #(
        .A_width (BW_ADDR),
        .SH_width($clog2(BW_ADDR))
    ) u_rotate_offset (
        .A    (addr1_bit_rev),
        .SH   (rotate_num),
        .SH_TC(1'b0),
        .B    (addr_1)
    );

endmodule
