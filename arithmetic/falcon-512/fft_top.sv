`include "falconsoar_pkg.sv"
`include "float_pkg.sv"

module fft_top
    import falconsoar_pkg::*;
#(
    parameter DW = 128  // the data size of a complex number
) (
    input                  clk           ,
    input                  rst_n         ,
    exec_operator_if.slave task_itf      ,
    fpu_if.master          fpu_itf       ,

    mem_inst_if.master_rd  buff_rd [6]   , // buff_rd [4],[5]is for read twiddle factor
    mem_inst_if.master_wr  buff_wr [4]
);

    localparam bit [4:0] FFT  = 5'b000_00;
    localparam bit [4:0] IFFT = 5'b001_00;

    wire  input_position_set = task_itf.input_task[4]; //0:positive; 1:negetive
    wire output_position_set = task_itf.input_task[5]; //0:positive; 1:negetive

    mem_addr_t fft_base_addr    ; assign fft_base_addr     = task_itf.input_task[TASK_REDUCE_BW - 0*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 1*MEM_ADDR_BITS];  // src0/dst0
    mem_addr_t twiddle_base_addr; assign twiddle_base_addr = task_itf.input_task[TASK_REDUCE_BW - 1*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 2*MEM_ADDR_BITS];  // default value is 0
    mem_addr_t fft_node_num     ; assign fft_node_num      = task_itf.input_task[TASK_REDUCE_BW - 3*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 4*MEM_ADDR_BITS];  // 16 lines: node 256; 32 lines: node 512

    wire [4:0] task_mode         = task_itf.input_task[10:6];

    wire       fft_mode = (task_mode != 0);  // 0: fft; 1: ifft
    wire [1:0] node_num = (fft_node_num == 'd64) ? 2'd1 : 2'd0;

    reg  [3:0] fft_stage_start_num;
    reg  [3:0] fft_stage_stop_num;

    assign fpu_itf.mode = (task_mode == FFT) ? 4'b0000 : 4'b0001;

    always_comb begin
        fft_stage_start_num = 'x;
        fft_stage_stop_num  = 'x;
        case(task_mode)
        FFT: begin
            fft_stage_start_num = '0;
            fft_stage_stop_num  = (fft_node_num == 10'd64) ? 4'd8 : 4'd7;
        end
        IFFT: begin
            fft_stage_start_num = (fft_node_num == 10'd64) ? 4'd8 : 4'd7;
            fft_stage_stop_num  = '0;
        end
        endcase
    end

    wire [WR_DELAY:1] start_d; 

    pipline_delay_bit #(.R(1),.P(1),.D(WR_DELAY)) u_dly_0 (.c(clk),.r(rst_n),.i(task_itf.start),.o(start_d));

    wire rd_start = start_d[1];

    //read pyhsical addr and enable generate
    logic       rd_en;
    mem_addr_t rd_logic_addr [2];
    mem_addr_t rd_base_addr [4];

    wire rd_swap = ((^rd_logic_addr[0]) ^ input_position_set);

    for(genvar i=0; i<4; i++) begin:gen_mem_rd
        assign buff_rd[i].en = rd_en;
        assign buff_rd[i].addr = (rd_logic_addr[i[1]] / 2) + rd_base_addr[i];
        assign rd_base_addr[i] = fft_base_addr + unsigned'(((rd_swap ? (i + 2) : i) % 4) * BANK_DEPTH);
    end

    logic      rd_en_w, rd_en_w_d2;
    mem_addr_t rd_addr_w, rd_addr_w_d1, rd_addr_w_d2;

    pipline_delay_bit #(.R(1),.P(0),.D(2)) u_dly_rd_w (.c(clk),.r(rst_n),.i(rd_en_w),.o(rd_en_w_d2));
    //pipeline_delay #(.DW($bits(rd_addr_w)),.DEPTH(1)) u_dly_addr_w (.clk(clk),.rst_n(rst_n),.en_i(rd_en_w),.d_i(rd_addr_w), .d_o(rd_addr_w_d2));
    always_ff @(posedge clk) rd_addr_w_d1 <= rd_addr_w;
    always_ff @(posedge clk) rd_addr_w_d2 <= rd_addr_w_d1;

    assign buff_rd[4].en = rd_en_w_d2;
    assign buff_rd[5].en = rd_en_w_d2;

    assign buff_rd[4].addr = rd_addr_w_d2 + twiddle_base_addr;
    assign buff_rd[5].addr = rd_addr_w_d2 + twiddle_base_addr + BANK_DEPTH;

    //assign buff_rd[4].en = rd_en_w;
    //assign buff_rd[5].en = rd_en_w;

    //assign buff_rd[4].addr = rd_addr_w + twiddle_base_addr;
    //assign buff_rd[5].addr = rd_addr_w + twiddle_base_addr + BANK_DEPTH;

    //write pyhsical addr generate, enbale signal direct connect to module 'u_wr_fsm' 'w_en'
    logic      wr_en;
    mem_addr_t wr_logic_addr [2];
    mem_addr_t wr_base_addr [4];

    wire wr_swap = ((^wr_logic_addr[0]) ^ output_position_set);

    logic [DW*4-1:0] permut_o_data [2];

    for(genvar i=0; i<4; i++) begin:gen_mem_wr
        assign buff_wr[i].en = wr_en;
        assign buff_wr[i].addr = (wr_logic_addr[i[1]] / 2) + wr_base_addr[i];
        assign buff_wr[i].data = permut_o_data[i[1]][i[0]*DW*2+:DW*2];
        assign wr_base_addr[i] = fft_base_addr + unsigned'(((wr_swap ? (i + 2) : i) % 4) * BANK_DEPTH);
    end

    //net to external fpu for compute
    wire permut_i_en = (task_mode == IFFT);
    wire permut_o_en = (task_mode ==  FFT);

    logic [DW*4-1:0] permut_i_data [2];

    assign permut_i_data[0] = {buff_rd[1].data,buff_rd[0].data};
    assign permut_i_data[1] = {buff_rd[3].data,buff_rd[2].data};

    assign fpu_itf.d_i[2] = {buff_rd[5].data,buff_rd[4].data};

    fft_addr_gen u_rd_fsm
    (
        .clk                ,
        .rst_n              ,
        .mode               (fft_mode           ), // i
        .node_num           (node_num           ), // i [1:0]
        .stage_start_num    (fft_stage_start_num), // i [3:0]
        .stage_stop_num     (fft_stage_stop_num ), // i [3:0]
        .start              (rd_start           ), // i
        .addr_vld           (rd_en              ), // o
        .addr0              (rd_logic_addr[0]   ), // o [6:0]
        .addr1              (rd_logic_addr[1]   ), // o [6:0]
        .addrw_vld          (rd_en_w            ), // o
        .addrw              (rd_addr_w          )  // o [5:0]
    );

    fft_input_permutation u_input_permutation
    (
        .perm   (permut_i_en        ), // i
        .in_0   (permut_i_data[0]   ), // i [DW*32-1:0]
        .in_1   (permut_i_data[1]   ), // i [DW*32-1:0]
        .out_0  (fpu_itf.d_i[0]     ), // o [DW*32-1:0]
        .out_1  (fpu_itf.d_i[1]     )  // o [DW*32-1:0]
    );

    wire wr_start = start_d[WR_DELAY];  // fixed by oyy

    wire wr_done;

    assign task_itf.op_done = wr_done;

    fft_addr_gen u_wr_fsm
    (
        .clk                ,
        .rst_n              ,
        .mode               (fft_mode           ), // i
        .node_num           (node_num           ), // i [1:0]
        .stage_start_num    (fft_stage_start_num), // i [3:0]
        .stage_stop_num     (fft_stage_stop_num ), // i [3:0]
        .start              (wr_start           ), // i
        .done               (wr_done            ), // o
        .addr_vld           (wr_en              ), // o
        .addr0              (wr_logic_addr[0]   ), // o [6:0]
        .addr1              (wr_logic_addr[1]   )  // o [6:0]
    );

    fft_output_permutation u_output_permutation
    (
        .perm   (permut_o_en        ), // i
        .in_0   (fpu_itf.d_o[0]     ), // i [DW*COEFFVEC-1:0]
        .in_1   (fpu_itf.d_o[1]     ), // i [DW*COEFFVEC-1:0]
        .out_0  (permut_o_data[0]   ), // o [DW*COEFFVEC-1:0]
        .out_1  (permut_o_data[1]   )  // o [DW*COEFFVEC-1:0]
    );

endmodule

module fft_input_permutation
#(
    parameter int unsigned DW  = 128,  // the size of complex number
    parameter int unsigned VEC = 4
)
(
    input                          perm,   // i
    input        [VEC-1:0][DW-1:0] in_0,   // i [DW*COEFFVEC-1:0]
    input        [VEC-1:0][DW-1:0] in_1,   // i [DW*COEFFVEC-1:0]
    output logic [VEC-1:0][DW-1:0] out_0,  // o [DW*COEFFVEC-1:0]
    output logic [VEC-1:0][DW-1:0] out_1   // o [DW*COEFFVEC-1:0]
);

    always_comb foreach(out_0[i]) out_0[i] = perm ? ((i < 2) ? in_0[(i*2)+0] : in_1[((i-2)*2)+0]) : in_0[i];
    always_comb foreach(out_1[i]) out_1[i] = perm ? ((i < 2) ? in_0[(i*2)+1] : in_1[((i-2)*2)+1]) : in_1[i];

endmodule

module fft_output_permutation
#(
    parameter int unsigned DW  = 128,  // the size of complex number
    parameter int unsigned VEC = 4
)
(
    input                          perm     ,
    input        [VEC-1:0][DW-1:0] in_0     ,
    input        [VEC-1:0][DW-1:0] in_1     ,
    output logic [VEC-1:0][DW-1:0] out_0    ,
    output logic [VEC-1:0][DW-1:0] out_1
);

    always_comb foreach(out_0[i]) out_0[i] = perm ? (i[0] ? in_1[(i/2)+0] : in_0[(i/2)+0]) : in_0[i];
    always_comb foreach(out_1[i]) out_1[i] = perm ? (i[0] ? in_1[(i/2)+2] : in_0[(i/2)+2]) : in_1[i];

endmodule

module fft_addr_gen 
    import falconsoar_pkg::*;
    import float_pkg::*;
(
    input                          clk,
    input                          rst_n,
    input                          mode,             // 0:fft 1:ifft
    input                    [1:0] node_num,         // node = 256 << node_num
    input                    [3:0] stage_start_num,
    input                    [3:0] stage_stop_num,
    input                          start,
    output                         done,
    output                         addr_vld,
    output reg [MEM_ADDR_BITS-1:0] addr0,
    output reg [MEM_ADDR_BITS-1:0] addr1,
    output                         addrw_vld,
    output reg [MEM_ADDR_BITS-1:0] addrw
);

    ///////////////////////////////////////

    wire stage_cnt_start = start;

    wire gk_stage_cnt;
    wire addr_cnt_done;

    reg stage_cnt_en;

    reg [3:0] stage_cnt;

    wire stage_cnt_run = addr_cnt_done;

    wire [3:0] stage_cnt_nxt = mode ? (stage_cnt - 1'd1) : (stage_cnt + 1'd1);

    wire [3:0] stage_cnt_l = start ? stage_start_num : stage_cnt_nxt;

    wire fft_done = (~mode) & (stage_cnt == (stage_stop_num + 4'd1));  // stop at stage 8
    wire ifft_done = (mode) & (stage_cnt == (stage_stop_num - 4'd1));  // stop at stage '-1' == 4'hF

    wire stage_cnt_done = (fft_done | ifft_done) & stage_cnt_en;

    assign gk_stage_cnt = start | (stage_cnt_en & stage_cnt_run & ~stage_cnt_done);

    always @(posedge clk) if (gk_stage_cnt) stage_cnt <= stage_cnt_l[3:0];

    always @(posedge clk, negedge rst_n) begin
        if (!rst_n) stage_cnt_en <= 1'b0;
        else if (stage_cnt_start) stage_cnt_en <= 1'b1;
        else if (stage_cnt_done) stage_cnt_en <= 1'b0;
    end

    wire [3:0] fft_stage = stage_cnt;

    assign done = stage_cnt_done;

    ///////////////////////////////////////

    wire       addr_cnt_start = start | (gk_stage_cnt & ~(stage_cnt == stage_stop_num));

    wire       stage_start_n256 = (node_num == 2'd0) & addr_cnt_start;
    wire       stage_done_n256;
    wire       addr_vld_n256;
    wire [5:0] addr0_n256;
    wire [5:0] addr1_n256;
    wire       addrw_vld_n256;
    wire [5:0] addrw_n256;

    wire       stage_start_n512 = (node_num == 2'd1) & addr_cnt_start;
    wire       stage_done_n512;
    wire       addr_vld_n512;
    wire [6:0] addr0_n512;
    wire [6:0] addr1_n512;
    wire       addrw_vld_n512;
    wire [6:0] addrw_n512;

    reg  [6:0] addrw_offset;

    always_comb begin
        addrw_offset = 6'dx;
        case(fft_stage)
            4'd0: addrw_offset = 'd0;
            4'd1: addrw_offset = 'd1;
            4'd2: addrw_offset = 'd2;
            4'd3: addrw_offset = 'd3;
            4'd4: addrw_offset = 'd5;
            4'd5: addrw_offset = 'd9;
            4'd6: addrw_offset = 'd17;
            4'd7: addrw_offset = 'd33;
            4'd8: addrw_offset = 'd65;
        endcase
    end

    assign addr_cnt_done = stage_done_n256 | stage_done_n512;
    assign addr_vld = addr_vld_n256 | addr_vld_n512;
    assign addrw_vld = addrw_vld_n256 | addrw_vld_n512;

    always_comb begin
        addr0 = 'hx;
        addr1 = 'hx;
        addrw = 'hx;
        case(node_num)
        2'd0: begin
            addr0 = addr0_n256;
            addr1 = addr1_n256;
            addrw = (addrw_offset + addrw_n256);
        end
        2'd1: begin
            addr0 = addr0_n512;
            addr1 = addr1_n512;
            addrw = (addrw_offset + addrw_n512);
        end
        endcase
    end

    ///////////////////////////////////////

    fft_addr_gen_256 u_fft_addr_gen_256
    (
        .clk      ,
        .rst_n    ,
        .stage    (fft_stage[3:0]),    // i [3:0]
        .start    (stage_start_n256),  // i
        .done     (stage_done_n256),   // o
        .addr_vld (addr_vld_n256),     // o
        .addr0    (addr0_n256),        // o [4:0]
        .addr1    (addr1_n256),        // o [4:0]
        .addrw_vld(addrw_vld_n256),    // o
        .addrw    (addrw_n256)         // o [4:0]
    );

    fft_addr_gen_512 u_fft_addr_gen_512
    (
        .clk      ,
        .rst_n    ,
        .stage    (fft_stage[3:0]),    // i [3:0]
        .start    (stage_start_n512),  // i
        .done     (stage_done_n512),   // o
        .addr_vld (addr_vld_n512),     // o
        .addr0    (addr0_n512),        // o [5:0]
        .addr1    (addr1_n512),        // o [5:0]
        .addrw_vld(addrw_vld_n512),    // o
        .addrw    (addrw_n512)         // o [5:0]
    );

endmodule

module fft_addr_gen_256  // In FFT, 512 refers to 512 complex points, 1024 float number points
(
    input              clk,
    input              rst_n,
    input        [3:0] stage,
    input              start,
    output logic       done,
    output logic       addr_vld,
    output logic [5:0] addr0,      //first vector
    output logic [5:0] addr1,      //second vector
    output logic       addrw_vld,
    output logic [5:0] addrw       //twiddle factor
);

    logic [$clog2(32)-1:0] cnt;

    counter_ce #(
        .BW($clog2(32))
    ) u_counter_ce  // counter with configurable end number
    (
        .clk   ,
        .rst_n ,
        .start (start),
        .num   ($clog2(32)'(32)),
        .run   (1'b1),
        .cnt   (cnt),
        .cnt_en(addr_vld),
        .last  (),
        .done  (done)
    );

    wire [$clog2(5)-1:0] rotate_num = stage % 6;  //stage % addr_cnt, addr_cnt = n_width-2 = 8-2 = 6

    wire [5:0] addr0_bit_rev = {1'b0, cnt[0], cnt[1], cnt[2], cnt[3], cnt[4]};
    wire [5:0] addr1_bit_rev = {1'b1, cnt[0], cnt[1], cnt[2], cnt[3], cnt[4]};

    rbsh #(
        .A_width (6),
        .SH_width(3)
    ) u_rotate_cnt (
        .A    (addr0_bit_rev),
        .SH   (rotate_num),
        .SH_TC(1'b0   ),
        .B    (addr0)
    );

    rbsh #(
        .A_width (6),
        .SH_width(3)
    ) u_rotate_offset (
        .A    (addr1_bit_rev),
        .SH   (rotate_num),
        .SH_TC(1'b0   ),
        .B    (addr1)
    );

    ///////////////////////////////////////
    /*
    always_comb begin
        addrw_vld = addr_vld & (cnt[4:0] == '0);  // 1
        case(stage)
        4'd3: addrw_vld = addr_vld & (cnt[3:0] == '0); // 2
        4'd4: addrw_vld = addr_vld & (cnt[2:0] == '0); // 4
        4'd5: addrw_vld = addr_vld & (cnt[1:0] == '0); // 8
        4'd6: addrw_vld = addr_vld & (cnt[  0] == '0); // 16
        4'd7: addrw_vld = addr_vld;  // 32
        endcase
    end

    assign addrw = cnt >> (4'd7 - stage);
    */

    assign addrw_vld = addr_vld ;
    assign addrw = cnt >> (4'd7 - stage); 

endmodule

module fft_addr_gen_512  // In FFT, 512 refers to 512 complex points, 1024 float number points
(
    input              clk,
    input              rst_n,
    input        [3:0] stage,
    input              start,
    output logic       done,
    output logic       addr_vld,
    output logic [6:0] addr0,      //first vector
    output logic [6:0] addr1,      //second vector
    output logic       addrw_vld,
    output logic [6:0] addrw       //twiddle factor
);

    logic [$clog2(64)-1:0] cnt;

    counter_ce #(
        .BW($clog2(64))
    ) u_counter_ce  // counter with configurable end number
    (
        .clk   ,
        .rst_n ,
        .start (start),
        .num   ($clog2(64)'(64)),
        .run   (1'b1),
        .cnt   (cnt),
        .cnt_en(addr_vld),
        .last  (),
        .done  (done)
    );

    wire [$clog2(7)-1:0] rotate_num = stage % 7;  //stage % addr_cnt, addr_cnt = n_width-2 = 9-2 = 7

    wire [6:0] addr0_bit_rev = {1'b0,cnt[0],cnt[1],cnt[2],cnt[3],cnt[4],cnt[5]};
    wire [6:0] addr1_bit_rev = {1'b1,cnt[0],cnt[1],cnt[2],cnt[3],cnt[4],cnt[5]};

    rbsh #(
        .A_width (7),
        .SH_width(3)
    ) u_rotate_cnt (
        .A    (addr0_bit_rev),
        .SH   (rotate_num),
        .SH_TC(1'b0),
        .B    (addr0)
    );

    rbsh #(
        .A_width (7),
        .SH_width(3)
    ) u_rotate_offset (
        .A    (addr1_bit_rev),
        .SH   (rotate_num),
        .SH_TC(1'b0),
        .B    (addr1)
    );

    ///////////////////////////////////////
    /*
    always_comb begin
        addrw_vld = addr_vld & (cnt[5:0] == '0); // 1
        case(stage)
        4'd3: addrw_vld = addr_vld & (cnt[4:0] == '0); // 2
        4'd4: addrw_vld = addr_vld & (cnt[3:0] == '0); // 4
        4'd5: addrw_vld = addr_vld & (cnt[2:0] == '0); // 8
        4'd6: addrw_vld = addr_vld & (cnt[1:0] == '0); // 16
        4'd7: addrw_vld = addr_vld & (cnt[  0] == '0); // 32
        4'd8: addrw_vld = addr_vld;  // 64
        endcase
    end

    assign addrw = cnt >> (4'd8 - stage);
    */

    assign addrw_vld = addr_vld ;
    assign addrw = cnt >> (4'd8 - stage);


endmodule
