`include "falconsoar_pkg.sv"

module samplerz
    import falconsoar_pkg::*;
(
    input                  clk              ,
    input                  rst_n            ,
    exec_operator_if.slave task_itf         ,
    mem_inst_if.master_rd  mem_rd           ,
    mem_inst_if.master_wr  mem_wr           
);

//////////////////////////////////////////////////////////////////////////////////
//task code
    localparam bit [3:0] SAMPLERZ_INITI = 4'd0;  // Initial Task is to generate first  Blocks PRNG data 
                                                      // to fill buffers in refill_control module
    localparam bit [3:0] SAMPLERZ_512   = 4'd1;  // Normal sampling operation in Falcon-512
    localparam bit [3:0] SAMPLERZ_1024  = 4'd2;  // Normal sampling operation in Falcon-1024

    wire [3:0]  task_type  = task_itf.input_task[14:11];  //
    wire        restart    = task_itf.input_task[15   ];  //
    wire [MEM_ADDR_BITS - 1:0] dst_addr   = task_itf.input_task[TASK_REDUCE_BW - 2*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 3*MEM_ADDR_BITS];  // This is for write dstination addr
    wire [MEM_ADDR_BITS - 1:0] src1_addr  = task_itf.input_task[TASK_REDUCE_BW - 1*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 2*MEM_ADDR_BITS];  // This is for sigma
    wire [MEM_ADDR_BITS - 1:0] src0_addr  = task_itf.input_task[TASK_REDUCE_BW - 0*MEM_ADDR_BITS - 1:TASK_REDUCE_BW - 1*MEM_ADDR_BITS];  // This is for mu and random

//////////////////////////////////////////////////////////////////////////////////
//generate start signal

    wire start = ((task_type == SAMPLERZ_512)   |
                  (task_type == SAMPLERZ_1024)  ) & task_itf.start;

    logic samp_again         ;
    logic done_refill_control; //This done is used only in SAMPLERZ_INITI
    logic done_pre_samp      ;
    logic done_samp_loop     ;
    logic done_berexp        ;
    logic done_chacha20      ;

    logic cnt ; // to count number of sampling, once sampling two value, output sample value and done

    always_ff @(posedge clk) if(start) cnt <= 1'b0; else if(done_berexp) cnt <= ~cnt;

    logic sample_init ;
    always_ff @(posedge clk) begin 
        if(~rst_n)                    sample_init <= 'd1 ; 
        else if(start && restart)    sample_init <= 'd1 ; 
        else if(done_refill_control) sample_init <= 'd0 ; 
    end
    //wire sample_start         = ((task_type == SAMPLERZ_512) | (task_type == SAMPLERZ_1024));  
    wire fetch_en;
    wire start_chacha20       = ((start & restart));
    wire start_refill_control = (sample_init & done_chacha20); // After chacha20 generate first blocks,
                                                               // it should start the refill_control module to fill its' buffer firetly
    wire start_pre_samp       = (start & (~sample_init) & ((task_type == SAMPLERZ_512) | (task_type == SAMPLERZ_1024))) |  (done_refill_control & sample_init);
    wire start_samp_loop      = done_pre_samp | samp_again | (done_berexp & ~cnt);
    wire start_berexp         = done_samp_loop;
    wire done_samplerz        = (done_berexp & cnt) ;

    assign task_itf.op_done = done_samplerz;

    wire [MEM_ADDR_BITS - 1:0] isigma_addr = src1_addr;
    wire [MEM_ADDR_BITS - 1:0] mu_addr     = src0_addr;
    wire [MEM_ADDR_BITS - 1:0] random_addr = 12'd130;

//////////////////////////////////////////////////////////////////////////////////
//nets
    wire        read_1byte      ;
    wire [79:0] dout_10         ;
    wire [ 7:0] dout_1          ;
    wire        part_en         ;

    wire read_10byte = start_samp_loop ;

    //wire [MEM_ADDR_BITS - 1:0] r_addr_refill_control ;
    wire        r_en_pre_samp   ;
    wire [MEM_ADDR_BITS - 1:0] r_addr_pre_samp ;

    wire [63:0] pre_samp2cal_0     ;
    wire [63:0] pre_samp2cal_1     ;
    wire        pre_samp_mode2cal  ;
    wire [63:0] samp_loop2cal_0    ;
    wire [63:0] samp_loop2cal_1    ;
    wire        samp_loop_mode2cal ;
    wire [63:0] cal_out            ;

    wire [63:0] fpr_isigma     ;
    wire [63:0] fpr_ccs        ;
    wire [63:0] fpr_r          ;
    wire [31:0] int_mu_floor   ;
    wire [63:0] fpr_r_l        ;
    wire [31:0] int_mu_floor_l ;
    wire [63:0] fpr_r_r        ;
    wire [31:0] int_mu_floor_r ;

    wire [63:0] fpr_x ;
    wire [31:0] int_z ;

    wire [31:0] int_sample_value ;
    reg  [63:0] sample_value ;
    wire [63:0] fpr_sample_value ;

    wire                       mem_rd_chacha20_en   ; //
    wire [MEM_ADDR_BITS - 1:0] mem_rd_chacha20_addr ; //
    wire [   BANK_WIDTH - 1:0] mem_rd_chacha20_data ; //

    wire [511:0] prng_data;  // This data is the prng data used bu samplerZ ,from chacha20 

    always_ff @(posedge clk) if((done_berexp) & (cnt == 0)) sample_value <= fpr_sample_value;

    assign int_sample_value = int_mu_floor + int_z;
    assign fpr_r = cnt ? fpr_r_r : fpr_r_l;
    assign int_mu_floor = cnt ? int_mu_floor_r : int_mu_floor_l;

    assign mem_rd.en   = sample_init & (~start_pre_samp) ?  mem_rd_chacha20_en  :r_en_pre_samp;
    assign mem_rd.addr = sample_init & (~start_pre_samp) ?  mem_rd_chacha20_addr:r_addr_pre_samp;
    assign mem_rd_chacha20_data = mem_rd.data; 

    assign mem_wr.en   = (done_samplerz & cnt);
    assign mem_wr.addr = dst_addr ;
    assign mem_wr.data = {128'h0,fpr_sample_value,sample_value};  

//////////////////////////////////////////////////////////////////////////////////
//instance
    refill_control i_refill_control
    (
        .clk              (   clk                     ), //i
        .rst_n            (   rst_n                   ), //i
        .start            (   start_refill_control    ), //i
        .init             (   sample_init             ), //i first use samplez, fullill buffer
        .fetch_en         (   fetch_en                ), //o
        .r_data           (   prng_data               ), //i [511:0]
        .read_10byte      (   read_10byte             ), //i
        .read_1byte       (   read_1byte              ), //i
        .dout_10          (   dout_10                 ), //o [ 79:0]
        .dout_1           (   dout_1                  ), //o [  7:0]
        .part_en          (   part_en                 ), //o 1: enable 'berexp' module work, 0: 'berexp' module pause
        .done             (   done_refill_control     )  //o
    );

    pre_samp i_pre_samp
    (
        .clk            (   clk                 ), //i
        .rst_n          (   rst_n               ), //i
        .start          (   start_pre_samp      ), //i start pre_samp
        .task_type      (   task_type           ), //i [  4:0]
        .r_en           (   r_en_pre_samp       ), //o
        .r_addr         (   r_addr_pre_samp     ), //o [  9:0]
        .r_data         (   mem_rd.data         ), //i [511:0]
        .mu_addr        (   mu_addr             ), //i [  9:0]
        .isigma_addr    (   isigma_addr         ), //i [  9:0]
        .cal2pre_samp   (   cal_out             ), //i [ 63:0]
        .dout2cal_0     (   pre_samp2cal_0      ), //o [ 63:0]
        .dout2cal_1     (   pre_samp2cal_1      ), //o [ 63:0]
        .choose2cal     (   pre_samp_mode2cal   ), //o choose use fpr_mul
        .fpr_isigma     (   fpr_isigma          ), //o [ 63:0]
        .fpr_ccs        (   fpr_ccs             ), //o [ 63:0]
        .fpr_r_l        (   fpr_r_l             ), //o [ 63:0]
        .int_mu_floor_l (   int_mu_floor_l      ), //o [ 31:0]
        .fpr_r_r        (   fpr_r_r             ), //o [ 63:0]
        .int_mu_floor_r (   int_mu_floor_r      ), //o [ 31:0]
        .done           (   done_pre_samp       )  //o
    );

    samp_loop i_samp_loop
    (
        .clk            (   clk                 ), //i
        .rst_n          (   rst_n               ), //i
        .start          (   start_samp_loop     ), //i
        .en             (   part_en             ), //i 1: enable samp_loop work
        .fpr_isigma     (   fpr_isigma          ), //i [63:0]
        .fpr_r          (   fpr_r               ), //i [63:0]
        .random_bytes   (   dout_10             ), //i [79:0] 10 bytes random_bytes
        .cal2samp_loop  (   cal_out             ), //i [63:0]
        .dout2cal_0     (   samp_loop2cal_0     ), //o [63:0]
        .dout2cal_1     (   samp_loop2cal_1     ), //o [63:0]
        .choose2cal     (   samp_loop_mode2cal  ), //o choose use fpr_mul
        .fpr_x          (   fpr_x               ), //o [63:0]
        .int_z          (   int_z               ), //o [31:0]
        .done           (   done_samp_loop      )  //o
    );

    berexp i_berexp
    (
        .clk            (   clk                 ), // i
        .rst_n          (   rst_n               ), // i
        .start          (   start_berexp        ), // i
        .en             (   part_en             ), // i
        .fpr_x          (   fpr_x               ), // i [63:0]
        .fpr_ccs        (   fpr_ccs             ), // i [63:0]
        .random_req     (   read_1byte          ), // o require a random byte
        .random_bytes   (   dout_1              ), // i [ 7:0]
        .samp_again     (   samp_again          ), // o
        .done           (   done_berexp         )  // o
    );

    fpr_cal i_fpr_cal
    (
        .clk            (   clk                 ),
        .data_a0        (   pre_samp2cal_0      ), // i [63:0]
        .data_a1        (   pre_samp2cal_1      ), // i [63:0]
        .choose_a       (   pre_samp_mode2cal   ), // i
        .data_b0        (   samp_loop2cal_0     ), // i [63:0]
        .data_b1        (   samp_loop2cal_1     ), // i [63:0]
        .choose_b       (   samp_loop_mode2cal  ), // i
        .data_out       (   cal_out             )  // o [63:0]
    );

    fp_i2flt_int32 fp_i2flt_U0 (
        .aclk(clk),                               // input wire aclk
        .s_axis_a_tvalid(1),                      // input wire s_axis_a_tvalid
        .s_axis_a_tdata(int_sample_value),        // input wire [15 : 0] s_axis_a_tdata
        .m_axis_result_tready(1),                 // input wire m_axis_result_tready
        .m_axis_result_tdata(fpr_sample_value)    // output wire [63 : 0] m_axis_result_tdata
    );

    chacha20 i_chacha20
    (
        .clk                 (  clk                 ),
        .rst_n               (  rst_n               ),
        .start               (  start_chacha20      ),  //start signal to generate PRNG data
        .restart             (  restart             ),  //restart signal to generate PRNG data using new Random seed
        .sample_init         (  sample_init         ),
        .fetch_en            (  fetch_en            ),
        .done                (  done_chacha20       ),  //generate PRNG data done!
        .src_addr            (  random_addr         ),  //the addr to fetch the PRNG seed
        .mem_rd_chacha20_en  ( mem_rd_chacha20_en   ),
        .mem_rd_chacha20_addr( mem_rd_chacha20_addr ),  
        .mem_rd_chacha20_data( mem_rd_chacha20_data ),   
        .data_o              (  prng_data           )   //PRNG data (512bits) output
    );
endmodule
