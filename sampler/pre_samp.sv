`include "sample_pkg.sv"
`include "falconsoar_pkg.sv"

module pre_samp
    import sample_pkg::*;
    import falconsoar_pkg::*;
(
    input                 clk               ,
    input                 rst_n             ,
    input                 start             , //start pre_samp
    input        [  3:0]  task_type         ,
    output                r_en              ,
    output logic [ MEM_ADDR_BITS - 1:0]  r_addr            ,
    input        [255:0]  r_data            , 
    input        [ MEM_ADDR_BITS - 1:0]  mu_addr           ,
    input        [ MEM_ADDR_BITS - 1:0]  isigma_addr       ,
    input        [ 63:0]  cal2pre_samp      ,
    output logic [ 63:0]  dout2cal_0        ,
    output logic [ 63:0]  dout2cal_1        ,
    output logic          choose2cal        , //choose use fpr_mul
    output logic [ 63:0]  fpr_isigma        ,
    output logic [ 63:0]  fpr_ccs           , //fpr range(0:-)
    output logic [ 63:0]  fpr_r_l           , //fpr range[0:1) , real part
    output logic [ 31:0]  int_mu_floor_l    , //signed , real part
    output logic [ 63:0]  fpr_r_r           , //fpr range[0:1) , imag part
    output logic [ 31:0]  int_mu_floor_r    , //signed , imag part
    output                done
);
    logic [ 1:0] isigma_index ;
    logic [ 3:0] cnt          ;
    logic [63:0] fpr_mu_r     ;
    logic [63:0] fpr_mu_l     ;
    logic [63:0] fpr_mu_sel       ;
    logic [31:0] int_mu_floor ;
    logic [31:0] int_mu_floor_r_redundancy ;
    logic [31:0] int_mu_floor_l_redundancy ;
    logic [31:0] result_flt2i ;
    logic [31:0] result_flt2i_redundancy ;    
    logic [63:0] result_fp_sub;
    logic [63:0] result_fp_sub_redundancy ;
    logic [63:0] fpr_mu_floor ;
    logic [63:0] fpr_mu_floor_redundancy  ;
    logic [63:0] fpr_sigma_min;

    //generate cnt
    always_ff @(posedge clk, negedge rst_n) begin
        if      (!rst_n)               cnt <= 0          ;
        else if ((cnt == 0) && start ) cnt <= 1          ;
        else if (cnt == 0)             cnt <= 0          ;
        else if (cnt == 11)            cnt <= 0          ; // complete two samplings
        else                           cnt <= cnt + 1'b1 ;
    end

    assign done = (cnt == 9) ;
    assign fpr_sigma_min = (task_type == SAMPLERZ_512) ? SIGMA_MIN_512: SIGMA_MIN_1024;
//////////////////////////////////////////////////////////////////////////////////
//generate r_en and r_addr in cycle 0 - 1
    assign r_en = ((cnt == 'd0) & start) | (cnt == 'd1);

    always_comb begin
        r_addr = 'x;
        case(cnt)
        3'd0: r_addr = mu_addr;
        3'd1: r_addr = isigma_addr;
        endcase
    end
//////////////////////////////////////////////////////////////////////////////////
    always_ff @(posedge clk, negedge rst_n) begin
        if      (!rst_n)                           isigma_index <= 'd0                ;
        else if (cnt == (SAMPLERZ_READ_DELAY + 1)) isigma_index <= isigma_index + 'd1 ;
    end
//////////////////////////////////////////////////////////////////////////////////
    always_ff @(posedge clk) if(cnt == (SAMPLERZ_READ_DELAY))       {fpr_mu_r,fpr_mu_l} <= r_data[127:0];
    always_ff @(posedge clk) if(cnt == (SAMPLERZ_READ_DELAY + 1))   fpr_isigma          <= r_data[64*isigma_index+:64];
    always_ff @(posedge clk) if(cnt == (SAMPLERZ_READ_DELAY + 4))   fpr_r_l             <= (result_fp_sub[63] == 1'd1)? result_fp_sub_redundancy : result_fp_sub ;
    always_ff @(posedge clk) if(cnt == (SAMPLERZ_READ_DELAY + 5))   fpr_r_r             <= (result_fp_sub[63] == 1'd1)? result_fp_sub_redundancy : result_fp_sub ;
    always_ff @(posedge clk) if(cnt == (SAMPLERZ_READ_DELAY + 5))   fpr_ccs             <= cal2pre_samp ;

    always_comb int_mu_floor_l_redundancy = int_mu_floor_l - 1 ; 
    always_ff @(posedge clk) begin
        if(cnt == (SAMPLERZ_READ_DELAY + 1))                            int_mu_floor_l <= result_flt2i ;
        else if(cnt == (SAMPLERZ_READ_DELAY + 4) && result_fp_sub[63])  int_mu_floor_l <= int_mu_floor_l_redundancy ;
    end

    always_comb int_mu_floor_r_redundancy = int_mu_floor_r - 1 ; 
    always_ff @(posedge clk) begin
        if(cnt == (SAMPLERZ_READ_DELAY + 2))                            int_mu_floor_r <= result_flt2i ;
        else if(cnt == (SAMPLERZ_READ_DELAY + 5) && result_fp_sub[63])  int_mu_floor_r <= int_mu_floor_r_redundancy ; 
    end

    always_comb begin
        fpr_mu_sel = 'x;
        case(cnt)
        (SAMPLERZ_READ_DELAY + 0):fpr_mu_sel = r_data[0+:64];
        (SAMPLERZ_READ_DELAY + 1):fpr_mu_sel = fpr_mu_r;
        (SAMPLERZ_READ_DELAY + 2):fpr_mu_sel = fpr_mu_l;
        (SAMPLERZ_READ_DELAY + 3):fpr_mu_sel = fpr_mu_r;
        endcase
    end

    always_comb begin
        int_mu_floor = 'x;
        case(cnt)
        4'd3:int_mu_floor = '0;
        4'd4:int_mu_floor = int_mu_floor_l;
        4'd5,
        4'd6,
        4'd7:int_mu_floor = int_mu_floor_r;
        endcase
    end

//////////////////////////////////////////////////////////////////////////////////
//for input and output of fprcal module
    assign dout2cal_0 = fpr_isigma;  // sigma_min * isigma
    assign dout2cal_1 = fpr_sigma_min;
    assign choose2cal = ((cnt == (SAMPLERZ_READ_DELAY + 2)));

// flt2i int floor mu
    fp_flt2i_int32_s u_fp_flt2i_int32_s_0 (
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid(1'd1),                      // input wire s_axis_a_tvalid
        .s_axis_a_tdata(fpr_mu_sel),             // input wire [63 : 0] s_axis_a_tdata
        .m_axis_result_tvalid( ),                    // output wire m_axis_result_tvalid
        .m_axis_result_tdata(result_flt2i)    // output wire [31 : 0] m_axis_result_tdata
    );
    // i2flt int floor mu 2 fpr floor mu
    fp_i2flt_int32_s u_fp_i2flt_int32_s_0 (
        .aclk(clk),                                   // input wire aclk
        .s_axis_a_tvalid(1),                          // input wire s_axis_a_tvalid
        .s_axis_a_tdata(result_flt2i),              // input wire [31 : 0] s_axis_a_tdata
        .m_axis_result_tvalid(  ),                    // output wire m_axis_result_tvalid
        .m_axis_result_tdata(fpr_mu_floor)     // output wire [63 : 0] m_axis_result_tdata
    );

    // i2flt int floor mu 2 fpr floor mu in redundancy
    always_comb result_flt2i_redundancy = result_flt2i + 32'hFFFF_FFFF ;//FIXME
    fp_i2flt_int32_s u_fp_i2flt_int32_s_1 (
        .aclk(clk),                                   // input wire aclk
        .s_axis_a_tvalid(1),                          // input wire s_axis_a_tvalid
        .s_axis_a_tdata(result_flt2i_redundancy),              // input wire [31 : 0] s_axis_a_tdata
        .m_axis_result_tvalid(  ),                    // output wire m_axis_result_tvalid
        .m_axis_result_tdata(fpr_mu_floor_redundancy)     // output wire [63 : 0] m_axis_result_tdata
    );
    // mu - fpr floor mu
    fp_sub_s u_fp_sub_s_0 (
        .aclk(clk),                                   // input wire aclk
        .s_axis_a_tvalid(1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(fpr_mu_sel),              // input wire [63 : 0] s_axis_a_tdata
        .s_axis_b_tvalid(1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(fpr_mu_floor),              // input wire [63 : 0] s_axis_b_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(result_fp_sub)    // output wire [63 : 0] m_axis_result_tdata
    );
    // mu - fpr floor mu in redundancy
    fp_sub_s u_fp_sub_s_1 (
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid(1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(fpr_mu_sel),              // input wire [63 : 0] s_axis_a_tdata
        .s_axis_b_tvalid(1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(fpr_mu_floor_redundancy),              // input wire [63 : 0] s_axis_b_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(result_fp_sub_redundancy)    // output wire [63 : 0] m_axis_result_tdata
    );

endmodule