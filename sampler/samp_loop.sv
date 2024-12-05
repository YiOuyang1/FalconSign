`include "sample_pkg.sv"
`include "falconsoar_pkg.sv"

module samp_loop
    import sample_pkg::*;
    import falconsoar_pkg::*;
(
    input              clk              ,
    input              rst_n            ,
    input              start            ,
    input              en               , // 1: enable samp_loop work
    input      [63:0]  fpr_isigma       ,
    input      [63:0]  fpr_r            ,
    input      [79:0]  random_bytes     , // 10 bytes random_bytes
    input      [63:0]  cal2samp_loop    ,
    output reg [63:0]  dout2cal_0       ,
    output reg [63:0]  dout2cal_1       ,
    output reg         choose2cal       , //choose use fpr_mul
    output reg [63:0]  fpr_x            , //fpr [0,-)
    output reg [31:0]  int_z            , //signed 32bits
    output             done
);

    logic [ 4:0] cnt                 ;
    logic [79:0] random_bytes_reg    ;
    logic [ 8:0] cmp_result          ;
    logic [ 4:0] cmp_result_bitsum   ;
    logic [31:0] int_z0              ;
    logic [31:0] int_z0_sqr          ;
    logic [63:0] result_i2flt_0      ;
    logic [63:0] result_i2flt_1      ;
    logic [63:0] fpr_z0_sqr          ;
    logic [63:0] half_isigma_sqr     ;
    logic [63:0] fpr_z_sub_r         ;

    logic [63:0] fp_sub_data_in_0    ;
    logic [63:0] fp_sub_data_in_1    ;
    logic [63:0] fp_sub_data_out     ;
    logic [63:0] fp_sub_data_out_pre ;

    //generate cnt
    always_ff @(posedge clk, negedge rst_n) begin
        if      (~rst_n)                     cnt <= 0          ;
        else if ((cnt == 0) && start )       cnt <= 1          ;
        else if ((cnt == 0) && en )          cnt <= 0          ;
        else if (cnt == 16)                  cnt <= 0          ;
        else if (en == 1 )                   cnt <= cnt + 1'b1 ;
    end

    assign done = (cnt == 16) ;
//////////////////////////////////////////////////////////////////////////////////
//generate cmp result and  in cycle 0 - 1
    wire [31:0] final_cmp_result_bitsum = int_z0 + cmp_result_bitsum;

    always @(posedge clk) begin 
        if ((cnt == 0) && start) random_bytes_reg <= random_bytes ; 
    end

    always @(posedge clk) begin
        if      ((cnt == 0) && en && start) int_z0 <=          cmp_result_bitsum ;
        else if ((cnt == 1) && en )         int_z0 <= int_z0 + cmp_result_bitsum ;
    end

    always @(posedge clk) begin
        if      ((cnt == 1) && en )          int_z <=  (random_bytes_reg[72] == 1)? (final_cmp_result_bitsum + 1) : (~(final_cmp_result_bitsum) + 1) ;
    end

    assign cmp_result_bitsum = cmp_result[0] + cmp_result[1] + cmp_result[2] + cmp_result[3] + cmp_result[4] + cmp_result[5] + cmp_result[6] + cmp_result[7] + cmp_result[8] ;

    always_comb foreach(cmp_result[i]) begin
        case(cnt)
        3'd0:   cmp_result[i] = (random_bytes[71:0] < RCDT[i]);
        3'd1:   cmp_result[i] = (random_bytes_reg[71:0] < RCDT[i+9]);
        default:cmp_result[i] = 1'b0;
        endcase
    end

//////////////////////////////////////////////////////////////////////////////////
//generate fpr_z0_sqr, fpr_z  in cycle 2
    assign int_z0_sqr = int_z0[4:0] * int_z0[4:0] ;

    fp_i2flt_int32_s u_fp_i2flt_int32_s_0 (
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid(1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(int_z),              // input wire [31 : 0] s_axis_a_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(result_i2flt_0)    // output wire [63 : 0] m_axis_result_tdata
    );

    fp_i2flt_int32_s u_fp_i2flt_int32_s_1 (
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid(1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(int_z0_sqr),              // input wire [31 : 0] s_axis_a_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(result_i2flt_1)    // output wire [63 : 0] m_axis_result_tdata
    );

    fp_sub_s u_fp_sub_s (
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid(1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(fp_sub_data_in_0),              // input wire [63 : 0] s_axis_a_tdata
        .s_axis_b_tvalid(1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(fp_sub_data_in_1),              // input wire [63 : 0] s_axis_b_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(fp_sub_data_out_pre)    // output wire [63 : 0] m_axis_result_tdata
    );

//////////////////////////////////////////////////////////////////////////////////
//generate n cycle 3 - 4
    always_ff @(posedge clk) if(en & ((cnt == 'd3) | (cnt == 'd6))) half_isigma_sqr <= cal2samp_loop;

    always_ff @(posedge clk) if(en) begin
        case(cnt)
        'd06: fpr_z_sub_r <= fp_sub_data_out; // reg fpr(z) - fpr(r)
        'd09: fpr_z_sub_r <= cal2samp_loop;   // reg (z-r)^2
        'd12: fpr_z_sub_r <= cal2samp_loop;   // reg (z-r)^2/(2sigma^2)
        endcase
    end

    always_ff @(posedge clk) if(en) begin
        case(cnt)
        'd07: fpr_z0_sqr <= cal2samp_loop; // reg z0^2/(2 sigma_max^2)
        endcase
    end

    always_comb begin
        case(cnt)
        'd0:begin//                            //(isigma)^2
            dout2cal_0 = fpr_isigma;
            dout2cal_1 = fpr_isigma;
            choose2cal = start;
        end
        'd3:begin//                            
            dout2cal_0 = cal2samp_loop;       //0.5 * (isigma)^2   
            dout2cal_1 = FPR_HALF;
            choose2cal = 1'b1;
        end
        'd4:begin//                      
            dout2cal_0 = result_i2flt_1;
            dout2cal_1 = HALF_ISIGMA_MAX_SQR;
            choose2cal = 1'b1;
        end
        'd6:begin//                            //(z-r)^2
            dout2cal_0 = fp_sub_data_out;
            dout2cal_1 = fp_sub_data_out;
            choose2cal = 1'b1;
        end
        'd9:begin//                           //(z-r)^2 * (0.5*isigma^2)
            dout2cal_0 = cal2samp_loop;
            dout2cal_1 = half_isigma_sqr;
            choose2cal = 1'b1;
        end
        default:begin
            dout2cal_0 = '0;
            dout2cal_1 = '0;
            choose2cal = 1'b0;
        end
        endcase
    end

    always_ff @(posedge clk) if(en) begin //zyh
        fp_sub_data_out <= fp_sub_data_out_pre; // zyh
    end

    always_comb begin
        case(cnt)
        'd3:begin//                            //(z-r)
            fp_sub_data_in_0 = result_i2flt_0;
            fp_sub_data_in_1 = fpr_r;
        end
        'd12:begin//                           //(z-r)^2 * (0.5*isigma^2) - z0^2/(2 sigma_max^2)
            fp_sub_data_in_0 = cal2samp_loop;
            fp_sub_data_in_1 = fpr_z0_sqr;
        end
        default:begin
            fp_sub_data_in_0 = 'd0;
            fp_sub_data_in_1 = 'd0;
        end
        endcase
    end

    always_ff @(posedge clk) if((cnt == 'd15) & en) fpr_x <= fp_sub_data_out;

endmodule
