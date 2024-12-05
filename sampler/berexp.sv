`include "sample_pkg.sv"
`include "falconsoar_pkg.sv"
module berexp
    import sample_pkg::*;
    import falconsoar_pkg::*;
(
    input             clk           ,
    input             rst_n         ,
    input             start         ,
    input             en            ,
    input      [63:0] fpr_x         ,
    input      [63:0] fpr_ccs       ,
    output logic      random_req    , //require a random byte
    input      [ 7:0] random_bytes  ,
    output logic      samp_again    ,
    output logic      done
);

    logic [5:0] cnt; //up to 47

    //generate cnt
    always_ff @(posedge clk, negedge rst_n) begin
        if     (~rst_n)                     cnt <= '0;
        else if((cnt == 0) && start && en ) cnt <= 6'd1;
        else if((cnt == 0) && en )          cnt <= 6'd0;
        else if((done == 1) && en)          cnt <= 6'd0;
        else if((samp_again == 1) && en)    cnt <= 6'd0;
        else if(en)                         cnt <= cnt + 1'b1;
    end

    logic        r_floor_flag       ;
    logic        z_floor_flag       ;
    logic        z_final_floor_flag ;

    logic [63:0] int_s              ;
    logic [63:0] int_s_redundancy   ;

    logic [63:0] fpr_r              ;
    logic [63:0] fpr_r_sel          ;

    logic [63:0] fpr_z              ; //reg the value to chosse floor result
    logic [63:0] int_z              ;
    logic [63:0] int_z_redundancy   ;
    logic [63:0] fpr_z_final        ;//reg the value to chosse floor result
    logic [63:0] int_z_final        ;
    logic [63:0] result_shitf;

    logic [63:0] fp_mult_data_in_0 ;
    logic [63:0] fp_mult_data_in_1 ;
    logic [63:0] fp_mult_data_out  ;

    logic [63:0] fp_flt2i_data_in  ;
    logic [63:0] fp_flt2i_data_out ;

    logic [63:0] fp_i2flt_data_in  ;
    logic [63:0] fp_i2flt_data_out ;

    logic [63:0] fp_sub_data_in_0  ;
    logic [63:0] fp_sub_data_in_1  ;
    logic [63:0] fp_sub_data_out   ;

    
//////////////////////////////////////////////////////////////////////////////////////////
    always_ff @(posedge clk) begin if(cnt == 'd08) r_floor_flag       <=  fp_sub_data_out[63]; end
    always_ff @(posedge clk) begin if(cnt == 'd17) z_floor_flag       <=  fp_sub_data_out[63]; end
    always_ff @(posedge clk) begin if(cnt == 'd18) z_final_floor_flag <=  fp_sub_data_out[63]; end

    always_ff @(posedge clk) begin 
        if(cnt == 'd03)                          int_s <= fp_flt2i_data_out ; 
        else if((cnt == 'd09) && (r_floor_flag)) int_s <= int_s_redundancy ; 
    end
    always_ff @(posedge clk) begin if(cnt == 'd08) fpr_r            <= fp_sub_data_out   ; end
    always_ff @(posedge clk) begin if(cnt == 'd12) fpr_z            <= fp_mult_data_out  ; end
    always_ff @(posedge clk) begin if(cnt == 'd13) int_z            <= fp_flt2i_data_out ; end
    always_ff @(posedge clk) begin if(cnt == 'd13) int_z_redundancy <= fp_flt2i_data_out - 1; end
    always_ff @(posedge clk) begin if(cnt == 'd14) fpr_z_final      <= fp_mult_data_out  ; end
    always_ff @(posedge clk) begin 
        if(cnt == 'd15)                                int_z_final <= fp_flt2i_data_out ; 
        else if ((cnt == 'd19) && z_final_floor_flag)  int_z_final <= fp_flt2i_data_out - 1;
        else if ((cnt == 'd39))                        int_z_final <= result_shitf;
    end

    always_comb int_s_redundancy = int_s - 1;
    //always_comb fpr_r_sel = r_floor_flag ? fp_sub_data_out : fpr_r ;
    always_ff @(posedge clk) fpr_r_sel = r_floor_flag ? fp_sub_data_out : fpr_r ;

///////////////////////////////////////////////////////////////////////////////////////////////
//this is for mult compute data sel
    always_comb begin
        case(cnt)
        6'd00: begin fp_mult_data_in_0 = fpr_x            ; fp_mult_data_in_1 = ILN2     ; end
        6'd04: begin fp_mult_data_in_0 = fp_i2flt_data_out; fp_mult_data_in_1 = LN2      ; end
        6'd05: begin fp_mult_data_in_0 = fp_i2flt_data_out; fp_mult_data_in_1 = LN2      ; end
        6'd10: begin fp_mult_data_in_0 = fpr_r_sel        ; fp_mult_data_in_1 = TWOPOW63 ; end
        6'd12: begin fp_mult_data_in_0 = fpr_ccs          ; fp_mult_data_in_1 = TWOPOW63 ; end
        default: begin fp_mult_data_in_0 = 'dx ; fp_mult_data_in_1 = 'dx ; end
        endcase
    end
///////////////////////////////////////////////////////////////////////////////////////////////
//this is for sub compute data sel
    always_comb begin
        case(cnt)
        6'd06: begin fp_sub_data_in_0 = fpr_x       ; fp_sub_data_in_1 = fp_mult_data_out  ; end
        6'd07: begin fp_sub_data_in_0 = fpr_x       ; fp_sub_data_in_1 = fp_mult_data_out  ; end
        6'd15: begin fp_sub_data_in_0 = fpr_z       ; fp_sub_data_in_1 = fp_i2flt_data_out ; end
        6'd16: begin fp_sub_data_in_0 = fpr_z_final ; fp_sub_data_in_1 = fp_i2flt_data_out ; end
        default: begin fp_sub_data_in_0 = 'dx ; fp_sub_data_in_1 = 'dx ; end
        endcase
    end
///////////////////////////////////////////////////////////////////////////////////////////////
//this is for flt2i data sel
    always_comb begin
        case(cnt)
        6'd02: fp_flt2i_data_in = fp_mult_data_out;
        6'd12: fp_flt2i_data_in = fp_mult_data_out;
        6'd14: fp_flt2i_data_in = fp_mult_data_out;
        default: fp_flt2i_data_in = 'dx;
        endcase
    end
///////////////////////////////////////////////////////////////////////////////////////////////
//this is for i2flt data sel
    always_comb begin
        case(cnt)
        6'd03: fp_i2flt_data_in = fp_flt2i_data_out;
        6'd04: fp_i2flt_data_in = int_s_redundancy ;
        6'd14: fp_i2flt_data_in = int_z            ;
        6'd15: fp_i2flt_data_in = fp_flt2i_data_out;
        default: fp_i2flt_data_in = 'dx ;
        endcase
    end

// compute fpr(s) = fpr(a) * fpr(b)
    fp_mult_s u_fp_mult_s (
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid(1'd1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(fp_mult_data_in_0),              // input wire [63 : 0] s_axis_a_tdata
        .s_axis_b_tvalid(1'd1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(fp_mult_data_in_1),              // input wire [63 : 0] s_axis_b_tdata
        .m_axis_result_tvalid(   ),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(fp_mult_data_out)     // output wire [63 : 0] m_axis_result_tdata
    );
// compute flt2i int64
    fp_flt2i_int64_s u_fp_flt2i_int64_s (
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid(1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(fp_flt2i_data_in),              // input wire [63 : 0] s_axis_a_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(fp_flt2i_data_out)    // output wire [63 : 0] m_axis_result_tdata
    );
// compute fpr(s) = fpr(a) * fpr(b)
    fp_i2flt_int64_s u_fp_i2flt_int64_s (
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid(1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(fp_i2flt_data_in),              // input wire [63 : 0] s_axis_a_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(fp_i2flt_data_out)    // output wire [63 : 0] m_axis_result_tdata
    );
// compute fpr(s) = fpr(a) - fpr(b)
    fp_sub_s u_fp_sub_s (
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid(1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(fp_sub_data_in_0),              // input wire [63 : 0] s_axis_a_tdata
        .s_axis_b_tvalid(1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(fp_sub_data_in_1),              // input wire [63 : 0] s_axis_b_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(fp_sub_data_out)    // output wire [63 : 0] m_axis_result_tdata
    );

    logic [127:0] products1;
    logic [127:0] products1_redundancy;
    logic [63:0] int_c;
    logic [63:0] int_c_pre;
    logic [63:0] int_y;
    logic [63:0] int_y_redundancy;
    logic [63:0] int_y_sel;
    logic [63:0] int_y_final;
    logic [127:0] products_z_y_final;


// products1 = int_z * int_y ;
    always_ff @(posedge clk) products1 <= int_z * int_y ;
    always_ff @(posedge clk) products1_redundancy <= int_z_redundancy * int_y_redundancy ;
    //always_ff @(posedge clk) products1 <= int_z * int_y ;
    //always_ff @(posedge clk) products1_redundancy <= int_z_redundancy * int_y_redundancy ;

    always_ff @(posedge clk) if(en) begin
        if     (cnt == 'd13) begin int_y <= C_TABLE[0]                 ; int_y_redundancy <= C_TABLE[0]                           ; end
        else if(cnt <  'd38) begin int_y <= int_c - products1[126:63]  ; int_y_redundancy <= int_c - products1_redundancy[126:63] ; end
    end
    
    always_ff @(posedge clk) int_c <= int_c_pre ;
    always_comb begin
        int_c_pre = 'x;
        case(cnt)
        'd13: int_c_pre = C_TABLE[ 1];
        'd14: int_c_pre = C_TABLE[ 1];
        'd15: int_c_pre = C_TABLE[ 2];
        'd16: int_c_pre = C_TABLE[ 2];
        'd17: int_c_pre = C_TABLE[ 3];
        'd18: int_c_pre = C_TABLE[ 3];
        'd19: int_c_pre = C_TABLE[ 4];
        'd20: int_c_pre = C_TABLE[ 4];
        'd21: int_c_pre = C_TABLE[ 5];
        'd22: int_c_pre = C_TABLE[ 5];
        'd23: int_c_pre = C_TABLE[ 6];
        'd24: int_c_pre = C_TABLE[ 6];
        'd25: int_c_pre = C_TABLE[ 7];
        'd26: int_c_pre = C_TABLE[ 7];
        'd27: int_c_pre = C_TABLE[ 8];
        'd28: int_c_pre = C_TABLE[ 8];
        'd29: int_c_pre = C_TABLE[ 9];
        'd30: int_c_pre = C_TABLE[ 9];
        'd31: int_c_pre = C_TABLE[10];
        'd32: int_c_pre = C_TABLE[10];
        'd33: int_c_pre = C_TABLE[11];
        'd34: int_c_pre = C_TABLE[11];
        'd35: int_c_pre = C_TABLE[12];
        'd36: int_c_pre = C_TABLE[12];
        endcase
    end

    always_comb int_y_sel = z_floor_flag ? int_y_redundancy: int_y ;
    always_comb products_z_y_final = (int_y_sel * int_z_final);
    always_ff @(posedge clk) int_y_final <= products_z_y_final[63+:64];
    //always_ff @(posedge clk) int_y_final <= ((int_y_sel * int_z_final) >> 63);

//////////////////////////////////////////////////////////////////////////////////
    logic [63:0] products2;


// (2 * ApproExp(r,ccs) - 1) >> s
    assign products2 = 2 * int_y_final - 1 ;

    assign result_shitf = products2 >> int_s[5:0];

//////////////////////////////////////////////////////////////////////////////////
//compare from 43-50
    logic [7:0] uniformbits ;
    logic       cmp_equl    ;
    logic       cmp_bigger  ;
    logic       cmp_smaller ;

    always_ff @(posedge clk) if(random_req & en) uniformbits <= random_bytes;

    assign random_req = (en & ~(done | samp_again)) & ((cnt == 'd38) | ((cnt > 'd38) & (cnt < 'd46) & cmp_equl));

    always_ff @(posedge clk) begin
        if     ((done | samp_again) & en )                     done <= 1'b0; // set 'done' to 1 just one cycle
        else if((cnt > 'd38) & (cnt < 'd47) & cmp_bigger & en) done <= 1'b1; // get satisfied value
        else                                                   done <= 1'b0;
    end

    always_ff @(posedge clk) begin
        if     ((done | samp_again) & en)                       samp_again <= 1'b0;  // set 'done' to 1 just one cycle
        else if((cnt > 'd38) & (cnt < 'd46) & en & cmp_smaller) samp_again <= 1'b1;  // not get satisfied value
        else if((cnt == 'd46) & en & ~cmp_bigger)               samp_again <= 1'b1;  // not get satisfied value
        else                                                    samp_again <= 1'b0;
    end

    always_comb begin
        cmp_equl    = 1'b0;
        cmp_bigger  = 1'bx;
        cmp_smaller = 1'bx;
        case(cnt)
        'd39:begin cmp_equl = (uniformbits ==     result_shitf[8*7+:8]); cmp_bigger = (uniformbits <     result_shitf[8*7+:8]); cmp_smaller = (uniformbits >     result_shitf[8*7+:8]); end
        'd40:begin cmp_equl = (uniformbits ==      int_z_final[8*6+:8]); cmp_bigger = (uniformbits <      int_z_final[8*6+:8]); cmp_smaller = (uniformbits >      int_z_final[8*6+:8]); end
        'd41:begin cmp_equl = (uniformbits ==      int_z_final[8*5+:8]); cmp_bigger = (uniformbits <      int_z_final[8*5+:8]); cmp_smaller = (uniformbits >      int_z_final[8*5+:8]); end
        'd42:begin cmp_equl = (uniformbits ==      int_z_final[8*4+:8]); cmp_bigger = (uniformbits <      int_z_final[8*4+:8]); cmp_smaller = (uniformbits >      int_z_final[8*4+:8]); end
        'd43:begin cmp_equl = (uniformbits ==      int_z_final[8*3+:8]); cmp_bigger = (uniformbits <      int_z_final[8*3+:8]); cmp_smaller = (uniformbits >      int_z_final[8*3+:8]); end
        'd44:begin cmp_equl = (uniformbits ==      int_z_final[8*2+:8]); cmp_bigger = (uniformbits <      int_z_final[8*2+:8]); cmp_smaller = (uniformbits >      int_z_final[8*2+:8]); end
        'd45:begin cmp_equl = (uniformbits ==      int_z_final[8*1+:8]); cmp_bigger = (uniformbits <      int_z_final[8*1+:8]); cmp_smaller = (uniformbits >      int_z_final[8*1+:8]); end
        'd46:begin cmp_equl = (uniformbits ==      int_z_final[8*0+:8]); cmp_bigger = (uniformbits <      int_z_final[8*0+:8]); cmp_smaller = (uniformbits >      int_z_final[8*0+:8]); end
        endcase
    end

endmodule


/*
module mulllllllllllllllllt(
    input[63:0] a ,
    input[63:0] b ,
    input[63:0] c ,
    output[63:0] d 
);
    wire [63:0] tmp0 = a[63:32] * b[63:32];
    wire [63:0] tmp1 = a[63:32] * b[31: 0];
    wire [63:0] tmp2 = a[31: 0] * b[63:32];
    wire [63:0] tmp3 = a[31: 0] * b[31: 0];

    wire [63:0] result0 = c - tmp0 - tmp1[63:32] - tmp2[63:32];
    wire [63:0] result1 = c - tmp0 - tmp1[63:32] - tmp2[63:32] - 1;
    wire [63:0] result2 = c - tmp0 - tmp1[63:32] - tmp2[63:32] - 2;
    wire [63:0] flag = tmp3[63:32] + tmp2[31: 0] + tmp1[31: 0] ;

    assign d = flag[33] ? result2 : (flag[32] ? result1 : result0) ;

endmodule
*/