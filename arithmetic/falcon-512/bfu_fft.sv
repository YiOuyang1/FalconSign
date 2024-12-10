module bfu_fft
    import falconsoar_pkg::*;
(
    input             clk       ,
    input             en        ,
    input      [ 3:0] mode      ,
    input      [63:0] d_i_a_re  ,
    input      [63:0] d_i_a_im  ,
    input      [63:0] d_i_b_re  ,
    input      [63:0] d_i_b_im  ,
    input      [63:0] d_i_c_re  ,
    input      [63:0] d_i_c_im  ,
    output reg [63:0] d_o_a_re  ,
    output reg [63:0] d_o_a_im  ,
    output reg [63:0] d_o_b_re  ,
    output reg [63:0] d_o_b_im
    );
    
    parameter ROUND = 3'b000;

    localparam FFT     = 4'd0 ;
    localparam IFFT    = 4'd1 ;
    localparam FPR_ADD = 4'd2 ;
    localparam FPR_SUB = 4'd3 ;
    localparam FPR_MUL = 4'd4 ;
    localparam FPC_ADD = 4'd5 ;
    localparam FPC_SUB = 4'd6 ;
    localparam FPC_MUL = 4'd7 ;
    localparam FPC_ADJ = 4'd8 ;
    localparam FPC_SQR = 4'd9 ;

    localparam FPR_ZERO = 64'h0000000000000000;
    localparam FPR_HALF = 64'h3FE0000000000000;
    localparam FPR_ONE  = 64'h3FF0000000000000;

//////////////////////////////////////////////////////////////////////////////////
// First Pipeline Segments, But there is no reg to pipe, just for name
    wire [63:0] z_add1;
    wire [63:0] z_add2;
    wire [63:0] z_sub1;
    wire [63:0] z_sub2;

    reg [63:0] a_re_line1;
    reg [63:0] a_im_line1;
    reg [63:0] b_re_line1;
    reg [63:0] b_im_line1;
    reg [63:0] c_re_line1;
    reg [63:0] c_im_line1;

    always_comb a_re_line1 = d_i_a_re ;
    always_comb a_im_line1 = d_i_a_im ;
    always_comb b_re_line1 = d_i_b_re ;
    always_comb b_im_line1 = ( mode == FPR_MUL)?    FPR_ZERO: d_i_b_im ;
    always_comb c_re_line1 = ((mode == FPR_SUB) || (mode == FPC_SUB))?   FPR_ONE : d_i_c_re ;
    always_comb c_im_line1 = ((mode == FPR_SUB) || (mode == FPR_MUL) || (mode == FPC_SUB))? FPR_ZERO: d_i_c_im ;

    //floating_point_add_2pip #(.sig_width(52), .exp_width(11), .ieee_compliance(1))
    //    fp_add1 ( .a(a_re_line1), .b(b_re_line1), .rnd(ROUND), .z(z_add1), .status() );
    floating_point_add_2pip fp_add1(
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(a_re_line1),              // input wire [63 : 0] s_axis_a_tdata
        .s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(b_re_line1),              // input wire [63 : 0] s_axis_b_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_add1)    // output wire [63 : 0] m_axis_result_tdata
    );
    //floating_point_add_2pip #(.sig_width(52), .exp_width(11), .ieee_compliance(1))
    //    fp_add2 ( .a(a_im_line1), .b(b_im_line1), .rnd(ROUND), .z(z_add2), .status() );
    floating_point_add_2pip fp_add2(
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(a_im_line1),              // input wire [63 : 0] s_axis_a_tdata
        .s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(b_im_line1),              // input wire [63 : 0] s_axis_b_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_add2)    // output wire [63 : 0] m_axis_result_tdata
    );
    //floating_point_sub_2pip #(.sig_width(52), .exp_width(11), .ieee_compliance(1))
    //    fp_sub1 ( .a(a_re_line1), .b(b_re_line1), .rnd(ROUND), .z(z_sub1), .status() );
    floating_point_sub_2pip fp_sub1(
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(a_re_line1),              // input wire [63 : 0] s_axis_a_tdata
        .s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(b_re_line1),              // input wire [63 : 0] s_axis_b_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_sub1)    // output wire [63 : 0] m_axis_result_tdata
    );
    //floating_point_sub_2pip #(.sig_width(52), .exp_width(11), .ieee_compliance(1))
    //    fp_sub2 ( .a(a_im_line1), .b(b_im_line1), .rnd(ROUND), .z(z_sub2), .status() );
    floating_point_sub_2pip fp_sub2(
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(a_im_line1),              // input wire [63 : 0] s_axis_a_tdata
        .s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(b_im_line1),              // input wire [63 : 0] s_axis_b_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_sub2)    // output wire [63 : 0] m_axis_result_tdata
    );  
//////////////////////////////////////////////////////////////////////////////////
// Second Pipeline Segments
    wire neg_c_im = ~c_im_line1[63];

    wire [63:0] z_mul1;
    wire [63:0] z_mul2;
    wire [63:0] z_mul3;
    wire [63:0] z_mul4;
    wire [63:0] z_mul5;
    wire [63:0] z_mul6;

    reg [63:0] a_re_pipe2, a_re_pipe2_pre, a_re_pipe2_pre2;
    reg [63:0] a_im_pipe2, a_im_pipe2_pre, a_im_pipe2_pre2;
    reg [63:0] b_re_pipe2, b_re_pipe2_pre, b_re_pipe2_pre2;
    reg [63:0] b_im_pipe2, b_im_pipe2_pre, b_im_pipe2_pre2;
    reg [63:0] c_re_pipe2, c_re_pipe2_pre, c_re_pipe2_pre2;
    reg [63:0] c_im_pipe2, c_im_pipe2_pre, c_im_pipe2_pre2;

    always_ff @(posedge clk) begin
        a_re_pipe2_pre2 <= a_re_line1;
        a_im_pipe2_pre2 <= a_im_line1;
        b_re_pipe2_pre2 <= b_re_line1;
        b_im_pipe2_pre2 <= b_im_line1; 
        c_re_pipe2_pre2 <= c_re_line1;
        c_im_pipe2_pre2 <= ((mode == IFFT) || (mode == FPC_ADJ))? {neg_c_im, c_im_line1[62:0]} : c_im_line1;
    end

    always_ff @(posedge clk) begin
        a_re_pipe2_pre <= a_re_pipe2_pre2 ;
        a_im_pipe2_pre <= a_im_pipe2_pre2 ;
        b_re_pipe2_pre <= b_re_pipe2_pre2 ;
        b_im_pipe2_pre <= b_im_pipe2_pre2 ;
        c_re_pipe2_pre <= c_re_pipe2_pre2 ;
        c_im_pipe2_pre <= c_im_pipe2_pre2 ;
    end

    always @(posedge clk) begin
        if(mode == FFT) begin
            a_re_pipe2 <= a_re_pipe2_pre;
            a_im_pipe2 <= a_im_pipe2_pre;
        end else if((mode == IFFT) || (mode == FPR_ADD) || (mode == FPC_ADD)) begin
            a_re_pipe2 <= z_add1;
            a_im_pipe2 <= z_add2;
        end
    end

    always_ff @(posedge clk) b_re_pipe2 <= ((mode == IFFT) || (mode == FPR_ADD) || (mode == FPC_ADD) || (mode == FPR_SUB) || (mode == FPC_SUB))? z_sub1 : b_re_pipe2_pre ;
    always_ff @(posedge clk) b_im_pipe2 <= ((mode == IFFT) || (mode == FPR_ADD) || (mode == FPC_ADD) || (mode == FPR_SUB) || (mode == FPC_SUB))? z_sub2 : b_im_pipe2_pre ;
    always_ff @(posedge clk) c_re_pipe2 <= c_re_pipe2_pre ; 
    always_ff @(posedge clk) c_im_pipe2 <= c_im_pipe2_pre ; 

    //DW_fp_mult #(.sig_width(52), .exp_width(11), .ieee_compliance(1))
    //    fp_mult1 ( .a(a_re_pipe2), .b(FPR_HALF), .rnd(ROUND), .z(z_mul1), .status() );
    fpr_mul_const_opt_2pipe fp_mult1(
        .aclk(clk),                                  // input wire aclk
        //.s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(a_re_pipe2),              // input wire [63 : 0] s_axis_a_tdata
        //.s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        //.s_axis_b_tdata(FPR_HALF),              // input wire [63 : 0] s_axis_b_tdata
        //.m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_mul1)    // output wire [63 : 0] m_axis_result_tdata
    ); 
    //DW_fp_mult #(.sig_width(52), .exp_width(11), .ieee_compliance(1))
    //    fp_mult2 ( .a(a_im_pipe2), .b(FPR_HALF), .rnd(ROUND), .z(z_mul2), .status() );
    fpr_mul_const_opt_2pipe fp_mult2(
        .aclk(clk),                                  // input wire aclk
        //.s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(a_im_pipe2),              // input wire [63 : 0] s_axis_a_tdata
        //.s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        //.s_axis_b_tdata(FPR_HALF),              // input wire [63 : 0] s_axis_b_tdata
        //.m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_mul2)    // output wire [63 : 0] m_axis_result_tdata
    ); 
    //fpr_mul fpr_mul3 (.clk(clk), .d_i_x(b_re_pipe2), .d_i_y(c_re_pipe2), .d_i_alu(d_i_alu[128*0+:128]),.d_o_alu_x(d_o_alu_x[64*0+:64]), .d_o_alu_y(d_o_alu_y[64*0+:64]), .d_o(z_mul3));
    fpr_mul_opt_2pipe fpr_mul3(
        .aclk(clk),                                  // input wire aclk
        //.s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(b_re_pipe2),              // input wire [63 : 0] s_axis_a_tdata
        //.s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(c_re_pipe2),              // input wire [63 : 0] s_axis_b_tdata
        //.m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_mul3)    // output wire [63 : 0] m_axis_result_tdata
    );     
    //fpr_mul fpr_mul4 (.clk(clk), .d_i_x(b_im_pipe2), .d_i_y(c_im_pipe2), .d_i_alu(d_i_alu[128*1+:128]),.d_o_alu_x(d_o_alu_x[64*1+:64]), .d_o_alu_y(d_o_alu_y[64*1+:64]), .d_o(z_mul4));
    fpr_mul_opt_2pipe fpr_mul4(
        .aclk(clk),                                  // input wire aclk
        //.s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(b_im_pipe2),              // input wire [63 : 0] s_axis_a_tdata
        //.s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(c_im_pipe2),              // input wire [63 : 0] s_axis_b_tdata
        //.m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_mul4)    // output wire [63 : 0] m_axis_result_tdata
    );  
    //fpr_mul fpr_mul5 (.clk(clk), .d_i_x(c_re_pipe2), .d_i_y(b_im_pipe2), .d_i_alu(d_i_alu[128*2+:128]),.d_o_alu_x(d_o_alu_x[64*2+:64]), .d_o_alu_y(d_o_alu_y[64*2+:64]), .d_o(z_mul5));
    fpr_mul_opt_2pipe fpr_mul5(
        .aclk(clk),                                  // input wire aclk
        //.s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(c_re_pipe2),              // input wire [63 : 0] s_axis_a_tdata
        //.s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(b_im_pipe2),              // input wire [63 : 0] s_axis_b_tdata
        //.m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_mul5)    // output wire [63 : 0] m_axis_result_tdata
    );    
    //fpr_mul fpr_mul6 (.clk(clk), .d_i_x(c_im_pipe2), .d_i_y(b_re_pipe2), .d_i_alu(d_i_alu[128*3+:128]),.d_o_alu_x(d_o_alu_x[64*3+:64]), .d_o_alu_y(d_o_alu_y[64*3+:64]), .d_o(z_mul6));
    fpr_mul_opt_2pipe fpr_mul6(
        .aclk(clk),                                  // input wire aclk
        //.s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(c_im_pipe2),              // input wire [63 : 0] s_axis_a_tdata
        //.s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(b_re_pipe2),              // input wire [63 : 0] s_axis_b_tdata
        //.m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_mul6)    // output wire [63 : 0] m_axis_result_tdata
    ); 
//////////////////////////////////////////////////////////////////////////////////
// Third Pipeline Segments
    reg [63:0] a_re_pipe3, a_re_pipe3_pre, a_re_pipe3_pre2;
    reg [63:0] a_im_pipe3, a_im_pipe3_pre, a_im_pipe3_pre2;
    reg [63:0] b_re_pipe3;
    reg [63:0] b_im_pipe3;
    reg [63:0] c_re_pipe3;
    reg [63:0] c_im_pipe3;

    always_ff @(posedge clk) begin
        a_re_pipe3_pre2 <= a_re_pipe2;
        a_im_pipe3_pre2 <= a_im_pipe2;
    end

    always_ff @(posedge clk) begin
        a_re_pipe3_pre <= a_re_pipe3_pre2;
        a_im_pipe3_pre <= a_im_pipe3_pre2;
    end

    always @(posedge clk) begin
        if((mode == FFT) || (mode == FPR_ADD) || (mode == FPC_ADD)) begin
            a_re_pipe3 <= a_re_pipe3_pre ;
            a_im_pipe3 <= a_im_pipe3_pre ;
        end
        else if(mode == IFFT) begin
            a_re_pipe3 <= z_mul1 ;
            a_im_pipe3 <= z_mul2 ;
        end
    end

    always_ff @(posedge clk) b_re_pipe3 = z_mul3;
    always_ff @(posedge clk) b_im_pipe3 = z_mul4;
    always_ff @(posedge clk) c_re_pipe3 = z_mul5;
    always_ff @(posedge clk) c_im_pipe3 = z_mul6;

//////////////////////////////////////////////////////////////////////////////////
// Fourth Pipeline Segments
    wire [63:0] z_sub3;
    wire [63:0] z_add3;
    wire [63:0] z_mul7;
    wire [63:0] z_mul8;
    wire [63:0] z_add4;
    wire [63:0] z_add5;
    wire [63:0] z_sub4;
    wire [63:0] z_sub5;

    reg [63:0] a_re_pipe4, a_re_pipe4_pre, a_re_pipe4_pre2;
    reg [63:0] a_im_pipe4, a_im_pipe4_pre, a_im_pipe4_pre2;
    reg [63:0] b_re_pipe4;
    reg [63:0] b_im_pipe4;

    always @(posedge clk) begin
        a_re_pipe4_pre2 <= a_re_pipe3 ;
        a_im_pipe4_pre2 <= a_im_pipe3 ;
    end

    always @(posedge clk) begin
        a_re_pipe4_pre <= a_re_pipe4_pre2 ;
        a_im_pipe4_pre <= a_im_pipe4_pre2 ;
    end

    always @(posedge clk) begin
        a_re_pipe4 <= a_re_pipe4_pre ;
        a_im_pipe4 <= a_im_pipe4_pre ;
        b_re_pipe4 <= z_sub3 ;
        b_im_pipe4 <= z_add3 ;
    end

    //floating_point_sub_2pip #(.sig_width(52), .exp_width(11), .ieee_compliance(1))
    //    fp_sub3 ( .a(z_mul3), .b(z_mul4), .rnd(ROUND), .z(z_sub3), .status() );
    floating_point_sub_2pip fp_sub3(
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(b_re_pipe3),              // input wire [63 : 0] s_axis_a_tdata
        .s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(b_im_pipe3),              // input wire [63 : 0] s_axis_b_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_sub3)    // output wire [63 : 0] m_axis_result_tdata
    ); 
    //floating_point_add_2pip #(.sig_width(52), .exp_width(11), .ieee_compliance(1))
    //    fp_add3 ( .a(z_mul5), .b(z_mul6), .rnd(ROUND), .z(z_add3), .status() );
    floating_point_add_2pip fp_add3(
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(c_re_pipe3),              // input wire [63 : 0] s_axis_a_tdata
        .s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(c_im_pipe3),              // input wire [63 : 0] s_axis_b_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_add3)    // output wire [63 : 0] m_axis_result_tdata
    ); 
    //DW_fp_mult #(.sig_width(52), .exp_width(11), .ieee_compliance(1))
    //    fp_mult7 ( .a(z_sub3), .b(FPR_HALF), .rnd(ROUND), .z(z_mul7), .status() );
    fpr_mul_const_opt_2pipe fp_mult7(
        .aclk(clk),                                  // input wire aclk
        //.s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(b_re_pipe4),              // input wire [63 : 0] s_axis_a_tdata
        //.s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        //.s_axis_b_tdata(FPR_HALF),              // input wire [63 : 0] s_axis_b_tdata
        //.m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_mul7)    // output wire [63 : 0] m_axis_result_tdata
    ); 
    //DW_fp_mult #(.sig_width(52), .exp_width(11), .ieee_compliance(1))
    //    fp_mult8 ( .a(z_add3), .b(FPR_HALF), .rnd(ROUND), .z(z_mul8), .status() );
    fpr_mul_const_opt_2pipe fp_mult8(
        .aclk(clk),                                  // input wire aclk
        //.s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(b_im_pipe4),              // input wire [63 : 0] s_axis_a_tdata
        //.s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        //.s_axis_b_tdata(FPR_HALF),              // input wire [63 : 0] s_axis_b_tdata
        //.m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_mul8)    // output wire [63 : 0] m_axis_result_tdata
    ); 

    //floating_point_add_2pip #(.sig_width(52), .exp_width(11), .ieee_compliance(1))
    //    fp_add4 ( .a(a_re_pipe4), .b(z_sub3), .rnd(ROUND), .z(z_add4), .status() );
    floating_point_add_2pip fp_add4(
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(a_re_pipe4),              // input wire [63 : 0] s_axis_a_tdata
        .s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(b_re_pipe4),              // input wire [63 : 0] s_axis_b_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_add4)    // output wire [63 : 0] m_axis_result_tdata
    );     
    //floating_point_add_2pip #(.sig_width(52), .exp_width(11), .ieee_compliance(1))
    //    fp_add5 ( .a(a_im_pipe4), .b(z_add3), .rnd(ROUND), .z(z_add5), .status() );
    floating_point_add_2pip fp_add5(
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(a_im_pipe4),              // input wire [63 : 0] s_axis_a_tdata
        .s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(b_im_pipe4),              // input wire [63 : 0] s_axis_b_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_add5)    // output wire [63 : 0] m_axis_result_tdata
    );     
    //floating_point_sub_2pip #(.sig_width(52), .exp_width(11), .ieee_compliance(1))
    //    fp_sub4 ( .a(a_re_pipe4), .b(z_sub3), .rnd(ROUND), .z(z_sub4), .status() );
    floating_point_sub_2pip fp_sub4(
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(a_re_pipe4),              // input wire [63 : 0] s_axis_a_tdata
        .s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(b_re_pipe4),              // input wire [63 : 0] s_axis_b_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_sub4)    // output wire [63 : 0] m_axis_result_tdata
    );    
    //floating_point_sub_2pip #(.sig_width(52), .exp_width(11), .ieee_compliance(1))
    //    fp_sub5 ( .a(a_im_pipe4), .b(z_add3), .rnd(ROUND), .z(z_sub5), .status() );
    floating_point_sub_2pip fp_sub5(
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid('d1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(a_im_pipe4),              // input wire [63 : 0] s_axis_a_tdata
        .s_axis_b_tvalid('d1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(b_im_pipe4),              // input wire [63 : 0] s_axis_b_tdata
        .m_axis_result_tvalid(),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(z_sub5)    // output wire [63 : 0] m_axis_result_tdata
    ); 
//////////////////////////////////////////////////////////////////////////////////
// Fifth Pipeline Segments
    reg [63:0] a_re_pipe5_pre, a_re_pipe5_pre2;
    reg [63:0] a_im_pipe5_pre, a_im_pipe5_pre2;
    reg [63:0] b_re_pipe5_pre, b_re_pipe5_pre2;
    reg [63:0] b_im_pipe5_pre, b_im_pipe5_pre2;

    always @(posedge clk) begin
        a_re_pipe5_pre2 <= a_re_pipe4 ; 
        a_im_pipe5_pre2 <= a_im_pipe4 ; 
        b_re_pipe5_pre2 <= b_re_pipe4 ; 
        b_im_pipe5_pre2 <= b_im_pipe4 ; 
    end

    always @(posedge clk) begin
        a_re_pipe5_pre <= a_re_pipe5_pre2 ; 
        a_im_pipe5_pre <= a_im_pipe5_pre2 ; 
        b_re_pipe5_pre <= b_re_pipe5_pre2 ; 
        b_im_pipe5_pre <= b_im_pipe5_pre2 ; 
    end

    always_comb d_o_a_re = ((mode == IFFT) || (mode == FPR_ADD) || (mode == FPC_ADD))?  a_re_pipe5_pre : z_add4 ;
    always_comb d_o_a_im = ((mode == IFFT) || (mode == FPR_ADD) || (mode == FPC_ADD))?  a_im_pipe5_pre : z_add5 ;
    always_comb d_o_b_re = (mode == IFFT)? z_mul7 :
                                               ((mode == FFT)? z_sub4 : b_re_pipe5_pre);
    always_comb d_o_b_im = (mode == IFFT)? z_mul8 :
                                               ((mode == FFT)? z_sub5 : b_im_pipe5_pre);
endmodule