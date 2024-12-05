module fpr_cal
(   
    input                clk        ,
    input        [63:0]  data_a0    ,
    input        [63:0]  data_a1    ,
    input                choose_a   ,
    input        [63:0]  data_b0    ,
    input        [63:0]  data_b1    ,
    input                choose_b   ,
    output logic [63:0]  data_out
);

    logic [63:0] mul_a, mul_b;

    always_ff@(posedge clk) begin
        if     (choose_a) mul_a <= data_a0;
        else if(choose_b) mul_a <= data_b0;
    end

    always_ff@(posedge clk) begin
        if     (choose_a) mul_b <= data_a1;
        else if(choose_b) mul_b <= data_b1;
    end

    fp_mult_s u_fp_mult_s (
        .aclk(clk),                                  // input wire aclk
        .s_axis_a_tvalid(1'd1),            // input wire s_axis_a_tvalid
        .s_axis_a_tdata(mul_a),              // input wire [63 : 0] s_axis_a_tdata
        .s_axis_b_tvalid(1'd1),            // input wire s_axis_b_tvalid
        .s_axis_b_tdata(mul_b),              // input wire [63 : 0] s_axis_b_tdata
        .m_axis_result_tvalid(   ),  // output wire m_axis_result_tvalid
        .m_axis_result_tdata(data_out)     // output wire [63 : 0] m_axis_result_tdata
    );

endmodule
