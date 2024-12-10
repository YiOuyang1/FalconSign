`include "falconsoar_pkg.sv"

module fpu
    import falconsoar_pkg::*;
#(
    parameter DW = 64  // In fpr mode
)
(
    input         clk    ,
    input         fpu_en ,
    fpu_if.slave  fpu_itf
);

    localparam int unsigned FPU_NUM = 4;

    for(genvar i=0; i<FPU_NUM; i++) begin:gen_bfu_fft
        bfu_fft i_bfu_fft
        (
            .clk        ,
            .en         (   fpu_en                          ),
            .mode       (   fpu_itf.mode                    ),
            .d_i_a_re   (   fpu_itf.d_i[0][(2*DW*i   )+:DW] ),
            .d_i_a_im   (   fpu_itf.d_i[0][(2*DW*i+DW)+:DW] ),
            .d_i_b_re   (   fpu_itf.d_i[1][(2*DW*i   )+:DW] ),
            .d_i_b_im   (   fpu_itf.d_i[1][(2*DW*i+DW)+:DW] ),
            .d_i_c_re   (   fpu_itf.d_i[2][(2*DW*i   )+:DW] ),
            .d_i_c_im   (   fpu_itf.d_i[2][(2*DW*i+DW)+:DW] ),
            .d_o_a_re   (   fpu_itf.d_o[0][(2*DW*i   )+:DW] ),
            .d_o_a_im   (   fpu_itf.d_o[0][(2*DW*i+DW)+:DW] ),
            .d_o_b_re   (   fpu_itf.d_o[1][(2*DW*i   )+:DW] ),
            .d_o_b_im   (   fpu_itf.d_o[1][(2*DW*i+DW)+:DW] )
        );
    end


endmodule
