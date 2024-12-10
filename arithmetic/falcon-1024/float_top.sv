`include "falconsoar_pkg.sv"

module float_top
    import falconsoar_pkg::*;
(
    input                   clk                   ,
    input                   rst_n                 ,
    exec_operator_if.slave  task_itf              ,
    mem_inst_if.master_rd   mem_rd  [4]           ,
    mem_inst_if.master_wr   mem_wr  [4]           ,
    mem_inst_if.master_rd   bank_rd [2]
);

    exec_operator_if task_itf_inter [5] (.*);

    mem_inst_if buff_rd_fft [6] (.*);
    mem_inst_if buff_wr_fft [4] (.*);

    mem_inst_if buff_rd_spl [6] (.*);
    mem_inst_if buff_wr_spl [4] (.*);

    mem_inst_if buff_rd_fpr [4] (.*);
    mem_inst_if buff_wr_fpr [4] (.*);

    mem_inst_if buff_rd_fpc [4] (.*);
    mem_inst_if buff_wr_fpc [4] (.*);

    mem_inst_if buff_rd_fmt [4] (.*);
    mem_inst_if buff_wr_fmt [4] (.*);

    fpu_if fpu_itf_inter [4] (.*);

    fpu_if fpu_itf (.*);

    logic fpu_en;

    float_fabric i_fabric (.*);

    fpu u_fpu (.*);

    fft_top u_fft_top
    (
        .clk        ,
        .rst_n      ,
        .task_itf   (   task_itf_inter[0]   ), // exec_operator_if.slave
        .fpu_itf    (   fpu_itf_inter[0]    ), // fpu_if.master
        .buff_rd    (   buff_rd_fft         ), // mem_inst_if.master_rd [6]
        .buff_wr    (   buff_wr_fft         )  // mem_inst_if.master_wr [4]
    );

    split_merge_top u_split_merge_top
    (
        .clk    ,
        .rst_n  ,
        .task_itf   (   task_itf_inter[1]   ), // exec_operator_if.slave
        .fpu_itf    (   fpu_itf_inter[1]    ), // fpu_if.master
        .buff_rd    (   buff_rd_spl         ), // mem_inst_if.master_rd [6]
        .buff_wr    (   buff_wr_spl         )  // mem_inst_if.master_wr [4]
    );

    fpr_top u_fpr_top
    (
        .clk        ,
        .rst_n      ,
        .task_itf   (   task_itf_inter[2]   ), // exec_operator_if.slave
        .fpu_itf    (   fpu_itf_inter[2]    ), // fpu_if.master
        .buff_rd    (   buff_rd_fpr         ), // mem_inst_if.master_rd [4]
        .buff_wr    (   buff_wr_fpr         )  // mem_inst_if.master_wr [4]
    );

    fpc_top u_fpc_top
    (
        .clk        ,
        .rst_n      ,
        .task_itf   (   task_itf_inter[3]   ), // exec_operator_if.slave
        .fpu_itf    (   fpu_itf_inter[3]    ), // fpu_if.master
        .buff_rd    (   buff_rd_fpc         ), // mem_inst_if.master_rd [4]
        .buff_wr    (   buff_wr_fpc         )  // mem_inst_if.master_wr [4]
    );
    fft_format_transfer_top u_fft_format_transfer_top
    (
        .clk        ,
        .rst_n      ,
        .task_itf   (   task_itf_inter[4]   ),

        .buff_rd    (   buff_rd_fmt         ), 
        .buff_wr    (   buff_wr_fmt         )
);


   time_counter float_time_counter(
   .clk,
   .rst_n,
   .start(task_itf.start),
   .done(task_itf.op_done)
);

endmodule
