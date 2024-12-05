`include "falconsoar_pkg.sv"

module samplerz_top
    import falconsoar_pkg::*;
(
    input clk                          ,
    input rst_n                        ,
    exec_operator_if.slave   task_itf  ,
    mem_inst_if.master_rd    mem_rd    ,
    mem_inst_if.master_wr    mem_wr      
);

    samplerz i_samplerz
    (
             .clk            (  clk        ) ,
             .rst_n          (  rst_n      ) ,
             .task_itf       (  task_itf   ) ,
             .mem_rd         (  mem_rd     ) ,
             .mem_wr         (  mem_wr     ) 
    );

    time_counter samplerz_time_counter(
    .clk,
    .rst_n,
    .start(task_itf.start),
    .done(task_itf.op_done)
);

endmodule
