`ifndef FLOAT_DFF_DONE

    `define FLOAT_DFF_DONE

    `include "float_pkg.sv"

    package float_pkg;

        parameter int unsigned FLOAT_DW = 128; // data bit width
        parameter int unsigned FLOAT_DN = 4;   // data num

        typedef logic [FLOAT_DW-1:0] float_vect_t;
        typedef logic [FLOAT_DW*FLOAT_DN-1:0] float_data_t;
      //typedef logic [7:0][FLOAT_DW-1:0] float_data_pack_t;

    endpackage // float_pkg

    interface fpu_if;

        import float_pkg::*;

        logic [3:0]  mode ;
        float_data_t d_i [3]; // write a is for external api to first fpu input
        float_data_t d_o [2]; // read a is for external api to first fpu result

        modport master
        (
            output mode ,
            output d_i  ,
            input  d_o
        );

        modport slave
        (
            input  mode ,
            input  d_i  ,
            output d_o
        );

    endinterface

`endif // FLOAT_DFF_DONE
