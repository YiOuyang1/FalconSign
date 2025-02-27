`ifndef TASK_DFF_DONE

    `define TASK_DFF_DONE

    package falconsoar_pkg;
        ///////////////////////////////////////////////////////////////////////////////////                  
        //here is about the memory banks
        parameter int BANK_NUM   = 4;
        parameter     BANK_DEPTH = 13'd2048; //BRAM depth = 1024, 36Kb = 36b * 1024; dual-port
        parameter int BANK_WIDTH = 256;  //ceil((4*256)/36) =  ceil((1024)/36) = 29;

        parameter int unsigned READLATENCY = 2; //This is read latency of BRAM 
        
        parameter int unsigned BANK_ADDR_BITS = $clog2(BANK_DEPTH);
        parameter int unsigned MEM_ADDR_BITS = $clog2(BANK_NUM*BANK_DEPTH);
        typedef logic [MEM_ADDR_BITS-1:0] mem_addr_t;
        typedef logic [BANK_WIDTH-1:0] mem_data_t;

        ///////////////////////////////////////////////////////////////////////////////////
        //fpu: less dsps; 
        parameter  FPU_DELAY = 7 + 4;   

        ///////////////////////////////////////////////////////////////////////////////////
        //float_module:
        parameter  WR_DELAY = 12 + 4 ;
        parameter  FORMAT_TRANS_DELAY =  WR_DELAY - FPU_DELAY;


        ///////////////////////////////////////////////////////////////////////////////////                  
        //here is some type definition used in SamplerZ module
        parameter int VEC_BW = 32;
        parameter int VEC_PER_ROW = 16;

        typedef logic [VEC_BW-1:0] vect_t;
        typedef logic [VEC_PER_ROW-1:0][VEC_BW-1:0] row_data_pack_t;

        parameter  SAMPLERZ_READ_DELAY =  4'd4 ;

        ///////////////////////////////////////////////////////////////////////////////////
        //here is about the architecture 
        parameter int EXEC_CLUSTER_NUM = 7; 

        //0:Keccak Hash message (SHAKE-256);
        //1:Hashtopoint 
        //2:fft-control controlA module; 
        //3:fft-sampling; 
        //4:fft-control controlB module;
        //5:post-process;
        //6:Huffman encoding module;

        ///////////////////////////////////////////////////////////////////////////////////
        //here is about the task-format
        parameter int unsigned TASK_BW = 72;
        parameter int unsigned TASK_REDUCE_BW = 68;
        parameter              TASK_ZERO = 68'd0;
    

        typedef logic [TASK_REDUCE_BW-1:0] task_reduce_t;
        typedef logic [TASK_BW       -1:0] task_complete_t;

       
        //here is FPU task format
        parameter BANK_SINGLE = 2'd0 ;
        parameter BANK_DOUBLE = 2'd1 ;
        parameter BANK_FOUR = 2'd2 ;

        parameter OP_FFT = 2'd0 ;
        parameter SUB_FFT  = 3'd0 ;
        parameter SUB_IFFT = 3'd1 ;
        parameter SUB_SPLIT_512 = 3'd2 ; 
        parameter SUB_MERGE_512 = 3'd3 ;
        parameter SUB_SPLIT_1024 = 3'd4 ; 
        parameter SUB_MERGE_1024 = 3'd5 ;

        parameter OP_FPR = 2'd1 ;
        parameter OP_FPC = 2'd2 ;
        parameter SUB_ADD = 3'd0 ;
        parameter SUB_SUB = 3'd1 ; 
        parameter SUB_MUL = 3'd2 ; 
        parameter SUB_ADJ = 3'd3 ; 
        parameter SUB_SQR = 3'd4 ;  

        parameter OP_FORMAL = 2'd3 ;

        ///////////////////////////////////////////////////////////////////////////////////
        parameter TASK_DUMMY  = 13'd0;
        parameter POS_SET     = 1'd0;
        parameter NEG_SET     = 1'd1;


    endpackage

    interface exec_operator_if;
    
        import falconsoar_pkg::*;
        logic         start;
        task_reduce_t input_task;
        logic         op_done;

        modport master
        (
            output start,
            output input_task,
            input  op_done
        );
        modport slave
        (
            input  start,
            input  input_task,
            output op_done
        );

    endinterface

    interface mem_inst_if;
    
        import falconsoar_pkg::*;
        logic       en      ;
        mem_addr_t  addr    ;
        mem_data_t  data    ;

        modport master_wr
        (
            output en   ,
            output addr ,
            output data
        );

        modport master_rd
        (
            output en   ,
            output addr ,
            input  data
        );

        modport slave_wr
        (
            input en    ,
            input addr  ,
            input data
        );

        modport slave_rd
        (
            input  en   ,
            input  addr ,
            output data
        );

    endinterface // buff_if



`endif // TASK_DFF_DONE
