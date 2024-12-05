`include "falconsoar_pkg.sv"

module refill_control
    import falconsoar_pkg::*;
(
    input                clk              ,
    input                rst_n            ,
    input                start            ,
    input                init             , //first use samplez, fullill buffer
    output logic         fetch_en         , //enable signal to run PRNG
    input        [511:0] r_data           , //read data (512bits) from PRNG
    input                read_10byte      ,
    input                read_1byte       ,
    output logic [ 79:0] dout_10          , //output 10 random bytes
    output logic [  7:0] dout_1           , //output 1 random byte
    output logic         part_en          , //1: enable 'samploop' and 'berexp' module work, 0: 'samploop' and 'berexp' module pause
    output logic         done
);

    localparam BLOCK_SIZE = 32 ;

    reg [255:0]  buffer ; // as 'fifo-like' buffer to provide random byte, and refill itself automatically
    assign part_en = 1;
//////////////////////////////////////////////////////////////////////////////////
// Generate control signal
    wire          block_nearly_empty ;
    wire          flag_fetch_data ;

    reg  [6:0] read_cnt     ; // the number of blocks already read
    reg  [4:0] req_cnt      ; // the number of random byte already used
    reg  [4:0] req_cnt_nxt  ; // the req_cnt value in next cycle

    assign flag_fetch_data  = (req_cnt[4] ^ req_cnt_nxt[4]) ;
    assign fetch_en         = (flag_fetch_data | (start & init)); // read a new block to buffer when blocks is not empty
    //assign done             = (fetch_en & init);
    logic done_r;
    always_ff@(posedge clk)  begin
        done_r <= (fetch_en & init);
    end
    assign done = done_r;

    always @(posedge clk or negedge rst_n) begin
        if      (~rst_n)                              read_cnt <= 0            ;
        else if (start & init)                        read_cnt <= 2            ; // initial fill buffer by using two blocks
        else if (fetch_en & (read_cnt == BLOCK_SIZE)) read_cnt <= 1            ; // after read read_cnt + 1
        else if (fetch_en)                            read_cnt <= read_cnt + 1 ; // after read read_cnt + 1
    end

    always @(*)begin
        if      (read_1byte  == 1)                                        req_cnt_nxt = (req_cnt + 5'd1)  ;   // to '+1'
        else if ((read_10byte == 1) & (req_cnt >= 23) & (read_cnt == 1))  req_cnt_nxt = 5'd10 ;   // to '+10'
        else if (read_10byte == 1)                                        req_cnt_nxt = (req_cnt + 5'd10) ;   // to '+10'
        else                                                              req_cnt_nxt =  req_cnt ;
    end

    always @(posedge clk or negedge rst_n) begin
        if      (~rst_n)                   req_cnt <= 0 ;
        else if (start & init)             req_cnt <= 0 ;
        else if (read_1byte | read_10byte) req_cnt <= req_cnt_nxt ;
    end

//////////////////////////////////////////////////////////////////////////////////
//TODO: need to adjust the order of filling buffer 
// buffer write and read
    always @(posedge clk) begin
        if (fetch_en & init)
            buffer <= r_data[0+:256] ;
        else if (fetch_en && read_cnt[1:0] == 2'b00)
            buffer <= (req_cnt[4] == 0)? {buffer[128+:128], r_data[128*0+:128]} : {r_data[128*0+:128], buffer[0+:128]};
        else if (fetch_en && read_cnt[1:0] == 2'b01)
            buffer <= (req_cnt[4] == 0)? {buffer[128+:128], r_data[128*1+:128]} : {r_data[128*1+:128], buffer[0+:128]};
        else if (fetch_en && read_cnt[1:0] == 2'b10)
            buffer <= (req_cnt[4] == 0)? {buffer[128+:128], r_data[128*2+:128]} : {r_data[128*2+:128], buffer[0+:128]};
        else if (fetch_en && read_cnt[1:0] == 2'b11)
            buffer <= (req_cnt[4] == 0)? {buffer[128+:128], r_data[128*3+:128]} : {r_data[128*3+:128], buffer[0+:128]};
    end

    assign dout_1 = buffer[req_cnt*8+:8];

    always_comb begin
        if(req_cnt <= 'd22) begin
            dout_10 = buffer[req_cnt*8+:80];
        end else begin
            dout_10 = 'x;
            case(req_cnt)
            5'd23: dout_10 = (read_cnt == 'd1) ? buffer[0*8+:80] : {buffer[0+:8*1],buffer[23*8+:(80-8*1)]};
            5'd24: dout_10 = (read_cnt == 'd1) ? buffer[0*8+:80] : {buffer[0+:8*2],buffer[24*8+:(80-8*2)]};
            5'd25: dout_10 = (read_cnt == 'd1) ? buffer[0*8+:80] : {buffer[0+:8*3],buffer[25*8+:(80-8*3)]};
            5'd26: dout_10 = (read_cnt == 'd1) ? buffer[0*8+:80] : {buffer[0+:8*4],buffer[26*8+:(80-8*4)]};
            5'd27: dout_10 = (read_cnt == 'd1) ? buffer[0*8+:80] : {buffer[0+:8*5],buffer[27*8+:(80-8*5)]};
            5'd28: dout_10 = (read_cnt == 'd1) ? buffer[0*8+:80] : {buffer[0+:8*6],buffer[28*8+:(80-8*6)]};
            5'd29: dout_10 = (read_cnt == 'd1) ? buffer[0*8+:80] : {buffer[0+:8*7],buffer[29*8+:(80-8*7)]};
            5'd30: dout_10 = (read_cnt == 'd1) ? buffer[0*8+:80] : {buffer[0+:8*8],buffer[30*8+:(80-8*8)]};
            5'd31: dout_10 = (read_cnt == 'd1) ? buffer[0*8+:80] : {buffer[0+:8*9],buffer[31*8+:(80-8*9)]};
            endcase
        end
    end

endmodule
