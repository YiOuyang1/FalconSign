`include "falconsoar_pkg.sv"
`include "sample_pkg.sv"

module chacha20
    import falconsoar_pkg::*;
(
    input                                 clk                  ,
    input                                 rst_n                ,
    input                                 start                ,  //start signal to generate PRNG data
    input                                 restart              ,  //restart signal to generate PRNG data using new Random seed
    input                                 sample_init          ,
    input                                 fetch_en             ,
    output                                done                 ,  //generate PRNG data done!
    input           [MEM_ADDR_BITS - 1:0] src_addr             ,  //the addr to fetch the PRNG seed
    output                                mem_rd_chacha20_en   ,  //
    output          [MEM_ADDR_BITS - 1:0] mem_rd_chacha20_addr ,  //
    input           [   BANK_WIDTH - 1:0] mem_rd_chacha20_data ,  //
    output          [              511:0] data_o                  //PRNG data (512bits) output
);

    localparam int unsigned CW [4] = '{32'h61707865,
                                       32'h3320646e,
                                       32'h79622d32,
                                       32'h6b206574};

////////////////////////////////////////////////////////////////////////////////////////

    mem_addr_t src; assign src = src_addr;

    ////////////////////////////////////////////////////////////////////////////////////////
    // control signal
    logic [7:0] cnt;
    logic [4:0] buff_idx;

    always_ff @(posedge clk, negedge rst_n) begin
        if (!rst_n) cnt <= '0;
        else if (start & restart) cnt <= 1;
        else if (start & (restart == 0)) cnt <= 4;
        else if (fetch_en & (buff_idx[4:0] == 'd31)) cnt <= 7;
        else if (cnt == 0) cnt <= 0;
        else if (cnt == 88) cnt <= 0;
        else cnt <= cnt + 1;
    end

    assign done = (cnt == 'd88);

    ////////////////////////////////////////////////////////////////////////////////////////
    // read bank signal
    logic [63:0] cc     [2];
    logic [63:0] cc_nxt [2];  //32 * 2

    logic         round_done;
    logic [383:0] init_state;  //32 * 12

    pulse_extender i_pulse_extender(.clk(clk), .pulse_in(start & restart), .pulse_out(mem_rd_chacha20_en)); //extend one cycle pulse to two cycle pulse
    assign mem_rd_chacha20_addr = src + cnt[1];

    assign round_done = (cnt == 'd23 + SAMPLERZ_READ_DELAY) | (cnt == 'd43 + SAMPLERZ_READ_DELAY) | (cnt == 'd63 + SAMPLERZ_READ_DELAY) | (cnt == 'd83 + SAMPLERZ_READ_DELAY);

    always_ff @(posedge clk) if(cnt == 'd1 + SAMPLERZ_READ_DELAY) init_state[255:0] <= mem_rd_chacha20_data;
    always_ff @(posedge clk) if(cnt == 'd2 + SAMPLERZ_READ_DELAY) init_state[383:256] <= mem_rd_chacha20_data[383 - 256 : 0];


    ////////////////////////////////////////////////////////////////////////////////////////
    // state permute
    row_data_pack_t state_parallel [2];
    row_data_pack_t result         [2];
    row_data_pack_t state_i        [2];
    row_data_pack_t state_o        [2];

    for(genvar i=0; i<2; i++) begin:gen_bank
        assign cc_nxt[i] = cc[i] + 'd2;

        always @(posedge clk) begin
            if     (cnt == 'd2 + SAMPLERZ_READ_DELAY)   cc[i] <= mem_rd_chacha20_data[447 - 256:384 - 256] + i[0];
            else if(round_done)                         cc[i] <= cc_nxt[i];
        end

        chacha20_round_2stage i_round_stage (clk, state_i[i], state_o[i]);

        always @(posedge clk) state_parallel[i] <= state_o[i];

        for(genvar j=0; j<4; j++) begin:gen_result_0_1
            assign result[i][j] = state_parallel[i][j] + CW[j];
        end

        for(genvar j=4; j<14; j++) begin:gen_result_4_14
            assign result[i][j] = state_parallel[i][j] + init_state[32*(j-4)+:32];
        end

        assign result[i][14] = state_parallel[i][14] + (init_state[32*10+:32] ^ cc[i][32*0+:32]);
        assign result[i][15] = state_parallel[i][15] + (init_state[32*11+:32] ^ cc[i][32*1+:32]);

        always_comb begin
            if ((cnt <= 'd3 + SAMPLERZ_READ_DELAY) | (cnt >= 'd84 + SAMPLERZ_READ_DELAY)) begin
                state_i[i] = {(init_state[32*10+:64] ^ cc[i]), init_state[0+:32*10],CW[3],CW[2],CW[1],CW[0]};
            end else if ((cnt == 'd23 + SAMPLERZ_READ_DELAY) | (cnt == 'd43 + SAMPLERZ_READ_DELAY) | (cnt == 'd63 + SAMPLERZ_READ_DELAY) | (cnt == 'd83 + SAMPLERZ_READ_DELAY)) begin
                state_i[i] = {(init_state[32*10+:64] ^ cc_nxt[i]),init_state[0+:32*10],CW[3],CW[2],CW[1],CW[0]};
            end else begin
                state_i[i] = state_parallel[i];
            end
        end
    end

    ////////////////////////////////////////////////////////////////////////////////////////
    // write output buffer  and write bank signal
    logic [4095:0] buff, buff_update;

    for(genvar j=0; j<16; j++) begin : L1
        assign buff_update[(256*j+32*0)+:32] = (cnt == 'd23 + SAMPLERZ_READ_DELAY)? result[0][j] : buff[(256*j+32*0)+:32];
        assign buff_update[(256*j+32*1)+:32] = (cnt == 'd23 + SAMPLERZ_READ_DELAY)? result[1][j] : buff[(256*j+32*1)+:32];
        assign buff_update[(256*j+32*2)+:32] = (cnt == 'd43 + SAMPLERZ_READ_DELAY)? result[0][j] : buff[(256*j+32*2)+:32];
        assign buff_update[(256*j+32*3)+:32] = (cnt == 'd43 + SAMPLERZ_READ_DELAY)? result[1][j] : buff[(256*j+32*3)+:32];
        assign buff_update[(256*j+32*4)+:32] = (cnt == 'd63 + SAMPLERZ_READ_DELAY)? result[0][j] : buff[(256*j+32*4)+:32];
        assign buff_update[(256*j+32*5)+:32] = (cnt == 'd63 + SAMPLERZ_READ_DELAY)? result[1][j] : buff[(256*j+32*5)+:32];
        assign buff_update[(256*j+32*6)+:32] = (cnt == 'd83 + SAMPLERZ_READ_DELAY)? result[0][j] : buff[(256*j+32*6)+:32];
        assign buff_update[(256*j+32*7)+:32] = (cnt == 'd83 + SAMPLERZ_READ_DELAY)? result[1][j] : buff[(256*j+32*7)+:32];
    end

    always_ff @(posedge clk) if(round_done) buff <= buff_update;

    always_ff @(posedge clk) begin
        if     (start & restart)           buff_idx <= '0;
        else if(fetch_en & sample_init)    buff_idx <= buff_idx + 'd2;
        else if(fetch_en & (~sample_init)) buff_idx <= buff_idx + 1'd1;
    end

    assign data_o = buff[512*buff_idx[4:2]+:512];

endmodule

module chacha20_round_2stage
    import falconsoar_pkg::*;
    import sample_pkg::*;
(
    input  clk,
    input  row_data_pack_t data_i   ,
    output row_data_pack_t data_o
);

    vect_t state_0 [16];

    vect_t state_0_r [16];

    qround i_qround_1 (
        data_i  [ 0],
        data_i  [ 4],
        data_i  [ 8],
        data_i  [12],
        state_0 [ 0],
        state_0 [ 4],
        state_0 [ 8],
        state_0 [12]
    );
    qround i_qround_2 (
        data_i  [ 1],
        data_i  [ 5],
        data_i  [ 9],
        data_i  [13],
        state_0 [ 1],
        state_0 [ 5],
        state_0 [ 9],
        state_0 [13]
    );
    qround i_qround_3 (
        data_i  [ 2],
        data_i  [ 6],
        data_i  [10],
        data_i  [14],
        state_0 [ 2],
        state_0 [ 6],
        state_0 [10],
        state_0 [14]
    );
    qround i_qround_4 (
        data_i  [ 3],
        data_i  [ 7],
        data_i  [11],
        data_i  [15],
        state_0 [ 3],
        state_0 [ 7],
        state_0 [11],
        state_0 [15]
    );

     
    for(genvar i=0;i<16;i++) begin:gen_stage_pipe
           always_ff@(posedge clk) state_0_r[i] <= state_0[i];
    end

    qround i_qround_5 (
        state_0_r [ 0],
        state_0_r [ 5],
        state_0_r [10],
        state_0_r [15],
        data_o  [ 0],
        data_o  [ 5],
        data_o  [10],
        data_o  [15]
    );
    qround i_qround_6 (
        state_0_r [ 1],
        state_0_r [ 6],
        state_0_r [11],
        state_0_r [12],
        data_o  [ 1],
        data_o  [ 6],
        data_o  [11],
        data_o  [12]
    );
    qround i_qround_7 (
        state_0_r [ 2],
        state_0_r [ 7],
        state_0_r [ 8],
        state_0_r [13],
        data_o  [ 2],
        data_o  [ 7],
        data_o  [ 8],
        data_o  [13]
    );
    qround i_qround_8 (
        state_0_r [ 3],
        state_0_r [ 4],
        state_0_r [ 9],
        state_0_r [14],
        data_o  [ 3],
        data_o  [ 4],
        data_o  [ 9],
        data_o  [14]
    );

endmodule

module qround
    import falconsoar_pkg::*;
    import sample_pkg::*;
(
    input  vect_t a_i   ,
    input  vect_t b_i   ,
    input  vect_t c_i   ,
    input  vect_t d_i   ,
    output vect_t a_o   ,
    output vect_t b_o   ,
    output vect_t c_o   ,
    output vect_t d_o
);

    vect_t a_0, a_2;
    vect_t b_1, b_3, b_shift_1, b_shift_3;
    vect_t c_1, c_3;
    vect_t d_0, d_2, d_shift_0, d_shift_2;

    assign a_0       = a_i + b_i;
    assign d_0       = d_i ^ a_0;
    assign d_shift_0 = {d_0[15:0],d_0[31:16]};
    assign c_1       = c_i + d_shift_0;
    assign b_1       = b_i ^ c_1;
    assign b_shift_1 = {b_1[19:0],b_1[31:20]};
    assign a_2       = a_0 + b_shift_1;
    assign d_2       = a_2 ^ d_shift_0;
    assign d_shift_2 = {d_2[23:0],d_2[31:24]};
    assign c_3       = c_1 + d_shift_2;
    assign b_3       = c_3 ^ b_shift_1;
    assign b_shift_3 = {b_3[24:0],b_3[31:25]};

    assign a_o       = a_2;
    assign b_o       = b_shift_3;
    assign c_o       = c_3;
    assign d_o       = d_shift_2;

endmodule

module pulse_extender (
    input  logic clk,       // Clock signal
    input  logic pulse_in,  // Input pulse signal
    output logic pulse_out // Extended pulse output
);

    logic pulse_delayed[1:0]; // Register to hold the delayed pulse

    // On every clock edge, capture the current pulse and delay it by one cycle
    always_ff @(posedge clk) begin
        pulse_delayed[0] <= pulse_in; // Delay the pulse by one cycle
        pulse_delayed[1] <= pulse_delayed[0]; // Delay the pulse by one cycle
    end

    // Output is high for two cycles when pulse_in is high
    // First cycle: when pulse_in is high
    // Second cycle: when pulse_delayed is high
    assign pulse_out = pulse_delayed[0] | pulse_delayed[1];

endmodule