module CNN_input_mem #(
    parameter       INPUT_BRAM_DEPTH                =       224 * 244,
    parameter       INPUT_BRAM_ADDRESS_WIDTH        =       $clog2(INPUT_BRAM_DEPTH),
    parameter       KERNEL_ROW_SIZE                 =       3,
    parameter       DATA_WIDTH                      =       32
    
)(
    input                                                       i_clock,
    input                                                       i_reset,
    input                                                       i_enable,
    input                                                       i_wenable,
    input                                                       i_renable,
    input               [INPUT_BRAM_ADDRESS_WIDTH - 1 : 0]      i_waddress,
    input               [INPUT_BRAM_ADDRESS_WIDTH - 1 : 0]      i_raddress,
    input               [DATA_WIDTH - 1 : 0]                    i_bram_data,
    output      logic   [DATA_WIDTH - 1 : 0]                    o_bram_data [0 : KERNEL_ROW_SIZE - 1],
    output      logic                                           o_reset_busy
);

// First bram 
logic                                       first_bram_reset_a_busy;
logic                                       first_bram_reset_b_busy;
logic [INPUT_BRAM_ADDRESS_WIDTH - 1 : 0]    addra_first_bram;
logic [DATA_WIDTH - 1 : 0]                  dout_a_first_bram;
logic [DATA_WIDTH - 1 : 0]                  din_a_first_bram;
logic [DATA_WIDTH - 1 : 0]                  dout_b_first_bram;
// Second bram
logic                                       second_bram_reset_a_busy;
logic                                       second_bram_reset_b_busy;
logic [INPUT_BRAM_ADDRESS_WIDTH - 1 : 0]    addra_second_bram;
logic [DATA_WIDTH - 1 : 0]                  dout_a_second_bram;
logic [DATA_WIDTH - 1 : 0]                  din_a_second_bram;
logic [DATA_WIDTH - 1 : 0]                  dout_b_second_bram;
// Third bram
logic                                       third_bram_reset_a_busy;
logic                                       third_bram_reset_b_busy;
logic [INPUT_BRAM_ADDRESS_WIDTH - 1 : 0]    addra_third_bram;
logic [DATA_WIDTH - 1 : 0]                  dout_a_third_bram;
logic [DATA_WIDTH - 1 : 0]                  din_a_third_bram;
logic [DATA_WIDTH - 1 : 0]                  dout_b_third_bram;

//delay
logic [INPUT_BRAM_ADDRESS_WIDTH - 1 : 0]    addra_second_bram_first_delay;
logic [INPUT_BRAM_ADDRESS_WIDTH - 1 : 0]    addra_second_bram_second_delay;
logic [INPUT_BRAM_ADDRESS_WIDTH - 1 : 0]    addra_third_bram_first_delay;
logic [INPUT_BRAM_ADDRESS_WIDTH - 1 : 0]    addra_third_bram_second_delay;

// Transfer flag
logic                                       first_transfer_active;
logic                                       second_transfer_active;
logic                                       third_transfer_active;
logic                                       transfer_active;
logic                                       internal_enable;


always_ff @(posedge i_clock, negedge i_reset) begin 
    if(!i_reset) begin
        addra_first_bram                <=  INPUT_BRAM_ADDRESS_WIDTH'('b0);
        addra_second_bram               <=  INPUT_BRAM_ADDRESS_WIDTH'('b0);
        addra_third_bram                <=  INPUT_BRAM_ADDRESS_WIDTH'('b0);
        addra_second_bram_first_delay   <=  INPUT_BRAM_ADDRESS_WIDTH'('b0);
        addra_second_bram_second_delay  <=  INPUT_BRAM_ADDRESS_WIDTH'('b0);
        addra_third_bram_first_delay    <=  INPUT_BRAM_ADDRESS_WIDTH'('b0);
        addra_third_bram_second_delay   <=  INPUT_BRAM_ADDRESS_WIDTH'('b0);
        din_a_first_bram                <=  DATA_WIDTH'('b0);
        din_a_second_bram               <=  DATA_WIDTH'('b0);
        din_a_third_bram                <=  DATA_WIDTH'('b0);
//        o_bram_data[0]                  <=  DATA_WIDTH'('b0);
//        o_bram_data[1]                  <=  DATA_WIDTH'('b0);
//        o_bram_data[2]                  <=  DATA_WIDTH'('b0);
        first_transfer_active           <=  1'b0;
        second_transfer_active          <=  1'b0;
        third_transfer_active           <=  1'b0;
    end 
    else begin
        if(i_enable && !o_reset_busy) begin
            if(i_wenable || transfer_active) begin
                if(i_wenable) begin
                    din_a_first_bram    <=  i_bram_data;
                    addra_first_bram    <=  i_waddress;
                    internal_enable     <=  1;
                end
                else begin
                    din_a_first_bram    <=  din_a_first_bram;
                    addra_first_bram    <=  addra_first_bram;
                    internal_enable     <=  0;
                end
                
                din_a_second_bram               <=  dout_a_first_bram;
                din_a_third_bram                <=  dout_a_second_bram;

                addra_second_bram_first_delay   <=  addra_first_bram;
                addra_second_bram_second_delay  <=  addra_second_bram_first_delay;
                addra_second_bram               <=  addra_second_bram_second_delay;
                
                addra_third_bram_first_delay    <=  addra_second_bram;
                addra_third_bram_second_delay   <=  addra_third_bram_first_delay;
                addra_third_bram                <=  addra_third_bram_second_delay;
                
                first_transfer_active           <=  (addra_first_bram != i_waddress);
                second_transfer_active          <=  (addra_second_bram != i_waddress);
                third_transfer_active           <=  (addra_third_bram != i_waddress);
                transfer_active                 <=  (addra_first_bram != i_waddress) || (addra_second_bram != i_waddress) || (addra_third_bram != i_waddress);
            end
        end
        else if (!i_wenable && !transfer_active || !o_reset_busy ) begin
            din_a_first_bram    <=  din_a_first_bram;
            din_a_second_bram   <=  din_a_second_bram;
            din_a_third_bram    <=  din_a_third_bram;
            addra_first_bram    <=  addra_first_bram;
            addra_second_bram   <=  addra_second_bram;
            addra_third_bram    <=  addra_third_bram;
        end
    end
end

    assign    o_bram_data[0]      =  dout_b_first_bram;
    assign    o_bram_data[1]      =  dout_b_second_bram;
    assign    o_bram_data[2]      =  dout_b_third_bram;

assign  o_reset_busy = first_bram_reset_a_busy || first_bram_reset_b_busy || second_bram_reset_a_busy || second_bram_reset_b_busy || third_bram_reset_a_busy || third_bram_reset_b_busy;

// Instantiate first BRAM
input_bram input_bram_first_inst (
    .clka(i_clock),
    .rsta(!i_reset),
    .ena(1'b1),
    .wea(internal_enable),
    .addra(addra_first_bram),
    .dina(din_a_first_bram),
    .douta(dout_a_first_bram),
    .clkb(i_clock),
    .rstb(!i_reset),
    .enb(i_renable),
    .web(),
    .addrb(i_raddress),
    .dinb(),
    .doutb(dout_b_first_bram),
    .rsta_busy(first_bram_reset_a_busy),
    .rstb_busy(first_bram_reset_b_busy)
);

// Instantiate second BRAM
input_bram input_bram_second_inst (
    .clka(i_clock),
    .rsta(!i_reset),
    .ena(1'b1),
    .wea(i_wenable || second_transfer_active),
    .addra(addra_second_bram),
    .dina(din_a_second_bram),
    .douta(dout_a_second_bram),
    .clkb(i_clock),
    .rstb(!i_reset),
    .enb(i_renable),
    .web(),
    .addrb(i_raddress),
    .dinb(),
    .doutb(dout_b_second_bram),
    .rsta_busy(second_bram_reset_a_busy),
    .rstb_busy(second_bram_reset_b_busy)
);

// Instantiate third BRAM
input_bram input_bram_third_inst (
    .clka(i_clock),
    .rsta(!i_reset),
    .ena(1'b1),
    .wea(i_wenable || transfer_active),
    .addra(addra_third_bram),
    .dina(din_a_third_bram),
    .douta(dout_a_third_bram),
    .clkb(i_clock),
    .rstb(!i_reset),
    .enb(i_renable),
    .web(),
    .addrb(i_raddress),
    .dinb(),
    .doutb(dout_b_third_bram),
    .rsta_busy(third_bram_reset_a_busy),
    .rstb_busy(third_bram_reset_b_busy)
);

endmodule
