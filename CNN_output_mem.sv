module CNN_output_mem #(
    parameter       OUTPUT_CHANNEL_WIDTH             =   7,
    parameter       OUTPUT_ROW_WIDTH                 =   2,
    parameter       OUTPUT_COL_WIDTH                 =   2,
    parameter       OUTPUT_BRAM_NUM                  =   4,
    parameter       OUTPUT_BRAM_DEPTH                =   1152,
    parameter       OUTPUT_BRAM_ADDRESS_WIDTH        =   $clog2(KERNEL_BRAM_DEPTH)
) (
    input                                                   i_clock,
    input                                                   i_reset,
    input                                                   i_enable,
    input                                                   i_renable,
    input                                                   i_wenable,
    input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_feature_channel,
    input           [OUTPUT_COL_WIDTH - 1 : 0]              i_output_feature_col,
    input           [OUTPUT_ROW_WIDTH - 1 : 0]              i_output_feature_row,
    input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_start_index_channel,
    input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_end_index_channel,
    //Write domain
    input           [DATA_WIDTH - 1 : 0]                    i_bram_data,
    input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_write_output_feature_channel_data_point,
    input           [OUTPUT_COL_WIDTH - 1 : 0]              i_write_output_feature_col_data_point,
    input           [OUTPUT_ROW_WIDTH - 1 : 0]              i_write_output_feature_row_data_point,
    //Read domain
    input           [OUTPUT_BRAM_ADDRESS_WIDTH - 1 : 0]     i_rd_address;
    output          [DATA_WIDTH - 1 : 0]                    o_bram_data
);

    logic           [OUTPUT_BRAM_ADDRESS_WIDTH - 1 : 0]     wr_address;

    Output_bram_address_decoder #(
        .OUTPUT_CHANNEL_WIDTH(OUTPUT_CHANNEL_WIDTH),
        .OUTPUT_ROW_WIDTH(OUTPUT_ROW_WIDTH),
        .OUTPUT_COL_WIDT(OUTPUT_COL_WIDT),
        .OUTPUT_BRAM_NUM(OUTPUT_BRAM_NUM),
        .OUTPUT_BRAM_DEPTH(OUTPUT_BRAM_DEPTH),
        .OUTPUT_BRAM_ADDRESS_WIDTH(OUTPUT_BRAM_ADDRESS_WIDTH)
    ) Output_bram_write_address_decoder_inst (
        .i_output_feature_channel(i_output_feature_channel),
        .i_output_feature_col(i_output_feature_col),
        .i_output_feature_row(i_output_feature_row),
        .i_output_start_index_channel(i_output_start_index_channel),
        .i_output_end_index_channel(i_output_end_index_channel),
        .i_output_feature_channel_data_point(i_write_output_feature_channel_data_point),
        .i_output_feature_col_data_point(i_write_output_feature_col_data_point),
        .i_output_feature_row_data_point(i_write_output_feature_row_data_point),
        .o_output_feature_address(wr_address)
    );

    output_bram output_bram_inst(
      .clka(i_clock),
      .ena(i_enable),
      .wea(i_wenable),
      .addra(wr_address),
      .dina(i_bram_data),
      .clkb(i_clock),
      .rstb(i_reset),
      .enb(i_renable),
      .addrb(i_rd_address),
      .doutb(o_bram_data),
      .rsta_busy(),
      .rstb_busy()
    );
    
endmodule