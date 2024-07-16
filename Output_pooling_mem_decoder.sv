module Output_pooling_mem_decoder #(
    parameter       OUTPUT_CHANNEL_WIDTH             =   7,
    parameter       OUTPUT_ROW_WIDTH                 =   2,
    parameter       OUTPUT_COL_WIDTH                 =   2,
    parameter       OUTPUT_BRAM_NUM                  =   4,
    parameter       OUTPUT_BRAM_DEPTH                =   1152,
    parameter       OUTPUT_BRAM_ADDRESS_WIDTH        =   $clog2(KERNEL_BRAM_DEPTH)
) (
    input                                                   i_pooling_enable,
    input           [OUTPUT_COL_WIDTH - 1 : 0]              i_output_pooling_size,
    input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_start_index_channel,
    input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_end_index_channel,
    input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_channel_data_point,
    input           [OUTPUT_COL_WIDTH - 1 : 0]              i_output_col_data_point,
    input           [OUTPUT_ROW_WIDTH - 1 : 0]              i_output_row_data_point,
    output  logic   [OUTPUT_BRAM_ADDRESS_WIDTH - 1 : 0]     o_output_address
);

    assign o_output_address =  (i_output_channel_data_point - i_output_start_index_channel) / 4 * i_output_pooling_size * i_output_pooling_size 
                                + i_output_row_data_point * i_output_pooling_size + i_output_col_data_point;
endmodule