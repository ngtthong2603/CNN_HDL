// module Output_bram_address_decoder #(
//     parameter       OUTPUT_CHANNEL_WIDTH             =   8,
//     parameter       OUTPUT_ROW_WIDTH                 =   6,
//     parameter       OUTPUT_COL_WIDTH                 =   6,
//     parameter       OUTPUT_BRAM_NUM                  =   4,
//     parameter       OUTPUT_BRAM_DEPTH                =   1152,
//     parameter       OUTPUT_BRAM_ADDRESS_WIDTH        =   $clog2(OUTPUT_BRAM_DEPTH)
// ) (
//     input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_feature_channel,
//     input           [OUTPUT_COL_WIDTH - 1 : 0]              i_output_feature_col,
//     input           [OUTPUT_ROW_WIDTH - 1 : 0]              i_output_feature_row,
//     input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_start_index_channel,
//     input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_end_index_channel,
//     input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_feature_channel_data_point,
//     input           [OUTPUT_COL_WIDTH - 1 : 0]              i_output_feature_col_data_point,
//     input           [OUTPUT_ROW_WIDTH - 1 : 0]              i_output_feature_row_data_point,
//     output  logic   [OUTPUT_BRAM_ADDRESS_WIDTH - 1 : 0]     o_output_feature_address
// );

//     assign o_output_feature_address =  (i_output_feature_channel_data_point - i_output_start_index_channel) / 4 * i_output_feature_row * i_output_feature_col 
//                                         + i_output_feature_row_data_point * i_output_feature_col + i_output_feature_col_data_point;

// endmodule
module Output_bram_address_decoder #(
    parameter       OUTPUT_CHANNEL_WIDTH             =   8,
    parameter       OUTPUT_ROW_WIDTH                 =   6,
    parameter       OUTPUT_COL_WIDTH                 =   6,
    parameter       OUTPUT_BRAM_NUM                  =   4,
    parameter       OUTPUT_BRAM_DEPTH                =   1152,
    parameter       OUTPUT_BRAM_ADDRESS_WIDTH        =   $clog2(OUTPUT_BRAM_DEPTH)
) (
    input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_feature_channel,
    input           [OUTPUT_COL_WIDTH - 1 : 0]              i_output_feature_col,
    input           [OUTPUT_ROW_WIDTH - 1 : 0]              i_output_feature_row,
    input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_start_index_channel,
    input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_end_index_channel,
    input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_feature_channel_data_point,
    input           [OUTPUT_COL_WIDTH - 1 : 0]              i_output_feature_col_data_point,
    input           [OUTPUT_ROW_WIDTH - 1 : 0]              i_output_feature_row_data_point,
    output  logic   [OUTPUT_BRAM_ADDRESS_WIDTH - 1 : 0]     o_output_feature_address
);

    // Compute intermediate values
    wire [OUTPUT_CHANNEL_WIDTH - 3 : 0] channel_offset;
    wire [OUTPUT_BRAM_ADDRESS_WIDTH - 1 : 0] row_offset;
    wire [OUTPUT_BRAM_ADDRESS_WIDTH - 1 : 0] col_offset;
    wire [OUTPUT_BRAM_ADDRESS_WIDTH - 1 : 0] base_address;

    assign channel_offset = (i_output_feature_channel_data_point - i_output_start_index_channel) >> 2;
    assign row_offset = channel_offset * i_output_feature_row * i_output_feature_col;
    assign col_offset = i_output_feature_row_data_point * i_output_feature_col + i_output_feature_col_data_point;
    assign base_address = row_offset + col_offset;

    // Assign the final address
    assign o_output_feature_address = base_address;

endmodule
