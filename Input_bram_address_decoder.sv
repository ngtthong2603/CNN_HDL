module Input_bram_address_decoder #(
    parameter       INPUT_CHANNEL_WIDTH             =       8,
    parameter       INPUT_ROW_WIDTH                 =       6,
    parameter       INPUT_COL_WIDTH                 =       6,
    parameter       INPUT_BRAM_DEPTH                =       224 * 244,
    parameter       INPUT_BRAM_DEPTH_WIDTH          =       $clog2(INPUT_BRAM_DEPTH)
) (
    input           [INPUT_CHANNEL_WIDTH - 1 : 0]           i_input_feature_channel,
    input           [INPUT_ROW_WIDTH - 1 : 0]               i_input_feature_row,
    input           [INPUT_ROW_WIDTH - 1 : 0]               i_input_start_index_batch_row,
    input           [INPUT_ROW_WIDTH - 1 : 0]               i_input_end_index_batch_row,
    input           [INPUT_COL_WIDTH - 1 : 0]               i_input_feature_col,
    input           [INPUT_CHANNEL_WIDTH - 1 : 0]           i_input_feature_channel_data_point,
    input           [INPUT_ROW_WIDTH - 1 : 0]               i_input_feature_row_data_point,
    input           [INPUT_COL_WIDTH - 1 : 0]               i_input_feature_col_data_point,
    output  logic   [INPUT_BRAM_DEPTH_WIDTH - 1 : 0]        o_input_feature_address
);

    logic   [INPUT_ROW_WIDTH - 1 : 0]               input_feature_batch_row_num;

    assign input_feature_batch_row_num  =   i_input_end_index_batch_row - i_input_start_index_batch_row + 1;
    assign o_input_feature_address      =   i_input_feature_channel_data_point * input_feature_batch_row_num * i_input_feature_col
                                            + (i_input_feature_row_data_point - i_input_start_index_batch_row) * i_input_feature_col + i_input_feature_col_data_point; 

endmodule