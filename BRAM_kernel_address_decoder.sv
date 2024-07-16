module BRAM_kernel_address_decoder #(
    parameter       KERNEL_FILTER_WIDTH              =   7,
    parameter       KERNEL_CHANNEL_WIDTH             =   7,
    parameter       KERNEL_ROW_WIDTH                 =   2,
    parameter       KERNEL_COL_WIDTH                 =   2,
    parameter       KERNEL_BRAM_NUM                  =   4,
    parameter       KERNEL_BRAM_DEPTH                =   1152,
    parameter       KERNEL_BRAM_ADDRESS_WIDTH        =   $clog2(KERNEL_BRAM_DEPTH)
) (
    input           [KERNEL_FILTER_WIDTH - 1 : 0]           i_kernel_filter,
    input           [KERNEL_CHANNEL_WIDTH - 1 : 0]          i_kernel_channel,
    input           [KERNEL_COL_WIDTH - 1 : 0]              i_kernel_col,
    input           [KERNEL_ROW_WIDTH - 1 : 0]              i_kernel_row,
    input           [KERNEL_FILTER_WIDTH - 1 : 0]           i_kernel_start_filter,
    input           [KERNEL_FILTER_WIDTH - 1 : 0]           i_kernel_end_filter,
    input           [KERNEL_FILTER_WIDTH - 1 : 0]           i_kernel_filter_data_point,
    input           [KERNEL_CHANNEL_WIDTH - 1 : 0]          i_kernel_channel_data_point,
    input           [KERNEL_COL_WIDTH - 1 : 0]              i_kernel_col_data_point,
    input           [KERNEL_ROW_WIDTH - 1 : 0]              i_kernel_row_data_point,
    output  logic   [KERNEL_BRAM_ADDRESS_WIDTH - 1 : 0]     o_kernel_address
);

    logic [KERNEL_FILTER_WIDTH - 1 : 0] relative_filter_index;
    logic [KERNEL_BRAM_ADDRESS_WIDTH - 1 : 0] element_offset;
    logic [KERNEL_BRAM_ADDRESS_WIDTH - 1 : 0] filter_offset;

    always_comb begin
        // Calculate the relative filter index within the batch
        relative_filter_index = i_kernel_filter_data_point - i_kernel_start_filter;

        // Calculate the offset for the filter within the BRAM
        filter_offset = (relative_filter_index / KERNEL_BRAM_NUM) * (i_kernel_channel * i_kernel_row * i_kernel_col);

        // Calculate the element offset within the filter
        element_offset = (i_kernel_channel_data_point * (i_kernel_row * i_kernel_col)) +
                         (i_kernel_row_data_point * i_kernel_col) +
                         i_kernel_col_data_point;

        // Combine filter offset and element offset
        o_kernel_address = filter_offset + element_offset;
    end

endmodule
