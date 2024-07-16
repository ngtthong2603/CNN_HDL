module CNN_kernel_weights_mem #(
    parameter       KERNEL_BRAM_NUM                  =       4,
    parameter       KERNEL_BRAM_DEPTH                =       1152,
    parameter       KERNEL_BRAM_ADDRESS_WIDTH        =       $clog2(KERNEL_BRAM_DEPTH),
    parameter       DATA_WIDTH                       =       32,
    parameter       KERNEL_FILTER_WIDTH              =       8,
    parameter       KERNEL_CHANNEL_WIDTH             =       8,
    parameter       KERNEL_ROW_WIDTH                 =       2,
    parameter       KERNEL_COL_WIDTH                 =       2
) (
    input                                                   i_clock,
    input                                                   i_reset,
    input                                                   i_enable,
    input                                                   i_renable,
    input                                                   i_wenable,
    input           [KERNEL_BRAM_ADDRESS_WIDTH - 1 : 0]     i_waddress,
    input           [DATA_WIDTH - 1 : 0]                    i_bram_data,
    input           [KERNEL_FILTER_WIDTH - 1 : 0]           i_kernel_filter,
    input           [KERNEL_CHANNEL_WIDTH - 1 : 0]          i_kernel_channel,
    input           [KERNEL_ROW_WIDTH - 1 : 0]              i_kernel_row,
    input           [KERNEL_COL_WIDTH - 1 : 0]              i_kernel_col,
    input           [KERNEL_FILTER_WIDTH - 1 : 0]           i_kernel_start_filter,
    input           [KERNEL_FILTER_WIDTH - 1 : 0]           i_kernel_end_filter,
    input           [KERNEL_FILTER_WIDTH - 1 : 0]           i_kernel_filter_data_point,
    input           [KERNEL_CHANNEL_WIDTH - 1 : 0]          i_kernel_channel_data_point,
    input           [KERNEL_ROW_WIDTH - 1 : 0]              i_kernel_row_data_point,
    input           [KERNEL_COL_WIDTH - 1 : 0]              i_kernel_col_data_point,
    output          [DATA_WIDTH - 1 : 0]                    o_bram_data,
    output                                                  o_reset_busy                  
);

    logic [KERNEL_BRAM_ADDRESS_WIDTH - 1 : 0] rd_address;

    logic                                     reset_busy_a;
    logic                                     reset_busy_b;
    
    assign      o_reset_busy    =   reset_busy_a || reset_busy_b;
    // Instantiate the address decoder
    BRAM_kernel_address_decoder #(
        .KERNEL_FILTER_WIDTH(KERNEL_FILTER_WIDTH),
        .KERNEL_CHANNEL_WIDTH(KERNEL_CHANNEL_WIDTH),
        .KERNEL_ROW_WIDTH(KERNEL_ROW_WIDTH),
        .KERNEL_COL_WIDTH(KERNEL_COL_WIDTH),
        .KERNEL_BRAM_NUM(KERNEL_BRAM_NUM),
        .KERNEL_BRAM_DEPTH(KERNEL_BRAM_DEPTH),
        .KERNEL_BRAM_ADDRESS_WIDTH(KERNEL_BRAM_ADDRESS_WIDTH)
    ) BRAM_kernel_address_decoder_inst (
        .i_kernel_filter(i_kernel_filter),
        .i_kernel_channel(i_kernel_channel),
        .i_kernel_col(i_kernel_col),
        .i_kernel_row(i_kernel_row),
        .i_kernel_start_filter(i_kernel_start_filter),
        .i_kernel_end_filter(i_kernel_end_filter),
        .i_kernel_filter_data_point(i_kernel_filter_data_point),
        .i_kernel_channel_data_point(i_kernel_channel_data_point),
        .i_kernel_col_data_point(i_kernel_col_data_point),
        .i_kernel_row_data_point(i_kernel_row_data_point),
        .o_kernel_address(rd_address)
    );

    // Instantiate the dual-port BRAM
    kernel_bram kernel_bram_inst (
        .clka(i_clock),
        .ena(1'b1),
        .wea(i_wenable),
        .addra(i_waddress),
        .dina(i_bram_data),
        .clkb(i_clock),
        .rstb(!i_reset),
        .enb(i_renable),
        .addrb(rd_address),
        .doutb(o_bram_data),
        .rsta_busy(reset_busy_a),
        .rstb_busy(reset_busy_b)
    );

endmodule
