module CNN_kernel_bias_mem #(
    parameter           DATA_WIDTH                      =   32,
    parameter           KERNEL_FILTER_WIDTH             =   8,
    parameter           KERNEL_BRAM_NUM                 =   4,
    parameter           KERNEL_BIAS_WIDTH               =   KERNEL_FILTER_WIDTH,
    parameter           KERNEL_BIAS_BRAM_ADDRESS_WIDTH  =   1
) (
    input                                                                   i_clock,
    input                                                                   i_reset,
    input                                                                   i_enable,
    input           [0:0]                                                   i_wenable [0 : KERNEL_BRAM_NUM - 1],
    input           [DATA_WIDTH - 1 : 0]                                    i_bram_data[0 : KERNEL_BRAM_NUM - 1],
    input           [KERNEL_BIAS_WIDTH - 1 : 0]                             i_kernel_bias_size,
    input           [KERNEL_BIAS_BRAM_ADDRESS_WIDTH - 1 : 0]                i_waddress [0 : KERNEL_BRAM_NUM - 1],
    input           [KERNEL_FILTER_WIDTH - 1 : 0]                           i_kernel_bias_data_point[0 : KERNEL_BRAM_NUM - 1],
    output          [DATA_WIDTH - 1 : 0]                                    o_kernel_bias_data [0 : KERNEL_BRAM_NUM - 1],
    output                                                                  o_reset_busy
);

    logic           [0:0]                                                   reset_busy_a [0 : KERNEL_BRAM_NUM - 1];
    logic           [0:0]                                                   reset_busy_b [0 : KERNEL_BRAM_NUM - 1];
    logic           [KERNEL_BIAS_BRAM_ADDRESS_WIDTH - 1 : 0]                kernel_bias_address [0 : KERNEL_BRAM_NUM - 1];

    assign  o_reset_busy    =   reset_busy_a[0] || reset_busy_a[1] || reset_busy_a[2] || reset_busy_a[3] || reset_busy_b[0] || reset_busy_b[1] || reset_busy_b[2] || reset_busy_b[3];

    genvar i;
    generate
    for(i = 0; i < KERNEL_BRAM_NUM ; i++) begin
        assign kernel_bias_address[i]  =   i_kernel_bias_data_point[i] / 4;
        kernel_bias_mem kernel_bias_bram_inst (
            .clka(i_clock),
            .ena(1'b1),
            .wea(i_wenable[i]),
            .addra(i_waddress[i]),
            .dina(i_bram_data[i]),
            .clkb(i_clock),
            .rstb(!i_reset),
            .enb(1'b1),
            .addrb(kernel_bias_address[i]),
            .doutb(o_kernel_bias_data[i]),
            .rsta_busy(reset_busy_a[i]),
            .rstb_busy(reset_busy_b[i])
        );
    end
    endgenerate
    
endmodule