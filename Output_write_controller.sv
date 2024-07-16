module Output_write_controller #(
    parameter       DATA_WIDTH                       =   32,
    parameter       FRAC_WIDTH                       =   16,
    parameter       OUTPUT_CHANNEL_WIDTH             =   8,
    parameter       OUTPUT_ROW_WIDTH                 =   2,
    parameter       OUTPUT_COL_WIDTH                 =   2,
    parameter       OUTPUT_BRAM_NUM                  =   4,
    parameter       OUTPUT_BRAM_DEPTH                =   1152,
    parameter       OUTPUT_BRAM_ADDRESS_WIDTH        =   $clog2(OUTPUT_BRAM_DEPTH)
) (
    input                                                   i_clock,
    input                                                   i_reset,
    input                                                   i_enable,
    input           [0:0]                                   i_renable [0 : OUTPUT_BRAM_NUM - 1],
    input                                                   i_wenable,
    input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_feature_channel,
    input           [OUTPUT_COL_WIDTH - 1 : 0]              i_output_feature_col,
    input           [OUTPUT_ROW_WIDTH - 1 : 0]              i_output_feature_row,
    input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_start_index_channel,
    input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_end_index_channel,
    //Write domain
    input                                                   i_accummulate_enable,
    input                                                   i_activate_enable,
    input           [DATA_WIDTH - 1 : 0]                    i_conv_result [0 : OUTPUT_BRAM_NUM - 1],
    input           [DATA_WIDTH - 1 : 0]                    i_bias_data [0 : OUTPUT_BRAM_NUM - 1],
    input           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_write_output_feature_channel_data_point [0 : OUTPUT_BRAM_NUM - 1],
    input           [OUTPUT_COL_WIDTH - 1 : 0]              i_write_output_feature_col_data_point,
    input           [OUTPUT_ROW_WIDTH - 1 : 0]              i_write_output_feature_row_data_point,
    //Read domain
    input           [OUTPUT_BRAM_ADDRESS_WIDTH - 1 : 0]     i_raddress [0 : OUTPUT_BRAM_NUM - 1],
    output          [DATA_WIDTH - 1 : 0]                    o_bram_data [0 : OUTPUT_BRAM_NUM - 1],
    output                                                  o_reset_busy
);
    // Address
    logic                                                   wen_first_delay;
    logic                                                   wen_second_delay;
    logic                                                   wen_third_delay;
    logic                                                   wen;

    logic           [DATA_WIDTH - 1 : 0]                    conv_result [0 : OUTPUT_BRAM_NUM - 1];

    logic                                                   activate_enable_first_delay;
    logic                                                   activate_enable_second_delay;
    logic                                                   activate_enable;
    logic           [OUTPUT_BRAM_ADDRESS_WIDTH - 1 : 0]     accum_raddress [0 : OUTPUT_BRAM_NUM - 1];
    logic           [OUTPUT_BRAM_ADDRESS_WIDTH - 1 : 0]     accum_address_first_delay [0 : OUTPUT_BRAM_NUM - 1];
    logic           [OUTPUT_BRAM_ADDRESS_WIDTH - 1 : 0]     accum_address_second_delay [0 : OUTPUT_BRAM_NUM - 1];
    logic           [OUTPUT_BRAM_ADDRESS_WIDTH - 1 : 0]     accum_waddress [0 : OUTPUT_BRAM_NUM - 1];
    // Bias
    logic           [DATA_WIDTH - 1 : 0]                    bias_data_delay [0 : OUTPUT_BRAM_NUM - 1];
    logic           [DATA_WIDTH - 1 : 0]                    bias_data [0 : OUTPUT_BRAM_NUM - 1];

    // data
    logic           [0:0]                                   acc_overflow [0 : OUTPUT_BRAM_NUM - 1];
    logic           [DATA_WIDTH - 1 : 0]                    accumulate_data [0 : OUTPUT_BRAM_NUM - 1];
    logic           [DATA_WIDTH - 1 : 0]                    bram_accumulate_data [0 : OUTPUT_BRAM_NUM - 1];
    logic           [DATA_WIDTH - 1 : 0]                    accumulate_result [0 : OUTPUT_BRAM_NUM - 1];
    logic           [DATA_WIDTH - 1 : 0]                    activated_result [0 : OUTPUT_BRAM_NUM - 1];
    // accumulate
    logic                                                   accumulate_enable_delay;
    logic                                                   accumulate_enable;

    logic           [0:0]                                   line_buffer_reset_busy_a    [0 : OUTPUT_BRAM_NUM - 1];
    logic           [0:0]                                   line_buffer_reset_busy_b    [0 : OUTPUT_BRAM_NUM - 1];
    logic           [0:0]                                   output_bram_reset_busy_a    [0 : OUTPUT_BRAM_NUM - 1];
    logic           [0:0]                                   output_bram_reset_busy_b    [0 : OUTPUT_BRAM_NUM - 1];

    assign          o_reset_busy            =       line_buffer_reset_busy_a[0] || line_buffer_reset_busy_a[1] || line_buffer_reset_busy_a[2] || line_buffer_reset_busy_a[3] || line_buffer_reset_busy_b[0] || line_buffer_reset_busy_b[1] || line_buffer_reset_busy_b[2] || line_buffer_reset_busy_b[3]
                                                ||  output_bram_reset_busy_a[0] || output_bram_reset_busy_a[1] || output_bram_reset_busy_a[2] || output_bram_reset_busy_a[3] || output_bram_reset_busy_b[0] || output_bram_reset_busy_b[1] || output_bram_reset_busy_b[2] || output_bram_reset_busy_b[3];

    always_ff @(posedge i_clock, negedge i_reset) begin 
        if(!i_reset) begin
            wen_first_delay                     <=      0;
            wen_second_delay                    <=      0;
            wen_third_delay                     <=      0;
            wen                                 <=      0;

            accumulate_enable_delay             <=      0;
            accumulate_enable                   <=      0;

            activate_enable_first_delay         <=      0;
            activate_enable_second_delay        <=      0;
            activate_enable                     <=      0;
            for(int i = 0; i < OUTPUT_BRAM_NUM; i++) begin
                accum_address_first_delay[i]           <=      '0;
                accum_address_second_delay[i]          <=      '0;
                accum_waddress[i]                      <=      '0;

                bias_data_delay[i]                     <=      '0;                
                bias_data[i]                           <=      '0;

                conv_result[i]                         <=      '0;

                accumulate_data[i]                     <=      '0;
            end
        end
        else begin
            if(i_enable) begin
                activate_enable_first_delay         <=      i_activate_enable;
                activate_enable_second_delay        <=      activate_enable_first_delay;
                activate_enable                     <=      activate_enable_second_delay;

                accumulate_enable_delay             <=      i_accummulate_enable;
                accumulate_enable                   <=      accumulate_enable_delay;

                wen_first_delay                     <=      i_wenable;
                wen_second_delay                    <=      wen_first_delay;
                wen_third_delay                     <=      wen_second_delay;
                wen                                 <=      wen_third_delay;
                for(int i = 0; i < OUTPUT_BRAM_NUM; i++) begin
                    accum_address_first_delay[i]           <=      accum_raddress[i];
                    accum_address_second_delay[i]          <=      accum_address_first_delay[i];
                    accum_waddress[i]                      <=      accum_address_second_delay[i];

                    bias_data_delay[i]                     <=      i_bias_data[i];
                    bias_data[i]                           <=      bias_data_delay[i];

                    conv_result[i]                         <=      i_conv_result[i];

                    accumulate_data[i]                     <=       (accumulate_enable_delay) ? bram_accumulate_data[i] : bias_data[i];
                end
            end
        end
    end
    generate
    genvar i;
    for(i = 0; i < OUTPUT_BRAM_NUM; i++) begin
        //assign          accumulate_data[i]      =       (accumulate_enable_delay) ? bram_accumulate_data[i] : bias_data[i];
        assign          activated_result[i]     =       (!activate_enable) ? accumulate_result[i] : (accumulate_result[i][DATA_WIDTH - 1] == 0) ? accumulate_result[i] : DATA_WIDTH'('b0);
        qadd #(
	        .Q(FRAC_WIDTH),
	        .N(DATA_WIDTH)
        ) qadd_inst (
            .a(accumulate_data[i]),
            .b(conv_result[i]),
            .c(accumulate_result[i]),
	        .add_overflow(acc_overflow[i])
        );
        Output_bram_address_decoder #(
            .OUTPUT_CHANNEL_WIDTH(OUTPUT_CHANNEL_WIDTH),
            .OUTPUT_ROW_WIDTH(OUTPUT_ROW_WIDTH),
            .OUTPUT_COL_WIDTH(OUTPUT_COL_WIDTH),
            .OUTPUT_BRAM_NUM(OUTPUT_BRAM_NUM),
            .OUTPUT_BRAM_DEPTH(OUTPUT_BRAM_DEPTH),
            .OUTPUT_BRAM_ADDRESS_WIDTH(OUTPUT_BRAM_ADDRESS_WIDTH)
        ) Output_bram_write_address_decoder_inst (
            .i_output_feature_channel(i_output_feature_channel),
            .i_output_feature_col(i_output_feature_col),
            .i_output_feature_row(i_output_feature_row),
            .i_output_start_index_channel(i_output_start_index_channel),
            .i_output_end_index_channel(i_output_end_index_channel),
            .i_output_feature_channel_data_point(i_write_output_feature_channel_data_point[i]),
            .i_output_feature_col_data_point(i_write_output_feature_col_data_point),
            .i_output_feature_row_data_point(i_write_output_feature_row_data_point),
            .o_output_feature_address(accum_raddress[i])
        );
        output_conv_bram line_buffer_inst (
            .clka(i_clock),
            .ena(1'b1),
            .wea(wen_third_delay),
            .addra(accum_waddress[i]),
            .dina(activated_result[i]),
            .clkb(i_clock),
            .rstb(!i_reset),
            .enb(1'b1),
            .addrb(accum_raddress[i]),
            .doutb(bram_accumulate_data[i]),
            .rsta_busy(line_buffer_reset_busy_a[i]),
            .rstb_busy(line_buffer_reset_busy_b[i])
        );
        output_conv_bram output_conv_bram_inst (
            .clka(i_clock),
            .ena(1'b1),
            .wea(activate_enable),
            .addra(accum_waddress[i]),
            .dina(activated_result[i]),
            .clkb(i_clock),
            .rstb(!i_reset),
            .enb(i_renable[i]),
            .addrb(i_raddress[i]),
            .doutb(o_bram_data[i]),
            .rsta_busy(output_bram_reset_busy_a[i]),
            .rstb_busy(output_bram_reset_busy_b[i])
        );
    end
    endgenerate
endmodule