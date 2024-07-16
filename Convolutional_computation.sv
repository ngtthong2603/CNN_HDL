
module Convolutional_computation #(
    // Data type parameters
    parameter   DATA_WIDTH                          =       32,
    parameter   FRAC_WIDTH                          =       16,
    // Kernel parameters
    parameter   KERNEL_FILTER_WIDTH                 =       8,
    parameter   KERNEL_CHANNEL_WIDTH                =       8,
    parameter   KERNEL_ROW_WIDTH                    =       2,
    parameter   KERNEL_COL_WIDTH                    =       2,
    parameter   KERNEL_BRAM_NUM                     =       4,
    parameter   KERNEL_WEIGHTS_BRAM_DEPTH           =       1152,
    parameter   KERNEL_BIAS_BRAM_DEPTH              =       128,
    parameter   KERNEL_BIAS_WIDTH                   =       KERNEL_FILTER_WIDTH,
    parameter   KERNEL_FIFO_DEPTH                   =       32,
    parameter   KERNEL_POINTER_WIDTH                =       $clog2(KERNEL_FIFO_DEPTH),
    parameter   KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH   =       $clog2(KERNEL_WEIGHTS_BRAM_DEPTH),
    parameter   KERNEL_BIAS_BRAM_ADDRESS_WIDTH      =       $clog2(KERNEL_BIAS_BRAM_DEPTH),
    // Input parameters
    parameter   INPUT_CHANNEL_WIDTH                 =       8,
    parameter   INPUT_ROW_WIDTH                     =       6,
    parameter   INPUT_COL_WIDTH                     =       6,
    parameter   INPUT_BRAM_DEPTH                    =       224 * 244,
    parameter   INPUT_BRAM_ADDRESS_WIDTH            =       $clog2(INPUT_BRAM_DEPTH),
    parameter   INPUT_FIFO_DEPTH                    =       32,
    parameter   INPUT_POINTER_WIDTH                 =       $clog2(INPUT_FIFO_DEPTH),
    parameter   INPUT_TOTAL_ELEMENT_WIDTH           =       $clog2(3*INPUT_FIFO_DEPTH),
    // Batch normalization signals
    parameter   BATCH_NORM_WEIGHTS_WIDTH            =       KERNEL_FILTER_WIDTH,
    parameter   BATCH_NORM_BIAS_WIDTH               =       KERNEL_FILTER_WIDTH,
    // Output parameters
    parameter   OUTPUT_CHANNEL_WIDTH                =       KERNEL_FILTER_WIDTH,
    parameter   OUTPUT_COL_WIDTH                    =       INPUT_COL_WIDTH,
    parameter   OUTPUT_ROW_WIDTH                    =       INPUT_ROW_WIDTH,
    parameter   OUTPUT_BRAM_NUM                     =       KERNEL_BRAM_NUM,
    parameter   OUTPUT_BRAM_DEPTH                   =       32*32,
    parameter   OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH   =       $clog2(OUTPUT_BRAM_DEPTH)

) (
    input                                                                   i_clock,
    input                                                                   i_reset,
    input                                                                   i_enable,
    input     [OUTPUT_BRAM_NUM - 1 : 0]                                     i_renable,  // Flattened [0:0] removed
    input     [OUTPUT_BRAM_NUM * OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH - 1 : 0] i_output_feature_raddress,  // Flattened
    input                                                                   i_input_feature_wenable,
    input     [KERNEL_BRAM_NUM - 1 : 0]                                     i_kernel_weights_wenable,  // Flattened [0:0] removed
    input     [KERNEL_BRAM_NUM - 1 : 0]                                     i_kernel_bias_wenable,  // Flattened [0:0] removed
    input                                                                   i_input_start_transfer_process,
    input                                                                   i_kernel_weights_start_transfer_process,
    input                                                                   i_load_new_filter,
    input     [INPUT_BRAM_ADDRESS_WIDTH - 1 : 0]                            i_input_feature_wraddress,
    input     [KERNEL_BRAM_NUM * KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH - 1 : 0] i_kernel_weights_wraddress,  // Flattened
    input     [KERNEL_BRAM_NUM * KERNEL_BIAS_BRAM_ADDRESS_WIDTH - 1 : 0]    i_kernel_bias_wraddress,  // Flattened
    input     [DATA_WIDTH - 1 : 0]                                          i_input_feature_data,
    input     [KERNEL_BRAM_NUM * DATA_WIDTH - 1 : 0]                        i_kernel_weights_data,
    input     [KERNEL_BRAM_NUM * DATA_WIDTH - 1 : 0]                        i_kernel_bias_data,
    input     [INPUT_ROW_WIDTH - 1 : 0]                                     i_input_feature_row,
    input     [INPUT_COL_WIDTH - 1 : 0]                                     i_input_feature_col,
    input     [INPUT_CHANNEL_WIDTH - 1 : 0]                                 i_input_feature_channel,
    input     [KERNEL_ROW_WIDTH - 1 : 0]                                    i_kernel_row,
    input     [KERNEL_COL_WIDTH - 1 : 0]                                    i_kernel_col,
    input     [KERNEL_CHANNEL_WIDTH - 1 : 0]                                i_kernel_channel,
    input     [KERNEL_FILTER_WIDTH - 1 : 0]                                 i_kernel_filter,
    input     [KERNEL_FILTER_WIDTH - 1 : 0]                                 i_kernel_start_index_batch_filter,
    input     [KERNEL_FILTER_WIDTH - 1 : 0]                                 i_kernel_end_index_batch_filter,
    input     [KERNEL_BIAS_WIDTH - 1 : 0]                                   i_kernel_bias_size,
    input     [OUTPUT_ROW_WIDTH - 1 : 0]                                    i_output_feature_row,
    input     [OUTPUT_COL_WIDTH - 1 : 0]                                    i_output_feature_col,
    input     [OUTPUT_CHANNEL_WIDTH - 1 : 0]                                i_output_feature_channel,
    input     [OUTPUT_CHANNEL_WIDTH - 1 : 0]                                i_output_feature_start_index_channel,
    input     [OUTPUT_CHANNEL_WIDTH - 1 : 0]                                i_output_feature_end_index_channel,
    output    [OUTPUT_BRAM_NUM * DATA_WIDTH - 1 : 0]                        o_bram_data,  // Flattened
    output    logic [DATA_WIDTH - 1 : 0]                                    o_output_index,
    output    logic                                                         o_kernel_weights_reset_busy,
    output    logic                                                         o_kernel_bias_reset_busy,
    output    logic                                                         o_input_feature_reset_busy,
    output    logic                                                         o_output_feature_reset_busy,
    output    logic                                                         o_row_done,
    output    logic                                                         o_processing_done
);

    // Kernel local parameters
    localparam      KERNEL_ROW_SIZE                     =       3;
    localparam      KERNEL_COL_SIZE                     =       3;
    // Input local parameters
    
    // Output local parameters
    localparam      OUTPUT_BRAM_ADDRESS_WIDTH           =       $clog2(OUTPUT_BRAM_DEPTH);
    // Controller local parameters
    localparam      KERNEL_ROW_MAX_SIZE                 =       3;
    localparam      KERNEL_COL_MAX_SIZE                 =       3;
    localparam      CONV_SIZE                           =       KERNEL_ROW_MAX_SIZE * KERNEL_COL_MAX_SIZE;
    // register bank siganls
    logic           [INPUT_ROW_WIDTH - 1 : 0]                   input_feature_row;
    logic           [INPUT_COL_WIDTH - 1 : 0]                   input_feature_col;
    logic           [INPUT_CHANNEL_WIDTH - 1 : 0]               input_feature_channel;
    logic           [KERNEL_ROW_WIDTH - 1 : 0]                  kernel_row;
    logic           [KERNEL_COL_WIDTH - 1 : 0]                  kernel_col;
    logic           [KERNEL_CHANNEL_WIDTH - 1 : 0]              kernel_channel;
    logic           [KERNEL_FILTER_WIDTH - 1 : 0]               kernel_filter;
    logic           [KERNEL_FILTER_WIDTH - 1 : 0]               kernel_start_index_batch_filter;
    logic           [KERNEL_FILTER_WIDTH - 1 : 0]               kernel_end_index_batch_filter;
    logic           [KERNEL_BIAS_WIDTH - 1 : 0]                 kernel_bias_size;
    logic           [OUTPUT_ROW_WIDTH - 1 : 0]                  output_feature_row;
    logic           [OUTPUT_ROW_WIDTH - 1 : 0]                  output_feature_col;
    logic           [OUTPUT_CHANNEL_WIDTH - 1 : 0]              output_feature_channel;
    logic           [OUTPUT_CHANNEL_WIDTH - 1 : 0]              output_feature_start_index_channel;
    logic           [OUTPUT_CHANNEL_WIDTH - 1 : 0]              output_feature_end_index_channel;
    // kernel signals
    logic                                                       kernel_weights_read_enable;
    logic                   [DATA_WIDTH - 1 : 0]                kernel_conv_weights  [0 : KERNEL_BRAM_NUM - 1][0 : CONV_SIZE - 1];
    logic                   [KERNEL_POINTER_WIDTH : 0]          kernel_element_num [0 : KERNEL_BRAM_NUM - 1];
    logic                                                       kernel_valid;
    // input signals
    logic                                                       input_read_enable;
    logic                   [DATA_WIDTH - 1 : 0]                input_conv    [0 : CONV_SIZE - 1];
    logic                   [INPUT_TOTAL_ELEMENT_WIDTH : 0]     input_element_num;
    logic                                                       input_valid;
    // output signals
    logic                   [DATA_WIDTH - 1 : 0]                output_conv_result [0 : OUTPUT_BRAM_NUM - 1];
    logic                   [OUTPUT_CHANNEL_WIDTH - 1 : 0]      output_feature_channel_data_point [0 : OUTPUT_BRAM_NUM - 1];
    logic                   [OUTPUT_ROW_WIDTH - 1 : 0]          output_feature_row_data_point;
    logic                   [OUTPUT_COL_WIDTH - 1 : 0]          output_feature_col_data_point;
    logic                   [OUTPUT_COL_WIDTH - 1 : 0]          output_feature_col_data_point_prev;
    logic                   [INPUT_CHANNEL_WIDTH - 1 : 0]       input_feature_channel_data_point_count;
    logic                                                       overflow;

    logic                                                       kernel_element_check;
    logic                                                       input_element_check;
    logic                   [DATA_WIDTH - 1 : 0]                bias_conv [0 : OUTPUT_BRAM_NUM - 1];
    logic                                                       accumulate_enable;

    logic                                                       input_mem_busy;
    logic                                                       output_mem_busy;
    logic                                                       kernel_weights_busy;
    logic                                                       kernel_bias_busy;

    logic                                                       output_write_enable;
    logic                                                       activate_enable;
    //temp
    logic                   [OUTPUT_COL_WIDTH - 1 : 0]          input_read_count;
    logic                   [OUTPUT_ROW_WIDTH - 1 : 0]          output_feature_row_access_data_point;
    logic                   [OUTPUT_COL_WIDTH - 1 : 0]          output_feature_col_access_data_point;

    logic            [KERNEL_BIAS_BRAM_ADDRESS_WIDTH - 1 : 0]   bias_address    [0 : OUTPUT_BRAM_NUM - 1];
    //temp variables
    logic     [0:0]                                             kernel_weights_wenable [KERNEL_BRAM_NUM - 1 : 0];
    logic     [KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH - 1 : 0]       kernel_weights_wraddress [KERNEL_BRAM_NUM - 1 : 0];
    logic     [0:0]                                             kernel_bias_wenable [KERNEL_BRAM_NUM - 1 : 0];
    logic     [KERNEL_BIAS_BRAM_ADDRESS_WIDTH - 1 : 0]          kernel_bias_wraddress [KERNEL_BRAM_NUM - 1 : 0];
    logic     [DATA_WIDTH - 1 : 0]                              kernel_weights_data [KERNEL_BRAM_NUM - 1 : 0];
    logic     [DATA_WIDTH - 1 : 0]                              kernel_bias_data [KERNEL_BRAM_NUM - 1 : 0];
    logic     [OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH - 1 : 0]       output_feature_raddress [OUTPUT_BRAM_NUM - 1 : 0];
    logic     [DATA_WIDTH - 1 : 0]                              output_bram_data [OUTPUT_BRAM_NUM - 1 : 0];
    logic     [0:0]                                             output_renable [0 : OUTPUT_BRAM_NUM - 1];
    //  Assignments
    assign output_renable[0]            =   i_renable[0];
    assign output_renable[1]            =   i_renable[1];
    assign output_renable[2]            =   i_renable[2];
    assign output_renable[3]            =   i_renable[3];
    // Assignments for i_kernel_weights_wenable
    assign kernel_weights_wenable[0]    =   i_kernel_weights_wenable[0];
    assign kernel_weights_wenable[1]    =   i_kernel_weights_wenable[1];
    assign kernel_weights_wenable[2]    =   i_kernel_weights_wenable[2];
    assign kernel_weights_wenable[3]    =   i_kernel_weights_wenable[3];
    // Assignments for i_kernel_weights_wenable
    assign kernel_bias_wenable[0]       =   i_kernel_bias_wenable[0];
    assign kernel_bias_wenable[1]       =   i_kernel_bias_wenable[1];
    assign kernel_bias_wenable[2]       =   i_kernel_bias_wenable[2];
    assign kernel_bias_wenable[3]       =   i_kernel_bias_wenable[3];
    // Assignments for i_kernel_weights_wraddress
    genvar i;
    generate
        for (i = 0; i < KERNEL_BRAM_NUM; i = i + 1) begin
            assign kernel_weights_wraddress[i] = i_kernel_weights_wraddress[(i + 1) * KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH - 1 : i * KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH];
            assign kernel_weights_data[i]      = i_kernel_weights_data[(i + 1) * DATA_WIDTH - 1 : i * DATA_WIDTH];
        end
    endgenerate
    // Assignments for i_kernel_bias_wraddress
    generate
        for (i = 0; i < KERNEL_BRAM_NUM; i = i + 1) begin
            assign kernel_bias_wraddress[i] = i_kernel_bias_wraddress[(i + 1) * KERNEL_BIAS_BRAM_ADDRESS_WIDTH - 1 : i * KERNEL_BIAS_BRAM_ADDRESS_WIDTH];
            assign kernel_bias_data[i]      = i_kernel_bias_data[(i + 1) * DATA_WIDTH - 1 : i * DATA_WIDTH];
        end
    endgenerate

    // Assignments for i_output_feature_raddress
    generate
        for (i = 0; i < OUTPUT_BRAM_NUM; i = i + 1) begin
            assign output_feature_raddress[i] = i_output_feature_raddress[(i + 1) * OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH - 1 : i * OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH];
        end
    endgenerate

    // Assignments for o_bram_data
    generate
        for (i = 0; i < OUTPUT_BRAM_NUM; i = i + 1) begin
            assign o_bram_data[(i + 1) * DATA_WIDTH - 1 : i * DATA_WIDTH] = output_bram_data[i];
        end
    endgenerate
    enum {conv_reset, conv_init, conv_wait, conv_update_weights, conv_processing_input} current_state, next_state;
    // temporary assignment
    assign      kernel_element_check    =   (kernel_element_num[0] >= CONV_SIZE) && (kernel_element_num[1] >= CONV_SIZE) && (kernel_element_num[2] >= CONV_SIZE) && (kernel_element_num[3] >= CONV_SIZE);
    assign      input_element_check     =   (input_element_num >= CONV_SIZE);
    assign      input_read_count        =   (!i_reset || (output_feature_col_data_point == OUTPUT_COL_WIDTH'('b0))) ? OUTPUT_COL_WIDTH'('b0) : output_feature_col_data_point - 1;
    assign      output_feature_row_access_data_point    =   (!i_reset || (output_feature_row_data_point == OUTPUT_ROW_WIDTH'('b0)))  ? OUTPUT_ROW_WIDTH'('b0) : output_feature_row_data_point - 1;
    assign      output_feature_col_access_data_point    =   (!i_reset || (output_feature_col_data_point == OUTPUT_COL_WIDTH'('b0)))  ? OUTPUT_COL_WIDTH'('b0) : output_feature_col_data_point - 1;

    // reset busy
    assign      o_input_feature_reset_busy              =   input_mem_busy;
    assign      o_kernel_weights_reset_busy             =   kernel_weights_busy;
    assign      o_kernel_bias_reset_busy                =   kernel_bias_busy;
    assign      o_output_feature_reset_busy             =   output_mem_busy;
    //  snapshot
    assign      o_output_index[27:20]                   =   (!i_reset)  ?   DATA_WIDTH'('b0)   :   output_feature_channel_data_point[3];
    assign      o_output_index[19:12]                   =   (!i_reset)  ?   DATA_WIDTH'('b0)   :   output_feature_channel_data_point[0];
    assign      o_output_index[11:6]                    =   (!i_reset)  ?   DATA_WIDTH'('b0)   :   output_feature_row_access_data_point;
    assign      o_output_index[5:0]                     =   (!i_reset)  ?   DATA_WIDTH'('b0)   :   output_feature_col_access_data_point;
    always_ff @(posedge i_clock, negedge i_reset) begin
        if (!i_reset) begin
            current_state                           <= conv_reset;
            output_feature_col_data_point           <= 0;
            output_feature_col_data_point_prev      <= 0;
            output_feature_row_data_point           <= 0;
            input_feature_channel_data_point_count  <= 0;
            accumulate_enable                       <= 0;
            activate_enable                         <= 0;
            o_processing_done                       <= 0;
            o_row_done                              <= 0;
            for (int i = 0; i < OUTPUT_BRAM_NUM; i++) begin
                output_feature_channel_data_point[i] <= 0;
            end
        end else begin
            if (i_enable) begin
                current_state <= next_state;
                if ((current_state == conv_processing_input && input_valid) || (current_state == conv_update_weights && input_valid && kernel_valid)) begin
                    if (output_feature_col_data_point == output_feature_col) begin
                        output_feature_col_data_point <= OUTPUT_COL_WIDTH'('b1);
                        if (input_feature_channel_data_point_count == input_feature_channel) begin
                            input_feature_channel_data_point_count <= INPUT_CHANNEL_WIDTH'('b1);
                            if (output_feature_channel_data_point[0] == output_feature_end_index_channel - 3 &&
                                output_feature_channel_data_point[1] == output_feature_end_index_channel - 2 &&
                                output_feature_channel_data_point[2] == output_feature_end_index_channel - 1 &&
                                output_feature_channel_data_point[3] == output_feature_end_index_channel) begin
                                output_feature_channel_data_point[0] <= output_feature_start_index_channel;
                                output_feature_channel_data_point[1] <= output_feature_start_index_channel + 1;
                                output_feature_channel_data_point[2] <= output_feature_start_index_channel + 2;
                                output_feature_channel_data_point[3] <= output_feature_start_index_channel + 3;
                                o_row_done                           <=     1;
                                if (output_feature_row_data_point == output_feature_row) begin
                                    output_feature_row_data_point        <= OUTPUT_ROW_WIDTH'('b1);
                                    o_processing_done                    <= 1;
                                end else begin
                                    output_feature_row_data_point <= output_feature_row_data_point + 1;
                                end
                            end else begin
                                output_feature_channel_data_point[0] <= output_feature_channel_data_point[0] + 4;
                                output_feature_channel_data_point[1] <= output_feature_channel_data_point[1] + 4;
                                output_feature_channel_data_point[2] <= output_feature_channel_data_point[2] + 4;
                                output_feature_channel_data_point[3] <= output_feature_channel_data_point[3] + 4;
                                o_row_done                           <=     0;
                            end
                        end else begin
                            input_feature_channel_data_point_count  <= input_feature_channel_data_point_count + 1;
                            o_row_done                              <=     0;
                        end
                    end else begin
                        output_feature_col_data_point <= output_feature_col_data_point + 1;
                        o_row_done                                  <=     0;
                    end
                    output_feature_col_data_point_prev      <=      output_feature_col_data_point;
                end else if (current_state == conv_wait) begin
                    output_feature_col_data_point               <=      output_feature_col_data_point;
                    output_feature_row_data_point               <=      output_feature_row_data_point;
                    input_feature_channel_data_point_count      <=      input_feature_channel_data_point_count;
                    output_feature_channel_data_point           <=      output_feature_channel_data_point;
                    o_row_done                                  <=      o_row_done;
                end
                else if(current_state == conv_init) begin
                    output_feature_channel_data_point[0]    <=      output_feature_start_index_channel;
                    output_feature_channel_data_point[1]    <=      output_feature_start_index_channel + 1;
                    output_feature_channel_data_point[2]    <=      output_feature_start_index_channel + 2;
                    output_feature_channel_data_point[3]    <=      output_feature_start_index_channel + 3;
                    output_feature_col_data_point           <=      OUTPUT_COL_WIDTH'('b1);
                    output_feature_row_data_point           <=      OUTPUT_ROW_WIDTH'('b1);
                    input_feature_channel_data_point_count  <=      INPUT_CHANNEL_WIDTH'('b1);
                    o_row_done                              <=      o_row_done;
                end
                if (input_feature_channel_data_point_count == 1 && input_feature_channel != 1) begin //
                    accumulate_enable   <=  0;
                    activate_enable     <=  0;
                end 
                else if(input_feature_channel_data_point_count == input_feature_channel) begin
                    accumulate_enable   <=  1;
                    activate_enable     <=  1;
                end
                else begin
                    accumulate_enable   <=  1;
                    activate_enable     <=  0;
                end
            end
        end
    end
    always_comb begin
        next_state      =       current_state;
        case (current_state) 
            conv_reset: begin
                input_read_enable                       =      0;
                kernel_weights_read_enable              =      0;
                output_write_enable                     =      0;
                if(i_enable) begin
                    next_state      =      conv_init;
                end
                else begin
                    next_state      =      next_state;
                end
            end
            conv_init: begin
                input_read_enable                       =      0;
                kernel_weights_read_enable              =      0;
                output_write_enable                     =      0;
                if(input_element_check && kernel_element_check) begin
                    next_state      =       conv_update_weights;
                end
                else begin
                    next_state      =       conv_init;
                end
            end
            conv_update_weights: begin
                input_read_enable                       =       1;
                kernel_weights_read_enable              =       1;
                output_write_enable                     =       1;
                if(input_element_check) begin
                    next_state      =       conv_processing_input;
                end
                else begin
                    next_state      =       conv_wait;
                end
            end
            conv_processing_input: begin
                input_read_enable                       =       1;
                kernel_weights_read_enable              =       0;
                output_write_enable                     =       1;
                if(input_element_check && kernel_element_check && output_feature_col_data_point_prev > output_feature_col) begin
                    next_state      =       conv_update_weights;
                end
                else if(input_element_check && output_feature_col_data_point_prev <= output_feature_col) begin
                    next_state      =       conv_processing_input;
                end
                else if(!input_element_check || (!kernel_element_check && output_feature_col_data_point_prev > output_feature_col)) begin
                    next_state      =       conv_wait;
                end
                // else if(!input_element_check && output_feature_col_data_point_prev <= output_feature_col) begin
                //     next_state      =       conv_wait;
                // end
                else begin
                    next_state      =       next_state;
                end
            end
            conv_wait: begin
                input_read_enable                       =       0;
                kernel_weights_read_enable              =       0;
                output_write_enable                     =       0;
                if(input_element_check && kernel_element_check && output_feature_col_data_point_prev >= output_feature_col) begin
                    next_state      =       conv_update_weights;
                end
                else if(input_element_check && output_feature_col_data_point_prev < output_feature_col) begin
                    next_state      =       conv_processing_input;
                end
                else begin
                    next_state      =       conv_wait;
                end
            end
        endcase
    end

    Input_transfer_controller #(
        .INPUT_BRAM_DEPTH(INPUT_BRAM_DEPTH),
        .INPUT_BRAM_ADDRESS_WIDTH(INPUT_BRAM_ADDRESS_WIDTH),
        .KERNEL_ROW_SIZE(KERNEL_ROW_SIZE),
        .KERNEL_COL_SIZE(KERNEL_COL_SIZE),
        .FIFO_DEPTH(INPUT_FIFO_DEPTH),
        .DATA_WIDTH(DATA_WIDTH),
        .INPUT_CHANNEL_WIDTH(INPUT_CHANNEL_WIDTH),
        .INPUT_ROW_WIDTH(INPUT_ROW_WIDTH),
        .INPUT_COL_WIDTH(INPUT_COL_WIDTH),
        .OUTPUT_CHANNEL_WIDTH(OUTPUT_CHANNEL_WIDTH),
        .CONV_SIZE(CONV_SIZE),
        .POINTER_WIDTH(INPUT_POINTER_WIDTH),
        .TOTAL_ELEMENT_WIDTH(INPUT_TOTAL_ELEMENT_WIDTH)
    ) Input_transfer_controller_inst (
        .i_clock(i_clock),
        .i_reset(i_reset),
        .i_enable(i_enable),
        .i_wenable(i_input_feature_wenable),
        .i_renable(input_read_enable),
        .i_start_transfer_process(i_input_start_transfer_process),
        .i_output_col(output_feature_col),
        .i_input_feature_col(input_feature_col),
        .i_input_feature_channel(input_feature_channel),
        .i_waddress(i_input_feature_wraddress),
        .i_bram_data(i_input_feature_data),
        .i_input_read_count(input_read_count),
        .i_output_feature_start_index_channel(output_feature_start_index_channel),
        .i_output_feature_end_index_channel(output_feature_end_index_channel),
        .o_fifo_full(),
        .o_fifo_empty(),
        .o_element_count(input_element_num),
        .o_input_data(input_conv),
        .o_reset_busy(input_mem_busy),
        .o_read_data_valid(input_valid)
    );
    
    Kernel_write_controller #(
        .DATA_WIDTH(DATA_WIDTH),
        .KERNEL_FILTER_WIDTH(KERNEL_FILTER_WIDTH),
        .KERNEL_CHANNEL_WIDTH(KERNEL_CHANNEL_WIDTH),
        .KERNEL_ROW_WIDTH(KERNEL_ROW_WIDTH),
        .KERNEL_COL_WIDTH(KERNEL_COL_WIDTH),
        .FIFO_DEPTH(KERNEL_FIFO_DEPTH),
        .KERNEL_ELEMENT_NUM(CONV_SIZE),
        .KERNEL_BRAM_NUM(KERNEL_BRAM_NUM),
        .KERNEL_BRAM_DEPTH(KERNEL_WEIGHTS_BRAM_DEPTH),
        .KERNEL_BRAM_ADDRESS_WIDTH(KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH),
        .POINTER_WIDTH(KERNEL_POINTER_WIDTH)
    ) Kernel_write_controller_inst (
        .i_clock(i_clock),
        .i_reset(i_reset),
        .i_enable(i_enable),
        .i_renable(kernel_weights_read_enable),
        .i_wenable(kernel_weights_wenable),
        .i_start_transfer_process(i_kernel_weights_start_transfer_process),
        .i_load_new_filter(i_load_new_filter),
        .i_bram_data(kernel_weights_data),
        .i_waddress(kernel_weights_wraddress),
        .i_kernel_filter(kernel_filter),
        .i_kernel_channel(kernel_channel),
        .i_kernel_row(kernel_row),
        .i_kernel_col(kernel_col),
        .i_kernel_start_filter(kernel_start_index_batch_filter),
        .i_kernel_end_filter(kernel_end_index_batch_filter),
        .o_kernel_data(kernel_conv_weights),
        .o_read_data_valid(kernel_valid),
        .o_element_count(kernel_element_num),
        .o_reset_busy(kernel_weights_busy)
    );

    Conv_unit #(
        .CONV_SIZE(CONV_SIZE),
        .KERNEL_BRAM_NUM(KERNEL_BRAM_NUM),
        .DATA_WIDTH(DATA_WIDTH),
        .FRACTION_WIDTH(FRAC_WIDTH)
    ) Conv_unit_inst (
        .i_clock(i_clock),
        .i_reset(i_reset),
        .i_global_enable(i_enable),
        .i_input_feature(input_conv),
        .i_kernel(kernel_conv_weights),
        .o_result(output_conv_result),
        .o_overflow(overflow)
    );

    CNN_kernel_bias_mem #(
        .DATA_WIDTH(DATA_WIDTH),
        .KERNEL_FILTER_WIDTH(KERNEL_FILTER_WIDTH),
        .KERNEL_BRAM_NUM(KERNEL_BRAM_NUM),
        .KERNEL_BIAS_WIDTH(KERNEL_BIAS_WIDTH),
        .KERNEL_BIAS_BRAM_ADDRESS_WIDTH(KERNEL_BIAS_BRAM_ADDRESS_WIDTH)
    ) CNN_kernel_bias_mem_inst (
        .i_clock(i_clock),
        .i_reset(i_reset),
        .i_enable(i_enable),
        .i_wenable(kernel_bias_wenable),
        .i_waddress(kernel_bias_wraddress),
        .i_bram_data(kernel_bias_data),
        .i_kernel_bias_size(output_feature_channel),
        .i_kernel_bias_data_point(output_feature_channel_data_point),
        .o_kernel_bias_data(bias_conv),
        .o_reset_busy(kernel_bias_busy)
    );

    Output_write_controller #(
        .DATA_WIDTH(DATA_WIDTH),
        .FRAC_WIDTH(FRAC_WIDTH),
        .OUTPUT_CHANNEL_WIDTH(OUTPUT_CHANNEL_WIDTH),
        .OUTPUT_ROW_WIDTH(OUTPUT_ROW_WIDTH),
        .OUTPUT_COL_WIDTH(OUTPUT_COL_WIDTH),
        .OUTPUT_BRAM_NUM(OUTPUT_BRAM_NUM),
        .OUTPUT_BRAM_DEPTH(OUTPUT_BRAM_DEPTH),
        .OUTPUT_BRAM_ADDRESS_WIDTH(OUTPUT_BRAM_ADDRESS_WIDTH)
    ) Output_write_controller_inst (
        .i_clock(i_clock),
        .i_reset(i_reset),
        .i_enable(i_enable),
        .i_renable(output_renable),
        .i_wenable(output_write_enable),
        .i_output_feature_channel(output_feature_channel),
        .i_output_feature_col(output_feature_col),
        .i_output_feature_row(output_feature_row),
        .i_output_start_index_channel(output_feature_start_index_channel),
        .i_output_end_index_channel(output_feature_end_index_channel),
        .i_accummulate_enable(accumulate_enable),
        .i_activate_enable(activate_enable),
        .i_conv_result(output_conv_result),
        .i_bias_data(bias_conv),
        .i_write_output_feature_channel_data_point(output_feature_channel_data_point),
        .i_write_output_feature_col_data_point(output_feature_col_access_data_point),
        .i_write_output_feature_row_data_point(output_feature_row_access_data_point),
        .i_raddress(output_feature_raddress),
        .o_bram_data(output_bram_data),
        .o_reset_busy(output_mem_busy)
    );
    
    // 
    assign      input_feature_row                   =      i_input_feature_row;
    assign      input_feature_col                   =      i_input_feature_col;
    assign      input_feature_channel               =      i_input_feature_channel;
    assign      kernel_row                          =      i_kernel_row;
    assign      kernel_col                          =      i_kernel_col;
    assign      kernel_channel                      =      i_kernel_channel;
    assign      kernel_filter                       =      i_kernel_filter;
    assign      kernel_start_index_batch_filter     =      i_kernel_start_index_batch_filter;
    assign      kernel_end_index_batch_filter       =      i_kernel_end_index_batch_filter;
    assign      kernel_bias_size                    =      i_kernel_bias_size;
    assign      output_feature_row                  =      i_output_feature_row;
    assign      output_feature_col                  =      i_output_feature_col;
    assign      output_feature_channel              =      i_output_feature_channel;
    assign      output_feature_start_index_channel  =      i_output_feature_start_index_channel;
    assign      output_feature_end_index_channel    =      i_output_feature_end_index_channel;
    
    
endmodule