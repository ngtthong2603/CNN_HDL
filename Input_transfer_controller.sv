module Input_transfer_controller #(
    parameter       INPUT_BRAM_DEPTH                =       224 * 244,
    parameter       INPUT_BRAM_ADDRESS_WIDTH        =       $clog2(INPUT_BRAM_DEPTH),
    parameter       KERNEL_ROW_SIZE                 =       3,
    parameter       KERNEL_COL_SIZE                 =       3,
    parameter       FIFO_DEPTH                      =       8,
    parameter       DATA_WIDTH                      =       32,
    parameter       INPUT_CHANNEL_WIDTH             =       8,
    parameter       INPUT_ROW_WIDTH                 =       6,
    parameter       INPUT_COL_WIDTH                 =       6,
    parameter       OUTPUT_CHANNEL_WIDTH            =       8,
    parameter       CONV_SIZE                       =       KERNEL_ROW_SIZE * KERNEL_COL_SIZE,
    parameter       POINTER_WIDTH                   =       $clog2(FIFO_DEPTH),
    parameter       TOTAL_ELEMENT_WIDTH             =       $clog2(3 * FIFO_DEPTH)
) (
    input                                                       i_clock,
    input                                                       i_reset,
    input                                                       i_enable,
    input                                                       i_wenable,
    input                                                       i_renable,
    input                                                       i_start_transfer_process,
    input               [INPUT_COL_WIDTH - 1 : 0]               i_output_col,
    input               [INPUT_COL_WIDTH - 1 : 0]               i_input_feature_col,
    input               [INPUT_CHANNEL_WIDTH - 1 : 0]           i_input_feature_channel,
    input               [INPUT_BRAM_ADDRESS_WIDTH - 1 : 0]      i_waddress,
    input               [DATA_WIDTH - 1 : 0]                    i_bram_data,
    input               [INPUT_COL_WIDTH - 1 : 0]               i_input_read_count,
    input               [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_feature_start_index_channel,
    input               [OUTPUT_CHANNEL_WIDTH - 1 : 0]          i_output_feature_end_index_channel,
    output     logic                                            o_fifo_full,
    output     logic                                            o_fifo_empty,
    output     logic    [TOTAL_ELEMENT_WIDTH : 0]               o_element_count,
    output              [DATA_WIDTH - 1 : 0]                    o_input_data    [0 : CONV_SIZE - 1],
    output                                                      o_reset_busy,
    output                                                      o_read_data_valid
);
    localparam          DELAY_DIFFERENT_PORT            =   3;
    localparam          DELAY_SAME_PORT                 =   2;
    localparam          FULL_DELAY_CYCLES               =   2;
    localparam          STOP_DELAY_CYCLES               =   2;

    logic           [INPUT_BRAM_ADDRESS_WIDTH - 1 : 0]      rd_address;
    logic           [INPUT_BRAM_ADDRESS_WIDTH - 1 : 0]      rd_access_address;
    logic           [DATA_WIDTH - 1 : 0]                    transfer_data [0 : KERNEL_ROW_SIZE - 1];
    logic           [DATA_WIDTH - 1 : 0]                    rdata_internal [0 : KERNEL_ROW_SIZE - 1][0 : KERNEL_COL_SIZE - 1];
    logic           [POINTER_WIDTH : 0]                     element_count [0 : KERNEL_ROW_SIZE - 1];
    logic                                                   fifo_wenable;
    logic                                                   ignore_en;
    logic                                                   fifo_full;
    logic                                                   fifo_empty;
    logic           [0:0]                                   read_data_valid [0 : KERNEL_ROW_SIZE - 1];
    logic           [1:0]                                   delay_counter;
    logic           [1:0]                                   transfer_counter;
    logic                                                   almost_full;
    logic           [0:0]                                   fifo_almost_full [0 : KERNEL_ROW_SIZE - 1];
    logic                                                   read_done;
    logic           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          loop_num;
    logic           [OUTPUT_CHANNEL_WIDTH - 1 : 0]          loop_count;


    enum {transfer_reset, transfer_ignore, transfer_active, transfer_wait, transfer_stop_delay, transfer_full_delay} current_state, next_state;

    assign      o_fifo_full         =   fifo_full;
    assign      o_fifo_empty        =   fifo_empty;
    assign      o_element_count     =   element_count[0] + element_count[1] + element_count[2];
    assign      o_read_data_valid   =   read_data_valid[0] || read_data_valid[1] || read_data_valid[2];
    assign      almost_full         =   fifo_almost_full[0] || fifo_almost_full[1] || fifo_almost_full[2];
    assign      rd_access_address   =   (!i_reset || rd_address == INPUT_BRAM_ADDRESS_WIDTH'('b0)) ? INPUT_BRAM_ADDRESS_WIDTH'('b0) : rd_address - 1;
    assign      read_done           =   ((rd_address == i_input_feature_channel * i_input_feature_col) && loop_count == loop_num) ? 1 : 0;
    assign      loop_num            =   (i_output_feature_end_index_channel - i_output_feature_start_index_channel) / 4;

    always_ff @(posedge i_clock, negedge i_reset) begin
        if(!i_reset) begin
            current_state       <=      transfer_reset; 
            rd_address          <=      INPUT_BRAM_ADDRESS_WIDTH'('b0);
            loop_count          <=      OUTPUT_CHANNEL_WIDTH'('b0);
            delay_counter       <=      2'd0;
            transfer_counter    <=      2'd0;
        end
        else begin
            if(i_enable) begin
                current_state   <=  next_state;
                if(next_state == transfer_ignore || next_state == transfer_active) begin
                    if(rd_address == i_input_feature_channel * i_input_feature_col) begin
                        rd_address      <=      INPUT_BRAM_ADDRESS_WIDTH'('b1);
                        if(loop_count == loop_num) begin
                            loop_count          <=      OUTPUT_CHANNEL_WIDTH'('b0);
                        end
                        else begin
                            loop_count          <=      loop_count + 1;
                        end
                    end
                    else begin
                        rd_address      <=      rd_address + 1;
                    end
                end
                if(current_state == transfer_full_delay || current_state == transfer_stop_delay) begin
                    delay_counter       <=   delay_counter + 1;
                end
                else begin
                    delay_counter       <=   2'd0;
                end
                if(current_state == transfer_ignore) begin
                    transfer_counter    <=  transfer_counter + 1;
                end
                else begin
                    transfer_counter    <=  2'd0;
                end
            end
        end
    end

    always_comb begin
        next_state      =       current_state;
        fifo_wenable    =       0;
        ignore_en       =       0;
        case (current_state)
            transfer_reset: begin
                fifo_wenable    =   0;
                ignore_en       =   0;
                if(i_start_transfer_process) begin
                    next_state  =   transfer_ignore;
                end
                else begin
                    next_state  =   next_state;
                end
            end 
            transfer_ignore: begin
                ignore_en       =   1;
                if(ignore_en && transfer_counter == 1) begin
                    next_state  =   transfer_active;
                end
                else if(almost_full) begin
                    next_state  =   transfer_full_delay;
                end
                else begin
                    next_state  =   next_state;
                end
            end
            transfer_active: begin
                fifo_wenable    =   1;
                ignore_en       =   0;
                if(read_done) begin
                    next_state  =   transfer_stop_delay;
                end
                else if(almost_full) begin
                    next_state  =   transfer_full_delay;
                end
                else begin
                    next_state  =   next_state;
                end
            end
            transfer_full_delay: begin
                fifo_wenable    =   1;
                ignore_en       =   0;
                if(delay_counter == FULL_DELAY_CYCLES - 1) begin
                    next_state  =   transfer_wait;
                end
                else begin
                    next_state  =   next_state;
                end
            end
            transfer_stop_delay: begin
                fifo_wenable    =   1;
                ignore_en       =   0;
                if(delay_counter == STOP_DELAY_CYCLES - 1) begin
                    next_state  =   transfer_wait;
                end
                else begin
                    next_state  =   next_state;
                end
            end
            transfer_wait: begin
                fifo_wenable    =   0;
                ignore_en       =   0;
                if(i_start_transfer_process && !almost_full) begin
                    next_state  =   transfer_ignore;
                end
                else begin
                    next_state  =   next_state;
                end
            end
            default: begin
                fifo_wenable    =       fifo_wenable;
                ignore_en       =       ignore_en;
                next_state      =       current_state;
            end
        endcase
    end
    genvar i, j;
    generate
    for(i = 0; i <= KERNEL_ROW_SIZE - 1; i++) begin
        Sub_Input_FIFO #(
            .DATA_WIDTH(DATA_WIDTH),
            .INPUT_CHANNEL_WIDTH(INPUT_CHANNEL_WIDTH),
            .FIFO_DEPTH(FIFO_DEPTH),
            .READ_PORTS(KERNEL_ROW_SIZE),
            .POINTER_WIDTH(POINTER_WIDTH)
        ) Sub_Input_FIFO_inst(
            .i_clock(i_clock),
            .i_reset(i_reset),
            .i_wenable(fifo_wenable),
            .i_wdata(transfer_data[i]),
            .i_renable(i_renable),
            .i_output_size(i_output_col),
            .i_valid_read_count(i_input_read_count),
            .o_rdata(rdata_internal[i]),
            .o_fifo_full(fifo_full),
            .o_fifo_empty(fifo_empty),
            .o_element_count(element_count[i]),
            .o_read_data_valid(read_data_valid[i]),
            .o_fifo_almost_full(fifo_almost_full[i])
        );
    end
    endgenerate
    generate
    for (j = 0; j < CONV_SIZE; j = j + 1) begin : gen_rdata
      assign o_input_data[j] = rdata_internal[j / 3][j % 3]; // Distribute read data to the 9 ports
    end
    endgenerate
    CNN_input_mem #(
        .INPUT_BRAM_DEPTH(INPUT_BRAM_DEPTH),
        .INPUT_BRAM_ADDRESS_WIDTH(INPUT_BRAM_ADDRESS_WIDTH),
        .KERNEL_ROW_SIZE(KERNEL_ROW_SIZE),
        .DATA_WIDTH(DATA_WIDTH)
    ) CNN_input_mem_inst (
        .i_clock(i_clock),
        .i_reset(i_reset),
        .i_enable(1'b1),
        .i_wenable(i_wenable),
        .i_renable(ignore_en || fifo_wenable), //
        .i_waddress(i_waddress),
        .i_raddress(rd_access_address),
        .i_bram_data(i_bram_data),
        .o_bram_data(transfer_data),
        .o_reset_busy(o_reset_busy)
    );
    
endmodule
