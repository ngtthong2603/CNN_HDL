module Kernel_write_controller #(
    parameter   DATA_WIDTH                          = 32,
    parameter   KERNEL_FILTER_WIDTH                 = 8,
    parameter   KERNEL_CHANNEL_WIDTH                = 8,
    parameter   KERNEL_ROW_WIDTH                    = 2,
    parameter   KERNEL_COL_WIDTH                    = 2,
    parameter   FIFO_DEPTH                          = 16,
    parameter   KERNEL_ELEMENT_NUM                  = 9,
    parameter   KERNEL_BRAM_NUM                     = 4,
    parameter   KERNEL_BRAM_DEPTH                   = 1152,
    parameter   KERNEL_BRAM_ADDRESS_WIDTH           = $clog2(KERNEL_BRAM_DEPTH),
    parameter   POINTER_WIDTH                       = $clog2(FIFO_DEPTH)
) (
    input                                                   i_clock,
    input                                                   i_reset,
    input                                                   i_enable,
    input                                                   i_renable,
    input           [0:0]                                   i_wenable [0 : KERNEL_BRAM_NUM - 1],
    input                                                   i_start_transfer_process,
    input                                                   i_load_new_filter,
    input           [DATA_WIDTH - 1 : 0]                    i_bram_data [0 : KERNEL_BRAM_NUM - 1],
    input           [KERNEL_BRAM_ADDRESS_WIDTH - 1 : 0]     i_waddress  [0 : KERNEL_BRAM_NUM - 1],
    input           [KERNEL_FILTER_WIDTH - 1 : 0]           i_kernel_filter,
    input           [KERNEL_CHANNEL_WIDTH - 1 : 0]          i_kernel_channel,
    input           [KERNEL_ROW_WIDTH - 1 : 0]              i_kernel_row,
    input           [KERNEL_COL_WIDTH - 1 : 0]              i_kernel_col,
    input           [KERNEL_FILTER_WIDTH - 1 : 0]           i_kernel_start_filter,
    input           [KERNEL_FILTER_WIDTH - 1 : 0]           i_kernel_end_filter,
    output          [DATA_WIDTH - 1 : 0]                    o_kernel_data   [0 : KERNEL_BRAM_NUM - 1][0 : KERNEL_ELEMENT_NUM - 1],
    output   logic                                          o_read_data_valid,
    output   logic  [POINTER_WIDTH : 0]                     o_element_count [0 : KERNEL_BRAM_NUM - 1],
    output                                                  o_reset_busy
);
    //  Kernel data point
    logic           [KERNEL_FILTER_WIDTH - 1 : 0]           kernel_filter_data_point [0 : KERNEL_BRAM_NUM - 1];
    logic           [KERNEL_CHANNEL_WIDTH - 1 : 0]          kernel_channel_data_point;
    logic           [KERNEL_ROW_WIDTH - 1 : 0]              kernel_row_data_point;
    logic           [KERNEL_COL_WIDTH - 1 : 0]              kernel_col_data_point;
    // temp
    logic           [KERNEL_CHANNEL_WIDTH - 1 : 0]          kernel_channel_access_data_point;
    logic           [KERNEL_ROW_WIDTH - 1 : 0]              kernel_row_access_data_point;
    logic           [KERNEL_COL_WIDTH - 1 : 0]              kernel_col_access_data_point;

    logic           [DATA_WIDTH - 1 : 0]                    transfer_data[0 : KERNEL_BRAM_NUM - 1];
    logic           [0:0]                                   fifo_full[0 : KERNEL_BRAM_NUM - 1];
    logic           [0:0]                                   fifo_empty[0 : KERNEL_BRAM_NUM - 1];
    logic                                                   internal_filter_done;
    logic           [0:0]                                   read_data_valid [0 : KERNEL_BRAM_NUM - 1];
    // FSM state variables
    typedef enum {ctrler_reset, ctrler_init, ctrler_read_delay, ctrler_stop_write_delay, ctrler_full_write_delay, ctrler_write, ctrler_wait} state_t;
    state_t current_state, next_state;
    logic                                                   fifo_wenable;
    logic                                                   bram_renable;
    logic                               [2:0]               rd_delay_count;
    logic                               [2:0]               wr_delay_count;
    logic                               [0:0]               fifo_almost_full [0 : KERNEL_BRAM_NUM - 1];
    logic                                                   almost_full;
    logic                               [0:0]               reset_busy [0 : KERNEL_BRAM_NUM - 1];

    assign  almost_full     =   fifo_almost_full[0] || fifo_almost_full[1] || fifo_almost_full[2] || fifo_almost_full[3];
    assign  o_reset_busy    =   reset_busy[0] || reset_busy[1] || reset_busy[2] || reset_busy[3];
    assign  kernel_channel_access_data_point    =   (!i_reset || kernel_channel_data_point  == KERNEL_CHANNEL_WIDTH'('b0))  ? KERNEL_CHANNEL_WIDTH'('b0) : kernel_channel_data_point - 1;
    assign  kernel_row_access_data_point        =   (!i_reset || kernel_row_data_point == KERNEL_ROW_WIDTH'('b0))  ? KERNEL_ROW_WIDTH'('b0) : kernel_row_data_point - 1;
    assign  kernel_col_access_data_point        =   (!i_reset || kernel_col_data_point == KERNEL_COL_WIDTH'('b0))  ? KERNEL_COL_WIDTH'('b0) : kernel_col_data_point - 1;
    assign  o_read_data_valid                   =   read_data_valid[0] && read_data_valid[1] && read_data_valid[2] && read_data_valid[3];

    always_ff @(posedge i_clock or negedge i_reset) begin 
        if(!i_reset) begin
            current_state <= ctrler_reset;
            kernel_col_data_point <= '0;
            kernel_row_data_point <= '0;
            kernel_channel_data_point <= '0;
            rd_delay_count  <=  '0;
            wr_delay_count  <=  '0;
            internal_filter_done <= 0;
            for (int i = 0; i < KERNEL_BRAM_NUM; i++) begin
                kernel_filter_data_point[i] <= KERNEL_FILTER_WIDTH'('b0);
            end
        end else begin
            if(i_enable) begin
                current_state <= next_state;

                if (next_state == ctrler_read_delay || next_state == ctrler_write) begin
                    // Increment kernel_col_data_point
                    if (kernel_col_data_point == i_kernel_col) begin
                        kernel_col_data_point <= KERNEL_COL_WIDTH'('b1);
                        // Increment kernel_row_data_point when kernel_col_data_point wraps around
                        if (kernel_row_data_point == i_kernel_row) begin
                            kernel_row_data_point <= KERNEL_ROW_WIDTH'('b1);
                            // Increment kernel_channel_data_point when kernel_row_data_point wraps around
                            if (kernel_channel_data_point == i_kernel_channel) begin
                                kernel_channel_data_point <= KERNEL_CHANNEL_WIDTH'('b1);
                                // Increment kernel_filter_data_point when kernel_channel_data_point wraps around
                                if(kernel_filter_data_point[0] == i_kernel_end_filter - 3 
                                && kernel_filter_data_point[1] == i_kernel_end_filter - 2 
                                && kernel_filter_data_point[2] == i_kernel_end_filter - 1
                                && kernel_filter_data_point[3] == i_kernel_end_filter) begin
                                    internal_filter_done <= 1;
                                end
                                else begin
                                    for (int i = 0; i < KERNEL_BRAM_NUM; i++) begin
                                        if (kernel_filter_data_point[i] < i_kernel_end_filter - 3 + i) begin
                                            kernel_filter_data_point[i] <= kernel_filter_data_point[i] + 4;
                                        end
                                    end
                                end
                            end else begin
                                kernel_channel_data_point <= kernel_channel_data_point + 1;
                            end
                        end else begin
                            kernel_row_data_point <= kernel_row_data_point + 1;
                        end
                    end else begin
                        kernel_col_data_point <= kernel_col_data_point + 1;
                        internal_filter_done <= 0;
                    end
                end
                else if(next_state == ctrler_full_write_delay || next_state == ctrler_stop_write_delay || next_state == ctrler_wait || internal_filter_done && bram_renable) begin
                    kernel_filter_data_point    <=      kernel_filter_data_point;
                    kernel_channel_data_point   <=      kernel_channel_data_point;
                    kernel_row_data_point       <=      kernel_row_data_point;
                    kernel_col_data_point       <=      kernel_col_data_point;
                end
                else if(next_state == ctrler_init) begin
                    kernel_filter_data_point[0] <=      i_kernel_start_filter;
                    kernel_filter_data_point[1] <=      i_kernel_start_filter + 1;
                    kernel_filter_data_point[2] <=      i_kernel_start_filter + 2;
                    kernel_filter_data_point[3] <=      i_kernel_start_filter + 3;
                    kernel_channel_data_point   <=      KERNEL_CHANNEL_WIDTH'('b1);
                    kernel_row_data_point       <=      KERNEL_ROW_WIDTH'('b1);
                    kernel_col_data_point       <=      KERNEL_COL_WIDTH'('b0);
                end
                if(next_state == ctrler_read_delay) begin
                    rd_delay_count <= rd_delay_count + 1;
                end
                else begin
                    rd_delay_count <= '0;
                end
                if(next_state == ctrler_full_write_delay || next_state == ctrler_stop_write_delay) begin
                    wr_delay_count <= wr_delay_count + 1;
                end
                else begin
                    wr_delay_count <= '0;
                end
            end
        end
    end

    always_comb begin
        next_state = current_state;
        fifo_wenable = 0;
        
        case (current_state)
            ctrler_reset: begin
                fifo_wenable    =   0;
                bram_renable    =   0;
                if (i_enable) begin
                    next_state = ctrler_init;
                end
            end
            ctrler_init: begin
                fifo_wenable    =   0;
                if (i_start_transfer_process && !(almost_full)) begin
                    next_state = ctrler_read_delay;
                end
            end
            ctrler_read_delay: begin
                fifo_wenable    =   0;
                bram_renable    =   1;
                if(rd_delay_count == 2) begin
                    next_state  =   ctrler_write;
                end
            end
            ctrler_write: begin
                fifo_wenable    =   1;
                bram_renable    =   1;
                if (almost_full) begin
                    next_state = ctrler_full_write_delay;
                end
                else if(!i_start_transfer_process || internal_filter_done) begin
                    next_state = ctrler_stop_write_delay;
                end
            end
            ctrler_full_write_delay: begin
                fifo_wenable    =   1;
                bram_renable    =   0;
                if(almost_full && wr_delay_count == 3) begin
                    next_state  =   ctrler_wait;
                end
                else if((i_start_transfer_process || internal_filter_done) && wr_delay_count == 3) begin
                    next_state  =   ctrler_read_delay;
                end
            end
            ctrler_stop_write_delay: begin
                fifo_wenable    =   1;
                bram_renable    =   0;
                if(wr_delay_count == 1) begin
                    next_state  =   ctrler_wait;
                end
                else if((i_start_transfer_process || internal_filter_done) && wr_delay_count == 1) begin
                    next_state  =   ctrler_read_delay;
                end
            end
            ctrler_wait: begin
                fifo_wenable    =   0;
                bram_renable    =   0;
                if (i_start_transfer_process && !(almost_full) && !internal_filter_done) begin
                    next_state = ctrler_read_delay;
                end else if (i_enable && i_load_new_filter) begin
                    next_state = ctrler_init;
                end
            end

            default: begin
                for (int i = 0; i < KERNEL_BRAM_NUM; i++) begin
                    kernel_filter_data_point[i] = kernel_filter_data_point[i];
                end
                kernel_channel_data_point = kernel_channel_data_point;
                kernel_row_data_point = kernel_row_data_point;
                kernel_col_data_point = kernel_col_data_point;
            end
        endcase
    end

    genvar i;
    generate
        for(i = 0; i < KERNEL_BRAM_NUM; i++) begin
            Kernel_Conv_FIFO #(
              .DATA_WIDTH(DATA_WIDTH),
              .INPUT_CHANNEL_WIDTH(KERNEL_CHANNEL_WIDTH),
              .FIFO_DEPTH(FIFO_DEPTH),
              .READ_PORTS(KERNEL_ELEMENT_NUM),
              .POINTER_WIDTH(POINTER_WIDTH)
            ) Kernel_Conv_FIFO_inst (
              .i_clock(i_clock),
              .i_reset(i_reset),
              .i_wenable(fifo_wenable),
              .i_wdata(transfer_data[i]),
              .i_renable(i_renable),
              .i_input_feature_channel(i_kernel_channel),                        
              .o_rdata(o_kernel_data[i]),
              .o_fifo_full(fifo_full[i]),
              .o_fifo_empty(fifo_empty[i]),
              .o_read_data_valid(read_data_valid[i]),
              .o_element_count(o_element_count[i]),
              .o_fifo_almost_full(fifo_almost_full[i])
            );

            CNN_kernel_weights_mem #(
                .KERNEL_BRAM_NUM(KERNEL_BRAM_NUM),
                .KERNEL_BRAM_DEPTH(KERNEL_BRAM_DEPTH),
                .KERNEL_BRAM_ADDRESS_WIDTH(KERNEL_BRAM_ADDRESS_WIDTH),
                .DATA_WIDTH(DATA_WIDTH),
                .KERNEL_FILTER_WIDTH(KERNEL_FILTER_WIDTH),
                .KERNEL_CHANNEL_WIDTH(KERNEL_CHANNEL_WIDTH),
                .KERNEL_ROW_WIDTH(KERNEL_ROW_WIDTH),
                .KERNEL_COL_WIDTH(KERNEL_COL_WIDTH)
            ) CNN_kernel_mem_inst (
                .i_clock(i_clock),
                .i_reset(i_reset),
                .i_enable(i_enable),
                .i_renable(bram_renable || fifo_wenable),
                .i_wenable(i_wenable[i]),
                .i_waddress(i_waddress[i]),
                .i_bram_data(i_bram_data[i]),
                .i_kernel_filter(i_kernel_filter),
                .i_kernel_channel(i_kernel_channel),
                .i_kernel_row(i_kernel_row),
                .i_kernel_col(i_kernel_col),
                .i_kernel_start_filter(i_kernel_start_filter),
                .i_kernel_end_filter(i_kernel_end_filter),
                .i_kernel_filter_data_point(kernel_filter_data_point[i]),
                .i_kernel_channel_data_point(kernel_channel_access_data_point),
                .i_kernel_row_data_point(kernel_row_access_data_point),
                .i_kernel_col_data_point(kernel_col_access_data_point),
                .o_bram_data(transfer_data[i]),
                .o_reset_busy(reset_busy[i])
            );
        end
    endgenerate

endmodule
