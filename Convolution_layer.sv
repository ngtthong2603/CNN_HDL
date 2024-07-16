module Convolution_layer (
    input wire clk,
    input wire resetn,

    // AXI4 Full Master Interface
    input wire [31:0] m_axi_awaddr,
    input wire [7:0] m_axi_awlen,
    input wire [2:0] m_axi_awsize,
    input wire [1:0] m_axi_awburst,
    input wire m_axi_awvalid,
    output wire m_axi_awready,
    input wire [31:0] m_axi_wdata,
    input wire [3:0] m_axi_wstrb,
    input wire m_axi_wlast,
    input wire m_axi_wvalid,
    output wire m_axi_wready,
    output wire [1:0] m_axi_bresp,
    output wire m_axi_bvalid,
    input wire m_axi_bready,
    input wire [31:0] m_axi_araddr,
    input wire [7:0] m_axi_arlen,
    input wire [2:0] m_axi_arsize,
    input wire [1:0] m_axi_arburst,
    input wire m_axi_arvalid,
    output wire m_axi_arready,
    output wire [31:0] m_axi_rdata,
    output wire [1:0] m_axi_rresp,
    output wire m_axi_rlast,
    output wire m_axi_rvalid,
    input wire m_axi_rready,

    // BRAM interface to your core
    output wire [31:0] bram_addr,
    output wire [31:0] bram_din,
    input wire [31:0] bram_dout,
    output wire bram_we,
    output wire bram_en
);
    // Internal signals for AXI
    reg [31:0] axi_rdata;
    reg axi_rvalid, axi_bvalid;
    reg [31:0] mem [0:255]; // Example BRAM memory
    reg [7:0] burst_counter;

    // AXI Write Address Channel
    assign m_axi_awready = !m_axi_awvalid || m_axi_bready;
    assign m_axi_wready = m_axi_awready;

    // AXI Read Address Channel
    assign m_axi_arready = !m_axi_arvalid || m_axi_rready;

    // Write Logic
    always @(posedge clk) begin
        if (!resetn) begin
            axi_bvalid <= 0;
            burst_counter <= 0;
        end else if (m_axi_wvalid && m_axi_awvalid && m_axi_wready && m_axi_awready) begin
            mem[m_axi_awaddr[9:2] + burst_counter] <= m_axi_wdata;
            burst_counter <= burst_counter + 1;
            if (m_axi_wlast) begin
                axi_bvalid <= 1;
                burst_counter <= 0;
            end
        end else if (m_axi_bready) begin
            axi_bvalid <= 0;
        end
    end
    assign m_axi_bvalid = axi_bvalid;
    assign m_axi_bresp = 2'b00; // OKAY response

    // Read Logic
    always @(posedge clk) begin
        if (!resetn) begin
            axi_rvalid <= 0;
            axi_rdata <= 0;
            burst_counter <= 0;
        end else if (m_axi_arvalid && m_axi_arready) begin
            axi_rdata <= mem[m_axi_araddr[9:2] + burst_counter];
            burst_counter <= burst_counter + 1;
            axi_rvalid <= 1;
            if (burst_counter == m_axi_arlen) begin
                axi_rvalid <= 1;
                burst_counter <= 0;
            end
        end else if (m_axi_rready) begin
            axi_rvalid <= 0;
        end
    end
    assign m_axi_rvalid = axi_rvalid;
    assign m_axi_rdata = axi_rdata;
    assign m_axi_rresp = 2'b00; // OKAY response
    assign m_axi_rlast = (burst_counter == m_axi_arlen);

    // BRAM interface logic
    assign bram_addr = m_axi_awaddr;
    assign bram_din = m_axi_wdata;
    assign bram_we = m_axi_wvalid && m_axi_awvalid && m_axi_wready && m_axi_awready;
    assign bram_en = m_axi_awvalid || m_axi_arvalid;

    Conv_controller #(
    // Data type parameters
    parameter   DATA_WIDTH                          =       32,
    parameter   FRAC_WIDTH                          =       16,
    parameter   REGISTER_BANK_ADDRESS_WIDTH         =       2,
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
    parameter   OUTPUT_BRAM_DEPTH                   =       32*32

) (
    input                                                       i_clock,
    input                                                       i_reset,
    input                                                       i_enable,
    input     [0:0]                                             i_renable [0 : OUTPUT_BRAM_NUM - 1],
    input                                                       i_input_feature_wenable,
    input     [0:0]                                             i_kernel_weights_wenable [0 : KERNEL_BRAM_NUM - 1],
    input     [0:0]                                             i_kernel_bias_wenable [0 : KERNEL_BRAM_NUM - 1],
    input                                                       i_register_bank_wenable,
    input                                                       i_input_start_transfer_process,
    input                                                       i_kernel_weights_start_transfer_process,
    input                                                       i_load_new_filter,
    input     [INPUT_BRAM_ADDRESS_WIDTH - 1 : 0]                i_input_feature_wraddress,
    input     [KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH - 1 : 0]       i_kernel_weights_wraddress,
    input     [KERNEL_BIAS_BRAM_ADDRESS_WIDTH - 1 : 0]          i_kernel_bias_wraddress,
    input     [REGISTER_BANK_ADDRESS_WIDTH - 1 : 0]             i_register_bank_wraddress,
    input     [DATA_WIDTH - 1 : 0]                              i_input_feature_data,
    input     [DATA_WIDTH - 1 : 0]                              i_kernel_weights_data,
    input     [DATA_WIDTH - 1 : 0]                              i_kernel_bias_data,
    input     [DATA_WIDTH - 1 : 0]                              i_register_bank_data,
    output    logic                                             o_reset_busy,
    output    [DATA_WIDTH - 1 : 0]                              o_bram_data [0 : OUTPUT_BRAM_NUM - 1]
);
endmodule
