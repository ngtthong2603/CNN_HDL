module Convolutional_controller #(
    localparam integer C_S_AXI_DATA_WIDTH   = 32,
    localparam integer C_S_AXI_ADDR_WIDTH	= 5,
    // Data type parameters
    parameter   DATA_WIDTH                          =       32,
    parameter   FRACTION_WIDTH                      =       16,
    // Kernel parameters
    parameter   KERNEL_FILTER_WIDTH                 =       8,
    parameter   KERNEL_CHANNEL_WIDTH                =       8,
    parameter   KERNEL_ROW_WIDTH                    =       2,
    parameter   KERNEL_COL_WIDTH                    =       2,
    parameter   KERNEL_WIDTH                        =       KERNEL_COL_WIDTH,
    parameter   KERNEL_BRAM_NUM                     =       4,
    parameter   KERNEL_WEIGHTS_BRAM_DEPTH           =       1152,
    parameter   KERNEL_BIAS_BRAM_DEPTH              =       32,
    parameter   KERNEL_BIAS_WIDTH                   =       KERNEL_FILTER_WIDTH,
    parameter   KERNEL_FIFO_DEPTH                   =       32,
    parameter   KERNEL_POINTER_WIDTH                =       $clog2(KERNEL_FIFO_DEPTH),
    parameter   KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH   =       $clog2(KERNEL_WEIGHTS_BRAM_DEPTH),
    parameter   KERNEL_BIAS_BRAM_ADDRESS_WIDTH      =       $clog2(KERNEL_BIAS_BRAM_DEPTH),
    // Input parameters
    parameter   INPUT_CHANNEL_WIDTH                 =       8,
    parameter   INPUT_ROW_WIDTH                     =       6,
    parameter   INPUT_COL_WIDTH                     =       6,
    parameter   INPUT_BRAM_DEPTH                    =       3072,
    parameter   INPUT_FEATURE_BRAM_ADDRESS_WIDTH    =       $clog2(INPUT_BRAM_DEPTH),
    parameter   INPUT_FIFO_DEPTH                    =       32,
    parameter   INPUT_POINTER_WIDTH                 =       $clog2(INPUT_FIFO_DEPTH),
    parameter   INPUT_TOTAL_ELEMENT_WIDTH           =       $clog2(3*INPUT_FIFO_DEPTH),
    parameter   INPUT_WIDTH                         =       INPUT_COL_WIDTH,
    // Output parameters
    parameter   OUTPUT_CHANNEL_WIDTH                =       KERNEL_FILTER_WIDTH,
    parameter   OUTPUT_COL_WIDTH                    =       INPUT_COL_WIDTH,
    parameter   OUTPUT_ROW_WIDTH                    =       INPUT_ROW_WIDTH,
    parameter   OUTPUT_BRAM_NUM                     =       KERNEL_BRAM_NUM,
    parameter   OUTPUT_BRAM_DEPTH                   =       4608,
    parameter   OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH   =       $clog2(OUTPUT_BRAM_DEPTH)
)(
    input           i_clock,
    input           i_reset_n,
    // INPUT FEATURE INTERFACE----------------------------------------------------------------------
    (* X_INTERFACE_PARAMETER = "MASTER_TYPE BRAM_CTRL,MEM_ECC NONE,MEM_WIDTH 32,MEM_SIZE 4096,READ_WRITE_MODE READ_WRITE" *) 
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 INPUT_FEATURE_BRAM_PORT_0 EN" *)
    input                                               input_feature_bram_en_0, // Chip Enable Signal (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 INPUT_FEATURE_BRAM_PORT_0 DOUT" *)
    output [31 : 0]                                     input_feature_bram_dout_0, // Data Out Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 INPUT_FEATURE_BRAM_PORT_0 DIN" *)
    input [31 : 0]                                      input_feature_bram_din_0, // Data In Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 INPUT_FEATURE_BRAM_PORT_0 WE" *)
    input [3 : 0]                                       input_feature_bram_we_0, // Byte Enables (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 INPUT_FEATURE_BRAM_PORT_0 ADDR" *)
    input [INPUT_FEATURE_BRAM_ADDRESS_WIDTH - 1 : 0]    input_feature_bram_addr_0, // Address Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 INPUT_FEATURE_BRAM_PORT_0 CLK" *)
    input                                               input_feature_bram_clk_0, // Clock Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 INPUT_FEATURE_BRAM_PORT_0 RST" *)
    input                                               input_feature_bram_rst_0, // Reset Signal (required)
    // KERNEL BIAS INTERFACE----------------------------------------------------------------------
    //BRAM 0
    (* X_INTERFACE_PARAMETER = "MASTER_TYPE BRAM_CTRL,MEM_ECC NONE,MEM_WIDTH 32,MEM_SIZE 4096,READ_WRITE_MODE READ_WRITE" *) 
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_0 EN" *)
    input                                               kernel_bias_bram_en_0, // Chip Enable Signal (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_0 DOUT" *)
    output [31 : 0]                                     kernel_bias_bram_dout_0, // Data Out Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_0 DIN" *)
    input [31 : 0]                                      kernel_bias_bram_din_0, // Data In Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_0 WE" *)
    input [3 : 0]                                       kernel_bias_bram_we_0, // Byte Enables (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_0 ADDR" *)
    input [KERNEL_BIAS_BRAM_ADDRESS_WIDTH - 1 : 0]      kernel_bias_bram_addr_0, // Address Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_0 CLK" *)
    input                                               kernel_bias_bram_clk_0, // Clock Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_0 RST" *)
    input                                               kernel_bias_bram_rst_0, // Reset Signal (required)
    // BRAM 1
    (* X_INTERFACE_PARAMETER = "MASTER_TYPE BRAM_CTRL,MEM_ECC NONE,MEM_WIDTH 32,MEM_SIZE 4096,READ_WRITE_MODE READ_WRITE" *) 
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_1 EN" *)
    input                                               kernel_bias_bram_en_1, // Chip Enable Signal (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_1 DOUT" *)
    output [31 : 0]                                     kernel_bias_bram_dout_1, // Data Out Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_1 DIN" *)
    input [31 : 0]                                      kernel_bias_bram_din_1, // Data In Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_1 WE" *)
    input [3 : 0]                                       kernel_bias_bram_we_1, // Byte Enables (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_1 ADDR" *)
    input [KERNEL_BIAS_BRAM_ADDRESS_WIDTH - 1 : 0]      kernel_bias_bram_addr_1, // Address Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_1 CLK" *)
    input                                               kernel_bias_bram_clk_1, // Clock Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_1 RST" *)
    input                                               kernel_bias_bram_rst_1, // Reset Signal (required)
    // BRAM 2
    (* X_INTERFACE_PARAMETER = "MASTER_TYPE BRAM_CTRL,MEM_ECC NONE,MEM_WIDTH 32,MEM_SIZE 4096,READ_WRITE_MODE READ_WRITE" *) 
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_2 EN" *)
    input                                               kernel_bias_bram_en_2, // Chip Enable Signal (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_2 DOUT" *)
    output [31 : 0]                                     kernel_bias_bram_dout_2, // Data Out Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_2 DIN" *)
    input [31 : 0]                                      kernel_bias_bram_din_2, // Data In Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_2 WE" *)
    input [3 : 0]                                       kernel_bias_bram_we_2, // Byte Enables (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_2 ADDR" *)
    input [KERNEL_BIAS_BRAM_ADDRESS_WIDTH - 1 : 0]      kernel_bias_bram_addr_2, // Address Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_2 CLK" *)
    input                                               kernel_bias_bram_clk_2, // Clock Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_2 RST" *)
    input                                               kernel_bias_bram_rst_2, // Reset Signal (required)
    // BRAM 3
    (* X_INTERFACE_PARAMETER = "MASTER_TYPE BRAM_CTRL,MEM_ECC NONE,MEM_WIDTH 32,MEM_SIZE 4096,READ_WRITE_MODE READ_WRITE" *) 
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_3 EN" *)
    input                                               kernel_bias_bram_en_3, // Chip Enable Signal (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_3 DOUT" *)
    output [31 : 0]                                     kernel_bias_bram_dout_3, // Data Out Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_3 DIN" *)
    input [31 : 0]                                      kernel_bias_bram_din_3, // Data In Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_3 WE" *)
    input [3 : 0]                                       kernel_bias_bram_we_3, // Byte Enables (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_3 ADDR" *)
    input [KERNEL_BIAS_BRAM_ADDRESS_WIDTH - 1 : 0]      kernel_bias_bram_addr_3, // Address Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_3 CLK" *)
    input                                               kernel_bias_bram_clk_3, // Clock Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_BIAS_BRAM_PORT_3 RST" *)
    input                                               kernel_bias_bram_rst_3, // Reset Signal (required)
    // KERNEL WEIGHTS INTERFACE----------------------------------------------------------------------
    //BRAM 0
    (* X_INTERFACE_PARAMETER = "MASTER_TYPE BRAM_CTRL,MEM_ECC NONE,MEM_WIDTH 32,MEM_SIZE 8192,READ_WRITE_MODE READ_WRITE" *) 
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_0 EN" *)
    input                                               kernel_weights_bram_en_0, // Chip Enable Signal (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_0 DOUT" *)
    output [31 : 0]                                     kernel_weights_bram_dout_0, // Data Out Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_0 DIN" *)
    input [31 : 0]                                      kernel_weights_bram_din_0, // Data In Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_0 WE" *)
    input [3 : 0]                                       kernel_weights_bram_we_0, // Byte Enables (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_0 ADDR" *)
    input [KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH - 1 : 0]   kernel_weights_bram_addr_0, // Address Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_0 CLK" *)
    input                                               kernel_weights_bram_clk_0, // Clock Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_0 RST" *)
    input                                               kernel_weights_bram_rst_0, // Reset Signal (required)
    // BRAM 1
    (* X_INTERFACE_PARAMETER = "MASTER_TYPE BRAM_CTRL,MEM_ECC NONE,MEM_WIDTH 32,MEM_SIZE 8192,READ_WRITE_MODE READ_WRITE" *) 
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_1 EN" *)
    input                                               kernel_weights_bram_en_1, // Chip Enable Signal (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_1 DOUT" *)
    output [31 : 0]                                     kernel_weights_bram_dout_1, // Data Out Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_1 DIN" *)
    input [31 : 0]                                      kernel_weights_bram_din_1, // Data In Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_1 WE" *)
    input [3 : 0]                                       kernel_weights_bram_we_1, // Byte Enables (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_1 ADDR" *)
    input [KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH - 1 : 0]   kernel_weights_bram_addr_1, // Address Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_1 CLK" *)
    input                                               kernel_weights_bram_clk_1, // Clock Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_1 RST" *)
    input                                               kernel_weights_bram_rst_1, // Reset Signal (required)
    // BRAM 2
    (* X_INTERFACE_PARAMETER = "MASTER_TYPE BRAM_CTRL,MEM_ECC NONE,MEM_WIDTH 32,MEM_SIZE 8192,READ_WRITE_MODE READ_WRITE" *) 
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_2 EN" *)
    input                                               kernel_weights_bram_en_2, // Chip Enable Signal (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_2 DOUT" *)
    output [31 : 0]                                     kernel_weights_bram_dout_2, // Data Out Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_2 DIN" *)
    input [31 : 0]                                      kernel_weights_bram_din_2, // Data In Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_2 WE" *)
    input [3 : 0]                                       kernel_weights_bram_we_2, // Byte Enables (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_2 ADDR" *)
    input [KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH - 1 : 0]   kernel_weights_bram_addr_2, // Address Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_2 CLK" *)
    input                                               kernel_weights_bram_clk_2, // Clock Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_2 RST" *)
    input                                               kernel_weights_bram_rst_2, // Reset Signal (required)
    // BRAM 3
    (* X_INTERFACE_PARAMETER = "MASTER_TYPE BRAM_CTRL,MEM_ECC NONE,MEM_WIDTH 32,MEM_SIZE 8192,READ_WRITE_MODE READ_WRITE" *) 
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_3 EN" *)
    input                                               kernel_weights_bram_en_3, // Chip Enable Signal (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_3 DOUT" *)
    output [31 : 0]                                     kernel_weights_bram_dout_3, // Data Out Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_3 DIN" *)
    input [31 : 0]                                      kernel_weights_bram_din_3, // Data In Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_3 WE" *)
    input [3 : 0]                                       kernel_weights_bram_we_3, // Byte Enables (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_3 ADDR" *)
    input [KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH - 1 : 0]   kernel_weights_bram_addr_3, // Address Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_3 CLK" *)
    input                                               kernel_weights_bram_clk_3, // Clock Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 KERNEL_WEIGHTS_BRAM_PORT_3 RST" *)
    input                                               kernel_weights_bram_rst_3, // Reset Signal (required)
    // OUTPUT FEATURE INTERFACE----------------------------------------------------------------------
    // BRAM 0
    (* X_INTERFACE_PARAMETER = "MASTER_TYPE BRAM_CTRL,MEM_ECC NONE,MEM_WIDTH 32,MEM_SIZE 4096,READ_WRITE_MODE READ_WRITE" *) 
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_0 EN" *)
    input                                               output_feature_bram_en_0, // Chip Enable Signal (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_0 DOUT" *)
    output [31 : 0]                                     output_feature_bram_dout_0, // Data Out Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_0 DIN" *)
    input [31 : 0]                                      output_feature_bram_din_0, // Data In Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_0 WE" *)
    input [3 : 0]                                       output_feature_bram_we_0, // Byte Enables (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_0 ADDR" *)
    input [OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH - 1 : 0]   output_feature_bram_addr_0, // Address Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_0 CLK" *)
    input                                               output_feature_bram_clk_0, // Clock Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_0 RST" *)
    input                                               output_feature_bram_rst_0, // Reset Signal (required)
    // BRAM 1
    (* X_INTERFACE_PARAMETER = "MASTER_TYPE BRAM_CTRL,MEM_ECC NONE,MEM_WIDTH 32,MEM_SIZE 4096,READ_WRITE_MODE READ_WRITE" *) 
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_1 EN" *)
    input                                               output_feature_bram_en_1, // Chip Enable Signal (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_1 DOUT" *)
    output [31 : 0]                                     output_feature_bram_dout_1, // Data Out Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_1 DIN" *)
    input [31 : 0]                                      output_feature_bram_din_1, // Data In Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_1 WE" *)
    input [3 : 0]                                       output_feature_bram_we_1, // Byte Enables (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_1 ADDR" *)
    input [OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH - 1 : 0]   output_feature_bram_addr_1, // Address Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_1 CLK" *)
    input                                               output_feature_bram_clk_1, // Clock Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_1 RST" *)
    input                                               output_feature_bram_rst_1, // Reset Signal (required)
    // BRAM 2
    (* X_INTERFACE_PARAMETER = "MASTER_TYPE BRAM_CTRL,MEM_ECC NONE,MEM_WIDTH 32,MEM_SIZE 4096,READ_WRITE_MODE READ_WRITE" *) 
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_2 EN" *)
    input                                               output_feature_bram_en_2, // Chip Enable Signal (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_2 DOUT" *)
    output [31 : 0]                                     output_feature_bram_dout_2, // Data Out Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_2 DIN" *)
    input [31 : 0]                                      output_feature_bram_din_2, // Data In Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_2 WE" *)
    input [3 : 0]                                       output_feature_bram_we_2, // Byte Enables (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_2 ADDR" *)
    input [OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH - 1 : 0]   output_feature_bram_addr_2, // Address Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_2 CLK" *)
    input                                               output_feature_bram_clk_2, // Clock Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_2 RST" *)
    input                                               output_feature_bram_rst_2, // Reset Signal (required)
    // BRAM 3
    (* X_INTERFACE_PARAMETER = "MASTER_TYPE BRAM_CTRL,MEM_ECC NONE,MEM_WIDTH 32,MEM_SIZE 4096,READ_WRITE_MODE READ_WRITE" *) 
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_3 EN" *)
    input                                               output_feature_bram_en_3, // Chip Enable Signal (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_3 DOUT" *)
    output [31 : 0]                                     output_feature_bram_dout_3, // Data Out Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_3 DIN" *)
    input [31 : 0]                                      output_feature_bram_din_3, // Data In Bus (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_3 WE" *)
    input [3 : 0]                                       output_feature_bram_we_3, // Byte Enables (optional)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_3 ADDR" *)
    input [OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH - 1 : 0]   output_feature_bram_addr_3, // Address Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_3 CLK" *)
    input                                               output_feature_bram_clk_3, // Clock Signal (required)
    (* X_INTERFACE_INFO = "xilinx.com:interface:bram:1.0 OUTPUT_FEATURE_BRAM_PORT_3 RST" *)
    input                                               output_feature_bram_rst_3, // Reset Signal (required)
    // REG_BANK_AXI----------------------------------------------------------------------
    // Global Clock Signal
    input wire  S_AXI_ACLK,
    // Global Reset Signal. This Signal is Active LOW
    input wire  S_AXI_ARESETN,
    // Write address (issued by master, acceped by Slave)
    input wire [C_S_AXI_ADDR_WIDTH-1 : 0] S_AXI_AWADDR,
    // Write channel Protection type. This signal indicates the
    // privilege and security level of the transaction, and whether
    // the transaction is a data access or an instruction access.
    input wire [2 : 0] S_AXI_AWPROT,
    // Write address valid. This signal indicates that the master signaling
    // valid write address and control information.
    input wire  S_AXI_AWVALID,
    // Write address ready. This signal indicates that the slave is ready
    // to accept an address and associated control signals.
    output wire  S_AXI_AWREADY,
    // Write data (issued by master, acceped by Slave) 
    input wire [C_S_AXI_DATA_WIDTH-1 : 0] S_AXI_WDATA,
    // Write strobes. This signal indicates which byte lanes hold
    // valid data. There is one write strobe bit for each eight
    // bits of the write data bus.    
    input wire [(C_S_AXI_DATA_WIDTH/8)-1 : 0] S_AXI_WSTRB,
    // Write valid. This signal indicates that valid write
    // data and strobes are available.
    input wire  S_AXI_WVALID,
    // Write ready. This signal indicates that the slave
    // can accept the write data.
    output wire  S_AXI_WREADY,
    // Write response. This signal indicates the status
    // of the write transaction.
    output wire [1 : 0] S_AXI_BRESP,
    // Write response valid. This signal indicates that the channel
    // is signaling a valid write response.
    output wire  S_AXI_BVALID,
    // Response ready. This signal indicates that the master
    // can accept a write response.
    input wire  S_AXI_BREADY,
    // Read address (issued by master, acceped by Slave)
    input wire [C_S_AXI_ADDR_WIDTH-1 : 0] S_AXI_ARADDR,
    // Protection type. This signal indicates the privilege
    // and security level of the transaction, and whether the
    // transaction is a data access or an instruction access.
    input wire [2 : 0] S_AXI_ARPROT,
    // Read address valid. This signal indicates that the channel
    // is signaling valid read address and control information.
    input wire  S_AXI_ARVALID,
    // Read address ready. This signal indicates that the slave is
    // ready to accept an address and associated control signals.
    output wire  S_AXI_ARREADY,
    // Read data (issued by slave)
    output wire [C_S_AXI_DATA_WIDTH-1 : 0] S_AXI_RDATA,
    // Read response. This signal indicates the status of the
    // read transfer.
    output wire [1 : 0] S_AXI_RRESP,
    // Read valid. This signal indicates that the channel is
    // signaling the required read data.
    output wire  S_AXI_RVALID,
    // Read ready. This signal indicates that the master can
    // accept the read data and response information.
    input wire  S_AXI_RREADY
);
    // local param
    localparam      FLATTEN_WIDTH   =   2;
    //
    wire        reset_busy_wait_request;
    // Register bank
    wire          [C_S_AXI_DATA_WIDTH - 1 : 0]            stt_reg;
    wire          [C_S_AXI_DATA_WIDTH - 1 : 0]            output_feature_snapshot;
    wire          [C_S_AXI_DATA_WIDTH - 1 : 0]            ctrl_reg;
    wire          [C_S_AXI_DATA_WIDTH - 1 : 0]            input_feature_config;
    wire          [C_S_AXI_DATA_WIDTH - 1 : 0]            kernel_config;
    wire          [C_S_AXI_DATA_WIDTH - 1 : 0]            kernel_batch_size_config;
    //
    // Input feature
    wire          [INPUT_ROW_WIDTH - 1 : 0]               input_row;
    wire          [INPUT_COL_WIDTH - 1 : 0]               input_col;
    wire          [INPUT_CHANNEL_WIDTH - 1 : 0]           input_channel;
    // Kernel
    wire          [KERNEL_ROW_WIDTH - 1 : 0]              kernel_row;
    wire          [KERNEL_COL_WIDTH - 1 : 0]              kernel_col;
    wire          [KERNEL_CHANNEL_WIDTH - 1 : 0]          kernel_channel;
    wire          [KERNEL_FILTER_WIDTH - 1 : 0]           kernel_filter;
    wire          [KERNEL_FILTER_WIDTH - 1 : 0]           kernel_start_index_batch_filter;
    wire          [KERNEL_FILTER_WIDTH - 1 : 0]           kernel_end_index_batch_filter;
    wire          [KERNEL_BIAS_WIDTH - 1 : 0]             kernel_bias_size;
    // Output feature
    wire          [OUTPUT_ROW_WIDTH - 1 : 0]              output_row;
    wire          [OUTPUT_ROW_WIDTH - 1 : 0]              output_col;
    wire          [OUTPUT_CHANNEL_WIDTH - 1 : 0]          output_channel;
    wire          [OUTPUT_CHANNEL_WIDTH - 1 : 0]          output_start_index_channel;
    wire          [OUTPUT_ROW_WIDTH - 1 : 0]              output_end_index_channel;
    // Input BRAM interface
    wire                                                  input_feature_bram_en;
    wire  [31 : 0]                                        input_feature_bram_dout;
    wire  [31 : 0]                                        input_feature_bram_din;
    wire  [0 : 0]                                         input_feature_bram_we;
    wire  [INPUT_FEATURE_BRAM_ADDRESS_WIDTH - 1 : 0]      input_feature_bram_addr;
    wire                                                  input_feature_bram_clk;
    wire                                                  input_feature_bram_rst;
    
    // Kernel weights BRAM interface
    wire  [KERNEL_BRAM_NUM - 1 : 0]                       kernel_weights_bram_en;
    wire  [KERNEL_BRAM_NUM * 32 - 1 : 0]                  kernel_weights_bram_dout;
    wire  [KERNEL_BRAM_NUM * 32 - 1 : 0]                  kernel_weights_bram_din;
    wire  [KERNEL_BRAM_NUM - 1 : 0]                       kernel_weights_bram_we;
    wire  [KERNEL_BRAM_NUM * KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH - 1 : 0] kernel_weights_bram_addr;
    wire  [KERNEL_BRAM_NUM - 1 : 0]                       kernel_weights_bram_clk;
    wire  [KERNEL_BRAM_NUM - 1 : 0]                       kernel_weights_bram_rst;
    
    // Kernel bias BRAM interface
    wire  [KERNEL_BRAM_NUM - 1 : 0]                       kernel_bias_bram_en;
    wire  [KERNEL_BRAM_NUM * 32 - 1 : 0]                  kernel_bias_bram_dout;
    wire  [KERNEL_BRAM_NUM * 32 - 1 : 0]                  kernel_bias_bram_din;
    wire  [KERNEL_BRAM_NUM - 1 : 0]                       kernel_bias_bram_we;
    wire  [KERNEL_BRAM_NUM * KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH - 1 : 0] kernel_bias_bram_addr;
    wire  [KERNEL_BRAM_NUM - 1 : 0]                       kernel_bias_bram_clk;
    wire  [KERNEL_BRAM_NUM - 1 : 0]                       kernel_bias_bram_rst;
    
    // Output BRAM interface
    wire  [OUTPUT_BRAM_NUM - 1 : 0]                       output_feature_bram_en;
    wire  [OUTPUT_BRAM_NUM * 32 - 1 : 0]                  output_feature_bram_dout;
    wire  [OUTPUT_BRAM_NUM * 32 - 1 : 0]                  output_feature_bram_din;
    wire  [OUTPUT_BRAM_NUM - 1 : 0]                       output_feature_bram_we;
    wire  [OUTPUT_BRAM_NUM * OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH - 1 : 0] output_feature_bram_addr;
    wire  [OUTPUT_BRAM_NUM - 1 : 0]                       output_feature_bram_clk;
    wire  [OUTPUT_BRAM_NUM - 1 : 0]                       output_feature_bram_rst;

    // Control register
    wire        [OUTPUT_BRAM_NUM - 1 : 0]       ps_read_enable;
    wire                                        load_new_batch_filter;
    wire                                        start_processing;
    wire                                        start_input_transfer;
    wire                                        start_kernel_weights_transfer;
    wire                                        stop_procesing;
    // Status register
    wire                                        bias_reset_busy;
    wire                                        weights_reset_busy;
    wire                                        input_reset_busy;
    wire                                        output_reset_busy;
    wire                                        processing_done;
    wire                                        row_done;
    // Snapshot
    wire        [C_S_AXI_DATA_WIDTH - 1 : 0]    output_snapshot;
    //temp signals
    wire                                        start_input_transfer_edge;
    // Input BRAM interface assignment
    assign      input_feature_bram_en           =       input_feature_bram_en_0;
    assign      input_feature_bram_dout_0       =       input_feature_bram_dout;
    assign      input_feature_bram_din          =       input_feature_bram_din_0;
    assign      input_feature_bram_we           =       |input_feature_bram_we_0;
    assign      input_feature_bram_addr         =       input_feature_bram_addr_0;
    assign      input_feature_bram_clk          =       input_feature_bram_clk_0;
    assign      input_feature_bram_rst          =       input_feature_bram_rst_0;

    // Kernel weights BRAM interface assignment
    assign      kernel_weights_bram_en[0]       =       kernel_weights_bram_en_0;
    assign      kernel_weights_bram_en[1]       =       kernel_weights_bram_en_1;
    assign      kernel_weights_bram_en[2]       =       kernel_weights_bram_en_2;
    assign      kernel_weights_bram_en[3]       =       kernel_weights_bram_en_3;

    assign      kernel_weights_bram_dout_0      =       kernel_weights_bram_dout[31:0];
    assign      kernel_weights_bram_dout_1      =       kernel_weights_bram_dout[63:32];
    assign      kernel_weights_bram_dout_2      =       kernel_weights_bram_dout[95:64];
    assign      kernel_weights_bram_dout_3      =       kernel_weights_bram_dout[127:96];

    assign      kernel_weights_bram_din[31:0]           =       kernel_weights_bram_din_0;
    assign      kernel_weights_bram_din[63:32]          =       kernel_weights_bram_din_1;
    assign      kernel_weights_bram_din[95:64]          =       kernel_weights_bram_din_2;
    assign      kernel_weights_bram_din[127:96]         =       kernel_weights_bram_din_3;

    assign      kernel_weights_bram_we[0]               =       kernel_weights_bram_we_0[3] || kernel_weights_bram_we_0[2] || kernel_weights_bram_we_0[1] || kernel_weights_bram_we_0[0];
    assign      kernel_weights_bram_we[1]               =       kernel_weights_bram_we_1[3] || kernel_weights_bram_we_1[2] || kernel_weights_bram_we_1[1] || kernel_weights_bram_we_1[0];
    assign      kernel_weights_bram_we[2]               =       kernel_weights_bram_we_2[3] || kernel_weights_bram_we_2[2] || kernel_weights_bram_we_2[1] || kernel_weights_bram_we_2[0];
    assign      kernel_weights_bram_we[3]               =       kernel_weights_bram_we_3[3] || kernel_weights_bram_we_3[2] || kernel_weights_bram_we_3[1] || kernel_weights_bram_we_3[0];

    assign      kernel_weights_bram_addr[KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH-1:0]                  = kernel_weights_bram_addr_0;
    assign      kernel_weights_bram_addr[2*KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH-1:KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH] = kernel_weights_bram_addr_1;
    assign      kernel_weights_bram_addr[3*KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH-1:2*KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH] = kernel_weights_bram_addr_2;
    assign      kernel_weights_bram_addr[4*KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH-1:3*KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH] = kernel_weights_bram_addr_3;

    assign      kernel_weights_bram_clk[0]      =       kernel_weights_bram_clk_0;
    assign      kernel_weights_bram_clk[1]      =       kernel_weights_bram_clk_1;
    assign      kernel_weights_bram_clk[2]      =       kernel_weights_bram_clk_2;
    assign      kernel_weights_bram_clk[3]      =       kernel_weights_bram_clk_3;

    assign      kernel_weights_bram_rst[0]      =       kernel_weights_bram_rst_0;
    assign      kernel_weights_bram_rst[1]      =       kernel_weights_bram_rst_1;
    assign      kernel_weights_bram_rst[2]      =       kernel_weights_bram_rst_2;
    assign      kernel_weights_bram_rst[3]      =       kernel_weights_bram_rst_3;

    // Kernel bias BRAM interface assignment
    assign      kernel_bias_bram_en[0]          =       kernel_bias_bram_en_0;
    assign      kernel_bias_bram_en[1]          =       kernel_bias_bram_en_1;
    assign      kernel_bias_bram_en[2]          =       kernel_bias_bram_en_2;
    assign      kernel_bias_bram_en[3]          =       kernel_bias_bram_en_3;

    assign      kernel_bias_bram_dout_0         =       kernel_bias_bram_dout[31:0];
    assign      kernel_bias_bram_dout_1         =       kernel_bias_bram_dout[63:32];
    assign      kernel_bias_bram_dout_2         =       kernel_bias_bram_dout[95:64];
    assign      kernel_bias_bram_dout_3         =       kernel_bias_bram_dout[127:96];

    assign      kernel_bias_bram_din[31:0]            =       kernel_bias_bram_din_0;
    assign      kernel_bias_bram_din[63:32]           =       kernel_bias_bram_din_1;
    assign      kernel_bias_bram_din[95:64]           =       kernel_bias_bram_din_2;
    assign      kernel_bias_bram_din[127:96]          =       kernel_bias_bram_din_3;

    assign      kernel_bias_bram_we[0]                =       |kernel_bias_bram_we_0;
    assign      kernel_bias_bram_we[1]                =       |kernel_bias_bram_we_1;
    assign      kernel_bias_bram_we[2]                =       |kernel_bias_bram_we_2;
    assign      kernel_bias_bram_we[3]                =       |kernel_bias_bram_we_3;

    assign      kernel_bias_bram_addr[KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH-1:0]                                        =   kernel_bias_bram_addr_0;
    assign      kernel_bias_bram_addr[2*KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH-1:KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH]      =   kernel_bias_bram_addr_1;
    assign      kernel_bias_bram_addr[3*KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH-1:2*KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH]    =   kernel_bias_bram_addr_2;
    assign      kernel_bias_bram_addr[4*KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH-1:3*KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH]    =   kernel_bias_bram_addr_3;

    assign      kernel_bias_bram_clk[0]         =       kernel_bias_bram_clk_0;
    assign      kernel_bias_bram_clk[1]         =       kernel_bias_bram_clk_1;
    assign      kernel_bias_bram_clk[2]         =       kernel_bias_bram_clk_2;
    assign      kernel_bias_bram_clk[3]         =       kernel_bias_bram_clk_3;

    assign      kernel_bias_bram_rst[0]         =       kernel_bias_bram_rst_0;
    assign      kernel_bias_bram_rst[1]         =       kernel_bias_bram_rst_1;
    assign      kernel_bias_bram_rst[2]         =       kernel_bias_bram_rst_2;
    assign      kernel_bias_bram_rst[3]         =       kernel_bias_bram_rst_3;

    // Output feature BRAM interface
    assign      output_feature_bram_en[0]       =       output_feature_bram_en_0;
    assign      output_feature_bram_en[1]       =       output_feature_bram_en_1;
    assign      output_feature_bram_en[2]       =       output_feature_bram_en_2;
    assign      output_feature_bram_en[3]       =       output_feature_bram_en_3;

    assign      output_feature_bram_dout_0      =       output_feature_bram_dout[31:0];
    assign      output_feature_bram_dout_1      =       output_feature_bram_dout[63:32];
    assign      output_feature_bram_dout_2      =       output_feature_bram_dout[95:64];
    assign      output_feature_bram_dout_3      =       output_feature_bram_dout[127:96];

    assign      output_feature_bram_din[31:0]            =       output_feature_bram_din_0;
    assign      output_feature_bram_din[63:32]           =       output_feature_bram_din_1;
    assign      output_feature_bram_din[95:64]           =       output_feature_bram_din_2;
    assign      output_feature_bram_din[127:96]          =       output_feature_bram_din_3;

    assign      output_feature_bram_we[0]                =       |output_feature_bram_we_0;
    assign      output_feature_bram_we[1]                =       |output_feature_bram_we_1;
    assign      output_feature_bram_we[2]                =       |output_feature_bram_we_2;
    assign      output_feature_bram_we[3]                =       |output_feature_bram_we_3;

    assign      output_feature_bram_addr[OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH-1:0]                                     = (ctrl_reg[0] == 1) ? output_feature_bram_addr_0 : output_feature_bram_addr[0];
    assign      output_feature_bram_addr[2*OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH-1:OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH]   = (ctrl_reg[1] == 1) ? output_feature_bram_addr_1 : output_feature_bram_addr[1];
    assign      output_feature_bram_addr[3*OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH-1:2*OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH] = (ctrl_reg[2] == 1) ? output_feature_bram_addr_2 : output_feature_bram_addr[2];
    assign      output_feature_bram_addr[4*OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH-1:3*OUTPUT_FEATURE_BRAM_ADDRESS_WIDTH] = (ctrl_reg[3] == 1) ? output_feature_bram_addr_3 : output_feature_bram_addr[3];

    assign      output_feature_bram_clk[0]      =       output_feature_bram_clk_0;
    assign      output_feature_bram_clk[1]      =       output_feature_bram_clk_1;
    assign      output_feature_bram_clk[2]      =       output_feature_bram_clk_2;
    assign      output_feature_bram_clk[3]      =       output_feature_bram_clk_3;

    assign      output_feature_bram_rst[0]      =       output_feature_bram_rst_0;
    assign      output_feature_bram_rst[1]      =       output_feature_bram_rst_1;
    assign      output_feature_bram_rst[2]      =       output_feature_bram_rst_2;
    assign      output_feature_bram_rst[3]      =       output_feature_bram_rst_3;

    // Input feature register bank assignment
    assign      input_row                       =       input_feature_config[INPUT_WIDTH - 1 : 0];
    assign      input_col                       =       input_feature_config[INPUT_WIDTH - 1 : 0];
    assign      input_channel                   =       input_feature_config[INPUT_CHANNEL_WIDTH + INPUT_WIDTH - 1 : INPUT_WIDTH];
    // Kernel register bank assignment
    assign      kernel_row                      =       kernel_config[KERNEL_WIDTH - 1 : 0];
    assign      kernel_col                      =       kernel_config[KERNEL_WIDTH - 1 : 0];
    assign      kernel_channel                  =       kernel_config[KERNEL_CHANNEL_WIDTH + KERNEL_WIDTH - 1 : KERNEL_WIDTH];
    assign      kernel_filter                   =       kernel_config[KERNEL_FILTER_WIDTH + KERNEL_CHANNEL_WIDTH + KERNEL_WIDTH - 1 : KERNEL_CHANNEL_WIDTH + KERNEL_WIDTH];
    assign      kernel_bias_size                =       kernel_config[KERNEL_FILTER_WIDTH + KERNEL_CHANNEL_WIDTH + KERNEL_WIDTH - 1 : KERNEL_CHANNEL_WIDTH + KERNEL_WIDTH];
    // Kernel batch size register bank assignment
    assign      kernel_start_index_batch_filter =       kernel_batch_size_config[KERNEL_FILTER_WIDTH - 1 : 0];
    assign      kernel_end_index_batch_filter   =       kernel_batch_size_config[2 * KERNEL_FILTER_WIDTH - 1 : KERNEL_FILTER_WIDTH];
    // Output feature register bank assignment
    assign      output_row                      =       (!i_reset_n) ? 6'b0 : (input_feature_config[INPUT_WIDTH - 1 : 0] - 6'b10);
    assign      output_col                      =       (!i_reset_n) ? 6'b0 : (input_feature_config[INPUT_WIDTH - 1 : 0] - 6'b10);
    assign      output_channel                  =       (!i_reset_n) ? 8'b0 : kernel_config[KERNEL_FILTER_WIDTH + KERNEL_CHANNEL_WIDTH + KERNEL_WIDTH - 1 : KERNEL_CHANNEL_WIDTH + KERNEL_WIDTH];
    assign      output_start_index_channel      =       kernel_batch_size_config[KERNEL_FILTER_WIDTH - 1 : 0];
    assign      output_end_index_channel        =       kernel_batch_size_config[2 * KERNEL_FILTER_WIDTH - 1 : KERNEL_FILTER_WIDTH];
    // Control direction
    assign      ps_read_enable                  =       ctrl_reg[8:5];
    assign      load_new_batch_filter           =       ctrl_reg[4];
    assign      stop_procesing                  =       ctrl_reg[3];
    assign      start_kernel_weights_transfer   =       ctrl_reg[2];
    assign      start_input_transfer            =       ctrl_reg[1];
    assign      start_processing                =       ctrl_reg[0];
    // Status direction
    assign      stt_reg[0]                      =       bias_reset_busy;
    assign      stt_reg[1]                      =       weights_reset_busy;
    assign      stt_reg[2]                      =       input_reset_busy;
    assign      stt_reg[3]                      =       output_reset_busy;
    assign      stt_reg[4]                      =       row_done;
    assign      stt_reg[5]                      =       processing_done;
    // Ouput snapshot
    assign      output_feature_snapshot         =       output_snapshot;
    Convolutional_register_bank_v1_0_S00_AXI #(
		.C_S_AXI_DATA_WIDTH(C_S_AXI_DATA_WIDTH),
		.C_S_AXI_ADDR_WIDTH(C_S_AXI_ADDR_WIDTH)
    ) Convolutional_register_bank_v1_0_S00_AXI_inst (
		.i_stt_reg(stt_reg),
		.i_output_feature_snapshot(output_feature_snapshot),
		.o_ctrl_reg(ctrl_reg),
		.o_input_feature_config(input_feature_config),
		.o_kernel_config(kernel_config),
		.o_kernel_batch_size_config(kernel_batch_size_config),
		.S_AXI_ACLK(S_AXI_ACLK),
		.S_AXI_ARESETN(S_AXI_ARESETN),
		.S_AXI_AWADDR(S_AXI_AWADDR),
		.S_AXI_AWPROT(S_AXI_AWPROT),
		.S_AXI_AWVALID(S_AXI_AWVALID),
		.S_AXI_AWREADY(S_AXI_AWREADY),
		.S_AXI_WDATA(S_AXI_WDATA),    
		.S_AXI_WSTRB(S_AXI_WSTRB),
		.S_AXI_WVALID(S_AXI_WVALID),
		.S_AXI_WREADY(S_AXI_WREADY),
		.S_AXI_BRESP(S_AXI_BRESP),
		.S_AXI_BVALID(S_AXI_BVALID),
		.S_AXI_BREADY(S_AXI_BREADY),
		.S_AXI_ARADDR(S_AXI_ARADDR),
		.S_AXI_ARPROT(S_AXI_ARPROT),
		.S_AXI_ARVALID(S_AXI_ARVALID),
		.S_AXI_ARREADY(S_AXI_ARREADY),
		.S_AXI_RDATA(S_AXI_RDATA),
		.S_AXI_RRESP(S_AXI_RRESP),
		.S_AXI_RVALID(S_AXI_RVALID),
		.S_AXI_RREADY(S_AXI_RREADY)
	);
    posedge_edge_detector (
        .sig(start_input_transfer),
        .clk(i_clock),
        .pe(start_input_transfer_edge)
    );
    Convolutional_computation #(
        .DATA_WIDTH(DATA_WIDTH),
        .FRAC_WIDTH(FRACTION_WIDTH),
        .KERNEL_FILTER_WIDTH(KERNEL_FILTER_WIDTH),
        .KERNEL_CHANNEL_WIDTH(KERNEL_CHANNEL_WIDTH),
        .KERNEL_ROW_WIDTH(KERNEL_ROW_WIDTH),
        .KERNEL_COL_WIDTH(KERNEL_COL_WIDTH),
        .KERNEL_BRAM_NUM(KERNEL_BRAM_NUM),
        .KERNEL_WEIGHTS_BRAM_DEPTH(KERNEL_WEIGHTS_BRAM_DEPTH),
        .KERNEL_BIAS_BRAM_DEPTH(KERNEL_BIAS_BRAM_DEPTH),
        .KERNEL_BIAS_WIDTH(KERNEL_BIAS_WIDTH),
        .KERNEL_FIFO_DEPTH(KERNEL_FIFO_DEPTH),
        .KERNEL_POINTER_WIDTH(KERNEL_POINTER_WIDTH),
        .KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH(KERNEL_WEIGHTS_BRAM_ADDRESS_WIDTH),
        .KERNEL_BIAS_BRAM_ADDRESS_WIDTH(KERNEL_BIAS_BRAM_ADDRESS_WIDTH),
        .INPUT_CHANNEL_WIDTH(INPUT_CHANNEL_WIDTH),
        .INPUT_ROW_WIDTH(INPUT_ROW_WIDTH),
        .INPUT_COL_WIDTH(INPUT_COL_WIDTH),
        .INPUT_BRAM_DEPTH(INPUT_BRAM_DEPTH),
        .INPUT_BRAM_ADDRESS_WIDTH(INPUT_FEATURE_BRAM_ADDRESS_WIDTH),
        .INPUT_FIFO_DEPTH(INPUT_FIFO_DEPTH),
        .INPUT_POINTER_WIDTH(INPUT_POINTER_WIDTH),
        .INPUT_TOTAL_ELEMENT_WIDTH(INPUT_TOTAL_ELEMENT_WIDTH),
        .OUTPUT_CHANNEL_WIDTH(OUTPUT_CHANNEL_WIDTH),
        .OUTPUT_COL_WIDTH(OUTPUT_COL_WIDTH),
        .OUTPUT_ROW_WIDTH(OUTPUT_ROW_WIDTH),
        .OUTPUT_BRAM_NUM(OUTPUT_BRAM_NUM),
        .OUTPUT_BRAM_DEPTH(OUTPUT_BRAM_DEPTH)
    ) Convolutional_computation_inst (
        .i_clock(i_clock),
        .i_reset(i_reset_n),
        .i_enable(start_processing),
        .i_renable(ps_read_enable),
        .i_output_feature_raddress(output_feature_bram_addr),
        .i_input_feature_wenable(input_feature_bram_we),
        .i_kernel_weights_wenable(kernel_weights_bram_we),
        .i_kernel_bias_wenable(kernel_bias_bram_we),
        .i_input_start_transfer_process(start_input_transfer_edge),
        .i_kernel_weights_start_transfer_process(start_kernel_weights_transfer),
        .i_load_new_filter(load_new_batch_filter),
        .i_input_feature_wraddress(input_feature_bram_addr),
        .i_kernel_weights_wraddress(kernel_weights_bram_addr),
        .i_kernel_bias_wraddress(kernel_bias_bram_addr),
        .i_input_feature_data(input_feature_bram_din),
        .i_kernel_weights_data(kernel_weights_bram_din),
        .i_kernel_bias_data(kernel_bias_bram_din),
        .i_input_feature_row(input_row),
        .i_input_feature_col(input_col),
        .i_input_feature_channel(input_channel),
        .i_kernel_row(kernel_row),
        .i_kernel_col(kernel_col),
        .i_kernel_channel(kernel_channel),
        .i_kernel_filter(kernel_filter),
        .i_kernel_start_index_batch_filter(kernel_start_index_batch_filter),
        .i_kernel_end_index_batch_filter(kernel_end_index_batch_filter),
        .i_kernel_bias_size(kernel_bias_size),
        .i_output_feature_row(output_row),
        .i_output_feature_col(output_col),
        .i_output_feature_channel(output_channel),
        .i_output_feature_start_index_channel(output_start_index_channel),
        .i_output_feature_end_index_channel(output_end_index_channel),
        .o_bram_data(output_feature_bram_dout),
        .o_output_index(output_snapshot),
        .o_kernel_weights_reset_busy(weights_reset_busy),
        .o_kernel_bias_reset_busy(bias_reset_busy),
        .o_input_feature_reset_busy(input_reset_busy),
        .o_output_feature_reset_busy(output_reset_busy),
        .o_row_done(row_done),
        .o_processing_done(processing_done)
    );
endmodule