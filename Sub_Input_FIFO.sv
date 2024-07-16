//Update read pointer
module Sub_Input_FIFO #(
  parameter   DATA_WIDTH              = 32,
  parameter   INPUT_CHANNEL_WIDTH     = 11,
  parameter   INPUT_COL_WIDTH         = 6,
  parameter   FIFO_DEPTH              = 32,
  parameter   READ_PORTS              = 3,
  parameter   POINTER_WIDTH           = $clog2(FIFO_DEPTH)
) (
  input                                       i_clock,
  input                                       i_reset,
  input                                       i_wenable,
  input    [DATA_WIDTH - 1 : 0]               i_wdata,
  input                                       i_renable,
  input    [INPUT_COL_WIDTH - 1 : 0]          i_output_size,
  input    [INPUT_COL_WIDTH - 1 :0]           i_valid_read_count,                        
  output   logic [DATA_WIDTH - 1 : 0]         o_rdata [0 : READ_PORTS - 1],
  output                                      o_fifo_full,
  output                                      o_fifo_empty,
  output   logic [POINTER_WIDTH : 0]          o_element_count,
  output   logic                              o_read_data_valid,
  output   logic                              o_fifo_almost_full
);

  localparam    SWITCH_STEP   =   3;

  logic [DATA_WIDTH - 1 : 0] mem [FIFO_DEPTH];
  logic [POINTER_WIDTH : 0]  w_pointer, w_next_pointer;
  logic [POINTER_WIDTH : 0]  r_pointer, r_next_pointer;

  // Calculate the element count based on the pointers
  logic [POINTER_WIDTH : 0] element_count;

  always @(posedge i_clock or negedge i_reset) begin
    if (!i_reset) begin
      w_pointer <= '0;
      r_pointer <= '0;
      o_read_data_valid   <=  0;
      for (int i = 0; i < READ_PORTS - 1; i++) begin
        o_rdata[i] <= '0;
      end
      for(int i = 0; i <= FIFO_DEPTH; i++) begin
        mem[i]  <=  '0;
      end
    end else begin
      w_pointer <= w_next_pointer;
      r_pointer <= r_next_pointer;
      if (i_wenable && !o_fifo_full) begin
        mem[w_pointer[POINTER_WIDTH - 1 : 0]] <= i_wdata;
      end
      if (i_renable && (element_count >= READ_PORTS)) begin
        o_read_data_valid   <=  1;
        for (int i = 0; i < READ_PORTS; i++) begin
          o_rdata[i] <= mem[(r_pointer[POINTER_WIDTH - 1 : 0] + i) % FIFO_DEPTH];
        end
      end
      else if(i_renable && !(element_count >= READ_PORTS)) begin
        o_read_data_valid   <=  0;
        o_rdata             <=  o_rdata;
      end
      else begin
        o_rdata             <=  o_rdata;
      end
    end
  end

  always_comb begin
    w_next_pointer      = w_pointer;
    r_next_pointer      = r_pointer;

    if (i_wenable && !o_fifo_full) begin
      w_next_pointer = w_pointer + 1;
    end

    if (i_renable && (element_count >= READ_PORTS)) begin
      r_next_pointer = (i_valid_read_count == i_output_size - 2) ? r_pointer + SWITCH_STEP : r_pointer + 1;
    end

    // Handle simultaneous read and write
    if (i_wenable && i_renable && (element_count >= READ_PORTS) && !o_fifo_full) begin
      w_next_pointer = w_pointer + 1;
      r_next_pointer = (i_valid_read_count == i_output_size - 2) ? r_pointer + SWITCH_STEP : r_pointer + 1;
    end
  end

  // Calculate element count based on write and read pointers
  assign element_count      =   (!i_reset) ? POINTER_WIDTH'('b0) : (w_pointer >= r_pointer) ? (w_pointer - r_pointer) : (2*FIFO_DEPTH - r_pointer + w_pointer);
  assign o_fifo_almost_full =   (element_count >= FIFO_DEPTH - 3);
  assign o_fifo_empty       =   (w_pointer == r_pointer);
  assign o_fifo_full        =   (w_pointer[POINTER_WIDTH] != r_pointer[POINTER_WIDTH] && w_pointer[POINTER_WIDTH - 1 : 0] == r_pointer[POINTER_WIDTH - 1 : 0]);
  assign o_element_count    =   element_count;

endmodule
