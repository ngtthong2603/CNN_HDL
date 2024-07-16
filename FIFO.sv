module FIFO #(
  parameter   DATA_WIDTH              = 32,
  parameter   INPUT_CHANNEL_WIDTH     = 11,
  parameter   INPUT_BRAM_NUM          = 4,
  parameter   FIFO_DEPTH              = 8
) (
  input                                     i_clock,
  input                                     i_reset,
  input                                     i_wenable,
  input    [DATA_WIDTH - 1 : 0]             i_wdata,
  input                                     i_renable,
  input    [INPUT_CHANNEL_WIDTH - 1 : 0]    i_input_feature_channel,                        
  output   logic [DATA_WIDTH - 1 : 0]       o_rdata,
  output                                    o_fifo_full,
  output                                    o_fifo_empty,
);

  localparam  POINTER_WIDTH           = $clog2(FIFO_DEPTH);

  logic [DATA_WIDTH - 1 : 0] mem [FIFO_DEPTH];
  logic [POINTER_WIDTH : 0]  w_pointer, w_next_pointer;
  logic [POINTER_WIDTH : 0]  r_pointer, r_next_pointer;
 
  always @(posedge i_clock, negedge i_reset) begin
    if (!i_reset) begin
      w_pointer <= '0;
      r_pointer <= '0;
      o_rdata   <= '0;
    end else begin
      w_pointer <= w_next_pointer;
      r_pointer <= r_next_pointer;
      mem[w_pointer[POINTER_WIDTH - 1 : 0]] <=  i_wdata;
      o_rdata      <= (i_renable) ? mem[r_pointer[POINTER_WIDTH-1:0]] : o_rdata ;
    end
  end

  always_comb begin
    w_next_pointer      = w_pointer;
    r_next_pointer      = r_pointer;
    if (i_wenable) begin
      w_next_pointer    = w_pointer + 1;
    end
    if (i_renable) begin
      r_next_pointer    = r_pointer + 1;
    end
  end

  assign o_fifo_empty = (w_pointer[POINTER_WIDTH] == r_pointer[POINTER_WIDTH]) && (w_pointer[POINTER_WIDTH-1:0] == r_pointer[POINTER_WIDTH-1:0]);
  assign o_fifo_full  = (w_pointer[POINTER_WIDTH] != r_pointer[POINTER_WIDTH]) && (w_pointer[POINTER_WIDTH-1:0] == r_pointer[POINTER_WIDTH-1:0]);

endmodule