
module accumulator #(
    parameter       DATA_WIDTH              =       32,
    parameter       FRAC_WIDTH              =       15
)(
    input                               i_clock,        // Clock signal
    input                               i_reset,      // Active low reset signal
    input                               i_enable,
    input       [DATA_WIDTH - 1 : 0]    i_new_data,   // Input value to accumulate
    input       [DATA_WIDTH - 1 : 0]    i_acc_data,
    output      [DATA_WIDTH - 1 : 0]    o_data, // Output accumulated value
    output                              o_overflow   // Overflow flag
);

    // Internal register to hold the accumulated value
    logic[DATA_WIDTH - 1 : 0] acc_reg;
    logic[DATA_WIDTH - 1 : 0] in_data;
    logic[DATA_WIDTH - 1 : 0] add_result;
    logic add_overflow; // Overflow flag from the adder

    // Instantiate the qadd module
    qadd #(
        .Q(FRAC_WIDTH), 
        .N(DATA_WIDTH)
    ) adder(
        .a(acc_reg),
        .b(in_data),
        .c(add_result), // Using the same line for input and output for accumulation
        .add_overflow(add_overflow)
    );

    assign o_data = add_result;
    assign o_overflow = add_overflow;

    // Process block for clock and reset
    always_ff @(posedge i_clock or negedge i_reset) begin
        if (!i_reset)  begin
            acc_reg     <=      DATA_WIDTH'('b0);
            in_data     <=      DATA_WIDTH'('b0);
        end 
        else begin
            if(i_enable) begin
                acc_reg     <=      i_acc_data;
                in_data     <=      i_new_data;
            end
            else begin
                acc_reg     <=      acc_reg;
                in_data     <=      in_data;
            end
        end
    end

endmodule
