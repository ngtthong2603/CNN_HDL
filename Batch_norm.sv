module Batch_norm #(
    parameter       DATA_WIDTH                  = 32,
    parameter       FRACTION_WIDTH              = 15
) (
    input           [DATA_WIDTH - 1 : 0]        i_batch_norm_data,
    input           [DATA_WIDTH - 1 : 0]        i_batch_norm_weight,
    input           [DATA_WIDTH - 1 : 0]        i_batch_norm_bias,
    output                                      o_batch_norm_overflow,
    output          [DATA_WIDTH - 1 : 0]        o_batch_norm_data
);

    logic       mult_overflow;
    logic       add_overflow;
    logic       mult_result;

    assign      o_batch_norm_overflow = mult_overflow || add_overflow;

    qmult #(
	    .Q(FRACTION_WIDTH),
	    .N(DATA_WIDTH)
    )mult_inst(
	    .i_multiplicand(i_batch_norm_data),
	    .i_multiplier(i_batch_norm_weight),
	    .o_result(mult_result),
	    .mult_overflow(mult_overflow)
	 );

    qadd #(
	    .Q(FRACTION_WIDTH),
	    .N(DATA_WIDTH)
    ) add_inst (
	    .a(mult_result),
	    .b(i_batch_norm_bias),
	    .c(o_batch_norm_data),
	    .add_overflow(add_overflow)
	 );
endmodule