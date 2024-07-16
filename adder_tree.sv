module adder_tree #(
    parameter       KERNEL_ELEMENT_NUM          =       9,
    parameter       DATA_WIDTH              =       32,
    parameter       FRAC_WIDTH              =       15
) (
    input           [DATA_WIDTH - 1 : 0]    i_data      [0 : KERNEL_ELEMENT_NUM - 1],
    input                                   i_reset,
    input                                   i_enable,
    output          [DATA_WIDTH - 1 : 0]    o_data,
    output                                  o_overflow
);

    
    logic            [DATA_WIDTH - 1 : 0]    o_temp     [0 : KERNEL_ELEMENT_NUM - 2];
    logic            [0 : 0]                 inst_overflow [0 : KERNEL_ELEMENT_NUM - 2];

    assign o_overflow   =   inst_overflow[0] || inst_overflow[1] || inst_overflow[2] || inst_overflow[3] || inst_overflow[4] || inst_overflow[5] || inst_overflow[6] || inst_overflow[7];
    assign o_data       =   o_temp[7];
    qadd #(
	    .Q(FRAC_WIDTH),
	    .N(DATA_WIDTH)
    ) qadd_0_inst (
        .a(i_data[0]),
        .b(i_data[1]),
        .c(o_temp[0]),
	    .add_overflow(inst_overflow[0])
    );

    qadd #(
	    .Q(FRAC_WIDTH),
	    .N(DATA_WIDTH)
    ) qadd_1_inst (
        .a(i_data[2]),
        .b(i_data[3]),
        .c(o_temp[1]),
	    .add_overflow(inst_overflow[1])
    );

    qadd #(
	    .Q(FRAC_WIDTH),
	    .N(DATA_WIDTH)
    ) qadd_2_inst (
        .a(i_data[4]),
        .b(i_data[5]),
        .c(o_temp[2]),
	    .add_overflow(inst_overflow[2])
    );

    qadd #(
	    .Q(FRAC_WIDTH),
	    .N(DATA_WIDTH)
    ) qadd_3_inst (
        .a(i_data[6]),
        .b(i_data[7]),
        .c(o_temp[3]),
	    .add_overflow(inst_overflow[3])
    );

    qadd #(
	    .Q(FRAC_WIDTH),
	    .N(DATA_WIDTH)
    ) qadd_4_inst (
        .a(o_temp[0]),
        .b(o_temp[1]),
        .c(o_temp[4]),
	    .add_overflow(inst_overflow[4])
    );

    qadd #(
	    .Q(FRAC_WIDTH),
	    .N(DATA_WIDTH)
    ) qadd_5_inst (
        .a(o_temp[2]),
        .b(o_temp[3]),
        .c(o_temp[5]),
	    .add_overflow(inst_overflow[5])
    );

    qadd #(
	    .Q(FRAC_WIDTH),
	    .N(DATA_WIDTH)
    ) qadd_6_inst (
        .a(o_temp[4]),
        .b(o_temp[5]),
        .c(o_temp[6]),
	    .add_overflow(inst_overflow[6])
    );

    qadd #(
	    .Q(FRAC_WIDTH),
	    .N(DATA_WIDTH)
    ) qadd_7_inst (
        .a(i_data[8]),
        .b(o_temp[6]),
        .c(o_temp[7]),
	    .add_overflow(inst_overflow[7])
    );

endmodule