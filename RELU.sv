module RELU #(
    parameter       DATA_WIDTH                  = 32,
    parameter       FRACTION_WIDTH              = 15
) (
    input           [DATA_WIDTH - 1 : 0]        i_relu_data,
    input           [DATA_WIDTH - 1 : 0]        i_max_relu,
    output                                      o_invalid_max_relu,
    output          [DATA_WIDTH - 1 : 0]        o_relu_data
);

    localparam      SIGN_WIDTH          =      1;
    localparam      INTEGER_WIDTH       =      DATA_WIDTH - FRACTION_WIDTH - SIGN_WIDTH;

    assign      o_invalid_max_relu      =       (i_max_relu[DATA_WIDTH - 1] == 1) ? 1 : 0;

    always_comb begin 
        if(i_relu_data[DATA_WIDTH - 1] == 1) begin
            o_relu_data     =    DATA_WIDTH'('b0);
        end
        else begin
            if(i_relu_data[DATA_WIDTH - SIGN_WIDTH - 1 : FRACTION_WIDTH] > i_max_relu[DATA_WIDTH - SIGN_WIDTH - 1 : FRACTION_WIDTH]) begin
                o_relu_data     =    i_max_relu;
            end
            else if(i_relu_data[DATA_WIDTH - SIGN_WIDTH - 1 : FRACTION_WIDTH] == i_max_relu[DATA_WIDTH - SIGN_WIDTH - 1 : FRACTION_WIDTH]) begin
                if(i_relu_data[FRACTION_WIDTH - 1 : 0] >= i_max_relu[FRACTION_WIDTH - 1 : 0]) begin
                    o_relu_data     =    i_max_relu;
                end
                else begin
                    o_relu_data     =    i_relu_data;
                end
            end
            else begin
                o_relu_data     =       i_relu_data;
            end
        end
    end

endmodule