module Conv_unit #(
    parameter       CONV_SIZE                   = 9,
    parameter       KERNEL_BRAM_NUM             = 4,
    parameter       DATA_WIDTH                  = 32,
    parameter       FRACTION_WIDTH              = 15
) (
    input                                       i_clock,
    input                                       i_reset,
    input                                       i_global_enable,
    input   logic   [DATA_WIDTH - 1 : 0]        i_input_feature     [0 : CONV_SIZE - 1],
    input   logic   [DATA_WIDTH - 1 : 0]        i_kernel            [0 : KERNEL_BRAM_NUM - 1][0 : CONV_SIZE - 1],
    output  logic   [DATA_WIDTH - 1 : 0]        o_result            [0 : KERNEL_BRAM_NUM - 1],
    output  logic                               o_overflow         
);
    logic       [DATA_WIDTH - 1 : 0]    input_feature       [0 : CONV_SIZE - 1];
    logic       [DATA_WIDTH - 1 : 0]    kernel              [0 : KERNEL_BRAM_NUM - 1][0 : CONV_SIZE - 1];
    logic       [DATA_WIDTH - 1 : 0]    mult_result         [0 : KERNEL_BRAM_NUM - 1][0 : CONV_SIZE - 1];
    logic       [DATA_WIDTH - 1 : 0]    adder_in            [0 : KERNEL_BRAM_NUM - 1][0 : CONV_SIZE - 1];
    logic       [DATA_WIDTH - 1 : 0]    adder_result        [0 : KERNEL_BRAM_NUM - 1];
    logic       [0 : 0]                 mult_overflow       [0 : KERNEL_BRAM_NUM - 1][0 : CONV_SIZE - 1];
    logic       [0 : 0]                 adder_overflow      [0 : KERNEL_BRAM_NUM - 1];
    logic                               temp_adder_overflow;
    logic                               temp_mult_overflow;

    assign      o_overflow  =   temp_adder_overflow || temp_mult_overflow;

    always_ff @(posedge i_clock)begin 
        if(!i_reset) begin
            for(int i = 0; i <= KERNEL_BRAM_NUM - 1; i++) begin
                for(int j = 0; j <= CONV_SIZE - 1; j++) begin
                    adder_in[i][j]  <=  DATA_WIDTH'('b0);
                    kernel[i][j]    <=  DATA_WIDTH'('b0);
                end
                o_result[i]         <=  DATA_WIDTH'('b0);
            end
            for(int i = 0; i <= CONV_SIZE - 1; i++) begin
                input_feature[i]    <=  DATA_WIDTH'('b0);
            end
        end
        else begin
            for(int i = 0; i <= KERNEL_BRAM_NUM - 1; i++) begin
                for(int j = 0; j <= CONV_SIZE - 1; j++) begin
                    adder_in[i][j]  <=  mult_result[i][j];
                    kernel[i][j]    <=  i_kernel[i][j];
                end
            end
            for(int i = 0; i <= CONV_SIZE - 1; i++) begin
                input_feature[i]    <=  i_input_feature[i];
            end
            o_result                <=  adder_result;
        end
    end
    always_comb begin
        automatic logic temp_mult = 0; // Temporary internal variable for mult_overflow
        automatic logic temp_add = 0;  // Temporary internal variable for adder_overflow
        
        for (int k = 0; k < KERNEL_BRAM_NUM; k++) begin
            for (int z = 0; z < CONV_SIZE; z++) begin
                temp_mult = temp_mult || mult_overflow[k][z];
            end
            temp_add = temp_add || adder_overflow[k];
        end

        temp_mult_overflow = temp_mult; // Assigning results to outputs
        temp_adder_overflow = temp_add;
    end
    genvar i,j;
    for(i = 0; i <= KERNEL_BRAM_NUM - 1; i++) begin
        for(j = 0; j <= CONV_SIZE - 1; j++) begin
            qmult #(
                .Q(FRACTION_WIDTH), 
                .N(DATA_WIDTH)
            ) qmult_0_0_inst (
                .i_multiplicand(i_kernel[i][j]),
                .i_multiplier(i_input_feature[j]),
                .o_result(mult_result[i][j]),
                .mult_overflow(mult_overflow[i][j])
            );
        end
        adder_tree #(
            .KERNEL_ELEMENT_NUM(CONV_SIZE),
            .DATA_WIDTH(DATA_WIDTH),
            .FRAC_WIDTH(FRACTION_WIDTH)     
        ) adder_tree_inst (
            .i_data(adder_in[i]),    
            .i_reset(i_reset),
            .i_enable(i_global_enable),
            .o_data(adder_result[i]),
            .o_overflow(adder_overflow[i])
        );
    end

endmodule