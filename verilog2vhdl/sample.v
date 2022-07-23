// sample HDL
module sample(din, reset_n, clk, dout);
	input	[7:0]	din;
	input			reset_n;
	input			clk;
	output	[7:0]	dout;

	parameter	CONSTVAL	= 8'b11001001;

	// function
	function [7:0] func0;
		input	[7:0]	x;
	begin
		func0 = (x + CONSTVAL);
	end
	endfunction

	parameter ST0	= 3'b100;
	parameter ST1	= 3'b010;
	parameter ST2	= 3'b001;
	reg 	[2:0]	state_reg;

	reg 	[7:0]	dout_reg;
	wire	[7:0]	childin, childout;			// multiple signal declaration

	always @(posedge clk or negedge reset_n) begin
		if (reset_n == 1'b0) begin				// async reset
			state_reg	<= ST0;
			dout_reg	<= {8{1'b0}};			// use of others
		end else begin
			case (state_reg)					// case statement
			ST0: begin
				state_reg	<= ST0;
				dout_reg	<= din;				// output signal
			end
			ST1: begin
				dout_reg	<= func0(dout_reg);	// function call
				state_reg	<= ST1;
			end
			ST2: begin
				if (dout_reg > 8'b10000000)		// if statement
					dout_reg	<= childout;
				state_reg	<= ST2;
			end
			default:
				state_reg	<= ST0;
			endcase
		end
	end

	// calling child module
	child	u1 (
		.i(childin),
		.o(childout)
	);

	assign	dout	= (state_reg == ST0) ? dout_reg : {8{1'b0}};	// select statement

endmodule
