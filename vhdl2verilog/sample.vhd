-- sample HDL
library ieee;
use     ieee.std_logic_1164.all;
use		ieee.std_logic_arith.all;
use		ieee.std_logic_unsigned.all;

entity sample is
	port (
    	din			: in  std_logic_vector(7 downto 0);
		reset_n		: in  std_logic;
		clk			: in  std_logic;
    	dout		: out std_logic_vector(7 downto 0)
	);
end sample;

architecture RTL of sample is
	constant CONSTVAL	: std_logic_vector(7 downto 0)	:= "11001001";

	component child
	port (
		i	: in  std_logic_vector(7 downto 0);
		o	: out std_logic_vector(7 downto 0)
	);
	end component;

	-- function
	function func0
		(	x : std_logic_vector(7 downto 0)
		) return std_logic_vector is
	begin
		return (x + CONSTVAL);
	end func0;

	type STATE_TYPE is (ST0, ST1, ST2);			-- enum type
	signal	state_reg	: STATE_TYPE;

	signal	dout_reg	: std_logic_vector(7 downto 0);

	signal	childin, childout	: std_logic_vector(7 downto 0);	-- multiple signal declaration

begin

	u0: process (clk, reset_n)
	begin
		if (reset_n = '0') then					-- async reset
			state_reg	<= ST0;
			dout_reg	<= (others => '0');		-- use of others
		elsif (clk'event and clk = '1') then
			case state_reg is					-- case statement
			when ST0 =>
				state_reg	<= ST0;
				dout_reg	<= din;				-- output signal
			when ST1 =>
				dout_reg	<= func0(dout_reg);	-- function call
				state_reg	<= ST1;
			when ST2 =>
				if (dout_reg > "10000000") then	-- if statement
					dout_reg	<= childout;
				end if;
				state_reg	<= ST2;
			when others =>
				state_reg	<= ST0;
			end case;
		end if;
	end process;

	-- calling child module
	u1: child
	port map (
		i	=> childin,
		o	=> childout
	);

	dout	<= dout_reg		when state_reg = ST0	else (others => '0');	-- when statement

end RTL;

-- end of file
