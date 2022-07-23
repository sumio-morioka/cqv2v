verilogparse.tab.c verilogparse.tab.h : verilogparse.y
	bison -d verilogparse.y

lex.yy.c : veriloglex.l
	flex -i veriloglex.l

all : verilog2vhdl.h verilog2vhdl.c verilogparse.tab.c lex.yy.c verilogparse.tab.h
	gcc -O2 -o verilog2vhdl verilog2vhdl.c verilogparse.tab.c lex.yy.c; rm verilogparse.tab.c verilogparse.tab.h lex.yy.c

clean :
	rm -f verilogparse.tab.c verilogparse.tab.h lex.yy.c verilog2vhdl
