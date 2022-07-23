vhdlparse.tab.c vhdlparse.tab.h : vhdlparse.y
	bison -d vhdlparse.y

lex.yy.c : vhdllex.l
	flex -i vhdllex.l

all : vhdl2verilog.h vhdl2verilog.c vhdlparse.tab.c lex.yy.c vhdlparse.tab.h
	gcc -O2 -o vhdl2verilog vhdl2verilog.c vhdlparse.tab.c lex.yy.c; rm vhdlparse.tab.c vhdlparse.tab.h lex.yy.c

clean :
	rm -f vhdlparse.tab.c vhdlparse.tab.h lex.yy.c vhdl2verilog
