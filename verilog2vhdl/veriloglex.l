%{
/////////////////////////////////////////////////////////////////////////////////////////
// Verilog2VHDL
// Copyright(c)2004 S.Morioka
// Contact: http://www02.so-net.ne.jp/~morioka/v2v.htm
//
// veriloglex.l		Verilog lexical analyzer
// how to compile: flex -i veriloglex.l
//
// 1. Ver1.00(2004/05/06)		Original code is written by S.Morioka
//
/////////////////////////////////////////////////////////////////////////////////////////

#include "verilog2vhdl.h"

int yylexlinenum;
int yywrap(void)
{
	return (1);
}

%}
%%
"//".*[\n]+					{ RegisterComment(CommentListTop, yytext, &yylexlinenum); }
\n[\n]+						{ RegisterComment(CommentListTop, yytext, &yylexlinenum); }
\n							{ yylexlinenum++; }
"/*"						{ // read file until "*/" is detected.
								int ch1, ch2;
								char *buf1, *buf2;
								ch1  = input();
								buf1 = (char *)malloc(sizeof (char) * 4);
								sprintf(buf1, "/*%c", (char)ch1);
								for (;;) {
									if ((ch2 = input()) == EOF)
										break;
									buf2 = buf1;
									buf1 = (char *)malloc(sizeof (char) * (strlen(buf2) + 2));
									sprintf(buf1, "%s%c", buf2, (char)ch2);
									free(buf2);
									if (ch1 == '*' && ch2 == '/')	break;
									else							ch1 = ch2;
								}

								for (; ch2 != EOF;) {	// detect LFs
									ch1 = input();
									if (ch1 != '\n') {
										unput(ch1);
										break;
									}
									buf2 = buf1;
									buf1 = (char *)malloc(sizeof (char) * (strlen(buf2) + 2));
									sprintf(buf1, "%s\n", buf2);
									free(buf2);
								}

								RegisterComment(CommentListTop, buf1, &yylexlinenum);
								free(buf1);
							}
always						{yylval = MallocTcell(T_ALWAYS, NULLSTR, yylexlinenum); return(T_ALWAYS);}
and							{yylval = MallocTcell(T_GATE_AND, NULLSTR, yylexlinenum); return(T_GATE_AND);}
assign						{yylval = MallocTcell(T_ASSIGN, NULLSTR, yylexlinenum); return(T_ASSIGN);}
begin						{yylval = MallocTcell(T_BEGIN, NULLSTR, yylexlinenum); return(T_BEGIN);}
buf							{yylval = MallocTcell(T_GATE_BUF, NULLSTR, yylexlinenum); return(T_GATE_BUF);}
case						{yylval = MallocTcell(T_CASE, NULLSTR, yylexlinenum); return(T_CASE);}
default						{yylval = MallocTcell(T_DEFAULT, NULLSTR, yylexlinenum); return(T_DEFAULT);}
else						{yylval = MallocTcell(T_ELSE, NULLSTR, yylexlinenum); return(T_ELSE);}
end							{yylval = MallocTcell(T_END, NULLSTR, yylexlinenum); return(T_END);}
endcase						{yylval = MallocTcell(T_ENDCASE, NULLSTR, yylexlinenum); return(T_ENDCASE);}
endfunction					{yylval = MallocTcell(T_ENDFUNC, NULLSTR, yylexlinenum); return(T_ENDFUNC);}
endmodule					{yylval = MallocTcell(T_ENDMODULE, NULLSTR, yylexlinenum); return(T_ENDMODULE);}
endtask						{yylval = MallocTcell(T_ENDTASK, NULLSTR, yylexlinenum); return(T_ENDTASK);}
function					{yylval = MallocTcell(T_FUNC, NULLSTR, yylexlinenum); return(T_FUNC);}
if							{yylval = MallocTcell(T_IF, NULLSTR, yylexlinenum); return(T_IF);}
inout						{yylval = MallocTcell(T_INOUT, NULLSTR, yylexlinenum); return(T_INOUT);}
input						{yylval = MallocTcell(T_INPUT, NULLSTR, yylexlinenum); return(T_INPUT);}
integer						{yylval = MallocTcell(T_INTEGER, NULLSTR, yylexlinenum); return(T_INTEGER);}
module						{yylval = MallocTcell(T_MODULE, NULLSTR, yylexlinenum); return(T_MODULE);}
nand						{yylval = MallocTcell(T_GATE_NAND, NULLSTR, yylexlinenum); return(T_GATE_NAND);}
negedge						{yylval = MallocTcell(T_NEGEDGE, NULLSTR, yylexlinenum); return(T_NEGEDGE);}
nor							{yylval = MallocTcell(T_GATE_NOR, NULLSTR, yylexlinenum); return(T_GATE_NOR);}
not							{yylval = MallocTcell(T_GATE_NOT, NULLSTR, yylexlinenum); return(T_GATE_NOT);}
or							{yylval = MallocTcell(T_GATE_OR, NULLSTR, yylexlinenum); return(T_GATE_OR);}
output						{yylval = MallocTcell(T_OUTPUT, NULLSTR, yylexlinenum); return(T_OUTPUT);}
parameter					{yylval = MallocTcell(T_PARAM, NULLSTR, yylexlinenum); return(T_PARAM);}
posedge						{yylval = MallocTcell(T_POSEDGE, NULLSTR, yylexlinenum); return(T_POSEDGE);}
reg							{yylval = MallocTcell(T_REG, NULLSTR, yylexlinenum); return(T_REG);}
task						{yylval = MallocTcell(T_TASK, NULLSTR, yylexlinenum); return(T_TASK);}
wire						{yylval = MallocTcell(T_WIRE, NULLSTR, yylexlinenum); return(T_WIRE);}
xnor						{yylval = MallocTcell(T_GATE_XNOR, NULLSTR, yylexlinenum); return(T_GATE_XNOR);}
xor							{yylval = MallocTcell(T_GATE_XOR, NULLSTR, yylexlinenum); return(T_GATE_XOR);}
"!="						{yylval = MallocTcell(T_LOGIC_NEQ, NULLSTR, yylexlinenum); return(T_LOGIC_NEQ);}
!							{yylval = MallocTcell(T_LOGIC_NOT, NULLSTR, yylexlinenum); return(T_LOGIC_NOT);}
"&&"						{yylval = MallocTcell(T_LOGIC_AND, NULLSTR, yylexlinenum); return(T_LOGIC_AND);}
&							{yylval = MallocTcell(T_AND, NULLSTR, yylexlinenum); return(T_AND);}
"||"						{yylval = MallocTcell(T_LOGIC_OR, NULLSTR, yylexlinenum); return(T_LOGIC_OR);}
"|"							{yylval = MallocTcell(T_OR, NULLSTR, yylexlinenum); return(T_OR);}
~							{yylval = MallocTcell(T_NOT, NULLSTR, yylexlinenum); return(T_NOT);}
"^"							{yylval = MallocTcell(T_XOR, NULLSTR, yylexlinenum); return(T_XOR);}
"<<"						{yylval = MallocTcell(T_LSHIFT, NULLSTR, yylexlinenum); return(T_LSHIFT);}
"<="						{yylval = MallocTcell(T_GE, NULLSTR, yylexlinenum); return(T_GE);}
"<"							{yylval = MallocTcell(T_GT, NULLSTR, yylexlinenum); return(T_GT);}
">>"						{yylval = MallocTcell(T_RSHIFT, NULLSTR, yylexlinenum); return(T_RSHIFT);}
">="						{yylval = MallocTcell(T_LE, NULLSTR, yylexlinenum); return(T_LE);}
">"							{yylval = MallocTcell(T_LS, NULLSTR, yylexlinenum); return(T_LS);}
"=="						{yylval = MallocTcell(T_LOGIC_EQ, NULLSTR, yylexlinenum); return(T_LOGIC_EQ);}
=							{yylval = MallocTcell(T_EQ, NULLSTR, yylexlinenum); return(T_EQ);}
"+"							{yylval = MallocTcell(T_PLUS, NULLSTR, yylexlinenum); return(T_PLUS);}
-							{yylval = MallocTcell(T_MINUS, NULLSTR, yylexlinenum); return(T_MINUS);}
"/"							{yylval = MallocTcell(T_DIV, NULLSTR, yylexlinenum); return(T_DIV);}
"*"							{yylval = MallocTcell(T_MULT, NULLSTR, yylexlinenum); return(T_MULT);}
"%"							{yylval = MallocTcell(T_MOD, NULLSTR, yylexlinenum); return(T_MOD);}
"("							{yylval = MallocTcell(T_LPAREN, NULLSTR, yylexlinenum); return(T_LPAREN);}
")"							{yylval = MallocTcell(T_RPAREN, NULLSTR, yylexlinenum); return(T_RPAREN);}
"["							{yylval = MallocTcell(T_LBRAKET, NULLSTR, yylexlinenum); return(T_LBRAKET);}
"]"							{yylval = MallocTcell(T_RBRAKET, NULLSTR, yylexlinenum); return(T_RBRAKET);}
"{"							{yylval = MallocTcell(T_LBRACE, NULLSTR, yylexlinenum); return(T_LBRACE);}
"}"							{yylval = MallocTcell(T_RBRACE, NULLSTR, yylexlinenum); return(T_RBRACE);}
;							{yylval = MallocTcell(T_SEMICOLON, NULLSTR, yylexlinenum); return(T_SEMICOLON);}
:							{yylval = MallocTcell(T_COLON, NULLSTR, yylexlinenum); return(T_COLON);}
,							{yylval = MallocTcell(T_COMMA, NULLSTR, yylexlinenum); return(T_COMMA);}
"."							{yylval = MallocTcell(T_DOT, NULLSTR, yylexlinenum); return(T_DOT);}
"?"							{yylval = MallocTcell(T_SEL, NULLSTR, yylexlinenum); return(T_SEL);}
"@"							{yylval = MallocTcell(T_AT, NULLSTR, yylexlinenum); return(T_AT);}
#							{yylval = MallocTcell(T_GENERIC, NULLSTR, yylexlinenum); return(T_GENERIC);}
[0-9]+'b[0-1XxZz_]+			{yylval = MallocTcell(T_WIDTH_BINDIGIT, yytext, yylexlinenum); return(T_WIDTH_BINDIGIT);}
[0-9]+'d[0-9_]+				{yylval = MallocTcell(T_WIDTH_DECDIGIT, yytext, yylexlinenum); return(T_WIDTH_DECDIGIT);}
[0-9]+'h[A-Fa-fXxZz0-9_]+	{yylval = MallocTcell(T_WIDTH_HEXDIGIT, yytext, yylexlinenum); return(T_WIDTH_HEXDIGIT);}
[0-9]+						{yylval = MallocTcell(T_DECDIGIT, yytext, yylexlinenum); return(T_DECDIGIT);}
[A-Za-z_][A-Za-z0-9_"$"]*	{yylval = MallocTcell(T_ID, yytext, yylexlinenum); return(T_ID);}
%%

// end of file
