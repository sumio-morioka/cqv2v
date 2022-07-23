%{
/////////////////////////////////////////////////////////////////////////////////////////
// Verilog2VHDL
// Copyright(c)2004 S.Morioka
// Contact: http://www02.so-net.ne.jp/~morioka/v2v.htm
//
// verilogparse.y	Verilog parser
// how to compile: bison -d verilogparse.y
//
// 1. Ver1.00(2004/05/06)		Original code is written by S.Morioka
//
/////////////////////////////////////////////////////////////////////////////////////////

#include "verilog2vhdl.h"
static void yyerror();

%}

%token		T_ALWAYS
%token		T_ASSIGN
%token		T_BEGIN
%token		T_CASE
%token		T_DEFAULT
%token		T_ELSE
%token		T_ELSIF
%token		T_END
%token		T_ENDCASE
%token		T_ENDFUNC
%token		T_ENDMODULE
%token		T_ENDTASK
%token		T_FUNC
%token		T_GATE_AND
%token		T_GATE_BUF
%token		T_GATE_NAND
%token		T_GATE_NOR
%token		T_GATE_NOT
%token		T_GATE_OR
%token		T_GATE_XNOR
%token		T_GATE_XOR
%token		T_IF
%token		T_INOUT
%token		T_INPUT
%token		T_INTEGER
%token		T_MODULE
%token		T_NEGEDGE
%token		T_OUTPUT
%token		T_PARAM
%token		T_POSEDGE
%token		T_REG
%token		T_TASK
%token		T_WIRE

%token		T_LPAREN
%token		T_RPAREN
%token		T_LBRAKET
%token		T_RBRAKET
%token		T_LBRACE
%token		T_RBRACE
%token		T_SEMICOLON
%token		T_COLON
%token		T_COMMA
%token		T_DOT
%token		T_SEL
%token		T_AT
%token		T_GENERIC
%token		T_WIDTH_BINDIGIT
%token		T_WIDTH_DECDIGIT
%token		T_WIDTH_HEXDIGIT
%token		T_DECDIGIT
%token		T_ID

%token		N_ALWAYS_COMB
%token		N_ALWAYS_SYNCSEQ
%token		N_ALWAYS_ASYNCSEQ
%token		N_ASYNCRST_IF
%token		N_CASECOND
%token		N_CONCAT
%token		N_COPYSIG
%token		N_DUMMY
%token		N_ENTITY
%token		N_EXPLIST
%token		N_FUNCCALL
%token		N_FUNCDEF
%token		N_MAPLIST
%token		N_NULL
%token		N_PAREN
%token		N_PORTMAP
%token		N_RANGE
%token		N_REDUCTION_AND
%token		N_REDUCTION_OR
%token		N_REDUCTION_XOR
%token		N_SENSLIST
%token		N_SIG
%token		N_SIG_WIDTH
%token		N_SIGLIST
%token		N_SIGSUBST
%token		N_SUBSTLIST
%token		N_TASKCALL
%token		N_TASKDEF
%token		N_VARSUBST

%left		T_LOGIC_AND
%left		T_LOGIC_OR
%right		T_LOGIC_NOT
%nonassoc	T_LOGIC_NEQ
%nonassoc	T_LOGIC_EQ
%left		T_AND
%left		T_OR
%left		T_XOR
%nonassoc	T_GE
%nonassoc	T_GT
%nonassoc	T_LE
%nonassoc	T_LS
%nonassoc	T_EQ
%nonassoc	T_LSHIFT
%nonassoc	T_RSHIFT
%right		T_NOT
%left		T_PLUS
%left		T_MINUS
%left		T_DIV
%left		T_MULT
%left		T_MOD
%left		N_UPLUS
%left		N_UMINUS

%%

top
	: module_list
		{ ParseTreeTop = $1; }
	;

module_list		/* RTL top */
	: module module_list
		{
			$$ = $1; SetNext($$,$2);
		}
	| module
		{
			$$ = $1; SetNext($$,NULLCELL);
		}
	;

module
	: T_MODULE entity_part body_part_top T_ENDMODULE
		{	/* T_MODULE info0=entity, info1=body */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$3);
			SetLine($$,CellLine($1));
			FreeTcell($4);

			SetSig($$,SigListTop); SigListTop = MakeNewSigList();						/* reset signal list */
			SetCmp($$,ComponentListTop); ComponentListTop = MakeNewComponentList();	/* reset type list */
			/* reset comment list ... unnecessary (common to all modules) */
			/* reset line list ... unnecessary (common to all modules) */
		}
	;

entity_part
	: T_ID T_LPAREN signame_list T_RPAREN T_SEMICOLON
		{	/* N_ENTITY info0=modulename, info1=siglist */
			$$ = MallocTcell(N_ENTITY,(char *)NULL,CellLine($5)); SetInfo0($$,$1); SetInfo1($$,$3);
			FreeTcell($2); FreeTcell($4); FreeTcell($5);
		}
	;

call_func_or_task
	: T_ID T_LPAREN parameter_list T_RPAREN
		{
			$$ = $1; SetInfo0($$,$3);
			SetLine($$,CellLine($4));
			FreeTcell($2); FreeTcell($4);
		}
	;

parameter_list
	: signame T_COMMA parameter_list
		{	/* N_SIGLIST info0=sig */
			$$ = MallocTcell(N_SIGLIST,(char *)NULL,CellLine($3)); SetInfo0($$,$1); SetNext($$,$3);
			FreeTcell($2);
		}
	| signame
		{	/* N_SIGLIST info0=sig */
			$$ = MallocTcell(N_SIGLIST,(char *)NULL,CellLine($1)); SetInfo0($$,$1); SetNext($$,NULLCELL);
		}
	;

signame_list
	: T_ID T_COMMA signame_list
		{	/* N_SIGLIST info0=sig */
			$$ = MallocTcell(N_SIGLIST,(char *)NULL,CellLine($3)); SetInfo0($$,$1); SetNext($$,$3);
			SetType($1,N_SIG);
			FreeTcell($2);
		}
	| T_ID
		{	/* N_SIGLIST info0=sig */
			$$ = MallocTcell(N_SIGLIST,(char *)NULL,CellLine($1)); SetInfo0($$,$1); SetNext($$,NULLCELL);
			SetType($1,N_SIG);
		}
	;

body_part_top
	: body_part body_part_top
		{	/* (body_part will already be a list if body_part == decralations) */
			TREECELL *ptr;
			$$ = $1;
			for (ptr = $1; NextCell(ptr) != NULLCELL; ptr = NextCell(ptr))
				;
			SetNext(ptr,$2);
		}
	| body_part
		{
			$$ = $1;
		}
	;

body_part
	: decralations
		{
			register TCELLPNT	ptr;
			$$ = $1;
			for (ptr = $$; ptr != NULLCELL; ptr = NextCell(ptr)) {
				Boolean	isinp, isout, issig;
				isinp	= isout	= issig	= False;
				switch (CellType(ptr)) {
				case T_INPUT:
					isinp	= True;
					break;
				case T_OUTPUT:
					isout	= True;
					break;
				case T_INOUT:
					isinp	= True;
					isout	= True;
					break;
				case T_REG:
					issig	= True;
					break;
				case T_WIRE:
					issig	= True;
					break;
				}
				if (CellType(ptr) != T_PARAM && RegisterSignal(SigListTop, ptr, issig, isinp, isout) == False) {
					fprintfError("Duplicate signal name (VHDL code is case insensitive).", CellLine(ptr));
				}
			}
		}
	| portmap_part
		{
			$$ = $1; SetNext($$,NULLCELL);
		}
	| assign_part
		{
			$$ = $1; SetNext($$,NULLCELL);
		}
	| always_part
		{
			$$ = $1; SetNext($$,NULLCELL);
		}
	| function_part
		{
			$$ = $1; SetNext($$,NULLCELL);
		}
	| task_part
		{
			$$ = $1; SetNext($$,NULLCELL);
		}
	| call_prim_gate
		{
			$$ = $1; SetNext($$,NULLCELL);
		}
	| call_func_or_task T_SEMICOLON
		{	/* N_TASKCALL info0=list */
			$$ = $1; SetType($$,N_TASKCALL); SetNext($$,NULLCELL);
		}
	;

decralations
	: signal_decralation decralations
		{
			$$ = $1; SetNext($$,$2);
		}
	| parameter_decralation decralations
		{
			$$ = $1; SetNext($$,$2);
		}
	| signal_decralation
		{
			$$ = $1; SetNext($$,NULLCELL);
		}
	| parameter_decralation
		{
			$$ = $1; SetNext($$,NULLCELL);
		}
	;

signal_decralation
	: T_INPUT signame_list T_SEMICOLON
		{	/* T_INPUT info0=width(NULL), info1=name, info2=array width(NULL) */
			$$ = $1; SetInfo0($$,NULLCELL); SetInfo1($$,$2); SetInfo2($$,NULLCELL);
			SetLine($$,CellLine($1));
			FreeTcell($3);
		}
	| T_INPUT sigwidth signame_list T_SEMICOLON
		{	/* T_INPUT info0=width, info1=name, info2=array width(NULL) */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$3); SetInfo2($$,NULLCELL);
			SetLine($$,CellLine($1));
			FreeTcell($4);
		}
	| T_OUTPUT signame_list T_SEMICOLON
		{	/* T_OUTPUT info0=width(NULL), info1=name, info2=array width(NULL) */
			$$ = $1; SetInfo0($$,NULLCELL); SetInfo1($$,$2); SetInfo2($$,NULLCELL);
			SetLine($$,CellLine($1));
			FreeTcell($3);
		}
	| T_OUTPUT sigwidth signame_list T_SEMICOLON
		{	/* T_OUTPUT info0=width, info1=name, info2=array width(NULL) */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$3); SetInfo2($$,NULLCELL);
			SetLine($$,CellLine($1));
			FreeTcell($4);
		}
	| T_INOUT signame_list T_SEMICOLON
		{	/* T_INOUT info0=width(NULL), info1=name, info2=array width(NULL) */
			$$ = $1; SetInfo0($$,NULLCELL); SetInfo1($$,$2); SetInfo2($$,NULLCELL);
			SetLine($$,CellLine($1));
			FreeTcell($3);
		}
	| T_INOUT sigwidth signame_list T_SEMICOLON
		{	/* T_INOUT info0=width, info1=name, info2=array width(NULL) */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$3); SetInfo2($$,NULLCELL);
			SetLine($$,CellLine($1));
			FreeTcell($4);
		}
	| T_REG signame_list T_SEMICOLON
		{	/* T_REG info0=width(NULL), info1=name, info2=array width(NULL) */
			$$ = $1; SetInfo0($$,NULLCELL); SetInfo1($$,$2); SetInfo2($$,NULLCELL);
			SetLine($$,CellLine($1));
			FreeTcell($3);
		}
	| T_REG sigwidth signame_list T_SEMICOLON
		{	/* T_REG info0=width, info1=name, info2=array width(NULL) */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$3); SetInfo2($$,NULLCELL);
			SetLine($$,CellLine($1));
			FreeTcell($4);
		}
	| T_WIRE signame_list T_SEMICOLON
		{	/* T_WIRE info0=width(NULL), info1=name, info2=array width(NULL) */
			$$ = $1; SetInfo0($$,NULLCELL); SetInfo1($$,$2); SetInfo2($$,NULLCELL);
			SetLine($$,CellLine($1));
			FreeTcell($3);
		}
	| T_WIRE sigwidth signame_list T_SEMICOLON
		{	/* T_WIRE info0=width, info1=name, info2=array width(NULL) */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$3); SetInfo2($$,NULLCELL);
			SetLine($$,CellLine($1));
			FreeTcell($4);
		}

	| T_INPUT sigwidth signame_list sigwidth T_SEMICOLON
		{	/* T_INPUT info0=width, info1=name, info2=array width */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$3); SetInfo2($$,$4);
			SetLine($$,CellLine($1));
			FreeTcell($5);
		}
	| T_OUTPUT sigwidth signame_list sigwidth T_SEMICOLON
		{	/* T_OUTPUT info0=width, info1=name, info2=array width */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$3); SetInfo2($$,$4);
			SetLine($$,CellLine($1));
			FreeTcell($5);
		}
	| T_INOUT sigwidth signame_list sigwidth T_SEMICOLON
		{	/* T_INOUT info0=width, info1=name, info2=array width */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$3); SetInfo2($$,$4);
			SetLine($$,CellLine($1));
			FreeTcell($5);
		}
	| T_REG sigwidth signame_list sigwidth T_SEMICOLON
		{	/* T_REG info0=width, info1=name, info2=array width */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$3); SetInfo2($$,$4);
			SetLine($$,CellLine($1));
			FreeTcell($5);
		}
	| T_WIRE sigwidth signame_list sigwidth T_SEMICOLON
		{	/* T_WIRE info0=width, info1=name, info2=array width */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$3); SetInfo2($$,$4);
			SetLine($$,CellLine($1));
			FreeTcell($5);
		}
	;

sigwidth
	: T_LBRAKET exp T_COLON exp T_RBRAKET
		{	/* N_RANGE info0=from, info1=to */
			$$ = $1; SetType($$,N_RANGE); SetInfo0($$,$2); SetInfo1($$,$4);
			SetLine($$,CellLine($1));
			FreeTcell($3); FreeTcell($5);
		}
	| T_LBRAKET exp T_RBRAKET
		{	/* N_RANGE info0=from, info1=to(NULL) */
			$$ = $1; SetType($$,N_RANGE); SetInfo0($$,$2); SetInfo1($$,NULLCELL);
			SetLine($$,CellLine($1));
			FreeTcell($3);
		}
	;

parameter_decralation
	: T_PARAM T_ID T_EQ exp T_SEMICOLON
		{	/* T_PARAM info0=param, info1=exp */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$4);
			SetLine($$,CellLine($1));
			FreeTcell($3); FreeTcell($5);
		}
	;

portmap_part
	: T_ID T_GENERIC T_LPAREN exp_list T_RPAREN T_ID T_LPAREN map_list T_RPAREN T_SEMICOLON
		{	/* N_PORTMAP info0=module, info1=label, info2=genelic, info3=maplist */
			$$ = $2; SetType($$,N_PORTMAP); SetInfo0($$,$1); SetInfo1($$,$6); SetInfo2($$,$4); SetInfo3($$,$8);
			SetLine($$,CellLine($1));
			ConvertExplist2Substlist($4);
			RegisterComponent(ComponentListTop, $$);
			FreeTcell($3); FreeTcell($5); FreeTcell($7); FreeTcell($9); FreeTcell($10);
		}
	| T_ID T_ID T_LPAREN map_list T_RPAREN T_SEMICOLON
		{	/* N_PORTMAP info0=module, info1=label, info2=genelic(NULL), info3=maplist */
			$$ = $3; SetType($$,N_PORTMAP); SetInfo0($$,$1); SetInfo1($$,$2); SetInfo2($$,NULLCELL); SetInfo3($$,$4);
			SetLine($$,CellLine($1));
			RegisterComponent(ComponentListTop, $$);
			FreeTcell($5); FreeTcell($6);
		}
	| T_ID T_GENERIC T_LPAREN exp_list T_RPAREN T_ID T_LPAREN exp_list T_RPAREN T_SEMICOLON
		{	/* N_PORTMAP info0=module, info1=label, info2=genelic, info3=maplist */
			$$ = $2; SetType($$,N_PORTMAP); SetInfo0($$,$1); SetInfo1($$,$6); SetInfo2($$,$4); SetInfo3($$,$8);
			SetLine($$,CellLine($1));
			ConvertExplist2Substlist($4);
			ConvertExplist2Substlist($8);
			RegisterComponent(ComponentListTop, $$);
			FreeTcell($3); FreeTcell($5); FreeTcell($7); FreeTcell($9); FreeTcell($10);
		}
	| T_ID T_ID T_LPAREN exp_list T_RPAREN T_SEMICOLON
		{	/* N_PORTMAP info0=module, info1=label, info2=genelic(NULL), info3=maplist */
			$$ = $3; SetType($$,N_PORTMAP); SetInfo0($$,$1); SetInfo1($$,$2); SetInfo2($$,NULLCELL); SetInfo3($$,$4);
			SetLine($$,CellLine($1));
			ConvertExplist2Substlist($4);
			RegisterComponent(ComponentListTop, $$);
			FreeTcell($5); FreeTcell($6);
		}
	;

exp_list
	: exp T_COMMA exp_list
		{	/* N_EXPLIST info0=exp */
			$$ = MallocTcell(N_EXPLIST,(char *)NULL,CellLine($3)); SetInfo0($$,$1); SetNext($$,$3);
			FreeTcell($2);
		}
	| exp
		{	/* N_EXPLIST info0=exp */
			$$ = MallocTcell(N_EXPLIST,(char *)NULL,CellLine($1)); SetInfo0($$,$1); SetNext($$,NULLCELL);
		}
	;

map_list
	: map_item T_COMMA map_list
		{
			$$ = $1; SetNext($$,$3);
			FreeTcell($2);
		}
	| map_item
		{
			$$ = $1; SetNext($$,NULLCELL);
		}
	;

map_item
	: T_DOT T_ID T_LPAREN exp T_RPAREN
		{	/* N_MAPLIST info0=port, info1=exp */
			$$ = $1; SetType($$,N_MAPLIST); SetInfo0($$,$2); SetInfo1($$,$4);
			SetLine($$,CellLine($5));
			FreeTcell($3); FreeTcell($5);
		}
	;

assign_part
	: T_ASSIGN signame T_EQ exp T_SEMICOLON
		{	/* T_ASSIGN info0=sig, info1=exp */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$4);
			SetLine($$,CellLine($5));
			ChkEvalOutput($4, SigListTop);
			FreeTcell($3); FreeTcell($5);
		}
	| T_ASSIGN signame T_EQ condexp T_SEMICOLON
		{	/* T_ASSIGN info0=sig, info1=exp */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$4);
			SetLine($$,CellLine($5));
			ChkEvalOutput($4, SigListTop);
			FreeTcell($3); FreeTcell($5);
		}
	;

gate_buf
	: T_GATE_BUF T_ID
		{
			$$	= $1;
			FreeTcell($2);
		}
	| T_GATE_BUF
		{
			$$	= $1;
		}
	;

gate_not
	: T_GATE_NOT T_ID
		{
			$$	= $1;
			FreeTcell($2);
		}
	| T_GATE_NOT
		{
			$$	= $1;
		}
	;

gate_and
	: T_GATE_AND T_ID
		{
			$$	= $1;
			FreeTcell($2);
		}
	| T_GATE_AND
		{
			$$	= $1;
		}
	;

gate_or
	: T_GATE_OR T_ID
		{
			$$	= $1;
			FreeTcell($2);
		}
	| T_GATE_OR
		{
			$$	= $1;
		}
	;

gate_xor
	: T_GATE_XOR T_ID
		{
			$$	= $1;
			FreeTcell($2);
		}
	| T_GATE_XOR
		{
			$$	= $1;
		}
	;

gate_nand
	: T_GATE_NAND T_ID
		{
			$$	= $1;
			FreeTcell($2);
		}
	| T_GATE_NAND
		{
			$$	= $1;
		}
	;

gate_nor
	: T_GATE_NOR T_ID
		{
			$$	= $1;
			FreeTcell($2);
		}
	| T_GATE_NOR
		{
			$$	= $1;
		}
	;

gate_xnor
	: T_GATE_XNOR T_ID
		{
			$$	= $1;
			FreeTcell($2);
		}
	| T_GATE_XNOR
		{
			$$	= $1;
		}
	;

call_prim_gate
	: gate_buf T_LPAREN signame T_COMMA signame T_RPAREN T_SEMICOLON
		{	/* T_ASSIGN info0=sig, info1=exp */
			$$ = $1; SetType($$,T_ASSIGN); SetInfo0($$,$3); SetInfo1($$,$5);
			SetLine($$,CellLine($7));
			ChkEvalOutput(CellInfo1($$), SigListTop);
			FreeTcell($2); FreeTcell($4); FreeTcell($6); FreeTcell($7);
		}
	| gate_not T_LPAREN signame T_COMMA signame T_RPAREN T_SEMICOLON
		{	/* T_ASSIGN info0=sig, info1=exp */
			$$ = $1; SetType($$,T_ASSIGN); SetInfo0($$,$3);
			SetLine($$,CellLine($7));

			// connect all inputs to gate
			SetInfo1($$,MallocTcell(T_NOT,(char *)NULL,CellLine($7)));	// R-val: append new cell
			SetInfo0(CellInfo1($$),$5);

			ChkEvalOutput(CellInfo1($$), SigListTop);
			FreeTcell($2); FreeTcell($4); FreeTcell($6); FreeTcell($7);
		}
	| gate_and T_LPAREN signame T_COMMA signame T_COMMA parameter_list T_RPAREN T_SEMICOLON
		{	/* T_ASSIGN info0=sig, info1=exp */
			register TCELLPNT	src, dist, nextsrc;
			$$ = $1; SetType($$,T_ASSIGN); SetInfo0($$,$3);
			SetLine($$,CellLine($9));

			// connect all inputs to gate
			SetInfo1($$,MallocTcell(T_AND,(char *)NULL,CellLine($9)));	// R-val: append new cell
			SetInfo0(CellInfo1($$),$5);
			for (src = $7, dist = CellInfo1($$); src != NULLCELL; src = nextsrc) {
				nextsrc	= NextCell(src);
				if (NextCell(src) == NULLCELL) {	// last parameter
					SetInfo1(dist,CellInfo0(src));
					FreeTcell(src);					// (N_SIGLIST)
				}
				else {
					SetInfo1(dist,MallocTcell(T_AND,(char *)NULL,CellLine($9)));	// append new cell
					SetInfo0(CellInfo1(dist),CellInfo0(src));
					dist	= CellInfo1(dist);
					FreeTcell(src);					// (N_SIGLIST)
				}
			}

			ChkEvalOutput(CellInfo1($$), SigListTop);
			FreeTcell($2); FreeTcell($4); FreeTcell($6); FreeTcell($8); FreeTcell($9);
		}
	| gate_or T_LPAREN signame T_COMMA signame T_COMMA parameter_list T_RPAREN T_SEMICOLON
		{	/* T_ASSIGN info0=sig, info1=exp */
			register TCELLPNT	src, dist, nextsrc;
			$$ = $1; SetType($$,T_ASSIGN); SetInfo0($$,$3);
			SetLine($$,CellLine($9));

			// connect all inputs to gate
			SetInfo1($$,MallocTcell(T_OR,(char *)NULL,CellLine($9)));	// R-val: append new cell
			SetInfo0(CellInfo1($$),$5);
			for (src = $7, dist = CellInfo1($$); src != NULLCELL; src = nextsrc) {
				nextsrc	= NextCell(src);
				if (NextCell(src) == NULLCELL) {	// last parameter
					SetInfo1(dist,CellInfo0(src));
					FreeTcell(src);					// (N_SIGLIST)
				}
				else {
					SetInfo1(dist,MallocTcell(T_OR,(char *)NULL,CellLine($9)));	// append new cell
					SetInfo0(CellInfo1(dist),CellInfo0(src));
					dist	= CellInfo1(dist);
					FreeTcell(src);					// (N_SIGLIST)
				}
			}

			ChkEvalOutput(CellInfo1($$), SigListTop);
			FreeTcell($2); FreeTcell($4); FreeTcell($6); FreeTcell($8); FreeTcell($9);
		}
	| gate_xor T_LPAREN signame T_COMMA signame T_COMMA parameter_list T_RPAREN T_SEMICOLON
		{	/* T_ASSIGN info0=sig, info1=exp */
			register TCELLPNT	src, dist, nextsrc;
			$$ = $1; SetType($$,T_ASSIGN); SetInfo0($$,$3);
			SetLine($$,CellLine($9));

			// connect all inputs to gate
			SetInfo1($$,MallocTcell(T_XOR,(char *)NULL,CellLine($9)));	// R-val: append new cell
			SetInfo0(CellInfo1($$),$5);
			for (src = $7, dist = CellInfo1($$); src != NULLCELL; src = nextsrc) {
				nextsrc	= NextCell(src);
				if (NextCell(src) == NULLCELL) {	// last parameter
					SetInfo1(dist,CellInfo0(src));
					FreeTcell(src);					// (N_SIGLIST)
				}
				else {
					SetInfo1(dist,MallocTcell(T_XOR,(char *)NULL,CellLine($9)));	// append new cell
					SetInfo0(CellInfo1(dist),CellInfo0(src));
					dist	= CellInfo1(dist);
					FreeTcell(src);					// (N_SIGLIST)
				}
			}

			ChkEvalOutput(CellInfo1($$), SigListTop);
			FreeTcell($2); FreeTcell($4); FreeTcell($6); FreeTcell($8); FreeTcell($9);
		}
	| gate_nand T_LPAREN signame T_COMMA signame T_COMMA parameter_list T_RPAREN T_SEMICOLON
		{	/* T_ASSIGN info0=sig, info1=exp */
			register TCELLPNT	src, dist, nextsrc;
			$$ = $1; SetType($$,T_ASSIGN); SetInfo0($$,$3);
			SetLine($$,CellLine($9));

			// connect all inputs to gate
			// R-val: append new cell
			SetInfo1($$,MallocTcell(T_NOT,(char *)NULL,CellLine($9)));
			SetInfo0(CellInfo1($$),MallocTcell(N_PAREN,(char *)NULL,CellLine($9)));
			SetInfo0(CellInfo0(CellInfo1($$)),MallocTcell(T_AND,(char *)NULL,CellLine($9)));

			SetInfo0(CellInfo0(CellInfo0(CellInfo1($$))),$5);
			for (src = $7, dist = CellInfo0(CellInfo0(CellInfo1($$))); src != NULLCELL; src = nextsrc) {
				nextsrc	= NextCell(src);
				if (NextCell(src) == NULLCELL) {	// last parameter
					SetInfo1(dist,CellInfo0(src));
					FreeTcell(src);					// (N_SIGLIST)
				}
				else {
					SetInfo1(dist,MallocTcell(T_AND,(char *)NULL,CellLine($9)));	// append new cell
					SetInfo0(CellInfo1(dist),CellInfo0(src));
					dist	= CellInfo1(dist);
					FreeTcell(src);					// (N_SIGLIST)
				}
			}

			ChkEvalOutput(CellInfo1($$), SigListTop);
			FreeTcell($2); FreeTcell($4); FreeTcell($6); FreeTcell($8); FreeTcell($9);
		}
	| gate_nor T_LPAREN signame T_COMMA signame T_COMMA parameter_list T_RPAREN T_SEMICOLON
		{	/* T_ASSIGN info0=sig, info1=exp */
			register TCELLPNT	src, dist, nextsrc;
			$$ = $1; SetType($$,T_ASSIGN); SetInfo0($$,$3);
			SetLine($$,CellLine($9));

			// connect all inputs to gate
			// R-val: append new cell
			SetInfo1($$,MallocTcell(T_NOT,(char *)NULL,CellLine($9)));
			SetInfo0(CellInfo1($$),MallocTcell(N_PAREN,(char *)NULL,CellLine($9)));
			SetInfo0(CellInfo0(CellInfo1($$)),MallocTcell(T_OR,(char *)NULL,CellLine($9)));

			SetInfo0(CellInfo0(CellInfo0(CellInfo1($$))),$5);
			for (src = $7, dist = CellInfo0(CellInfo0(CellInfo1($$))); src != NULLCELL; src = nextsrc) {
				nextsrc	= NextCell(src);
				if (NextCell(src) == NULLCELL) {	// last parameter
					SetInfo1(dist,CellInfo0(src));
					FreeTcell(src);					// (N_SIGLIST)
				}
				else {
					SetInfo1(dist,MallocTcell(T_OR,(char *)NULL,CellLine($9)));	// append new cell
					SetInfo0(CellInfo1(dist),CellInfo0(src));
					dist	= CellInfo1(dist);
					FreeTcell(src);					// (N_SIGLIST)
				}
			}

			ChkEvalOutput(CellInfo1($$), SigListTop);
			FreeTcell($2); FreeTcell($4); FreeTcell($6); FreeTcell($8); FreeTcell($9);
		}
	| gate_xnor T_LPAREN signame T_COMMA signame T_COMMA parameter_list T_RPAREN T_SEMICOLON
		{	/* T_ASSIGN info0=sig, info1=exp */
			register TCELLPNT	src, dist, nextsrc;
			$$ = $1; SetType($$,T_ASSIGN); SetInfo0($$,$3);
			SetLine($$,CellLine($9));

			// connect all inputs to gate
			// R-val: append new cell
			SetInfo1($$,MallocTcell(T_NOT,(char *)NULL,CellLine($9)));
			SetInfo0(CellInfo1($$),MallocTcell(N_PAREN,(char *)NULL,CellLine($9)));
			SetInfo0(CellInfo0(CellInfo1($$)),MallocTcell(T_XOR,(char *)NULL,CellLine($9)));

			SetInfo0(CellInfo0(CellInfo0(CellInfo1($$))),$5);
			for (src = $7, dist = CellInfo0(CellInfo0(CellInfo1($$))); src != NULLCELL; src = nextsrc) {
				nextsrc	= NextCell(src);
				if (NextCell(src) == NULLCELL) {	// last parameter
					SetInfo1(dist,CellInfo0(src));
					FreeTcell(src);					// (N_SIGLIST)
				}
				else {
					SetInfo1(dist,MallocTcell(T_XOR,(char *)NULL,CellLine($9)));	// append new cell
					SetInfo0(CellInfo1(dist),CellInfo0(src));
					dist	= CellInfo1(dist);
					FreeTcell(src);					// (N_SIGLIST)
				}
			}

			ChkEvalOutput(CellInfo1($$), SigListTop);
			FreeTcell($2); FreeTcell($4); FreeTcell($6); FreeTcell($8); FreeTcell($9);
		}
	;

condexp
	: exp T_SEL exp T_COLON exp
		{	/* T_SEL info0=cond, info1=then, info2=else */
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3); SetInfo2($$,$5);
			ConvertParen2Dummy($$);
			SetLine($$,CellLine($5));
			FreeTcell($4);
		}
	| exp T_SEL condexp T_COLON exp
		{	/* T_SEL info0=cond, info1=then, info2=else */
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3); SetInfo2($$,$5);
			ConvertParen2Dummy($$);
			SetLine($$,CellLine($5));
			FreeTcell($4);
		}
	| exp T_SEL exp T_COLON condexp
		{	/* T_SEL info0=cond, info1=then, info2=else */
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3); SetInfo2($$,$5);
			ConvertParen2Dummy($$);
			SetLine($$,CellLine($5));
			FreeTcell($4);
		}
	| exp T_SEL condexp T_COLON condexp
		{	/* T_SEL info0=cond, info1=then, info2=else */
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3); SetInfo2($$,$5);
			ConvertParen2Dummy($$);
			SetLine($$,CellLine($5));
			FreeTcell($4);
		}
	| T_LPAREN condexp T_RPAREN
		{	/* N_PAREN info0=exp */
			$$ = $1; SetType($$,N_PAREN); SetInfo0($$,$2);
			SetLine($$,CellLine($3));
			FreeTcell($3);
		}
	;

always_part
	: T_ALWAYS T_AT T_LPAREN sensitivity_list_comb T_RPAREN T_BEGIN always_bodies T_END
		{	/* N_ALWAYS_COMB info0=sensitivity, info1=body */
			$$ = $1; SetType($$,N_ALWAYS_COMB); SetInfo0($$,$4); SetInfo1($$,$7);
			SetLine($$,CellLine($1));
			FreeTcell($2); FreeTcell($3); FreeTcell($5); FreeTcell($6); FreeTcell($8);
		}
	| T_ALWAYS T_AT T_LPAREN sensitivity_list_comb T_RPAREN T_BEGIN always_body T_END
		{	/* N_ALWAYS_COMB info0=sensitivity, info1=body */
			$$ = $1; SetType($$,N_ALWAYS_COMB); SetInfo0($$,$4); SetInfo1($$,$7);
			SetLine($$,CellLine($1));
			FreeTcell($2); FreeTcell($3); FreeTcell($5); FreeTcell($6); FreeTcell($8);
		}
	| T_ALWAYS T_AT T_LPAREN sensitivity_list_comb T_RPAREN always_body
		{	/* N_ALWAYS_COMB info0=sensitivity, info1=body */
			$$ = $1; SetType($$,N_ALWAYS_COMB); SetInfo0($$,$4); SetInfo1($$,$6);
			SetLine($$,CellLine($1));
			FreeTcell($2); FreeTcell($3); FreeTcell($5);
		}

	| T_ALWAYS T_AT T_LPAREN sensitivity_item_seq T_RPAREN T_BEGIN always_bodies T_END
		{	/* N_ALWAYS_SYNCSEQ info0=sensitivity(clk), info1=body */
			$$ = $1; SetType($$,N_ALWAYS_SYNCSEQ); SetInfo0($$,$4); SetInfo1($$,$7);
			SetLine($$,CellLine($1));
			FreeTcell($2); FreeTcell($3); FreeTcell($5); FreeTcell($6); FreeTcell($8);
		}
	| T_ALWAYS T_AT T_LPAREN sensitivity_item_seq T_RPAREN T_BEGIN always_body T_END
		{	/* N_ALWAYS_SYNCSEQ info0=sensitivity(clk), info1=body */
			$$ = $1; SetType($$,N_ALWAYS_SYNCSEQ); SetInfo0($$,$4); SetInfo1($$,$7);
			SetLine($$,CellLine($1));
			FreeTcell($2); FreeTcell($3); FreeTcell($5); FreeTcell($6); FreeTcell($8);
		}
	| T_ALWAYS T_AT T_LPAREN sensitivity_item_seq T_RPAREN always_body
		{	/* N_ALWAYS_SYNCSEQ info0=sensitivity(clk), info1=body */
			$$ = $1; SetType($$,N_ALWAYS_SYNCSEQ); SetInfo0($$,$4); SetInfo1($$,$6);
			SetLine($$,CellLine($1));
			FreeTcell($2); FreeTcell($3); FreeTcell($5);
		}

	| T_ALWAYS T_AT T_LPAREN sensitivity_item_seq T_GATE_OR sensitivity_item_seq T_RPAREN T_BEGIN always_bodies T_END
		{	/* N_ALWAYS_ASYNCSEQ info0=sensitivity(clk), info1=body, info2=sensitivity(reset) */
			$$ = $1; SetType($$,N_ALWAYS_ASYNCSEQ); SetInfo1($$,$9);
			if (UsedSignalinIfCond($9,CellInfo0($4)) == False && UsedSignalinIfCond($9,CellInfo0($6)) == True) {
				SetInfo0($$,$4); 	// clk
				SetInfo2($$,$6);	// reset
			}
			else if (UsedSignalinIfCond($9,CellInfo0($4)) == True && UsedSignalinIfCond($9,CellInfo0($6)) == False) {
				SetInfo0($$,$6); 	// clk
				SetInfo2($$,$4);	// reset
			}
			else {
				yyerror("$Can not determine clock signal.");
			}

			if (CellType($9) == T_IF) {
				CellType($9) = N_ASYNCRST_IF;
				if (CellInfo2($9) != NULLCELL && CellType(CellInfo2($9)) == T_ELSIF)
					CellType(CellInfo2($9)) = T_IF;
			}
			else {
				yyerror("$Unsupported type of description for async reset.");
			}

			SetLine($$,CellLine($1));
			FreeTcell($2); FreeTcell($3); FreeTcell($5); FreeTcell($7); FreeTcell($8); FreeTcell($10);
		}
	| T_ALWAYS T_AT T_LPAREN sensitivity_item_seq T_GATE_OR sensitivity_item_seq T_RPAREN T_BEGIN always_body T_END
		{	/* N_ALWAYS_ASYNCSEQ info0=sensitivity(clk), info1=body, info2=sensitivity(reset) */
			$$ = $1; SetType($$,N_ALWAYS_ASYNCSEQ); SetInfo1($$,$9);
			if (UsedSignalinIfCond($9,CellInfo0($4)) == False && UsedSignalinIfCond($9,CellInfo0($6)) == True) {
				SetInfo0($$,$4); 	// clk
				SetInfo2($$,$6);	// reset
			}
			else if (UsedSignalinIfCond($9,CellInfo0($4)) == True && UsedSignalinIfCond($9,CellInfo0($6)) == False) {
				SetInfo0($$,$6); 	// clk
				SetInfo2($$,$4);	// reset
			}
			else {
				yyerror("$Can not determine clock signal.");
			}

			if (CellType($9) == T_IF) {
				CellType($9) = N_ASYNCRST_IF;
				if (CellInfo2($9) != NULLCELL && CellType(CellInfo2($9)) == T_ELSIF)
					CellType(CellInfo2($9)) = T_IF;
			}
			else {
				yyerror("$Unsupported type of description for async reset.");
			}

			SetLine($$,CellLine($1));
			FreeTcell($2); FreeTcell($3); FreeTcell($5); FreeTcell($7); FreeTcell($8); FreeTcell($10);
		}
	| T_ALWAYS T_AT T_LPAREN sensitivity_item_seq T_GATE_OR sensitivity_item_seq T_RPAREN always_body
		{	/* N_ALWAYS_ASYNCSEQ info0=sensitivity(clk), info1=body, info2=sensitivity(reset) */
			$$ = $1; SetType($$,N_ALWAYS_ASYNCSEQ); SetInfo1($$,$8);
			if (UsedSignalinIfCond($8,CellInfo0($4)) == False && UsedSignalinIfCond($8,CellInfo0($6)) == True) {
				SetInfo0($$,$4); 	// clk
				SetInfo2($$,$6);	// reset
			}
			else if (UsedSignalinIfCond($8,CellInfo0($4)) == True && UsedSignalinIfCond($8,CellInfo0($6)) == False) {
				SetInfo0($$,$6); 	// clk
				SetInfo2($$,$4);	// reset
			}
			else {
				yyerror("$Can not determine clock signal.");
			}

			if (CellType($8) == T_IF) {
				CellType($8) = N_ASYNCRST_IF;
				if (CellInfo2($8) != NULLCELL && CellType(CellInfo2($8)) == T_ELSIF)
					CellType(CellInfo2($8)) = T_IF;
			}
			else {
				yyerror("$Unsupported type of description for async reset.");
			}

			SetLine($$,CellLine($1));
			FreeTcell($2); FreeTcell($3); FreeTcell($5); FreeTcell($7);
		}
	;

sensitivity_list_comb
	: sensitivity_item_comb T_GATE_OR sensitivity_list_comb
		{
			$$ = $1; SetNext($$,$3);
			FreeTcell($2);
		}
	| sensitivity_item_comb
		{
			$$ = $1; SetNext($$,NULLCELL);
		}
	;

sensitivity_item_comb
	: signame
		{	/* N_SENSLIST info0=sig */
			$$ = MallocTcell(N_SENSLIST,(char *)NULL,CellLine($1)); SetInfo0($$,$1);
		}
	;

sensitivity_item_seq
	: T_POSEDGE signame
		{	/* T_POSEDGE info0=sig */
			$$ = $1; SetInfo0($$,$2);
		}
	| T_NEGEDGE signame
		{	/* T_NEGEDGE info0=sig */
			$$ = $1; SetInfo0($$,$2);
		}
	;

always_bodies
	: always_body always_bodies
		{
			$$ = $1; SetNext($$,$2);
		}
	| always_body always_body
		{
			$$ = $1; SetNext($$,$2); SetNext($2,NULLCELL);
		}
	;

always_body
	: subst_part
		{
			$$ = $1; SetNext($$,NULLCELL);
		}
	| if_part
		{
			$$ = $1; SetNext($$,NULLCELL);
		}
	| case_part
		{
			$$ = $1; SetNext($$,NULLCELL);
		}
	| call_func_or_task T_SEMICOLON
		{	/* N_TASKCALL info0=list */
			$$ = $1; SetType($$,N_TASKCALL); SetNext($$,NULLCELL);
		}
	;

subst_part
	: signame T_GE exp T_SEMICOLON
		{	/* N_SIGSUBST info0=sig, info1=exp */
			$$ = $2; SetType($$,N_SIGSUBST); SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($4));
			ChkEvalOutput($3, SigListTop);
			FreeTcell($4);
		}
	| signame T_EQ exp T_SEMICOLON
		{	/* N_VARSUBST info0=sig, info1=exp */
			$$ = $2; SetType($$,N_VARSUBST); SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($4));
			ChkEvalOutput($3, SigListTop);
			FreeTcell($4);
		}
	| signame T_GE condexp T_SEMICOLON
		{	/* covert to T_IF */
			ChkEvalOutput($3, SigListTop);

			ConvertCondexp2If($3, N_SIGSUBST, $1);
			$$ = $3;
			SetLine($$,CellLine($4));
			FreeTree($1); FreeTcell($2); FreeTcell($4);
		}
	| signame T_EQ condexp T_SEMICOLON
		{	/* convert to T_IF */
			ChkEvalOutput($3, SigListTop);

			ConvertCondexp2If($3, N_VARSUBST, $1);
			$$ = $3;
			SetLine($$,CellLine($4));
			FreeTree($1); FreeTcell($2); FreeTcell($4);
		}
	;

if_part
	: T_IF exp T_BEGIN always_bodies T_END else_part
		{	/* T_IF info0=cond, info1=then, info2=else */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$4); SetInfo2($$,$6);
			SetLine($$,CellLine($1));
			ChkEvalOutput($2, SigListTop);
			FreeTcell($3); FreeTcell($5);
			if (CellType($6) == T_IF)
				CellType($6) = T_ELSIF;
		}
	| T_IF exp T_BEGIN always_body T_END else_part
		{	/* T_IF info0=cond, info1=then, info2=else */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$4); SetInfo2($$,$6);
			SetLine($$,CellLine($1));
			ChkEvalOutput($2, SigListTop);
			FreeTcell($3); FreeTcell($5);
			if (CellType($6) == T_IF)
				CellType($6) = T_ELSIF;
		}
	| T_IF exp always_body else_part
		{	/* T_IF info0=cond, info1=then, info2=else */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$3); SetInfo2($$,$4);
			SetLine($$,CellLine($1));
			ChkEvalOutput($2, SigListTop);
			if (CellType($4) == T_IF)
				CellType($4) = T_ELSIF;
		}
	| T_IF exp T_BEGIN always_bodies T_END
		{	/* T_IF info0=cond, info1=then, info2=else(NULL) */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$4); SetInfo2($$,NULLCELL);
			SetLine($$,CellLine($1));
			ChkEvalOutput($2, SigListTop);
			FreeTcell($3); FreeTcell($5);
		}
	| T_IF exp T_BEGIN always_body T_END
		{	/* T_IF info0=cond, info1=then, info2=else(NULL) */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$4); SetInfo2($$,NULLCELL);
			SetLine($$,CellLine($1));
			ChkEvalOutput($2, SigListTop);
			FreeTcell($3); FreeTcell($5);
		}
	| T_IF exp always_body
		{	/* T_IF info0=cond, info1=then, info2=else(NULL) */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$3); SetInfo2($$,NULLCELL);
			SetLine($$,CellLine($1));
			ChkEvalOutput($2, SigListTop);
		}

	| T_IF exp T_BEGIN T_END else_part
		{	/* T_IF info0=cond, info1=then (N_NULL), info2=else */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,MallocTcell(N_NULL,NULLSTR,CellLine($4))); SetInfo2($$,$5);
			SetLine($$,CellLine($1));
			ChkEvalOutput($2, SigListTop);
			FreeTcell($3); FreeTcell($4);
			if (CellType($5) == T_IF)
				CellType($5) = T_ELSIF;
		}
	| T_IF exp else_part
		{	/* T_IF info0=cond, info1=then (N_NULL), info2=else */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,MallocTcell(N_NULL,NULLSTR,CellLine($2))); SetInfo2($$,$3);
			SetLine($$,CellLine($1));
			ChkEvalOutput($2, SigListTop);
			if (CellType($3) == T_IF)
				CellType($3) = T_ELSIF;
		}
	| T_IF exp T_BEGIN T_END
		{	/* T_IF info0=cond, info1=then (N_NULL), info2=else(NULL) */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,MallocTcell(N_NULL,NULLSTR,CellLine($4))); SetInfo2($$,NULLCELL);
			SetLine($$,CellLine($1));
			ChkEvalOutput($2, SigListTop);
			FreeTcell($3); FreeTcell($4);
		}
	;

else_part
	: T_ELSE T_BEGIN always_bodies T_END
		{	
			$$ = $3;
			SetLine($$,CellLine($1));
			FreeTcell($1); FreeTcell($2); FreeTcell($4);
		}
	| T_ELSE T_BEGIN always_body T_END
		{	
			$$ = $3;
			SetLine($$,CellLine($1));
			FreeTcell($1); FreeTcell($2); FreeTcell($4);
		}
	| T_ELSE always_body
		{	
			$$ = $2;
			FreeTcell($1);
		}

	| T_ELSE T_BEGIN T_END
		{	
			$$ = MallocTcell(N_NULL,NULLSTR,CellLine($1));
			FreeTcell($1); FreeTcell($2); FreeTcell($3);
		}
	;

case_part
	: T_CASE signame caseitem_top T_ENDCASE
		{	/* T_CASE info0=exp, info1=item  */
			$$ = $1; SetInfo0($$,$2); SetInfo1($$,$3);
			SetLine($$,CellLine($1));
			ChkEvalOutput($2, SigListTop);
			FreeTcell($4);
		}
	;

caseitem_top
	: caseitem caseitem_top
		{
			$$ = $1; SetNext($$,$2);
		}
	| caseitem
		{
			$$ = $1; SetNext($$,NULLCELL);
		}
	;

caseitem
	: literal T_COLON T_BEGIN always_bodies T_END
		{	/* N_CASECOND info0=cond exp, info1=body  */
			$$ = $2; SetType($$,N_CASECOND); SetInfo0($$,$1); SetInfo1($$,$4);
			SetLine($$,CellLine($1));
			FreeTcell($3); FreeTcell($5);
		}
	| literal T_COLON T_BEGIN always_body T_END
		{	/* N_CASECOND info0=cond exp, info1=body  */
			$$ = $2; SetType($$,N_CASECOND); SetInfo0($$,$1); SetInfo1($$,$4);
			SetLine($$,CellLine($1));
			FreeTcell($3); FreeTcell($5);
		}
	| literal T_COLON always_body
		{	/* N_CASECOND info0=cond exp, info1=body  */
			$$ = $2; SetType($$,N_CASECOND); SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($1));
		}
	| T_DEFAULT T_COLON T_BEGIN always_bodies T_END
		{	/* N_CASECOND info0=cond exp, info1=body  */
			$$ = $2; SetType($$,N_CASECOND); SetInfo0($$,$1); SetInfo1($$,$4);
			SetLine($$,CellLine($1));
			FreeTcell($3); FreeTcell($5);
		}
	| T_DEFAULT T_COLON T_BEGIN always_body T_END
		{	/* N_CASECOND info0=cond exp, info1=body  */
			$$ = $2; SetType($$,N_CASECOND); SetInfo0($$,$1); SetInfo1($$,$4);
			SetLine($$,CellLine($1));
			FreeTcell($3); FreeTcell($5);
		}
	| T_DEFAULT T_COLON always_body
		{	/* N_CASECOND info0=cond exp, info1=body  */
			$$ = $2; SetType($$,N_CASECOND); SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($1));
		}


	| literal T_COLON T_BEGIN T_END
		{	/* N_CASECOND info0=cond exp, info1=body (N_NULL) */
			$$ = $2; SetType($$,N_CASECOND); SetInfo0($$,$1); SetInfo1($$,MallocTcell(N_NULL,NULLSTR,CellLine($4)));
			SetLine($$,CellLine($1));
			FreeTcell($3); FreeTcell($4);
		}
	| literal T_COLON
		{	/* N_CASECOND info0=cond exp, info1=body (N_NULL) */
			$$ = $2; SetType($$,N_CASECOND); SetInfo0($$,$1); SetInfo1($$,MallocTcell(N_NULL,NULLSTR,CellLine($2)));
			SetLine($$,CellLine($1));
		}
	| T_DEFAULT T_COLON T_BEGIN T_END
		{	/* N_CASECOND info0=cond exp, info1=body (N_NULL) */
			$$ = $2; SetType($$,N_CASECOND); SetInfo0($$,$1); SetInfo1($$,MallocTcell(N_NULL,NULLSTR,CellLine($4)));
			SetLine($$,CellLine($1));
			FreeTcell($3); FreeTcell($4);
		}
	| T_DEFAULT T_COLON
		{	/* N_CASECOND info0=cond exp, info1=body (N_NULL) */
			$$ = $2; SetType($$,N_CASECOND); SetInfo0($$,$1); SetInfo1($$,MallocTcell(N_NULL,NULLSTR,CellLine($2)));
			SetLine($$,CellLine($1));
		}
	;

function_part
	: T_FUNC sigwidth T_ID T_SEMICOLON decralations T_BEGIN always_bodies T_END T_ENDFUNC
		{	/* N_FUNCDEF info0=ID, info1=out width, info2=decralations, info3=body */
			$$ = $1; SetType($$,N_FUNCDEF); SetInfo0($$,$3); SetInfo1($$,$2); SetInfo2($$,$5); SetInfo3($$,$7);
			SetLine($$,CellLine($1));
			FreeTcell($4); FreeTcell($6); FreeTcell($8); FreeTcell($9);
		}
	| T_FUNC sigwidth T_ID T_SEMICOLON decralations T_BEGIN always_body T_END T_ENDFUNC
		{	/* N_FUNCDEF info0=ID, info1=out width, info2=decralations, info3=body */
			$$ = $1; SetType($$,N_FUNCDEF); SetInfo0($$,$3); SetInfo1($$,$2); SetInfo2($$,$5); SetInfo3($$,$7);
			SetLine($$,CellLine($1));
			FreeTcell($4); FreeTcell($6); FreeTcell($8); FreeTcell($9);
		}
	| T_FUNC sigwidth T_ID T_SEMICOLON decralations always_body T_ENDFUNC
		{	/* N_FUNCDEF info0=ID, info1=out width, info2=decralations, info3=body */
			$$ = $1; SetType($$,N_FUNCDEF); SetInfo0($$,$3); SetInfo1($$,$2); SetInfo2($$,$5); SetInfo3($$,$6);
			SetLine($$,CellLine($1));
			FreeTcell($4); FreeTcell($7);
		}
	| T_FUNC T_ID T_SEMICOLON decralations T_BEGIN always_bodies T_END T_ENDFUNC
		{	/* N_FUNCDEF info0=ID, info1=out width(NULL), info2=decralations, info3=body */
			$$ = $1; SetType($$,N_FUNCDEF); SetInfo0($$,$2); SetInfo1($$,NULLCELL); SetInfo2($$,$4); SetInfo3($$,$6);
			SetLine($$,CellLine($1));
			FreeTcell($3); FreeTcell($5); FreeTcell($7); FreeTcell($8);
		}
	| T_FUNC T_ID T_SEMICOLON decralations T_BEGIN always_body T_END T_ENDFUNC
		{	/* N_FUNCDEF info0=ID, info1=out width(NULL), info2=decralations, info3=body */
			$$ = $1; SetType($$,N_FUNCDEF); SetInfo0($$,$2); SetInfo1($$,NULLCELL); SetInfo2($$,$4); SetInfo3($$,$6);
			SetLine($$,CellLine($1));
			FreeTcell($3); FreeTcell($5); FreeTcell($7); FreeTcell($8);
		}
	| T_FUNC T_ID T_SEMICOLON decralations always_body T_ENDFUNC
		{	/* N_FUNCDEF info0=ID, info1=out width(NULL), info2=decralations, info3=body */
			$$ = $1; SetType($$,N_FUNCDEF); SetInfo0($$,$2); SetInfo1($$,NULLCELL); SetInfo2($$,$4); SetInfo3($$,$5);
			SetLine($$,CellLine($1));
			FreeTcell($3); FreeTcell($6);
		}
	;

task_part
	: T_TASK T_ID T_SEMICOLON decralations T_BEGIN always_bodies T_END T_ENDTASK
		{	/* N_TASKDEF info0=ID, info1=decralations, info2=body */
			$$ = $1; SetType($$,N_TASKDEF); SetInfo0($$,$2); SetInfo1($$,$4); SetInfo2($$,$6);
			SetLine($$,CellLine($1));
			FreeTcell($3); FreeTcell($5); FreeTcell($7); FreeTcell($8);
		}
	| T_TASK T_ID T_SEMICOLON decralations T_BEGIN always_body T_END T_ENDTASK
		{	/* N_TASKDEF info0=ID, info1=decralations, info2=body */
			$$ = $1; SetType($$,N_TASKDEF); SetInfo0($$,$2); SetInfo1($$,$4); SetInfo2($$,$6);
			SetLine($$,CellLine($1));
			FreeTcell($3); FreeTcell($5); FreeTcell($7); FreeTcell($8);
		}
	| T_TASK T_ID T_SEMICOLON decralations always_body T_ENDTASK
		{	/* N_TASKDEF info0=ID, info1=decralations, info2=body */
			$$ = $1; SetType($$,N_TASKDEF); SetInfo0($$,$2); SetInfo1($$,$4); SetInfo2($$,$5);
			SetLine($$,CellLine($1));
			FreeTcell($3); FreeTcell($6);
		}
	;

exp
	: exp T_LOGIC_OR exp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| logicandexp
		{
			$$ = $1;
		}
	;

logicandexp
	: logicandexp T_LOGIC_AND logicandexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| orexp
		{
			$$ = $1;
		}
	;

orexp
	: orexp T_OR orexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| andexp
		{
			$$ = $1;
		}
	;

andexp
	: andexp T_AND andexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| andexp T_XOR andexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| eqexp
		{
			$$ = $1;
		}
	;

eqexp
	: eqexp T_LOGIC_EQ eqexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| eqexp T_LOGIC_NEQ eqexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| compexp
		{
			$$ = $1;
		}
	;

compexp
	: compexp T_GE compexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| compexp T_LE compexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| compexp T_GT compexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| compexp T_LS compexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| shiftexp
		{
			$$ = $1;
		}
	;

shiftexp
	: shiftexp T_RSHIFT shiftexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| shiftexp T_LSHIFT shiftexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| addexp
		{
			$$ = $1;
		}
	;

addexp
	: addexp T_PLUS addexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| addexp T_MINUS addexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| T_MINUS T_DECDIGIT	%prec N_UMINUS
		{
			char	*newstr;
			$$ = $2; FreeTcell($1);
			newstr = (char *)malloc(sizeof (char) * (strlen(CellStr($2)) + 2));
			sprintf(newstr, "-%s", CellStr($2));
			free(CellStr($2));
			CellStr($2) = newstr;
		}
	| T_PLUS T_DECDIGIT		%prec N_UPLUS
		{
			$$ = $2; FreeTcell($1);
		}
	| multexp
		{
			$$ = $1;
		}
	;

multexp
	: multexp T_MULT multexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| multexp T_DIV multexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| multexp T_MOD multexp
		{
			$$ = $2; SetInfo0($$,$1); SetInfo1($$,$3);
			SetLine($$,CellLine($3));
		}
	| notexp
		{
			$$ = $1;
		}
	;

notexp
	: T_NOT literal
		{
			$$ = $1; SetInfo0($$,$2);
			SetLine($$,CellLine($2));
		}
	| T_LOGIC_NOT literal
		{
			$$ = $1; SetInfo0($$,$2);
			SetLine($$,CellLine($2));
		}
	| T_AND literal
		{	/* N_REDUCTION_AND info0=exp */
			$$ = $1; SetType($$,N_REDUCTION_AND); SetInfo0($$,$2);
			SetLine($$,CellLine($2));
		}
	| T_OR literal
		{	/* N_REDUCTION_OR info0=exp */
			$$ = $1; SetType($$,N_REDUCTION_OR); SetInfo0($$,$2);
			SetLine($$,CellLine($2));
		}
	| T_XOR literal
		{	/* N_REDUCTION_XOR info0=exp */
			$$ = $1; SetType($$,N_REDUCTION_XOR); SetInfo0($$,$2);
			SetLine($$,CellLine($2));
		}
	| literal
		{
			$$ = $1;
		}
	;

literal
	: T_WIDTH_BINDIGIT
		{
			$$ = $1;
		}
	| T_WIDTH_DECDIGIT
		{
			$$ = $1;
		}
	| T_WIDTH_HEXDIGIT
		{
			$$ = $1;
		}
	| T_DECDIGIT
		{
			$$ = $1;
		}
	| signame
		{
			$$ = $1;
		}
	| T_LBRACE exp T_LBRACE exp T_RBRACE T_RBRACE
		{	/* N_COPYSIG info0=copy times, info1=expr */
			$$ = $1; SetType($$,N_COPYSIG); SetInfo0($$,$2); SetInfo1($$,$4);
			SetLine($$,CellLine($6));
			FreeTcell($3); FreeTcell($5); FreeTcell($6);
		}
	| T_LPAREN exp T_RPAREN
		{	/* N_PAREN info0=exp */
			$$ = $1; SetType($$,N_PAREN); SetInfo0($$,$2);
			SetLine($$,CellLine($3));
			FreeTcell($3);
		}
	| call_func_or_task
		{	/* N_FUNCCALL info0=list */
			$$ = $1; SetType($$,N_FUNCCALL);
		}
	;

signame
	: T_ID
		{	/* N_SIG */
			$$ = $1; SetType($$,N_SIG);
		}
	| T_ID sigwidth
		{	/* N_SIG_WIDTH info0=width */
			$$ = $1; SetType($$,N_SIG_WIDTH); SetInfo0($$,$2);
			SetLine($$,CellLine($2));
		}
	| T_LBRACE exp_list T_RBRACE
		{	/* N_CONCAT info0=list */
			$$ = $1; SetType($$,N_CONCAT); SetInfo0($$,$2);
			SetLine($$,CellLine($3));
			FreeTcell($3);
		}
	| T_LPAREN signame T_RPAREN
		{
			$$ = $2;
			SetLine($$,CellLine($3));
			FreeTcell($1); FreeTcell($3);
		}
	;

%%

void fprintfError(char *s, int line)	// (unuse yylexlinenum for line number)
{
	ParseError = True;
	fprintf(stderr, "ERROR: in line %d, %s\n", line, s);
}

void yyerror(char *s)
{
	ParseError = True;
	if (*s == '$')
		fprintf(stderr, "ERROR: in line %d, %s\n", yylexlinenum, s + 1);
	else
		fprintf(stderr, "ERROR: parse error in line %d\n", yylexlinenum);
    return;
}

// end of file
