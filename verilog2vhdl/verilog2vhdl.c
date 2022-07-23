/////////////////////////////////////////////////////////////////////////////////////////
// Verilog2VHDL
// Copyright(c)2004 S.Morioka
// Contact: http://www02.so-net.ne.jp/~morioka/v2v.htm
//
// verilog2vhdl.c	Verilog2VHDL main
//
// 1. Ver1.00(2004/05/06)		Original code is written by S.Morioka
//
/////////////////////////////////////////////////////////////////////////////////////////

#include "verilog2vhdl.h"

#define TMPFNAME	"verilog2vhdl.tmp"
#define PASS1_FNAME	"___v2v_vhdl_tmp___.txt"

#define max(x,y)	(((x) > (y)) ? (x) : (y))
#define topchar(s)	(*(s))
#define lastchar(s)	(*(s + (strlen(s) - 1)))

int				Sysid;
TCELLPNT		ParseTreeTop;
Boolean			ParseError;
SIGLIST			*SigListTop;
COMMENTLIST		*CommentListTop;
COMPONENTLIST	*ComponentListTop;
LINELIST		*LineListTop;


//////////////////////////////////////////////////////////////////////
// Memory allocation/free
//////////////////////////////////////////////////////////////////////

///////////////////////// TREECELL type (parse tree NODE) /////////////////////////

// malloc NODE
// parameter:
// 		typ:		Type of token
// 		str:		ID or number value (if exists)
// 		linenum:	HDL line number
// return:	Node cell
TCELLPNT MallocTcell(int typ, char *str, int linenum)
{
	register TCELLPNT	retval;
	if ((retval = (TCELLPNT)malloc(sizeof (TREECELL))) == NULLCELL) {
		fprintf(stderr, "INTERNAL ERROR: malloc failure in MallocTcell()\n");
		return (NULLCELL);
	}

	CellLine(retval)	= linenum;
	CellType(retval)	= typ;
	if (str == NULLSTR)
		CellStr(retval)	= NULLSTR;
	else {
		if ((CellStr(retval) = (char *)malloc(strlen(str) + 1)) == NULLSTR) {
			fprintf(stderr, "INTERNAL ERROR: malloc failure in MallocTcell()\n");
			free(retval);
			return (NULLCELL);
		}
		else
			strcpy(CellStr(retval), str);
	}

	SetNext(retval, NULLCELL);
	SetInfo0(retval, NULLCELL);
	SetInfo1(retval, NULLCELL);
	SetInfo2(retval, NULLCELL);
	SetInfo3(retval, NULLCELL);
	SetInfo4(retval, NULLCELL);
	SetInfo5(retval, NULLCELL);
	SetSig(retval, NULLSIG);
	SetCmp(retval, NULLCMP);

	return (retval);
}

// Copy NODE
// parameter:
//		src:	node cell
// return:	copy of the cell	(NOTE: signal list and component list are NOT copied)
TCELLPNT CopyTcell(TCELLPNT src)
{
	register TCELLPNT	retval;
	if (src == NULLCELL)
		return (NULLCELL);

	if ((retval = (TCELLPNT)malloc(sizeof (TREECELL))) == NULLCELL) {
		fprintf(stderr, "INTERNAL ERROR: malloc failure in CopyTcell()\n");
		return (NULLCELL);
	}

	CellLine(retval)	= CellLine(src);
	CellType(retval)	= CellType(src);

	if (CellStr(src) == NULLSTR)
		CellStr(retval)	= NULLSTR;
	else {
		if ((CellStr(retval) = (char *)malloc(strlen(CellStr(src)) + 1)) == NULLSTR) {
			fprintf(stderr, "INTERNAL ERROR: malloc failure in CopyTcell()\n");
			free(retval);
			return (NULLCELL);
		}
		else
			strcpy(CellStr(retval), CellStr(src));
	}

	NextCell(retval)	= NextCell(src);
	CellInfo0(retval)	= CellInfo0(src);
	CellInfo1(retval)	= CellInfo1(src);
	CellInfo2(retval)	= CellInfo2(src);
	CellInfo3(retval)	= CellInfo3(src);
	CellInfo4(retval)	= CellInfo4(src);
	CellInfo5(retval)	= CellInfo5(src);

	CellSig(retval)		= NULLSIG;	// CAUTION: uncopy signal list
	CellCmp(retval)		= NULLCMP;	// CAUTION: uncopy component list

	return (retval);
}

// Copy parse tree
// parameter:
//		src:	node cell
// return:	copy of the tree	(NOTE: signal list and component list are NOT copied)
TCELLPNT CopyTree(TCELLPNT src)
{
	register TCELLPNT	retval;
	if (src == NULLCELL)
		return (NULLCELL);

	retval	= CopyTcell(src);
	if (retval != NULLCELL) {
		CellInfo0(retval)	= CopyTree(CellInfo0(src));
		CellInfo1(retval)	= CopyTree(CellInfo1(src));
		CellInfo2(retval)	= CopyTree(CellInfo2(src));
		CellInfo3(retval)	= CopyTree(CellInfo3(src));
		CellInfo4(retval)	= CopyTree(CellInfo4(src));
		CellInfo5(retval)	= CopyTree(CellInfo5(src));
	}
	return (retval);
}

// free NODE
// parameter:
//		cell:	node cell
void FreeTcell(TCELLPNT cell)
{
	if (cell == NULLCELL)
		return;

	if (CellStr(cell) != NULLSTR)
		free(CellStr(cell));
	FreeSigList(CellSig(cell));
	FreeComponentList(CellCmp(cell));
	free(cell);
}

// free parse-tree
// parameter:
//		top:	top of parse tree
void FreeTree(TCELLPNT top)
{
	if (top == NULLCELL)
		return;

	FreeTree(CellInfo0(top));
	FreeTree(CellInfo1(top));
	FreeTree(CellInfo2(top));
	FreeTree(CellInfo3(top));
	FreeTree(CellInfo4(top));
	FreeTree(CellInfo5(top));

	FreeTcell(top);
}

///////////////////////// SIGLIST type /////////////////////////

// make new signal list
// return:	Top of signal list
SIGLIST *MakeNewSigList(void)
{
	register SIGLIST	*retval = (SIGLIST *)malloc(sizeof (SIGLIST));
	if (retval != NULLSIG) {
		NextSig(retval)	= NULLSIG;
		SigStr(retval)	= NULLSTR;
	}
	return (retval);
}

// free signal list
// parameter:
//		top:	top of signal list
void	FreeSigList(SIGLIST *top)
{
	register	SIGLIST *ptr, *nextptr;
	if (top == NULLSIG)
		return;
	for (ptr = top; ptr != NULLSIG; ptr = nextptr) {
		nextptr	= NextSig(ptr);
		if (SigStr(ptr) != NULLSTR)
			free(SigStr(ptr));
		free(ptr);
	}
}

// append new signal name to signal list
// parameter:
//		top:	Top of signal list
//		inpsig:	Parse tree of signal declaration (T_INPUT/T_OUTPUT/T_INOUT/T_REG/T_WIRE)
//		issig:	True if the signal type is internal wire/reg
//		isinp:	True if the signal direction is input
//		isout:	True if the signal direction is output
// return:	False if a signal with the same name is already in signal list
Boolean RegisterSignal(SIGLIST *top, TCELLPNT inpsig, Boolean issig, Boolean isinp, Boolean isout)
{
	register TCELLPNT	inpptr;
	register SIGLIST	*listptr;

	if (top == (SIGLIST *)NULL || inpsig == NULLCELL)
		return (False);

	for (inpptr = CellInfo1(inpsig); inpptr != NULLCELL; inpptr = NextCell(inpptr)) {	// for each signal (signame_list)
		register TCELLPNT	sig;
		sig	= CellInfo0(inpptr);	// (signame, N_SIG/N_SIG_WIDTH)

		for (listptr = top; NextSig(listptr) != (SIGLIST *)NULL; listptr = NextSig(listptr)) {
#ifdef UNIX
			if (!strcasecmp(CellStr(sig), SigStr(listptr))) {
#endif
#ifdef WINDOWS
			if (!stricmp(CellStr(sig), SigStr(listptr))) {
#endif
				// same signal is found.
				// check if the one is I/O port and another is internal wire/reg
				// (if so, it is not an error)
				if (	(SigIsOut(listptr) == True && CellType(inpsig) == T_REG)
					 || (SigIsOut(listptr) == True && CellType(inpsig) == T_WIRE)
					 || (SigIsSig(listptr) == True && CellType(inpsig) == T_OUTPUT)
				) {
					SigIsOut(listptr)	= True;
					SigIsSig(listptr)	= True;
					return (True);
				}

				if (	(SigIsInp(listptr) == True && CellType(inpsig) == T_WIRE)
					 || (SigIsSig(listptr) == True && CellType(inpsig) == T_INPUT)
				) {
					SigIsInp(listptr)	= True;
					SigIsSig(listptr)	= True;
					return (True);
				}

				if (SigIsSig(listptr) == True && CellType(inpsig) == T_INOUT) {
					SigIsInp(listptr)	= True;
					SigIsOut(listptr)	= True;
					SigIsSig(listptr)	= True;
					return (True);
				}

				return (False);		// duplicate signal decralations
			}
		}

		if ((SigStr(listptr) = (char *)malloc(strlen(CellStr(sig)) + 1)) == (char *)NULL)
			return (False);
		strcpy(SigStr(listptr), CellStr(sig));

		SigType(listptr)	= inpsig;
		SigIsSig(listptr)	= issig;
		SigIsInp(listptr)	= isinp;
		SigIsOut(listptr)	= isout;
		SigRefer(listptr)	= False;

		NextSig(listptr)	= MakeNewSigList();
	}

	return (True);
}

// search a signal in signal list
// parameter:
//		top:	Top of signal list
//		name:	Signal name
// return:	 If the given signal name is registered in the list, pointer to signal list cell.  Otherwise NULL.
SIGLIST *SearchSignal(SIGLIST *top, char *name)
{
	register SIGLIST	*ptr;
	if (top == (SIGLIST *)NULL || name == NULLSTR)
		return ((SIGLIST *)NULL);

	for (ptr = top; NextSig(ptr) != (SIGLIST *)NULL; ptr = NextSig(ptr)) {
		if (!strcmp(name, SigStr(ptr)))
			return (ptr);
	}
	return ((SIGLIST *)NULL);
}

// check if a signal name corresponds to I/O port
// parameter:
//		top:	Top of signal list
//		name:	Signal name
// return:	 True, if the given signal name corresponds to I/O port
Boolean IsInOut(SIGLIST *top, char *name)
{
	register SIGLIST	*ptr;
	if (top == (SIGLIST *)NULL || name == NULLSTR)
		return (False);

	for (ptr = top; NextSig(ptr) != (SIGLIST *)NULL; ptr = NextSig(ptr)) {
		if (!strcmp(name, SigStr(ptr))) {
			if (SigIsOut(ptr) == True || SigIsInp(ptr) == True)
				return (True);
			return (False);
		}
	}
	return (False);
}

// check number of internal wire/reg
// parameter:
//		top:	Top of parse tree of signal declarations
//		sig:	Top of signal list
// return:	 number of internal wire/reg
int ChkValidSigDefNum(TCELLPNT top, SIGLIST *sig)
{
	TCELLPNT	ptr;
	int			retval;
	if (top == NULLCELL || sig == NULLSIG)
		return (0);
	if (CellType(top) != N_SIGLIST)
		return (0);
	for (ptr = top, retval = 0; ptr != NULLCELL; ptr = NextCell(ptr)) {
		if (CellType(CellInfo0(ptr)) != N_SIG)
			continue;
		if (IsInOut(sig, CellStr(CellInfo0(ptr))) == True)
			continue;
		retval++;
	}
	return (retval);
}

///////////////////////// COMPONENTLIST type /////////////////////////

// make new component list
// return:	top of comment list
COMPONENTLIST *MakeNewComponentList(void)
{
	register COMPONENTLIST	*retval;
	retval = (COMPONENTLIST *)malloc(sizeof (COMPONENTLIST));
	if (retval != NULLCMP) {
		NextCmp(retval)	= NULLCMP;
		CmpStr(retval)	= NULLSTR;
	}
	return (retval);
}

// free component list
// parameter:
//		top:	top of component list
void	FreeComponentList(COMPONENTLIST *top)
{
	register	COMPONENTLIST *ptr, *nextptr;
	if (top == NULLCMP)
		return;
	for (ptr = top; ptr != NULLCMP; ptr = nextptr) {
		nextptr	= NextCmp(ptr);
		if (CmpStr(ptr) != NULLSTR)
			free(CmpStr(ptr));
		free(ptr);
	}
}

// append new component to component list
// parameter:
//		top:		top of component list
//		map:		top of parse tree of component declaration
void RegisterComponent(COMPONENTLIST *top, TCELLPNT map)
{
	register COMPONENTLIST	*ptr;
	if (top == NULLCMP || map == NULLCELL)
		return;

	for (ptr = top; NextCmp(ptr) != NULLCMP; ptr = NextCmp(ptr)) {
		if (!strcmp(CmpStr(ptr), CellStr(CellInfo0(map))))
			return;		// already exist
	}

	if ((CmpStr(ptr) = (char *)malloc(strlen(CellStr(CellInfo0(map))) + 1)) == (char *)NULL)
		return;
	strcpy(CmpStr(ptr), CellStr(CellInfo0(map)));
	CmpTree(ptr) = map;
	NextCmp(ptr) = MakeNewComponentList();
}

///////////////////////// COMMENTLIST type /////////////////////////

// make new comment list
// return:	top of comment list
COMMENTLIST *MakeNewCommentList(void)
{
	register	COMMENTLIST	*retval;
	retval	= (COMMENTLIST *)malloc(sizeof (COMMENTLIST));
	if (retval != (COMMENTLIST *)NULL) {
		NextComment(retval)	= (COMMENTLIST *)NULL;
		CommentStr(retval)	= NULLSTR;
	}
	else {
		fprintf(stderr, "INTERNAL ERROR: malloc failure in MakeNewCommentList()\n");
	}
	return (retval);
}

// free comment list
// parameter:
//		top:	top of comment list
void	FreeCommentList(COMMENTLIST *top)
{
	register	COMMENTLIST *ptr, *nextptr;
	if (top == (COMMENTLIST *)NULL)
		return;
	for (ptr = top; ptr != (COMMENTLIST *)NULL; ptr = nextptr) {
		nextptr	= NextComment(ptr);
		if (CommentStr(ptr) != NULLSTR)
			free(CommentStr(ptr));
		free(ptr);
	}
}

// append new comment to comment list
// parameter:
//		top:		top of comment list
//		str:		comment
//		linenum:	HDL line number (NOTE: pointer to int value)
void RegisterComment(COMMENTLIST *top, char *str, int *linenum)
{
	register COMMENTLIST *ptr;
	register char		 *newbuf;
	if (top == (COMMENTLIST *)NULL)
		return;

	// set pointer to the last of the comment list
	for (ptr = top; NextComment(ptr) != (COMMENTLIST *)NULL; ptr = NextComment(ptr))
		;

	CommentLine(ptr)	= *linenum;
	newbuf				= (char *)malloc(strlen(str) * 4 + 10);		// "*4 + 10" ... margin
	if (*str == '/' && *(str + 1) == '/') {	// single line comment
		register char *src, *dist;
		dist = newbuf;
		sprintf(dist, "--");
		dist += 2;
		src  = str + 2;
		for (; *src != '\0';) {
			if (*src == '\n')
				(*linenum) += 1;
			*dist++ = *src++;
		}
		*dist = '\0';
	}
	else if (*str == '/' && *(str + 1) == '*') {	// traditional-C like comments
		register char *src, *dist;
		dist = newbuf;
		sprintf(dist, "--");
		dist += 2;
		src  = str + 2;

		for (; *src != '*' || *(src + 1) != '/';) {
			if (*src == 0x0D) {
				*dist++ = *src++;
				continue;
			}
			else if (*src == 0x0A) {
				*dist++ = *src++;
				if (*(src + 1) != '\0') {
					*dist++ = '-';
					*dist++ = '-';
				}
				(*linenum) += 1;
			}
			else {
				*dist++ = *src++;
			}
		}

		for (; *src != '\0';) {
			*dist++ = *src++;
			if (*src == '\n')
				(*linenum) += 1;
		}
		*dist = '\0';
	}
	else {	// (if empty lines)
		strcpy(newbuf, str);
		(*linenum) += strlen(str);
	}

	CommentStr(ptr)		= (char *)malloc(strlen(newbuf) + 1);
	strcpy(CommentStr(ptr), newbuf);
	free(newbuf);

	CommentPrn(ptr)		= False;
	NextComment(ptr)	= MakeNewCommentList();
}


//////////////////////////////////////////////////////////////////////
// Parse-tree modification functions
//////////////////////////////////////////////////////////////////////

// convert "sig <= cond ? exp : exp" to an IF statement, if it is in an always statement.
//			(to: if cond then sig <= exp else sig <= exp)
// parameter:
//		top:		top of parse tree of expr (T_SEL)
//		substtype:	signal substitution ('<=', N_SIGSUBST) or variable substitution (':=', N_VARSUBST)
//		lval:		L value of substitution
void ConvertCondexp2If(TCELLPNT top, int substtype, TCELLPNT lval)
{
	if (top == NULLCELL)
		return;

	switch (CellType(top)) {
	case T_SEL:
		CellType(top) = T_IF;
		ConvertCondexp2If(CellInfo1(top), substtype, lval);	// then
		ConvertCondexp2If(CellInfo2(top), substtype, lval);	// else
		break;

	case N_PAREN:
	case N_DUMMY:
		ConvertCondexp2If(CellInfo0(top), substtype, lval);
		break;

	default:
		{	
			register TCELLPNT	rval;
			rval	= CopyTcell(top);
			CellType(top)	= substtype;		// N_SIGSUBST/N_VARSUBST
			CellInfo0(top)	= CopyTree(lval);	// sig
			CellInfo1(top)	= rval;
		}
		break;
	}
}

// remove excess parentheses by replacing dummy nodes (for 'when' statement)
// parameter:
//		top:		top of parse tree of expr (T_SEL)
void ConvertParen2Dummy(TCELLPNT top)
{
	if (top == NULLCELL)
		return;

	switch (CellType(top)) {
	case T_SEL:
		ConvertParen2Dummy(CellInfo1(top));		// then
		ConvertParen2Dummy(CellInfo2(top));		// else
		break;

	case N_PAREN:
		if (CellType(CellInfo0(top)) == N_PAREN || CellType(CellInfo0(top)) == T_SEL) {
			CellType(top)	= N_DUMMY;
		}
		ConvertParen2Dummy(CellInfo0(top));
		break;

	case N_DUMMY:
		ConvertParen2Dummy(CellInfo0(top));
		break;

	default:
		break;
	}
}

// check if a given signal is referred in an IF statement or not (for analyzing 'always' statement)
// parameter:
//		top:		top of parse tree of IF statement
//		sig:		signal node (T_ID/N_SIG/N_SIG_WIDTH)
// return:	True if the signal is referred
Boolean UsedSignalinIfCond(TCELLPNT top, TCELLPNT sig)
{
	if (top == NULLCELL || sig == NULLCELL)
		return (False);

	switch (CellType(top)) {
	case T_IF:
		if (UsedSignalinIfCond(CellInfo0(top), sig) == True)	// cond
			return (True);
		if (UsedSignalinIfCond(CellInfo1(top), sig) == True)	// then
			return (True);
		if (UsedSignalinIfCond(CellInfo2(top), sig) == True)	// else
			return (True);
		return (False);

	case T_AND:
	case T_OR:
	case T_XOR:
	case T_LOGIC_AND:
	case T_LOGIC_OR:
	case T_LOGIC_EQ:
	case T_LOGIC_NEQ:
		if (UsedSignalinIfCond(CellInfo0(top), sig) == True)	// left
			return (True);
		if (UsedSignalinIfCond(CellInfo1(top), sig) == True)	// right
			return (True);
		return (False);

	case N_PAREN:
	case N_DUMMY:
	case T_NOT:
	case T_LOGIC_NOT:
		return (UsedSignalinIfCond(CellInfo0(top), sig));

	case T_ID:
	case N_SIG:
	case N_SIG_WIDTH:
		if (!strcmp(CellStr(top), CellStr(sig)))
			return (True);
		return (False);

	default:
		break;
	}
	return (False);
}

// replace node type from N_EXPLIST into N_SUBSTLIST	(used in portmap part)
// parameter:
//		top:		top of parse tree (N_EXPLIST)
void ConvertExplist2Substlist(TCELLPNT top)
{
	register TCELLPNT	ptr;
	if (top == NULLCELL)
		return;
	for (ptr = top; ptr != NULLCELL; ptr = NextCell(ptr)) {
		CellType(ptr) = N_SUBSTLIST;
	}
}


//////////////////////////////////////////////////////////////////////
// evaluate expressions and etc
//////////////////////////////////////////////////////////////////////

// evaluate signal-width of expression
// parameter:
//		top:	Top of parse tree of expression
// return:	width, if it was determined.  Otherwise, -1.
int EvalExpWidth(TCELLPNT top)
{
	int	lval, rval;
	if (top == NULLCELL)
		return (-1);
	switch (CellType(top)) {
	case N_DUMMY:
	case N_PAREN:
	case T_NOT:				/* T_NOT */
		return (EvalExpWidth(CellInfo0(top)));

	case T_LOGIC_NOT:		/* T_LOGIC_NOT */
	case T_LOGIC_OR:		/* T_LOGIC_OR */
	case T_LOGIC_AND:		/* T_LOGIC_AND */
	case T_LOGIC_EQ:		/* T_LOGIC_EQ */
	case T_LOGIC_NEQ:		/* T_LOGIC_NEQ */
	case T_GE:				/* T_GE */
	case T_LE:				/* T_LE */
	case T_GT:				/* T_GT */
	case T_LS:				/* T_LS */
	case N_REDUCTION_AND:	/* N_REDUCTION_AND info0=exp */
	case N_REDUCTION_OR:	/* N_REDUCTION_OR info0=exp */
	case N_REDUCTION_XOR:	/* N_REDUCTION_XOR info0=exp */
		return (1);

	case T_OR:				/* T_OR */
	case T_AND:				/* T_AND */
	case T_XOR:				/* T_XOR */
	case T_PLUS:			/* T_PLUS */
	case T_MINUS:			/* T_MINUS */
		lval = EvalExpWidth(CellInfo0(top));
		rval = EvalExpWidth(CellInfo1(top));
		if (lval < 0 && rval < 0)
			return (-1);
		else if (rval < 0)
			return (lval);
		else if (lval < 0)
			return (rval);
		else if (rval == lval)
			return (rval);
		else {	// rval != lval
			fprintf(stderr, "signal width difference\n");
			return (max(lval,rval));
		}
		break;

	case T_RSHIFT:			/* T_RSHIFT */
	case T_LSHIFT:			/* T_LSHIFT */
		return (EvalExpWidth(CellInfo0(top)));	// == L width

	case T_WIDTH_BINDIGIT:	/* T_WIDTH_BINDIGIT */
	case T_WIDTH_DECDIGIT:	/* T_WIDTH_DECDIGIT */
	case T_WIDTH_HEXDIGIT:	/* T_WIDTH_HEXDIGIT */
		{
			register int	retval = 0;
			register char	*ptr;
			for (ptr = CellStr(top); *ptr != '\'' && *ptr != '\0'; ptr++)
				retval	= (retval * 10) + (*ptr - '0');
			return (retval);
		}

	case N_COPYSIG:	/* N_COPYSIG info0=copy times, info1=expr */
		if (EvalExpValue(CellInfo0(top), &lval) == False)
			return (-1);
		if ((rval = EvalExpWidth(CellInfo1(top))) < 0)
			return (-1);
		return (lval * rval);

	case N_CONCAT:	/* N_CONCAT info0=list */
		{
			// check exp-list
			register	TCELLPNT	ptr;
			register	int			retval, val;
			for (ptr = CellInfo0(top), retval = 0; ptr != NULLCELL; ptr = NextCell(ptr)) {
				if ((val = EvalExpWidth(CellInfo0(ptr))) < 0)
					return (-1);
				retval	+= val;
			}
			return (retval);
		}

	case T_DIV:			/* T_DIV */
	case T_MOD:			/* T_MOD */
	case T_MULT:		/* T_MULT */
	case T_DECDIGIT:	/* T_DECDIGIT */
	case N_FUNCCALL:	/* N_FUNCCALL info0=list */
	case N_SIG_WIDTH:	/* N_SIG_WIDTH info0=width */
	case T_ID:
	case N_SIG:			/* N_SIG */
	default:
		break;
	}
	return (-1);
}

// evaluate integer value
// parameter:
//		top:	Top of parse tree
//		ret:	Pointer to result variable
// return:	True if evaluating value is successful without overflow.
Boolean EvalExpValue(TCELLPNT top, int *ret)
{
	int	lval, rval;
	if (top == NULLCELL)
		return (False);

	switch (CellType(top)) {
	case N_DUMMY:
	case N_PAREN:
		return (EvalExpValue(CellInfo0(top), ret));

	case T_PLUS:	/* T_PLUS */
		if (EvalExpValue(CellInfo0(top), &lval) == False
				|| EvalExpValue(CellInfo1(top), &rval) == False)
			return (False);
		*ret	= lval + rval;
		return (True);

	case T_MINUS:	/* T_MINUS */
		if (EvalExpValue(CellInfo0(top), &lval) == False
				|| EvalExpValue(CellInfo1(top), &rval) == False)
			return (False);
		*ret	= lval - rval;
		return (True);

	case T_DIV:			/* T_DIV */
		if (EvalExpValue(CellInfo0(top), &lval) == False
				|| EvalExpValue(CellInfo1(top), &rval) == False)
			return (False);
		if (rval == 0)
			return (False);
		*ret	= lval / rval;
		return (True);

	case T_MOD:			/* T_MOD */
		if (EvalExpValue(CellInfo0(top), &lval) == False
				|| EvalExpValue(CellInfo1(top), &rval) == False)
			return (False);
		if (rval == 0)
			return (False);
		*ret	= lval % rval;
		return (True);

	case T_MULT:		/* T_MULT */
		if (EvalExpValue(CellInfo0(top), &lval) == False
				|| EvalExpValue(CellInfo1(top), &rval) == False)
			return (False);
		*ret	= lval * rval;
		return (True);

	case T_WIDTH_BINDIGIT:	/* T_WIDTH_BINDIGIT */
		{
			register int	retval;
			register int	width;
			register char	*ptr;
			for (width = 0, ptr = CellStr(top); *ptr != '\'' && *ptr != '\0'; ptr++)
				width	= (width * 10) + (*ptr - '0');
			if (width > 31)
				return (False);	// (overflow)
			if (*ptr != '\0')	// (*ptr == '\'')
				ptr++;
			if (*ptr != '\0')	// (*ptr == 'b')
				ptr++;
			for (retval = 0; *ptr != '\0'; ptr++)
				retval	= (retval * 2) + (*ptr - '0');
			*ret	= retval;
			return (True);
		}

	case T_WIDTH_DECDIGIT:	/* T_WIDTH_DECDIGIT */
		{
			register int	retval;
			register int	width;
			register char	*ptr;
			for (width = 0, ptr = CellStr(top); *ptr != '\'' && *ptr != '\0'; ptr++)
				width	= (width * 10) + (*ptr - '0');
			if (width > 31)
				return (False);	// (overflow)
			if (*ptr != '\0')	// (*ptr == '\'')
				ptr++;
			if (*ptr != '\0')	// (*ptr == 'd')
				ptr++;
			for (retval = 0; *ptr != '\0'; ptr++)
				retval	= (retval * 10) + (*ptr - '0');
			*ret	= retval;
			return (True);
		}

	case T_WIDTH_HEXDIGIT:	/* T_WIDTH_HEXDIGIT */
		{
			register int	retval;
			register int	width;
			register char	*ptr;
			for (width = 0, ptr = CellStr(top); *ptr != '\'' && *ptr != '\0'; ptr++)
				width	= (width * 10) + (*ptr - '0');
			if (width > 31)
				return (False);	// (overflow)
			if (*ptr != '\0')	// (*ptr == '\'')
				ptr++;
			if (*ptr != '\0')	// (*ptr == 'h')
				ptr++;
			for (retval = 0; *ptr != '\0'; ptr++) {
				if ('0' <= *ptr && *ptr <= '9')
					retval	= (retval * 16) + (*ptr - '0');
				else if ('a' <= *ptr && *ptr <= 'f')
					retval	= (retval * 16) + (*ptr - 'a' + 10);
				else if ('A' <= *ptr && *ptr <= 'F')
					retval	= (retval * 16) + (*ptr - 'A' + 10);
			}
			*ret	= retval;
			return (True);
		}

	case T_DECDIGIT:	/* T_DECDIGIT */
		{
			register int	retval;
			register int	width;
			register char	*ptr;
			Boolean			flag = False;	// plus
			for (ptr = CellStr(top), retval = 0, width = 0; *ptr != '\0'; ptr++) {
				if (*ptr == '-') {
					if (flag == False)
						flag = True;	// minus
					else
						flag = False;
					continue;
				}
				retval	= (retval * 10) + (*ptr - '0');
				width	+= 4;		// (less than or equal to 4bit)
				if (width > 31)
					return (False);	// (overflow)
			}
			if (flag == True)
				*ret	= retval * (-1);
			else
				*ret	= retval;
			return (True);
		}

	default:
		break;
	}
	return (False);
}

// check if an output signal is referred in expressions etc.
// parameter:
//		top:	Top of parse tree
//		sig:	Top of signal list
// (return:	SigRefer marks in the signal list will be set)
void ChkEvalOutput(TCELLPNT top, SIGLIST *sig)
{
	if (top == NULLCELL || sig == NULLSIG)
		return;

	switch (CellType(top)) {
	case N_DUMMY:
	case N_PAREN:
	case T_NOT:				/* T_NOT */
	case T_LOGIC_NOT:		/* T_LOGIC_NOT */
	case N_REDUCTION_AND:	/* N_REDUCTION_AND info0=exp */
	case N_REDUCTION_OR:	/* N_REDUCTION_OR info0=exp */
	case N_REDUCTION_XOR:	/* N_REDUCTION_XOR info0=exp */
		ChkEvalOutput(CellInfo0(top), sig);
		return;

	case T_LOGIC_OR:		/* T_LOGIC_OR */
	case T_LOGIC_AND:		/* T_LOGIC_AND */
	case T_OR:				/* T_OR */
	case T_AND:				/* T_AND */
	case T_XOR:				/* T_XOR */
	case T_LOGIC_EQ:		/* T_LOGIC_EQ */
	case T_LOGIC_NEQ:		/* T_LOGIC_NEQ */
	case T_GE:				/* T_GE */
	case T_LE:				/* T_LE */
	case T_GT:				/* T_GT */
	case T_LS:				/* T_LS */
	case T_RSHIFT:			/* T_RSHIFT */
	case T_LSHIFT:			/* T_LSHIFT */
	case T_PLUS:			/* T_PLUS */
	case T_MINUS:			/* T_MINUS */
	case T_MULT:			/* T_MULT */
	case T_DIV:				/* T_DIV */
	case T_MOD:				/* T_MOD */
		ChkEvalOutput(CellInfo0(top), sig);
		ChkEvalOutput(CellInfo1(top), sig);
		return;

	case T_SEL:
		ChkEvalOutput(CellInfo0(top), sig);
		ChkEvalOutput(CellInfo1(top), sig);
		ChkEvalOutput(CellInfo2(top), sig);
		return;

	case N_COPYSIG:		/* N_COPYSIG info0=copy times, info1=expr */
		ChkEvalOutput(CellInfo1(top), sig);
		return;

	case N_CONCAT:		/* N_CONCAT info0=list */
	case N_FUNCCALL:	/* N_FUNCCALL info0=list */
		{
			TCELLPNT	ptr;	// N_SIGLIST or N_EXPLIST
			for (ptr = CellInfo0(top); ptr != NULLCELL; ptr = NextCell(ptr)) {
				ChkEvalOutput(CellInfo0(ptr), sig);
			}
		}
		return;

	case N_SIG_WIDTH:	/* N_SIG_WIDTH info0=width */
	case N_SIG:			/* N_SIG */
	case T_ID:
		{
			SIGLIST *sigptr;
			if ((sigptr = SearchSignal(sig, CellStr(top))) == NULLSIG)
				return;
			if (SigIsOut(sigptr) == True) {
				SigRefer(sigptr) = True;
			}
		}
		return;

	case T_WIDTH_BINDIGIT:	/* T_WIDTH_BINDIGIT */
	case T_WIDTH_DECDIGIT:	/* T_WIDTH_DECDIGIT */
	case T_WIDTH_HEXDIGIT:	/* T_WIDTH_HEXDIGIT */
	case T_DECDIGIT:		/* T_DECDIGIT */
	default:
		break;
	}
	return;
}


//////////////////////////////////////////////////////////////////////
// Write PASS1 output
//////////////////////////////////////////////////////////////////////

// print tabs
// parameter:
//		fp:		Output file pointer
//		id:		Number of tabs
static void fprintfTab(FILE *fp, int indent)
{
	register int	i;
	for (i = 0; i < indent; i++)
		fprintf(fp, "\t");
}

// print warnings
// parameter:
//		fp:		Output file pointer
//		line:	HDL line number
//		msg:	Warning message. Format: "%s\0%s" where the first string is warning number and the second is message body.
static void fprintfWarning(FILE *fp, int line, char *msg)
{
	char	*msg_top, *id;
	id	= msg;
	for (msg_top = msg; *msg_top != '\0'; msg_top++)
		;
	msg_top++;

	if (line > 0) {
		fprintf(fp, "\n-- WARNING(%s) in line %d: %s\n", id, line, msg_top);
		fprintf(stderr, "WARNING(%s) in line %d: %s\n", id, line, msg_top);
	}
	else {
		fprintf(fp, "\n-- WARNING(%s): %s\n", id, msg_top);
		fprintf(stderr, "WARNING(%s): %s\n", id, msg_top);
	}
}

// print signal name (append prefix/suffix, if necessary)
// parameter:
//		fp:			Output file pointer
//		name:		Signal name
//		funcname:	Function name, if priting function definition.  Otherwise NULL.
void fprintfSigName(FILE *fp, char *name, char *funcname)
{
	if (name == NULLSTR)
		return;
	if (funcname == NULLSTR || strcmp(name, funcname)) {
		if (topchar(name) == '_')
			fprintf(fp, "%s", UNDERSCORE_PREFIX);
		fprintf(fp, "%s", name);
		if (lastchar(name) == '_')
			fprintf(fp, "%s", UNDERSCORE_SUFFIX);
	}
	else {	// the given signal is function return variable
		fprintf(fp, "%s%s", name, FUNCRET_SUFFIX);
	}
}

// print internal wire/reg
// parameter:
//		fp:			Output file pointer
//		top:		Top of parse tree of signal name list
//		sig:		Top of signal list
void fprintfValidSigDef(FILE *fp, TCELLPNT top, SIGLIST *sig)
{
	TCELLPNT	ptr;
	Boolean		isfirst;
	if (top == NULLCELL || sig == NULLSIG)
		return;
	if (CellType(top) != N_SIGLIST)
		return;
	for (ptr = top, isfirst = True; ptr != NULLCELL; ptr = NextCell(ptr)) {
		if (CellType(CellInfo0(ptr)) != N_SIG)
			continue;
		if (IsInOut(sig, CellStr(CellInfo0(ptr))) == True)
			continue;

		if (isfirst == False)
			fprintf(fp, ", ");
		isfirst	= False;

		fprintfSigName(fp, CellStr(CellInfo0(ptr)), (char *)NULL);
	}
}

// print component declarations
// parameter:
//		fp:			Output file pointer
//		cmptop:		Top of component list
//		indent:		number of tabs
void fprintfComponentDecralations(FILE *fp, COMPONENTLIST *cmptop, int indent)
{
	register COMPONENTLIST	*cmpptr;
	register TCELLPNT		paramptr;

	if (cmptop == NULLCMP)
		return;
	for (cmpptr = cmptop; NextCmp(cmpptr) != (COMPONENTLIST *)NULL; cmpptr = NextCmp(cmpptr)) {
fprintfWarning(fp, -1, WARN_0_CMPDEC);

		fprintfTab(fp, indent);
		fprintf(fp, "component %s\n", CmpStr(cmpptr));

		if (CellInfo2(CmpTree(cmpptr)) != NULLCELL) {
			// generic
			fprintfTab(fp, indent);
			fprintf(fp, "generic (\n");

			for (paramptr = CellInfo2(CmpTree(cmpptr)); paramptr != NULLCELL; paramptr = NextCell(paramptr)) {
				fprintfTab(fp, indent + 1);
				fprintf(fp, "?\t: ? std_logic_vector(? downto 0) := ?");

				if (NextCell(paramptr) != NULLCELL)
					fprintf(fp, ";\n");
				else
					fprintf(fp, "\n");
			}

			fprintfTab(fp, indent);
			fprintf(fp, ");\n");
		}

		// print ports
		fprintfTab(fp, indent);
		fprintf(fp, "port (\n");
		for (paramptr = CellInfo3(CmpTree(cmpptr)); paramptr != NULLCELL; paramptr = NextCell(paramptr)) {
			if (CellType(paramptr) == N_EXPLIST) {
				fprintfTab(fp, indent + 1);
				fprintf(fp, "?\t: ? std_logic_vector(? downto ?)");
			}
			else { // N_MAPLIST
				fprintfTab(fp, indent + 1);
				fprintf(fp, "%s\t: ? std_logic_vector(? downto 0)", CellStr(CellInfo0(paramptr)));
			}

			if (NextCell(paramptr) != NULLCELL)
				fprintf(fp, ";\n");
			else
				fprintf(fp, "\n");
		}

		fprintfTab(fp, indent);
		fprintf(fp, ");\n");

		fprintfTab(fp, indent);
		fprintf(fp, "end component;\n");
		fprintf(fp, "\n");
	}
}

// print comments
// parameter:
//		fp:			Output file pointer
//		top:		Top of comment list
//		linenum:	HDL line number (NOTE: comments UNTIL this line will be printed)
//		pretab:		Tab number (usually 0)
// return:	True if some comments are printed out.
static Boolean fprintfComment(FILE *fp, COMMENTLIST *top, int linenum, int pretab)
{
	register COMMENTLIST *ptr;
	register Boolean	retval;
	if (top == (COMMENTLIST *)NULL || linenum <= 0)
		return (False);
	for (ptr = top, retval = False; NextComment(ptr) != (COMMENTLIST *)NULL; ptr = NextComment(ptr)) {
		register int i;
		if (CommentPrn(ptr) == True)
			continue;
		if (linenum > 0 && linenum < CommentLine(ptr))
			return (retval);
		if (*(CommentStr(ptr)) != '\n') {
			for (i = 0; i < pretab; i++)
				fprintf(fp, "\t");
		}
		fprintf(fp, "%s", CommentStr(ptr));
		CommentPrn(ptr)	= True;
		retval = True;
	}
	return (retval);
}


// print VHDL output
// parameter:
//		fp:			Output file pointer
//		sw:			print sw: False to disable print (only to traverse parse tree)
//		top:		Top of parse tree
//		sig:		Top of signal list
//		com:		Top of comment list
//		cmp:		Top of component list
//		indent:		Tab number
//		flag0:		Pointer to (int)flag0, required expression type: logic(1) or arithmetic(0)
//		flag1:		Pointer to (int)flag1, general purpose variable: clock signal name (for always sentence)
//		flag2:		Pointer to (int)flag2, general purpose variable: inside(1) or outside(0) of always sentence
//		flag3:		Pointer to (int)flag3, general purpose variable: inside(funcname) or outside(NULL) of function definition
//		flag4:		Pointer to (int)flag4, general purpose variable: change name of referred output-signal (1) or not (0)
//		flag5:		Pointer to (int)flag5, general purpose variable: inside(procname) or outside(NULL) of procedure definition
static void fprintfVHDL(FILE *fp, Boolean sw, TCELLPNT top, SIGLIST *sig, COMMENTLIST *com, COMPONENTLIST *cmp, int indent, int *flag0, int *flag1, int *flag2, int *flag3, int *flag4, int *flag5)
{
	register int	pre_flag0 = *flag0;
	register int	pre_flag1 = *flag1;
	register int	pre_flag2 = *flag2;
	register int	pre_flag3 = *flag3;
	register int	pre_flag4 = *flag4;
	register int	pre_flag5 = *flag5;

	if (top == NULLCELL)
		return;

	switch (CellType(top)) {
	case N_DUMMY:
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);
		break;

//////////////////////////////////////////////////////////////////////
// TOP
//////////////////////////////////////////////////////////////////////
	case T_MODULE:	/* T_MODULE info0=entity, info1=body */
		*flag2 = 0;						// (outside of always sentence)
		*flag3 = (int)((char *)NULL);	// (outside of function definition)
		*flag4 = 0;						// (unchange name of referred output signal)
		*flag5 = (int)((char *)NULL);	// (outside of procedure definition)

		if (sw == True) {
			fprintf(fp, "%c%s", REGION_HEAD, REGION_MARK);
			fprintfComment(fp, com, CellLine(top) - 1, indent);	// print remaining comments
		}
		fprintfVHDL(fp, sw, CellInfo0(top), CellSig(top), com, CellCmp(top), indent, flag0, flag1, flag2, flag3, flag4, flag5);		// entity

		if (sw == True) {
			fprintf(fp, "%c%s", REGION_DEF, REGION_MARK);
			fprintfComponentDecralations(fp, CellCmp(top), indent + 1);
			fprintfComment(fp, com, CellLine(CellInfo1(top)) - 1, indent);	// print remaining comments
		}

		fprintfVHDL(fp, sw, CellInfo1(top), CellSig(top), com, CellCmp(top), indent + 1, flag0, flag1, flag2, flag3, flag4, flag5);	// body

		if (sw == True) {
			fprintf(fp, "%c%s", REGION_BODY, REGION_MARK);
			fprintf(fp, "\nend RTL;\n");
			if (NextCell(top) != NULLCELL)
				fprintf(fp, "\n");
			fprintfComment(fp, com, CellLine(top), indent);	// print remaining comments
		}

		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);		// (next module)
		break;

	case N_FUNCDEF:	/* N_FUNCDEF info0=ID, info1=out width, info2=decralations, info3=body */
		*flag3 = (int)CellStr(CellInfo0(top));		// (inside of function definition)

		if (sw == True) {
			fprintf(fp, "%c%s", REGION_DEF, REGION_MARK);
			fprintfComment(fp, com, CellLine(top), indent);
			fprintfTab(fp, indent);
			fprintf(fp, "function %s (\n", CellStr(CellInfo0(top)));
		}

		fprintfVHDL(fp, sw, CellInfo2(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// decralations

		if (sw == True) {
fprintfWarning(fp, CellLine(CellInfo2(top)), WARN_1_STDLGC);
			fprintfTab(fp, indent);
			fprintf(fp, ") return std_logic_vector is");

fprintfWarning(fp, CellLine(CellInfo2(top)), WARN_2_FUNCRET);
			fprintfTab(fp, indent + 1);
			fprintf(fp, "variable %s%s\t: std_logic_vector(? downto ?);\n", CellStr(CellInfo0(top)), FUNCRET_SUFFIX);

			fprintfTab(fp, indent);
			fprintf(fp, "begin\n");
		}

		fprintfVHDL(fp, sw, CellInfo3(top), sig, com, cmp, indent + 1, flag0, flag1, flag2, flag3, flag4, flag5);	// body

		if (sw == True) {
			fprintfTab(fp, indent + 1);
			fprintf(fp, "return %s%s;\n", CellStr(CellInfo0(top)), FUNCRET_SUFFIX);

			fprintfTab(fp, indent);
			fprintf(fp, "end %s;\n\n", CellStr(CellInfo0(top)));
		}

		*flag3 = (int)((char *)NULL);				// (outside of function definition)
		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);		// (next module)
		break;

	case N_TASKDEF:	/* N_TASKDEF info0=ID, info1=decralations, info2=body */
		*flag5 = (int)CellStr(CellInfo0(top));		// (inside of procedure definition)

		if (sw == True) {
			fprintf(fp, "%c%s", REGION_DEF, REGION_MARK);
			fprintfComment(fp, com, CellLine(top), indent);
			fprintfTab(fp, indent);
			fprintf(fp, "procedure %s (\n", CellStr(CellInfo0(top)));
		}

		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// decralations

		if (sw == True) {
			fprintfTab(fp, indent);
			fprintf(fp, ") is\n");
fprintfWarning(fp, CellLine(CellInfo2(top)), WARN_3_PROCPAR);
			fprintfTab(fp, indent);
			fprintf(fp, "begin\n");
		}

		fprintfVHDL(fp, sw, CellInfo2(top), sig, com, cmp, indent + 1, flag0, flag1, flag2, flag3, flag4, flag5);	// body

		if (sw == True) {
			fprintfTab(fp, indent);
			fprintf(fp, "end %s;\n\n", CellStr(CellInfo0(top)));
		}

		*flag5 = (int)((char *)NULL);		// (outside of procedure definition)
		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);		// (next module)
		break;

//////////////////////////////////////////////////////////////////////
// entity
//////////////////////////////////////////////////////////////////////
	case N_ENTITY:	/* N_ENTITY info0=modulename, info1=siglist */
		if (sw == True) {
			fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments
			fprintf(fp, "library ieee;\n");
			fprintf(fp, "use ieee.std_logic_1164.all;\n");
			fprintf(fp, "use ieee.std_logic_arith.all;\n");
			fprintf(fp, "use ieee.std_logic_unsigned.all;\n");
			fprintf(fp, "\n");
			fprintf(fp, "entity %s is\n", CellStr(CellInfo0(top)));
			fprintf(fp, "\tport (\n");
		}

		if (sw == True) {
			fprintf(fp, "%c%s", REGION_SEP, REGION_MARK);
			fprintfComment(fp, com, CellLine(CellInfo1(top)), indent);

			fprintf(fp, "\t);\n");
			fprintf(fp, "end %s;\n", CellStr(CellInfo0(top)));
			fprintf(fp, "\n");
			fprintf(fp, "architecture RTL of %s is\n", CellStr(CellInfo0(top)));
		}
		break;

	case N_SIGLIST:	/* N_SIGLIST info0=sig/exp */
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);		// name
		if (sw == True && NextCell(top) != NULLCELL)
			fprintf(fp, ", ");

		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

	case T_INPUT:	/* T_INPUT info0=width, info1=name */
		if (sw == True) {
			if (pre_flag3 == (int)((char *)NULL) && pre_flag5 == (int)((char *)NULL))
				fprintf(fp, "%c%s", REGION_IO, REGION_MARK);
			fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments

			if (CellInfo2(top) != NULLCELL) {
fprintfWarning(fp, CellLine(CellInfo2(top)), WARN_4_TYPEDEF);
			}
			fprintfTab(fp, indent + 1);
		}

		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);		// name
		if (sw == True) {
			if (pre_flag3 == (int)((char *)NULL))	// outside of function def
				fprintf(fp, "\t: in  ");
			else
				fprintf(fp, "\t: ");

			if (CellInfo2(top) != NULLCELL) {
				fprintf(fp, "array (");
				fprintfVHDL(fp, sw, CellInfo2(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// width
				fprintf(fp, ") of ");
			}
		}

		if (sw == True && CellInfo0(top) == NULLCELL)
			fprintf(fp, "std_logic");
		else {
			if (sw == True)
				fprintf(fp, "std_logic_vector(");
			fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// width
			if (sw == True)
				fprintf(fp, ")");
		}

		if (sw == True) {
			if (NextCell(top) != NULLCELL)
				fprintf(fp, ";");
			if (fprintfComment(fp, com, CellLine(CellInfo1(top)), indent) == False)
				fprintf(fp, "\n");
		}

		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

	case T_OUTPUT:	/* T_OUTPUT info0=width, info1=name */
		if (sw == True) {
			if (pre_flag3 == (int)((char *)NULL) && pre_flag5 == (int)((char *)NULL))
				fprintf(fp, "%c%s", REGION_IO, REGION_MARK);
			fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments

			if (CellInfo2(top) != NULLCELL) {
fprintfWarning(fp, CellLine(CellInfo2(top)), WARN_4_TYPEDEF);
			}
			fprintfTab(fp, indent + 1);
		}

		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);		// name
		if (sw == True) {
			if (pre_flag3 == (int)((char *)NULL))	// outside of function def
				fprintf(fp, "\t: out ");
			else
				fprintf(fp, "\t: ");

			if (CellInfo2(top) != NULLCELL) {
				fprintf(fp, "array (");
				fprintfVHDL(fp, sw, CellInfo2(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// width
				fprintf(fp, ") of ");
			}
		}

		if (sw == True && CellInfo0(top) == NULLCELL)
			fprintf(fp, "std_logic");
		else {
			if (sw == True)
				fprintf(fp, "std_logic_vector(");
			fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// width
			if (sw == True)
				fprintf(fp, ")");
		}

		if (sw == True) {
			if (NextCell(top) != NULLCELL)
				fprintf(fp, ";");
			if (fprintfComment(fp, com, CellLine(CellInfo1(top)), indent) == False)
				fprintf(fp, "\n");
		}

		// write referred output signals
		if (sw == True && pre_flag3 == (int)((char *)NULL) && pre_flag5 == (int)((char *)NULL)) {
			TCELLPNT	listptr;
			for (listptr = CellInfo1(top); listptr != NULLCELL; listptr = NextCell(listptr)) {
				SIGLIST		*sigptr;
				TCELLPNT	sigcell;
				sigcell	= CellInfo0(listptr);
				if (CellType(sigcell) != N_SIG)
					continue;
				if ((sigptr = SearchSignal(sig, CellStr(sigcell))) == NULLSIG)
					continue;
				if (SigRefer(sigptr) == False || SigIsOut(sigptr) == False)
					continue;

				fprintf(fp, "%c%s", REGION_DEF, REGION_MARK);
				fprintfTab(fp, indent);
				fprintf(fp, "signal %s%s\t:", CellStr(sigcell), REFOUTSIG_SUFFIX);
				if (CellInfo0(top) == NULLCELL)
					fprintf(fp, "std_logic;");
				else {
					fprintf(fp, "std_logic_vector(");
					fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// width
					fprintf(fp, ");");
				}
				fprintf(fp, "\t-- appended by Verilog2VHDL\n");

				fprintf(fp, "%c%s", REGION_BODY, REGION_MARK);
				fprintfTab(fp, indent);
				fprintf(fp, "%s\t<= %s%s;", CellStr(sigcell), CellStr(sigcell), REFOUTSIG_SUFFIX);
				fprintf(fp, "\t-- appended by Verilog2VHDL\n");

			}
		}

		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

	case T_INOUT:	/* T_INOUT info0=width, info1=name */
		if (sw == True) {
			if (pre_flag3 == (int)((char *)NULL) && pre_flag5 == (int)((char *)NULL))
				fprintf(fp, "%c%s", REGION_IO, REGION_MARK);
			fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments

			if (CellInfo2(top) != NULLCELL) {
fprintfWarning(fp, CellLine(CellInfo2(top)), WARN_4_TYPEDEF);
			}
			fprintfTab(fp, indent + 1);
		}

		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);		// name
		if (sw == True) {
			if (pre_flag3 == (int)((char *)NULL))	// outside of function def
				fprintf(fp, "\t: inout ");
			else
				fprintf(fp, "\t: ");

			if (CellInfo2(top) != NULLCELL) {
				fprintf(fp, "array (");
				fprintfVHDL(fp, sw, CellInfo2(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// width
				fprintf(fp, ") of ");
			}
		}

		if (sw == True && CellInfo0(top) == NULLCELL)
			fprintf(fp, "std_logic");
		else {
			if (sw == True)
				fprintf(fp, "std_logic_vector(");
			fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// width
			if (sw == True)
				fprintf(fp, ")");
		}

		if (sw == True) {
			if (NextCell(top) != NULLCELL)
				fprintf(fp, ";");
			if (fprintfComment(fp, com, CellLine(CellInfo1(top)), indent) == False)
				fprintf(fp, "\n");
		}

		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

	case T_REG:		/* T_REG info0=width, info1=name */
	case T_WIRE:	/* T_WIRE info0=width, info1=name */
		if (ChkValidSigDefNum(CellInfo1(top), sig) > 0) {	// (remove input/output signals)
			if (sw == True) {
				if (pre_flag3 == (int)((char *)NULL) && pre_flag5 == (int)((char *)NULL))
					fprintf(fp, "%c%s", REGION_DEF, REGION_MARK);
				fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments

				if (CellInfo2(top) != NULLCELL) {
fprintfWarning(fp, CellLine(CellInfo2(top)), WARN_4_TYPEDEF);
				}
				fprintfTab(fp, indent);
				fprintf(fp, "signal ");
			}

			fprintfValidSigDef(fp, CellInfo1(top), sig);	// (do not print input/output signals)
			if (sw == True) {
				fprintf(fp, "\t: ");

				if (CellInfo2(top) != NULLCELL) {
					fprintf(fp, "array (");
					fprintfVHDL(fp, sw, CellInfo2(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// width
					fprintf(fp, ") of ");
				}
			}

			if (sw == True && CellInfo0(top) == NULLCELL)
				fprintf(fp, "std_logic;");
			else {
				if (sw == True)
					fprintf(fp, "std_logic_vector(");
				fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// width
				if (sw == True)
					fprintf(fp, ");");
			}

			if (sw == True) {
				if (fprintfComment(fp, com, CellLine(CellInfo1(top)), indent) == False)
					fprintf(fp, "\n");
			}
		}
		else {	// (only output signal is defined)
			if (sw == True) {
				fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments
				if (pre_flag3 == (int)((char *)NULL) && pre_flag5 == (int)((char *)NULL))
					fprintf(fp, "%c%s", REGION_DEF, REGION_MARK);
				fprintfComment(fp, com, CellLine(CellInfo1(top)), indent);
			}
		}
		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

	case N_RANGE:	/* N_RANGE info0=from, info1=to */
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// from
		if (sw == True && CellInfo1(top) != NULLCELL) {
			fprintf(fp, " downto ");
			*flag0 = 0;	/* exp:arithmetic */
			fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// to
		}
		break;

	case T_PARAM:	/* T_PARAM info0=param, info1=exp */
		if (sw == True) {
			if (pre_flag3 == (int)((char *)NULL) && pre_flag5 == (int)((char *)NULL))
				fprintf(fp, "%c%s", REGION_DEF, REGION_MARK);
			fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments

			if (EvalExpWidth(CellInfo1(top)) < 0) {
fprintfWarning(fp, CellLine(CellInfo1(top)), WARN_5_SIGWIDTH);
			}

			if (pre_flag3 == (int)((char *)NULL) && pre_flag5 == (int)((char *)NULL))
				fprintfTab(fp, indent);
			else
				fprintfTab(fp, indent + 1);
			fprintf(fp, "constant ");
		}

		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);		// name
		if (sw == True) {
			int	width;

			fprintf(fp, "\t: ");

			width = EvalExpWidth(CellInfo1(top));
			if (width < 0)
				fprintf(fp, "std_logic_vector(? downto ?)");
			else if (width == 0)
				fprintf(fp, "std_logic");
			else
				fprintf(fp, "std_logic_vector(%d downto 0)", width - 1);

			fprintf(fp, " := ");
		}

		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);		// exp
		if (sw == True) {
			fprintf(fp, ";");
			if (fprintfComment(fp, com, CellLine(CellInfo1(top)), indent) == False)
				fprintf(fp, "\n");
		}

		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

//////////////////////////////////////////////////////////////////////
// body
//////////////////////////////////////////////////////////////////////
	case N_PORTMAP:	/* N_PORTMAP info0=module, info1=label, info2=genelic, info3=maplist */
		if (sw == True) {
			fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments
			fprintf(fp, "%c%s", REGION_BODY, REGION_MARK);
			fprintfTab(fp, indent);
			fprintf(fp, "%s: %s\n", CellStr(CellInfo1(top)), CellStr(CellInfo0(top)));
		}

		if (CellInfo2(top) != NULLCELL) {
			if (sw == True) {
				fprintfTab(fp, indent);
				fprintf(fp, "generic map (\n");
			}
			fprintfVHDL(fp, sw, CellInfo2(top), sig, com, cmp, indent + 1, flag0, flag1, flag2, flag3, flag4, flag5);	// generic map
			if (sw == True) {
				fprintfTab(fp, indent);
				fprintf(fp, ")");
				if (fprintfComment(fp, com, CellLine(CellInfo2(top)), indent) == False)
					fprintf(fp, "\n");
			}
		}

		if (sw == True) {
			fprintfTab(fp, indent);
			fprintf(fp, "port map (\n");
		}
		fprintfVHDL(fp, sw, CellInfo3(top), sig, com, cmp, indent + 1, flag0, flag1, flag2, flag3, flag4, flag5);	// map list
		if (sw == True) {
			fprintfTab(fp, indent);
			fprintf(fp, ");");
			if (fprintfComment(fp, com, CellLine(CellInfo3(top)), indent) == False)
				fprintf(fp, "\n");
		}

		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

	case N_EXPLIST:	/* N_EXPLIST info0=exp */
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent + 1, flag0, flag1, flag2, flag3, flag4, flag5);	// exp
		if (sw == True && NextCell(top) != NULLCELL)
			fprintf(fp, ", ");

		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

	case N_SUBSTLIST:	/* N_SUBSTLIST info0=exp */
		if (sw == True)
			fprintfTab(fp, indent);
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent + 1, flag0, flag1, flag2, flag3, flag4, flag5);	// exp
		if (sw == True && NextCell(top) != NULLCELL)
			fprintf(fp, ", ");
		if (sw == True) {
			if (fprintfComment(fp, com, CellLine(CellInfo0(top)), indent) == False)
				fprintf(fp, "\n");
		}

		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

	case N_MAPLIST:	/* N_MAPLIST info0=port, info1=exp */
		if (sw == True) {
			fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments
			fprintfTab(fp, indent);
		}
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// port
		if (sw == True)
			fprintf(fp, "\t=> ");
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// exp
		if (sw == True) {
			if (NextCell(top) != NULLCELL)
				fprintf(fp, ",");
			if (fprintfComment(fp, com, CellLine(CellInfo1(top)), indent) == False)
				fprintf(fp, "\n");
		}
		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

	case N_TASKCALL:	/* N_TASKCALL info0=list */
		if (sw == True) {
			fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments
			if (pre_flag3 == (int)((char *)NULL) && pre_flag5 == (int)((char *)NULL))
				fprintf(fp, "%c%s", REGION_BODY, REGION_MARK);
			fprintfTab(fp, indent);
			fprintf(fp, "%s(", CellStr(top));
		}
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// list
		if (sw == True) {
			fprintf(fp, ");");
			if (fprintfComment(fp, com, CellLine(CellInfo0(top)), indent) == False)
				fprintf(fp, "\n");
		}

		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

	case T_ASSIGN:	/* T_ASSIGN info0=sig, info1=exp */
		if (sw == True) {
			fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments
			if (pre_flag3 == (int)((char *)NULL) && pre_flag5 == (int)((char *)NULL))
				fprintf(fp, "%c%s", REGION_BODY, REGION_MARK);
			fprintfTab(fp, indent);
		}

		*flag0 = 0;	/* exp:arithmetic */
		*flag4 = 1;	/* change referred output name */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// sig
		*flag4 = pre_flag4;

		if (sw == True)
			fprintf(fp, "\t<= ");

		*flag0 = 0;	/* exp:arithmetic */
		*flag4 = 1;	/* change referred output name */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent + 1, flag0, flag1, flag2, flag3, flag4, flag5);	// exp
		*flag4 = pre_flag4;

		if (sw == True) {
			fprintf(fp, ";");
			if (fprintfComment(fp, com, CellLine(CellInfo1(top)), indent) == False)
				fprintf(fp, "\n");
		}
		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

	case T_SEL:		/* T_SEL info0=cond, info1=then, info2=else */
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// then
		if (sw == True)
			fprintf(fp, "\twhen ");
		*flag0 = 1;	/* exp:logic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// cond
		if (sw == True) {
			fprintf(fp, "\n");
			fprintfTab(fp, indent + 1);
			fprintf(fp, "else ");
		}
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo2(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// else
		break;

	case N_PAREN:	/* N_PAREN info0=exp */
		if (sw == True)
			fprintf(fp, "(");
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);
		if (sw == True)
			fprintf(fp, ")");
		break;

	case N_ALWAYS_COMB:		/* N_ALWAYS_COMB info0=sensitivity, info1=body */
	case N_ALWAYS_SYNCSEQ:	/* N_ALWAYS_SYNCSEQ info0=sensitivity(clk), info1=body */
	case N_ALWAYS_ASYNCSEQ:	/* N_ALWAYS_ASYNCSEQ info0=sensitivity(clk), info1=body, info2=sensitivity(reset) */
		*flag2 = 1;		// (inside of always sentence)

		if (sw == True) {
			fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments
			if (pre_flag3 == (int)((char *)NULL) && pre_flag5 == (int)((char *)NULL))
				fprintf(fp, "%c%s", REGION_BODY, REGION_MARK);
			fprintfTab(fp, indent);
			fprintf(fp, "v2v_pr_%d:process (", Sysid++);
		}
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// sensitivity
		if (sw == True) {
			if (CellType(top) == N_ALWAYS_ASYNCSEQ)
				fprintf(fp, ", %s", CellStr(CellInfo0(CellInfo2(top))));
			fprintf(fp, ")");
			if (fprintfComment(fp, com, CellLine(CellInfo0(top)), indent) == False)
				fprintf(fp, "\n");
			fprintfTab(fp, indent);
			fprintf(fp, "begin\n");
		}

		if (CellType(top) == N_ALWAYS_COMB)
			fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent + 1, flag0, flag1, flag2, flag3, flag4, flag5);	// body
		else if (CellType(top) == N_ALWAYS_SYNCSEQ) {
			if (sw == True) {
				fprintfTab(fp, indent + 1);
				fprintf(fp, "if (%s'event and %s = ", CellStr(CellInfo0(CellInfo0(top))), CellStr(CellInfo0(CellInfo0(top))));
				if (CellType(CellInfo0(top)) == T_POSEDGE)
					fprintf(fp, "'1') then\n");
				else
					fprintf(fp, "'0') then\n");
			}
			fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent + 2, flag0, flag1, flag2, flag3, flag4, flag5);	// body
			if (sw == True) {
				fprintfTab(fp, indent + 1);
				fprintf(fp, "end if;\n");
			}
		}
		else { /* N_ALWAYS_SYNCSEQ */
			*flag1	= (int)CellInfo0(top);		// clock
			fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent + 1, flag0, flag1, flag2, flag3, flag4, flag5);	// body
		}

		if (sw == True) {
			fprintfComment(fp, com, CellLine(CellInfo1(top)) - 1, indent);		// print remaining comments
			fprintfTab(fp, indent);
			fprintf(fp, "end process;\n");
		}

		*flag2 = 0;		// (outside of always sentence)
		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

	case T_POSEDGE:	/* T_POSEDGE info0=sig */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// sig
		break;

	case T_NEGEDGE:	/* T_NEGEDGE info0=sig */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// sig
		break;

	case N_SENSLIST:	/* N_SENSLIST info0=sig */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// sig
		if (sw == True && NextCell(top) != NULLCELL)
			fprintf(fp, ", ");
		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next list
		break;

	case N_SIGSUBST:	/* N_SIGSUBST info0=sig, info1=exp */
		if (sw == True) {
			fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments
			fprintfTab(fp, indent);
		}

		*flag0 = 0;	/* exp:arithmetic */
		*flag4 = 1;	/* change referred output name */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// sig
		*flag4 = pre_flag4;

		if (sw == True)
			fprintf(fp, "\t<= ");

		*flag0 = 0;	/* exp:arithmetic */
		*flag4 = 1;	/* change referred output name */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// exp
		*flag4 = pre_flag4;

		if (sw == True) {
			fprintf(fp, ";");
			if (fprintfComment(fp, com, CellLine(CellInfo1(top)), indent) == False)
				fprintf(fp, "\n");
		}
		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

	case N_VARSUBST:	/* N_VARSUBST info0=sig, info1=exp */
		if (sw == True) {
			fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments
			fprintfTab(fp, indent);
		}

		*flag0 = 0;	/* exp:arithmetic */
		*flag4 = 1;	/* change referred output name */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// sig
		*flag4 = pre_flag4;

		if (sw == True) {
			if (pre_flag2 == 0)
				fprintf(fp, "\t:= ");
			else
				fprintf(fp, "\t<= ");
		}

		*flag0 = 0;	/* exp:arithmetic */
		*flag4 = 1;	/* change referred output name */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// exp
		*flag4 = pre_flag4;

		if (sw == True) {
			fprintf(fp, ";");
			if (fprintfComment(fp, com, CellLine(CellInfo1(top)), indent) == False)
				fprintf(fp, "\n");
			if (pre_flag2 == 1) {
fprintfWarning(fp, CellLine(CellInfo1(top)), WARN_6_BLKSUBST);
			}
		}
		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

	case T_IF:			/* T_IF info0=cond, info1=then, info2=else */
	case T_ELSIF:		/* T_ELSIF info0=cond, info1=then, info2=else */
	case N_ASYNCRST_IF:	/* N_ASYNCRST_IF info0=cond, info1=then, info2=else */
		if (sw == True) {
			fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments
			fprintfTab(fp, indent);
			if (CellType(top) != T_ELSIF)
				fprintf(fp, "if ");
			else
				fprintf(fp, "elsif ");
		}

		*flag0 = 1;	/* exp:logic */
		*flag4 = 1;	/* change referred output name */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// cond
		*flag4 = pre_flag4;

		if (sw == True) {
			fprintf(fp, " then");
			if (fprintfComment(fp, com, CellLine(CellInfo0(top)), indent) == False)
				fprintf(fp, "\n");
		}
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent + 1, flag0, flag1, flag2, flag3, flag4, flag5);	// then

		if (CellInfo2(top) != NULLCELL) {
			if (CellType(top) == N_ASYNCRST_IF) {
				if (sw == True) {
					fprintfTab(fp, indent);
					fprintf(fp, "elsif (%s'event and %s = ", CellStr(CellInfo0((TCELLPNT)(*flag1))), CellStr(CellInfo0((TCELLPNT)(*flag1))));
				if (CellType((TCELLPNT)(*flag1)) == T_POSEDGE)
					fprintf(fp, "'1') then\n");
				else
					fprintf(fp, "'0') then\n");
				}
				fprintfVHDL(fp, sw, CellInfo2(top), sig, com, cmp, indent + 1, flag0, flag1, flag2, flag3, flag4, flag5);	// else
				if (sw == True) {
					fprintfTab(fp, indent);
					fprintf(fp, "end if;\n");
				}
			}
			else if (CellType(CellInfo2(top)) != T_ELSIF) {
				if (sw == True) {
					fprintfTab(fp, indent);
					fprintf(fp, "else\n");
				}
				fprintfVHDL(fp, sw, CellInfo2(top), sig, com, cmp, indent + 1, flag0, flag1, flag2, flag3, flag4, flag5);	// else
			}
			else {
				fprintfVHDL(fp, sw, CellInfo2(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// else
			}
		}

		if (sw == True) {
			if (CellInfo2(top) != NULLCELL)
				fprintfComment(fp, com, CellLine(CellInfo2(top)), indent);		// print remaining comments
			if (CellType(top) == T_IF) {
				fprintfTab(fp, indent);
				fprintf(fp, "end if;\n");
			}
		}

		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

	case T_CASE:	/* T_CASE info0=exp, info1=item  */
		if (sw == True) {
			fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments
			fprintfTab(fp, indent);
			fprintf(fp, "case (");
		}

		*flag0 = 0;	/* exp:arithmetic */
		*flag4 = 1;	/* change referred output name */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// cond
		*flag4 = pre_flag4;

		if (sw == True) {
			fprintf(fp, ") is");
			if (fprintfComment(fp, com, CellLine(CellInfo0(top)), indent) == False)
				fprintf(fp, "\n");
		}

		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// item
		if (sw == True) {
			fprintfComment(fp, com, CellLine(CellInfo1(top)), indent);		// print remaining comments
			fprintfTab(fp, indent);
			fprintf(fp, "end case;\n");
		}

		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next signal
		break;

	case N_CASECOND:	/* N_CASECOND info0=cond exp, info1=body  */
		if (sw == True) {
			fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments
			fprintfTab(fp, indent);
			fprintf(fp, "when ");
		}
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// cond
		if (sw == True) {
			fprintf(fp, " =>\n");
			fprintfComment(fp, com, CellLine(CellInfo0(top)), indent);		// print remaining comments
		}
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent + 1, flag0, flag1, flag2, flag3, flag4, flag5);	// body
		if (sw == True)
			fprintfComment(fp, com, CellLine(CellInfo1(top)), indent);		// print remaining comments

		fprintfVHDL(fp, sw, NextCell(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// next item
		break;

	case T_DEFAULT:
		if (sw == True)
			fprintf(fp, "others");
		break;

//////////////////////////////////////////////////////////////////////
// expression
//////////////////////////////////////////////////////////////////////
	case T_LOGIC_OR:	/* T_LOGIC_OR */
		*flag0 = pre_flag0;
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " or ");
		*flag0 = pre_flag0;
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_LOGIC_AND:	/* T_LOGIC_AND */
		*flag0 = pre_flag0;
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " and ");
		*flag0 = pre_flag0;
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_OR:	/* T_OR */
		*flag0 = pre_flag0;
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " or ");
		*flag0 = pre_flag0;
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_AND:	/* T_AND */
		*flag0 = pre_flag0;
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " and ");
		*flag0 = pre_flag0;
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_XOR:	/* T_XOR */
		*flag0 = pre_flag0;
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " xor ");
		*flag0 = pre_flag0;
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_LOGIC_EQ:	/* T_LOGIC_EQ */
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " = ");
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_LOGIC_NEQ:	/* T_LOGIC_NEQ */
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " /= ");
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_GE:	/* T_GE */
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " <= ");
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_LE:	/* T_LE */
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " >= ");
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_GT:	/* T_GT */
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " < ");
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_LS:	/* T_LS */
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " > ");
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_RSHIFT:	/* T_RSHIFT */
		if (sw == True) {
			fprintf(fp, "\n");
fprintfWarning(fp, CellLine(top), WARN_7_SHIFT);
		}
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " srl ");
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_LSHIFT:	/* T_LSHIFT */
		if (sw == True) {
			fprintf(fp, "\n");
fprintfWarning(fp, CellLine(top), WARN_7_SHIFT);
		}
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " sll ");
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_PLUS:	/* T_PLUS */
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " + ");
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_MINUS:	/* T_MINUS */
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " - ");
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_MULT:	/* T_MULT */
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " * ");
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_DIV:	/* T_DIV */
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " / ");
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_MOD:	/* T_MOD */
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		if (sw == True)
			fprintf(fp, " mod ");
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write R
		break;

	case T_NOT:	/* T_NOT */
		if (sw == True)
			fprintf(fp, "not ");
		*flag0 = pre_flag0;
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);
		break;

	case T_LOGIC_NOT:	/* T_LOGIC_NOT */
		if (sw == True)
			fprintf(fp, "not ");
		*flag0 = 1;	/* exp:logic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);
		break;

	case N_REDUCTION_AND:	/* N_REDUCTION_AND info0=exp */
		*flag0 = 0;	/* exp:arithmetic */
		if (sw == True) {
			fprintf(fp, "\n");
fprintfWarning(fp, CellLine(top), WARN_8_REDUCT);
			fprintf(fp, " & ");
		}
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		break;

	case N_REDUCTION_OR:	/* N_REDUCTION_OR info0=exp */
		*flag0 = 0;	/* exp:arithmetic */
		if (sw == True) {
			fprintf(fp, "\n");
fprintfWarning(fp, CellLine(top), WARN_8_REDUCT);
			fprintf(fp, " | ");
		}
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		break;

	case N_REDUCTION_XOR:	/* N_REDUCTION_XOR info0=exp */
		*flag0 = 0;	/* exp:arithmetic */
		if (sw == True) {
			fprintf(fp, "\n");
fprintfWarning(fp, CellLine(top), WARN_8_REDUCT);
			fprintf(fp, " ^ ");
		}
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// write L
		break;

	case T_WIDTH_BINDIGIT:	/* T_WIDTH_BINDIGIT */
		if (sw == True) {
			char *ptr;
			for (ptr = CellStr(top); *ptr != 'b' && *ptr != 'B'; ptr++)
				;
			ptr++;
			if (pre_flag0 != 0)
				fprintf(fp, "(");
			if (strlen(ptr) == 1)
				fprintf(fp, "'%s'", ptr);
			else
				fprintf(fp, "\"%s\"", ptr);
			if (pre_flag0 != 0)
				fprintf(fp, " = '1')");
		}
		break;

	case T_WIDTH_DECDIGIT:	/* T_WIDTH_DECDIGIT */
		if (sw == True) {
			char *cpystr, *width, *digit;
			cpystr = (char *)malloc(strlen(CellStr(top)) + 1);
			strcpy(cpystr, CellStr(top));

			for (digit = CellStr(top), width = cpystr; *digit != 'd' && *digit != 'D'; digit++, width++)
				;
			digit++;			// digit points top of number
			*(--width) = '\0';	// *(--width) was '''

			if (pre_flag0 != 0)
				fprintf(fp, "(");
			fprintf(fp, "CONV_STD_LOGIC_VECTOR(%s,%s)", digit, cpystr);
			if (pre_flag0 != 0)
				fprintf(fp, " = '1')");

			free(cpystr);
		}
		break;

	case T_WIDTH_HEXDIGIT:	/* T_WIDTH_HEXDIGIT */
		if (sw == True) {
			char *ptr;
			for (ptr = CellStr(top); *ptr != 'h' && *ptr != 'H'; ptr++)
				;
			ptr++;
			if (pre_flag0 != 0)
				fprintf(fp, "(");
			fprintf(fp, "To_StdLogicVector(X\"%s\")", ptr);
			if (pre_flag0 != 0)
				fprintf(fp, " = '1')");
		}
		break;

	case T_DECDIGIT:	/* T_DECDIGIT */
		if (sw == True) {
			if (pre_flag0 != 0)
				fprintf(fp, "(");
			fprintf(fp, "%s", CellStr(top));
			if (pre_flag0 != 0)
				fprintf(fp, " = '1')");
		}
		break;

	case N_FUNCCALL:	/* N_FUNCCALL info0=list */
		if (sw == True) {
			if (pre_flag0 != 0)
				fprintf(fp, "(");
			fprintf(fp, "%s(", CellStr(top));
		}
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// list
		if (sw == True) {
			fprintf(fp, ")");
			if (pre_flag0 != 0)
				fprintf(fp, " = '1')");
		}
		break;

	case N_COPYSIG:	/* N_COPYSIG info0=copy times, info1=expr */
		{
			int times, i;
			if (EvalExpValue(CellInfo0(top), &times) == True && times > 0) {
				if (sw == True)
					fprintf(fp, "(");
				for (i = 1; i <= times; i++) {
					*flag0 = 0;	/* exp:arithmetic */
					fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// exp
					if (i < times && sw == True)
						fprintf(fp, " & ");
				}
				if (sw == True)
					fprintf(fp, ")");
			}
			else {
				if (sw == True) {
					fprintf(fp, "\n");
fprintfWarning(fp, CellLine(top), WARN_9_CONCAT);
					fprintf(fp, "{");
				}
				fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// times
				if (sw == True)
					fprintf(fp, "{");
				fprintfVHDL(fp, sw, CellInfo1(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// exp
				if (sw == True)
					fprintf(fp, "}}");
			}
		}
		break;

	case N_SIG_WIDTH:	/* N_SIG_WIDTH info0=width */
		if (sw == True) {
			if (pre_flag0 != 0)
				fprintf(fp, "(");

			{
				SIGLIST	*sigptr = SearchSignal(sig, CellStr(top));
				if (*flag4 == 1 && sigptr != NULLSIG && SigRefer(sigptr) == True)
					fprintf(fp, "%s%s", CellStr(top), REFOUTSIG_SUFFIX);
				else
					fprintfSigName(fp, CellStr(top), (char *)pre_flag3);
			}

			fprintf(fp, "(");
		}
		*flag0 = 0;	/* exp:arithmetic */
		fprintfVHDL(fp, sw, CellInfo0(top), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// width
		if (sw == True) {
			fprintf(fp, ")");
			if (pre_flag0 != 0)
				fprintf(fp, " = '1')");
		}
		break;

	case T_ID:
	case N_SIG:	/* N_SIG */
		if (sw == True) {
			if (pre_flag0 != 0)
				fprintf(fp, "(");

			{
				SIGLIST	*sigptr = SearchSignal(sig, CellStr(top));
				if (*flag4 == 1 && sigptr != NULLSIG && SigRefer(sigptr) == True)
					fprintf(fp, "%s%s", CellStr(top), REFOUTSIG_SUFFIX);
				else
					fprintfSigName(fp, CellStr(top), (char *)pre_flag3);
			}

			if (pre_flag0 != 0)
				fprintf(fp, " = '1')");
		}
		break;

	case N_CONCAT:	/* N_CONCAT info0=list */
		{
			TCELLPNT ptr;
			if (sw == True) {
				if (pre_flag0 != 0)
					fprintf(fp, "(");
				fprintf(fp, "(");
			}
			for (ptr = CellInfo0(top); ptr != NULLCELL; ptr = NextCell(ptr)) {
				/* N_EXPLIST info0=exp */
				*flag0 = 0;	/* exp:arithmetic */
				fprintfVHDL(fp, sw, CellInfo0(ptr), sig, com, cmp, indent, flag0, flag1, flag2, flag3, flag4, flag5);	// exp
				if (sw == True && NextCell(ptr) != NULLCELL)
					fprintf(fp, " & ");
			}
			if (sw == True) {
				fprintf(fp, ")");
				if (pre_flag0 != 0)
					fprintf(fp, " = '1')");
			}
		}
		break;

	case N_NULL:
		if (sw == True) {
			fprintfComment(fp, com, CellLine(top) - 1, indent);		// print remaining comments
			fprintfTab(fp, indent);
			fprintf(fp, "null;");
			if (fprintfComment(fp, com, CellLine(top), indent) == False)
				fprintf(fp, "\n");
		}
		break;

	default:
		if (sw == True)
			fprintf(stderr, "INTERNAL ERROR: fprintfVHDL(): unknown node %d\n", CellType(top));
	}

	fflush(fp);
}


//////////////////////////////////////////////////////////////////////
// PASS2(re-order lines) functions
//////////////////////////////////////////////////////////////////////

// make new line list
// return:	Top of line list
LINELIST *MakeNewLineList(void)
{
	register LINELIST	*retval;
	retval = (LINELIST *)malloc(sizeof (LINELIST));
	if (retval != (LINELIST *)NULL) {
		NextLine(retval)	= (LINELIST *)NULL;
		LineStr(retval)		= NULLSTR;
	}
	else {
		fprintf(stderr, "INTERNAL ERROR: malloc failure in MakeNewLineList()\n");
	}
	return (retval);
}

// free line list
// parameter:
//		top:	top of line list
void	FreeLineList(LINELIST *top)
{
	register	LINELIST *ptr, *nextptr;
	if (top == (LINELIST *)NULL)
		return;
	for (ptr = top; ptr != (LINELIST *)NULL; ptr = nextptr) {
		nextptr	= NextLine(ptr);
		if (LineStr(ptr) != NULLSTR)
			free(LineStr(ptr));
		free(ptr);
	}
}

// append new line to line list
// parameter:
//		top:		top of line list
//		line:		line string
//		typ:		line region code
void RegisterLine(LINELIST *top, char *line, char typ)
{
	register LINELIST	*ptr;
	if (top == (LINELIST *)NULL)
		return;
	for (ptr = top; NextLine(ptr) != (LINELIST *)NULL; ptr = NextLine(ptr))
		;

	LineStr(ptr)	= (char *)malloc(strlen(line) + 1);
	strcpy(LineStr(ptr), line);
	LineType(ptr)	= typ;
	LinePrn(ptr)	= False;
	NextLine(ptr)	= MakeNewLineList();
}

// erase the last semicolon in the given string
// parameter:
//		buf:		string
static void eraseSemicolon(char *buf)
{
	register	char *ptr;
	if (buf == NULLSTR)
		return;
	for (ptr = buf; *ptr != '\0' && *ptr != ';'; ptr++)
		;
	if (*ptr == ';')
		*ptr = ' ';
}

// check if the last charactor of the given string is ';' or not
// parameter:
//		buf:		string
// return:	True, if it is ';'
static Boolean containSemicolon(char *buf)
{
	register	char *ptr;
	if (buf == NULLSTR)
		return (False);
	for (ptr = buf; *ptr != '\0' && *ptr != ';'; ptr++) {
		if (*ptr == '-' && *(ptr + 1) == '-')
			break;
	}
	if (*ptr == ';')
		return (True);
	return (False);
}

// check if the given line is the last I/O definition or not.
// parameter:
//		pos:		line
// return:	True, if it is the last I/O definition.
static Boolean isLastIOdef(LINELIST *pos)
{
	register LINELIST	*ptr;
	if (pos == (LINELIST *)NULL || NextLine(pos) == (LINELIST *)NULL)
		return (True);
	for (ptr = NextLine(pos); NextLine(ptr) != (LINELIST *)NULL; ptr = NextLine(ptr)) {
		if (LinePrn(ptr) == True)
			continue;
		else if (LineType(ptr) == REGION_HEAD)
			return (True);
		else if (LineType(ptr) == REGION_IO && containSemicolon(LineStr(ptr)) == True)
			return (False);
	}
	return (True);
}

// write lines
// parameter:
//		fp:			output file pointer
//		top:		top of line list
//		typ:		region (type) of lines to be printed. (Lines whose region is other than 'typ' will not be printed)
// return:	False, if no line is printed
Boolean fprintfLine(FILE *fp, LINELIST *top, char typ)
{
	register LINELIST	*ptr;
	register Boolean	retval;
	if (top == (LINELIST *)NULL)
		return (False);
	for (ptr = top, retval = False; NextLine(ptr) != (LINELIST *)NULL; ptr = NextLine(ptr)) {
		if (LinePrn(ptr) == True)
			continue;
		else if (typ == REGION_HEAD) {	// print while REGION_HEAD continues
			if (LineType(ptr) != REGION_HEAD)
				return (retval);
			fprintf(fp, "%s", LineStr(ptr));
			LinePrn(ptr)	= True;
			retval			= True;
		}
		else {	// print until REGION_HEAD comes
			if (LineType(ptr) == REGION_HEAD)
				return (retval);
			else if (LineType(ptr) != typ)
				continue;
			else {
				if (LineType(ptr) == REGION_IO && isLastIOdef(ptr) == True)
					eraseSemicolon(LineStr(ptr));

				fprintf(fp, "%s", LineStr(ptr));
				LinePrn(ptr)	= True;
				retval			= True;
			}
		}
	}
	return (retval);
}


//////////////////////////////////////////////////////////////////////
// Main
//////////////////////////////////////////////////////////////////////

void main(int argc, char *argv[])
{
	FILE 	*fptmp, *fpout;
	char	*outfname, *ptr;
	int		argctr;
	int		flag0, flag1, flag2, flag3, flag4, flag5;	// (for fprintfVHDL)

	fprintf(stderr, "%s\n", TITLE);

	if (argc < 2) {
		fprintf(stderr, "usage: verilog2vhdl inputfile1 inputfile2 ...\n");
#ifdef WINDOWS
		getchar();
#endif
		exit(1);
	}

	for (argctr = 1; argctr < argc; argctr++) {	// for each input files
	// open files
		if ((yyin = fopen(argv[argctr], "rt")) == (FILE *)NULL) {
			fprintf(stderr, "Can not open input file %s\n", argv[argctr]);
			continue;
		}

		// (PASS1 temporarly file)
	if ((fptmp = fopen(PASS1_FNAME, "wt")) == (FILE *)NULL) {
			fprintf(stderr, "Can not create temporary file %s\n", PASS1_FNAME);
			continue;
		}

		// (make output file name)
		outfname = (char *)malloc(strlen(argv[argctr]) + 100);
		strcpy(outfname, argv[argctr]);
		for (ptr = outfname + strlen(outfname) - 1; ptr != outfname; ptr--) {
			if (*ptr == '\\')
				break;
			if (*ptr == '.') {
				*ptr = '\0';
				break;
			}
		}
		strcat(outfname, ".vhd");

		if (!strcmp(outfname, argv[argctr])) {	// check input file == output file or not
			fprintf(stderr, "Can not overwrite input file %s by output file\n", outfname);
			continue;
		}
		if ((fpout = fopen(outfname, "wt")) == (FILE *)NULL) {
			fprintf(stderr, "Can not create output file %s\n", outfname);
			continue;
		}

	// init data structure
		ParseTreeTop	= NULLCELL;
		ParseError		= False;

		SigListTop		= MakeNewSigList();
		ComponentListTop= MakeNewComponentList();
		CommentListTop	= MakeNewCommentList();
		LineListTop		= MakeNewLineList();

		fprintf(stderr, "\nInput file: %s\n", argv[argctr]);

		yylexlinenum	= 1;
		yyout			= fopen(TMPFNAME, "wt");

	// main
		yyrestart(yyin);	// init flex
		yyparse();			// parse

		fclose(yyin);
		fclose(yyout);
		unlink(TMPFNAME);

		if (ParseError == False) {
			char 	linetype;

			Sysid = 0;

		// PASS1: generate code
			fprintfVHDL(fptmp, True, ParseTreeTop, NULLSIG, CommentListTop, NULLCMP, 0, &flag0, &flag1, &flag2, &flag3, &flag4, &flag5);
			fclose(fptmp);

		// PASS2: sort lines
			// read PASS1 output
			if ((fptmp = fopen(PASS1_FNAME, "rt")) == (FILE *)NULL) {
				fprintf(stderr, "Can not open temporary file %s\n", PASS1_FNAME);
				unlink(PASS1_FNAME);
				fclose(fpout);
				unlink(outfname);
				continue;
			}
			for (linetype = REGION_HEAD; !feof(fptmp);) {
				char	linebuf[1024 * 64];			// (line buf: 64KB)
				fgets((char *)linebuf, 1024 * 64 - 1, fptmp);
				if (!strcmp(linebuf + 1, REGION_MARK)) {
					linetype = linebuf[0];
					continue;
				}
				if (!feof(fptmp))
					RegisterLine(LineListTop, linebuf, linetype);
			}
			fclose(fptmp);
			unlink(PASS1_FNAME);	// erase PASS1 output

			// write PASS2 output
			fprintf(fpout, "-- Converted from %s\n", argv[argctr]);
			fprintf(fpout, "-- by %s\n\n", TITLE);

			for (;;) {
				if (fprintfLine(fpout, LineListTop, REGION_HEAD) == False)	// HEADER
					break;
				fprintfLine(fpout, LineListTop, REGION_IO);		// I/O declaration
				fprintfLine(fpout, LineListTop, REGION_SEP);	// separators
				fprintfLine(fpout, LineListTop, REGION_DEF);	// definitions
				fprintf(fpout, "\nbegin\n\n");
				fprintfLine(fpout, LineListTop, REGION_BODY);	// body
			}

			fclose(fpout);
		}
		else {	// parse error
			fclose(fptmp);
			fclose(fpout);
			unlink(outfname);
		}

	// release memory
		free(outfname);
		FreeTree(ParseTreeTop);
		FreeSigList(SigListTop);
		FreeComponentList(ComponentListTop);
		FreeCommentList(CommentListTop);
		FreeLineList(LineListTop);
	}

	fprintf(stderr, "Complete.\n");
#ifdef WINDOWS
	getchar();
#endif

	exit(0);
}

// end of file
