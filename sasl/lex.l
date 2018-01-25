/* scanner for sasl */
     
%{
#include <math.h>	/* need this for the call to atof() below */
#include <string.h>	/* need this for strchr() */
#include "lex.h"	/* token definitions etc */
%}

SIGN	 [+-]
DIGIT    [0-9]
EXP	 [eE]{SIGN}?{DIGIT}+

NAME       [A-Za-z][A-Za-z0-9_]*


	static tok lex_token;
	static int lex_got_token = 0;	/* is lex_token ready and unconsumed? */

	int lex_col = 0;	/* current column (fixed width character set ;-), source program columns are numbered 1..  */
	int lex_margin = 0;	/* offside column, token is offside when (col <= margin), initially 0 */
	int lex_got_newline = 0;	/* 1 iff current token is preceded by newline (since previous token) */
	static int margins[1000] = {0};
	static int m = 0;	 /* margins[m] is current offside setting */

	tok_const lex_tc;	/* indicator set when tok_constant found - indicates which constant is found */

	tok_op lex_oper;       	/* indicator set when tok_operator found - which operator */
	char lex_oper_prio;	/* indicator set when tok_operator found - operator priority */
	char *lex_oper_fix;	/* indicator set when tok_operator found - "i"=infix, "p"=prefix, "o"=postfix also "ip" if can be prefix *or* infix */
	char lex_oper_assoc;	/* indicator set when tok_operator found - 'r' right associative, 'l' left acssociative, 'n'=non-associative, '? ???*/

	void lex_offside() /* set current column as limit of offside */
	{
	  m++; /* assert(m<max) */
	  margins[m] = lex_margin = lex_col;
	  if (debug>1)
	    fprintf(stderr,"offside:%d\n", lex_margin);
	}

	static void scan();
	void lex_onside()
	{
	  m--; /* assert(m>=0) */
	  lex_margin = margins[m];
	  if (debug>1)
	    fprintf(stderr,"onside:%d\n", lex_margin);
	  scan();/* is a token revelealed by the new margin? */
	}

	/* if token is offsideable and is offside, return tok_offside; otherwise return the token updating lex_col */
	/* off!==0 => offsideable */

	/* add width of current token to lex_col, allowing for tabs */
	static void grow()
	{
	  char *s;
	  for (s = yytext; *s; s++) {
	    if (*s == '\n')
	      lex_col = 0;
	    if (*s == '\t')
	      lex_col = ((lex_col / 8) + 1) * 8;
	    else
	      lex_col++;
	  }
	}

	/* grow lex_col for current match and return the token */
	static tok indent(tok t)
	{
	  grow();
	  return t;
	}

	static tok off(tok t);

%x STRING
	int lex_nesting = 0;
%%

	/* delimiters */

WHERE		{ LEX_DEBUG("A Where: %s\n", yytext); return off(tok_where); }
"->"		{ LEX_DEBUG("A then: %s\n", yytext); return off(tok_then); }

"?"		{ LEX_DEBUG("A question mark: %s\n", yytext); return indent(tok_question_mark); }

"("		{ LEX_DEBUG("A left paren: %s\n", yytext); return off(tok_left_paren); }
")"		{ LEX_DEBUG("A right paren: %s\n", yytext); return off(tok_right_paren); }
";"		{ LEX_DEBUG("A semi-colon: %s\n", yytext); return off(tok_semicolon); }
","		{ LEX_DEBUG("A comma: %s\n", yytext); return off(tok_comma); }

"<-"		{ LEX_DEBUG("A generator: %s\n", yytext); return off(tok_generator); /* ZF */ }
"{"		{ LEX_DEBUG("A left paren: %s\n", yytext);  return off(tok_left_brace); /* ZF */}
"}"		{ LEX_DEBUG("A right paren: %s\n", yytext); return off(tok_right_brace); /* ZF */ }

	/* Constants - all tok_constant, with associated tok_const_xxx subtype */
	/* numbers */

{DIGIT}+	{ LEX_DEBUG2( "An integer: %s (%d)\n", yytext, atoi( yytext ) ); lex_tc = tok_const_integer; return off(tok_constant); /* prefer to make integers; TODO make float iff integer has too many digts */ }

	/* floating point numbers
	atof() format definition:
		"decimal floating point expression. It consists of the following parts:
		1.(optional) plus or minus sign
		2.nonempty sequence of decimal digits optionally containing decimal point character (as determined by the current C locale) (defines significand)
		3.(optional) e or E followed with optional minus or plus sign and nonempty sequence of decimal digits (defines exponent)"

	need three cases to cover "non-empty sequenc of decimal digits optionally including decimal point", the rest is simple
	note leading SIGN +/- is dealt with as unary operator below
	*/     
{DIGIT}+{EXP} |
{DIGIT}+"."{DIGIT}*{EXP}? |
{DIGIT}*"."{DIGIT}+{EXP}?	{ LEX_DEBUG2( "A float: %s (%g)\n", yytext, atof(yytext)); lex_tc = tok_const_floating; return off(tok_constant); }

	/* truth values */

TRUE	{ LEX_DEBUG("A boolconst: %s\n", yytext); lex_tc = tok_const_true; return off(tok_constant); }
FALSE	{ LEX_DEBUG("A boolconst: %s\n", yytext); lex_tc = tok_const_false; return off(tok_constant); }

	/* characters */
"%".		{ LEX_DEBUG("A charconst: %s\n", yytext+1); lex_tc = tok_const_char; return off(tok_constant); /* the character is in yytext[1] */ }
NL	{ LEX_DEBUG("A special char: %s\n", yytext); lex_tc = tok_const_special_char_NL; return off(tok_constant); /* the special char is in yytext[0]*/ }
NP	{ LEX_DEBUG("A special char: %s\n", yytext); lex_tc = tok_const_special_char_NP; return off(tok_constant); /* the special char is in yytext[0]*/ }
SP	{ LEX_DEBUG("A special char: %s\n", yytext); lex_tc = tok_const_special_char_SP; return off(tok_constant); /* the special char is in yytext[0]*/ }
TAB	{ LEX_DEBUG("A special char: %s\n", yytext); lex_tc = tok_const_special_char_TAB; return off(tok_constant); /* the special char is in yytext[0]*/ }

	/* nil */
"()"		{ LEX_DEBUG("A nil: %s\n", yytext); lex_tc = tok_const_nil; return off(tok_constant); }

	/* end of Constants */

	/* string */

'		{ BEGIN(STRING); LEX_DEBUG("Start string: %s\n", yytext); lex_nesting = 1; lex_tc = tok_const_string_start; return off(tok_constant); }
<STRING>'	{ LEX_DEBUG("Start nested string: %s\n", yytext); lex_nesting++; lex_tc = tok_const_string_start_nested; return indent(tok_constant); }
<STRING>"\""	{
  			if (--lex_nesting <= 0) 
			{
				BEGIN(INITIAL);	/* reset prefix */	
 				LEX_DEBUG("String end: %s\n", yytext);
				lex_tc = tok_const_string_end;
			}
			else
			{
    				LEX_DEBUG("String nested end: %s\n", yytext);
				lex_tc = tok_const_string_end_nested;
			}
			return indent(tok_constant);
		}
<STRING>.|\n	{ LEX_DEBUG("String item: %s\n", yytext); lex_tc = tok_const_string_char; return indent(tok_constant); /* allow string to contain newlines */ }

	/* End Constants */

	/* comments */

"||".*\n	{ LEX_DEBUG("A Comment: %s", yytext); yyless(yyleng - 1); grow(); /* put back the newline terminator - consume without returning */}

	/* whitespace */
^[ \t]+	{ grow();  /* leading whitespace - calculate indentation level - consume without returning */ }

[ \t]+		{ LEX_DEBUG2("Whitespace \"%s\" (%lu characters)\n", yytext, strlen(yytext)); grow();  /* no need to return a token */ }

\n		{ LEX_DEBUG0("A Newline.\n"); lex_got_newline = 1; grow();  /* no need to return a token */ }

	/* Operators - in order from 1 of increasing binding power */
":"	{ lex_oper_fix = "i"; lex_oper_assoc = 'r'; lex_oper = op_colon; return off(tok_operator); /* <<-- lhs/rhs symbol overload to be sorted in parser */ }
"++"	{ lex_oper_fix = "i"; lex_oper_assoc = 'r'; lex_oper = op_plusplus; return off(tok_operator); }
"--"	{ lex_oper_fix = "i"; lex_oper_assoc = 'r'; lex_oper = op_minusminus; return off(tok_operator); }

"."	{ lex_oper_fix = "i"; lex_oper_assoc = 'r'; lex_oper = op_period; return off(tok_operator);
	  /* period - [Turner 1983]: not in the language definition, but *is* in the prelude */
	  /* we choose to make is the most binding infix operator, binding to the right */
	  /* "f.g.h x" is interpreted as f.g.(h x) and in turn as "f (g (h x))" */
	}     

".."	{ lex_oper_fix = "i"; lex_oper_assoc = 'n'; lex_oper = op_range; return off(tok_operator); }	/* ZF */

"..."	{ lex_oper_fix = "o"; lex_oper_assoc = '?'; lex_oper = op_range_unbounded; return off(tok_operator); }	/* ZF */

"|"	{ lex_oper_fix = "i"; lex_oper_assoc = 'l'; lex_oper = op_or; return off(tok_operator); }

"&"	{ lex_oper_fix = "i"; lex_oper_assoc = 'l'; lex_oper = op_and; return off(tok_operator); }

"~"	{ lex_oper_fix = "p"; lex_oper_assoc = '?'; lex_oper = op_unary_not; return off(tok_operator); }

">>"	{ lex_oper_fix = "i"; lex_oper_assoc = 'l'; lex_oper = op_much_greater; return off(tok_operator); }
">"	{ lex_oper_fix = "i"; lex_oper_assoc = 'l'; lex_oper = op_greater; return off(tok_operator); }
">="	{ lex_oper_fix = "i"; lex_oper_assoc = 'l'; lex_oper = op_greater_equal; return off(tok_operator); }
"="	{ lex_oper_fix = "i"; lex_oper_assoc = 'l'; lex_oper = op_equal; return off(tok_operator); }
"∼="	{ lex_oper_fix = "i"; lex_oper_assoc = 'l'; lex_oper = op_not_equal; return off(tok_operator); }
"<="	{ lex_oper_fix = "i"; lex_oper_assoc = 'l'; lex_oper = op_less_equal; return off(tok_operator); }
"<"	{ lex_oper_fix = "i"; lex_oper_assoc = 'l'; lex_oper = op_less; return off(tok_operator); }
"<<"	{ lex_oper_fix = "i"; lex_oper_assoc = 'l'; lex_oper = op_much_less; return off(tok_operator); }

"+"	{ lex_oper_fix = "ip"; lex_oper_assoc = 'l'; lex_oper = op_plus; return off(tok_operator); } /* <<-- prefix/infix TODO different assoc if necessary */
"-"	{ lex_oper_fix = "ip"; lex_oper_assoc = 'l'; lex_oper = op_minus; return off(tok_operator); } /* <<-- prefix/infix TODO different assoc if necessary */

"\*"	{ lex_oper_fix = "i"; lex_oper_assoc = 'l'; lex_oper = op_times; return off(tok_operator); }
"/"	{ lex_oper_fix = "i"; lex_oper_assoc = 'l'; lex_oper = op_divide; return off(tok_operator); }
"DIV"	{ lex_oper_fix = "i"; lex_oper_assoc = 'l'; lex_oper = op_int_divide; return off(tok_operator); }
"REM"	{ lex_oper_fix = "i"; lex_oper_assoc = 'l'; lex_oper = op_rem; return off(tok_operator); }

"\*\*"	{ lex_oper_fix = "i"; lex_oper_assoc = 'r'; lex_oper = op_power; return off(tok_operator); }

"#"	{ lex_oper_fix = "p"; lex_oper_assoc = '?'; lex_oper = op_unary_count; return off(tok_operator); }	/* ZF */


	/* directives - for interactive use, not part of the sasl languages itself */

^DEF.*		{ LEX_DEBUG( "DEF: %s\n", yytext); return indent(tok_def); }
	
^COUNT.* |
^GC.* |
^GET.* |
^HELP.* |
^INTERACTIVE.* |
^OBJECT.* |
^OFF.* |
^READ.* |	/* TODO READ is a <simple> not a Command ... fix it */
^RESET.* |
^TO.* |
^WRITE.*	{ LEX_DEBUG( "A command: %s\n", yytext); return indent(tok_command); 	/* "interactive" user commands - yytext includes remainder of line to be analysed */ }
^"!".*		{ LEX_DEBUG( "A shell command: %s\n", yytext+1); return indent(tok_command); /* shell escape - yytext includes remainder of line to be executed */ }

	 /* names */     

{NAME}		{ LEX_DEBUG( "A name: %s\n", yytext ); return off(tok_name); }

	/* Unrecognised */

.		{ LEX_DEBUG2( "Unrecognized character: %s (%u)\n", yytext, (unsigned) yytext[0] ); grow(); }

%%
     

int yywrap() { return(1);}	/* Return True - yes exit when first EOF encountered */

/* 
 * lexer - lex...
 * flex(1) does such a beautiful job we don't need much here
 */

/*
 * lex_get_prio(oper, fix) - return the priority for given operator, 
 * if oper has more than one prio then use assoc to dsiambiguate
 * Used to set extern lex_oper_prio
 */
static int lex_get_prio(tok_op op, char fix)
{
  switch(op) {
  case op_colon: return 1;
  case op_plusplus: return 1;
  case op_minusminus: return 1;

  case op_range: return 2;

  case op_range_unbounded: return 3;

  case op_or: return 4;

  case op_and: return 5;

  case op_unary_not: return 6;

  case op_much_greater: return 7;
  case op_greater: return 7;
  case op_greater_equal: return 7;
  case op_equal: return 7;
  case op_not_equal: return 7;
  case op_less_equal: return 7;
  case op_less: return 7;
  case op_much_less: return 7;

  case op_plus: return (fix == 'i' ? 8 : 9);
  case op_minus: return (fix == 'i' ? 8 : 9);

  case op_times: return 10;
  case op_divide: return 10;
  case op_int_divide: return 10;
  case op_rem: return 10;

  case op_power: return 11;

  case op_period: return 12;

  case op_unary_count: return 13;

  /*no 'default:' - compiler then checks all cases are covered */

  }
  /*NOTREACHED*/
}

int lex_depth = 0; /* how many characters before the current token eg 0 when no indent, 8 when one tab indent */
int lex_new_depth =0;	/* "lex_depth = yyleng" = how many characters to end of current token */

/*
 * lex_looking_at - conditionally get token.
 * returns 1 and consumes token iff the lookahead token is t
 * otherwise 0 is returned and the lookahead token is unchanged
 *
 * NB lex_looking_at() can be called multiple times between calls to yylex()
 */

static void scan()
{
  /* ensure lookahead token is set - may be initial token, or following offside */
  if (!lex_got_token || lex_token == tok_offside) {
    lex_token = yylex();
    lex_got_token = 1;
  }
  else if (lex_token == tok_offside)
    LEX_DEBUG0("scan: got token but it's tok_offside\n");/* error */
}

/* check if token is offside, return tok_offside, otherwise indent and return the token */
static tok off(tok t)
{
  if (lex_col < lex_margin) {
    LEX_DEBUG("It's offside: %s\n", yytext);
    yyless(0);	/* put the token back for reprocessing when margin changes */
    return tok_offside;
  }
  grow();
  return t;
}

/*
 * consume() - use the current token (and its preceeding newline)
 */
static void consume()
{
      lex_got_newline = 0;
      lex_got_token = 0;
}

/* match current token and consume it */
int lex_looking_at(tok t)
{
  /* ensure lookahead token is set */
  scan();

  /* if match then consume lookahead token */
  if (lex_token == t) {
    consume();
    return 1;
    }
  return 0;
}

#ifdef nolonger
/* examine token without consuming it */
/* NB when parsing need to use this, as lex_got_token may be 0 */
int lex_peeking_at(tok t)
{
  scan();
  return lex_token == t;
}
#endif

int lex_peeking_at_simple()
{
  scan();
  return (lex_token == tok_name) ||
    (lex_token == tok_constant) ||
    (lex_token == tok_left_paren);
}

/* match either current token, or else a newline since previous token, followed by <simple> */

/* looking_at(tok_semicolon) || (got_newline && peeking_at(<simple>)) */
/* if looking_at(tok_semicolon) must have another clause
 * if newline then clause is optional - if the next token is onside then clause else not 
 */
int lex_looking_at_or_onside_newline(tok t)
{
  if (lex_looking_at(t))
    return 1;

  if (lex_got_newline && lex_peeking_at_simple()) {
    lex_got_newline = 0;
    return 1;
  }

  return 0;
}

/* match a token; if "no_newline" then only match when no newline has been seen since previus token */
int lex_looking_at_no_newline(tok t, char no_newline)
{
  /* ensure token is set */
  scan();

  if (no_newline && lex_got_newline)
    return 0; /* do not consume token or newline */

  if (lex_token == t) {
    consume();
    return 1;
  }
  return 0;
}

/*
 * special case to match specific operator tokens */
int lex_looking_at_operator(tok_op op)
{
  /* ensure token is set */
  scan();

  /* if match then consume lookahead token */
  if (lex_token == tok_operator && lex_oper == op) {
    consume();
    return 1;
  }
  return 0;  
}

/* 
 * special case to identify operator classes, and of priority>=p (more binding or same)
 * return operator priority (>0) of matched operators, otherwise 0
 */
static char the_fix[] = " ";

int lex_looking_at_operator_fix_prio(char fix, char prio)
{
  /* ensure lookahead token is set */
  scan();

  /* if match then consume lookahead token, and set priority - which depending on 'fix' */
  if (lex_token == tok_operator && (strchr(lex_oper_fix, fix) != NULL)) {
    /* got a operator with matching fix */
    the_fix[0] = fix; lex_oper_fix = the_fix; /* make this "sticky" - set the lex_oper_fix even if prio is wrong */
    lex_oper_prio = lex_get_prio(lex_oper, fix);
    LEX_DEBUG2("An operator: prio %d fix \"%s\"\n", lex_oper_prio, lex_oper_fix);
    /* check priority */
    if (lex_oper_prio >= prio) { 
      consume();
      return lex_oper_prio;
    }
  }
  return 0;
}
