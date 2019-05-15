#include <stdio.h>

#include "common.h"

#include "lex.h"
#include "make.h"

#include "parse.h"

/*
 * Helper functions
 */

/* recover from error */
pointer parse_reset()
{
  (void) make_reset();
  return NIL;
}

/* report parse error */
static int parse_err(char *f, char *msg1, char *msg2)
{
  (void) parse_reset();
  err_parse(f, msg1, msg2);
  /*NOTREACHED*/
  return 0;
}

/* locate undefined names in and expr and raise an error */
/* NB must only be called when there is no possibility of cycles in n */
/*
 * check ()   = ()
 * deprecated for make_bind() style defs: check def  =  check exprlist WHERE (n:namelist:exprlist) = def
 * check (a:x) = isatom a -> (isname a -> ERROR; check x); (check a : check x)?
 */

/*
 * Iterative check - MacOS stack is limited to depth 512 then aborts
 */
static pointer *stack;
static pointer *sp;
static unsigned stack_size = 0;

#define Stacked (sp-stack)
#define Push(p) (Assert(Stacked < stack_size), *sp++ = (p))
#define Pop     (Assert(Stacked > 0),         (*--sp))

#define Limit 12  /*arbitrary limit to output */

static void parse_check_log()
{
  pointer *pp;
  
  /* ToDo user friendly diagnostics */
  for (pp = sp - 1; pp > stack; pp--)
    out_debug_limit(*pp, Limit);

  return;
}

static pointer parse_check_do(pointer n, char *msg)
{
  
  while (1) {
    
    while (IsStruct(n)) {
      Push(n);
      n = H(n);
    }
    
    if (IsName(n)) {
      parse_check_log();
      parse_err("undefined name", Name(n), msg);
    }
    
    if (Stacked == 0)
      return n;
    
    n = Pop;
    Assert(IsStruct(n));
    n = T(n);
  }
}

/* parse_check - error if unbound names found */
pointer parse_check(pointer n, char *msg)
{
  extern unsigned refc_inuse(void); /**/
  
  /* Assert(is_tree(n)) - NO loops! */
  
  stack_size = refc_inuse();
  sp = stack = new_table(stack_size , sizeof(pointer));
  
  parse_check_do(n, msg);
  
  free_table(stack);
  
  return n;
}

#undef Stacked
#undef Push
#undef Pop

/*
  <name>		::= alphbetic alphanumeric..		Note .. means 0 or more

  <constant>	::= <numeral> | <charconst> | <boolconst> | ()| <string> | 

  <boolconst>	::= true | false
  <charconst>	::= %<any char> | SP | NL | NP | TAB
  <string>	::= ‘<any message not containing unmatched quotes>’’
*/

/* 
 * parser - parse...
 */

/* introduce all the parse_xx funtions to allow mutual recursion */
int parse_formal(void);
int parse_struct(void);
int parse_namelist(void);
int parse_condexp(void);
int parse_rhs(void);
int parse_clause(void);
int parse_defs(void);
int parse_expr(void);
int parse_comb(void);
int parse_simple(char no_newline);
int parse_opexp(void);
int parse_opx(char prio);
int parse_deflist(void);
pointer parse_program(void);

static int parse_err(char *f, char *msg1, char *msg2);

/*
 * (1)	<formal> ::= <name> | <constant> | (<namelist>)
 *                      1         2             3
 * return(size-of formal)
 */
int parse_formal()
{
  Debug("parse_formal");

  if (lex_looking_at(tok_name)) {
    Maker0("formal<=name",1,1);
    return 1;
  }
  
  if (lex_looking_at(tok_constant)) {
    Maker0("formal<=constant",1,2);
    return 1;
  }
  
  if (lex_looking_at(tok_left_paren)) {
    int s = parse_namelist();
    if (lex_looking_at(tok_right_paren)) {
       Maker1("formal<= ( namelist )",1,3);
       return s;
    }
    else
      return parse_err("parse_formal","expecting \')\'","<formal> ::= ( <namelist> )");
  }
 return parse_err("parse_formal","name constant or \'(\' not found","<name> | <constant> | (<namelist>)");
}

/*
 * (2)  <struct> ::= (<formal>) | <formal> [:<struct> ]* where * means 0 or more NB this differes from the grammar in [Tuenr 1983] - otherwise can't parse "1 WHERE f (a:x) = ..."
 *                                1           2        3
 * return length of list of structs or else 1 for (<formal>)
 */
int parse_struct()
{
  int s;
  
  Debug("parse_struct\n");
  
  if (lex_looking_at(tok_left_paren)) {
    Maker1("struct<= ( formal )", 2,1);
    parse_formal();
    if (lex_looking_at(tok_right_paren)) {
      return 1;
    }
    else
      return parse_err("parse_stuct","expecting \')\' got:","<struct> ::= (<formal>)");
  }
  
  parse_formal();
  s = 1;
  Maker1("struct<= formal ...", 2,2);
  
  while (lex_looking_at_operator(op_colon)) {
    s++;
    (void) parse_struct();
    Maker2("struct<= formal : struct", 2,3);
  }
  
  return s;
}

/*
 * (3)  <namelist> ::= <struct> | <struct>, | <struct> [, <struct>]+ where + means 1 or more
 *                         1          2                        3N
 * return(length-of structlist)
 */
int parse_namelist()
{
  int count;
  
  Debug("parse_namelist\n");
  
  parse_struct();
  count = 1;
  
  if (lex_looking_at(tok_comma)) {
    if (lex_peeking_at_simple()) {
      parse_struct();
      count++;
      while (lex_looking_at(tok_comma)) {
        count++;
        parse_struct();
      }
      MakerN(count, "namelist<=struct,struct", 3,3);
    } else {
      Maker1("namelist<=struct,", 3,2);
    }
  } else {
    Maker1("namelist<= struct", 3,1);
  }
  
  return count;
}

/* parse_condexp() parse_rhs()

   Note from Turner SASL Manual 1983:

   Offside rule

   The offside rule is enforced in the following form (which is slightly weaker than the one described in sec- tion III but which has been found more convenient in practice).
   (i) In a definition every part of the expression following the ‘‘=’’ must be to the right of the col- umn containing the ‘‘=’’
   (ii) In a conditional expression every part of the expression following the ‘‘→’’ must be to the right of the column containing the ‘‘→’’
*/

/*
 *	<condexp> ::= <opexp> → <condexp>; <condexp> | <listexp>
 *	<listexp> ::= <opexp>, . . . ,<opexp> | <opexp>, | <opexp>
 *	re-written to:
 * (x)	<condexp> ::= <opexp> → <condexp>; <condexp> | <opexp>, . . . ,<opexp> | <opexp>, | <opexp>
 *	re-re-written to:
 * (4)  <condexp> ::= <opexp> | <opexp>, | <opexp> [, <opexp>]+ | <opexp> → <condexp>; <condexp> where + means 0 or more
 *                       1           2                   3N                   4            5
 */
/* Turner 1983: "Finally note the convention that the delimiter ‘‘;’’ can be omitted provided a newline is taken instead." */ 

int parse_condexp()
{
  int count;
  
  Debug("parse_condexp\n");
  
  /* opexp */
  parse_opexp();
  count = 1;
  
  if (lex_looking_at(tok_comma)) {
    if (lex_peeking_at_simple()) {
      parse_opexp();
      count++;
      while (lex_looking_at(tok_comma)) {
        count++;
        parse_opexp();
      }
      MakerN(count,"condexp<=opexp,opexp", 4,3);
    } else {
      Maker1("condexp<=opexp,",4,2);
    }
  } else if (lex_looking_at(tok_then)) {
    /* conditional */
    /* then-part */
    
    lex_offside();
    parse_condexp();
    lex_onside();
    
    Maker2("condexp<=opexp->condexp ; ...",4,4);
    
    if (lex_looking_at_or_onside_newline(tok_semicolon)) {
      /* else-part */
      parse_condexp();
      return Maker2("condexp<=opexp -> condexp ; condexp",4,5);
    } else
      return parse_err("parse_condexp","expecting \';\' before else-part, got:", yytext);
  } else {
    Maker1("condexp<=opexp", 4, 1);
  }
  
  return 1;
}

/*
 *	<rhs> ::= <formal><rhs> | <formal> = <expr>
 * rewritten as
 * (5)  <rhs> ::= <formal>+ = <expr> | = <expr>    + means 1 or more
 *                               1         2
 * returns how many formals (>=0)
 */

int parse_rhs()
{
  int formals;
  
  Debug("parse_rhs\n");
  
  for (formals = 0; !lex_looking_at_operator(op_equal); formals++)
    parse_formal();
  
  lex_offside();
  parse_expr();
  lex_onside();
  
  MakerN(1 + formals /* include the expr */, "rhs<=formal* = expr",5,1);
  
  return formals;
}

/*
 * (6)	<clause> ::= <namelist> = <expr> | <name><rhs>
 *                       1          2         3    4
 * returns number-of-formals in rhs (>=0)
 */
int parse_clause()
{
  int s;
  
  Debug("parse_clause\n");

  s = parse_namelist();
  //?bug xxx should parse "a=1" as namelist=expr but does not;  changing this could simplify parse_rhs() nicely
//  ... or not
  if (s > 1) { /* namelist */
    Maker1("clause<=namelist ...",6,1);
    if (lex_looking_at_operator(op_equal)) {
      lex_offside();
      parse_expr();
      lex_onside();
      Maker2("clause<=namelist = expr",6,2);
      return 0; /* no formals */
    } else
      return parse_err("parse_clause","expecting \'=\' to follow namelist","clause<=namelist = expr");
  } 

  if (s == 1) { /* name */
    int f;
    Maker1("clause<=name...",6,3);
    f = parse_rhs();  /* how many formals */
    Maker2("clause<=name rhs ",6,4);
    return f;
  }

  return parse_err("parse_clause","expecting name","clause<=namelist = expr | clause <= name rhs");
}

/*
 *	<defs> ::= <clause> ; <defs> | <clause>
 *	re-written to:
 * (7)	<defs> ::= <clause> [; <clause>]*	* means 0 or more
 *                     1        2         3
 */


/*
 * helper function - check names used in defs
 * returns 1 if ok, otherwise 0
 *
 * usage: call each time a new def is added in sequence
 *
 * (1) repeated name = expr with no formals "pi = 3.14 ... pi = 22/7"
 * (2) repeated name formals1 ... name formals2 with dirrecnt number of formals "insert () = (); insert a () = a,"
 * (3) repeated non-adjacent names "sin x = ...; cos x = ...; sin x = ..."
 *
 * NB a namelist may validly contain reperated names eg "l, x, x = L" [1983 SASL Manual], or simply "match (a:a:x) = TRUE"
 */

/* return number of clauses (not counting multi-clause definitions as one clause) */
int parse_defs_do(int rule, char *rule_name)
{
  int d; /* how many defs */
  int f; /* how many formals in clause */
  int new_f;
  
  Debug(rule_name);
  
  /* always at least one clause */
  f = parse_clause();
  d = 1;
  Maker1(rule_name, rule, 1);
  
  for (/**/ ; lex_looking_at_or_onside_newline(tok_semicolon); d++) {
    /* if looking_at(tok_semicolon) must have another clause
     * if newline then clause is optional - there another clause only if the next token is onside
     */
    Debug("defs2\n"); /* temp */
    new_f = parse_clause();
    Maker2i(rule_name, rule, 2, f); /* maker needs number of formals in *previous* clause */
    f = new_f;
  }

  return d;
}

int parse_defs()
{
  return parse_defs_do(7, "defs<=clause+\n");
}

/*
 *	<expr> ::= <expr> WHERE <defs> | <condexp>
 *	re-written to:
 * (8)	<expr> ::= <condexp> [WHERE <defs>]1	* means 0 or more - xxx how can there be more than one?  "(expr where defs) where defs"  eg (a+b where a=1) where b=2?
 *                     1               2
 */
int parse_expr()
{
  Debug("parse_expr\n");

  parse_condexp();
  Maker1("expr<=condexp",8,1);

  while (lex_looking_at(tok_where)) {
    parse_defs();
    Maker2("expr<=condexp WHERE defs ",8,2);
  }

  return 1;
}

/*
 *	<comb> ::= <comb> *whitespace* <simple> | <simple>
 * NB following Turner 1983, whitespace does not include newlines
 *
 *	re-written as
 * (9)	<comb> ::= <simple>+	+ means one or more BUT simple may be EMPTY and we return NIL - handled in opexp and condexp by checking for the NIL
 xxx shouldn't we prohibit the NIL return?
 * 	 SASL Manual 1983: Note also that functional application is more binding than any operator
 *
 * A function and its arguments need to be on the same line; otherwise the following fragment of sasl tries to apply '1' to '2':
 TRUE->1
 2?
 * rather than treating 2 as the else-part, separated by newline.
 */

int parse_comb()
{
  Debug("parse_comb\n");

  if (parse_simple(0)) { /* preceeding newlines allowed */
    Maker1("comb<=simple",9,1);
    while (parse_simple(1)) /* no preceeding newlines */
      Maker2("comb<=simple+",9,2);
    return 1;
  }

  Debug("comb <= <empty>\n");
  return 0; /* permit failure */
}


/*
 * (10)	<simple ::= <name> | <constant> | ( <expr> ) [[| <zfexpr> ]]
 * TODO READ/WRITE are also <simple>
 * 
 * parse a <simple> return NIL without raising error if none found 
 */
int parse_simple(char no_newline)
{
  Debug1("parse_simple%s\n", no_newline?", no newline":"");

  if (lex_looking_at_no_newline(tok_name, no_newline))
    return Maker0("simple<=name",10,1);
  else if (lex_looking_at_no_newline(tok_constant, no_newline))
    return Maker0("simple<=constant",10,2);
  else if (lex_looking_at_no_newline(tok_left_paren, no_newline)) {
    parse_expr();
    if (lex_looking_at(tok_right_paren))
      return Maker1("simple<= ( expr )",10,3);
    else
      return parse_err("parse_simple","expecting \')\' after expr","<simple> ::= (<expr>)");
  }
  
  Debug("simple <= <empty>\n");
  return 0; /* permit failure */
}


/*
 *      <opexp> ::= <prefix><opexp> | <opexp><infix><opexp> | <comb>


 opexp ::= prefix opexp | comb [infix opexp]
 *
 * re-written as
 * 	<opexp> ::= <prefix><comb> | <comb>[[<infix><comb>][infix comb]*]* | {not yet <comb><postfix>)	where * means 0 or more *
 * 	NB <comb> can be <*nothing-*> - which is wrong in some cases!
 * 
 * re-written as:
 * 	<opexp> ::= ... | <opxN>* where * means 0 or more
 * 	<opxN>  ::= <prefixN> <opxN> 
 *		  | <comb> <infixN> <opxN>  [opx]* where N is the *minimum* priority of the express * rewritten as:
 * 	<opexp> ::= ... | <opxN>* where * means 0 or more
 * 	<opxN>  ::= <prefixN>? <comb> where ? means 0 or 1 (NB we do not allow multiple prefix operators eg "--2" or (~~TRUE) as a opx; [could possobly fold them at compile time?])
 *		  | <comb> [<infixN> <opxN>]* where N is the *minimum* priority of the expresssion's operators
 *
 * returns NIL when nothing parsed 
 */


/*
 *      <opexp> ::= <prefix><opexp> | <opexp><infix><opexp> | <comb>
 * rewritten as
 * (11)	<opexp> ::= <opx0> [<infix> <opx0>]+ where + means one or more
 * (12)	<opxN>	::= <prefixN> <opxN> | <opxN><infixN><opxN> | <comb> 

 * rewritten as
 * (11)	<opexp> ::= <opx0> <opx0>* where * means none or more
 *                     1      2
 * (12)	<opxN>	::= <prefixN> <comb> | <comb> [<infixN><opxN>]* | <opxN> <postfixN> where * means none or more
 *                      1        2        3       4      5
 *		where 'N' means operators of priority >= N
 */

int parse_opx(char prio)
{
  char done = 0;
  char p = prio;

  Debug("parse_opx\n");

  /* prefix */
  if (lex_looking_at_operator_fix_prio('p', p)) { /* prefix */
    Assert(lex_oper_prio>=prio);
    Maker0("opx<=prefix ...", 12,1);
    
    p = lex_oper_prio;
    if (parse_opx(p))
      Maker2("opx<=prefix comb", 12,2);
    else
      return parse_err("parse_opx", "no operand for prefix operator","opx<=prefix comb");
  } else {
    if (parse_comb())
      Maker1("opx<=comb", 12,3);
    else
      return 0; /* <comb> is optional */
  }
  
  /* infix */
  while (lex_looking_at_operator_fix_prio('i', prio)) { /* infix */
   Assert(lex_oper_prio>=prio);
    if (done && (p == lex_oper_prio))
      return parse_err("parse_opx","operator cannot appear together twice (not associative)","opex<=opex infix opexp");

    Maker1("opx<= opx prefix ...", 12,4);
    p = lex_oper_prio;

    /* choose priority based on assoc, and prevent adjacent operators when assoc is 'n' */
    /* Happily all sasl operators of a given priority have the same 'assoc' */
    switch (lex_oper_assoc) {
    case 'n': done = 1; /*FALLTHRU*/ /* 'n' - don't allow loop */
    case 'l': p++; break;
    case 'r': break;
    default: return parse_err("parse_opx", "parser error assoc", "!!");
    }
    
    if (parse_opx(p)) /* optional here due to priority */
      Maker2("opx<=opx prefix opx", 12,5);
    /*was   else
      return 0; */ /* incomplete here */
  }
    
  /* postfix */
  if (lex_looking_at_operator_fix_prio('o', prio)) 
    Maker1("opx<=opx postfix", 12,6);
  
  return 1; /* got one! */
}

int parse_opexp()
{
  Debug("parse_opexp\n");

  if (parse_opx(0)) {
    Maker1("opexp<=opx", 11,1);
    while (parse_opx(0))
      Maker2("opexp<=opexp opx", 11,2);
    return 1;
  } else {
    return 0; /* optional */
    /*was
      return parse_err("parse_opexp","opx <= <empty>","<opexp> ::= <opx>");*/
  }
  /*NOTREACHED*/
}

/*
 * (13) <deflist> ::= <clause> [; <clause>]*
 *                        1          2
 */
int parse_deflist()
{
  return parse_defs_do(13, "deflist<=clause+\n");
}

/*
 * (14)	<program> ::= <expr>? | def <deflist>?
 *                      1        2     3
 */
pointer parse_program()
{
  Debug("parse_program\n");
  
  if (lex_looking_at(tok_def)) {
    Maker0("program<=DEF ...", 14,2);
    parse_deflist();
    
    if(lex_looking_at(tok_question_mark)) {
      Maker1("program<=defs ?", 14,3);
      return parse_check(defs, "program<=defs ?\n");   /*xxx parse_check is NOT correct here or "the most recent"defs? */
    } else {
      parse_err("parse_program","expecting \'?\'","<program> ::= DEF <defs>?");
      return NIL;
    }
  } else {
    parse_expr();
    if(lex_looking_at(tok_question_mark)) {
      Maker1("program<=expr?", 14,1);
      return parse_check(root, "program<=expr?\n");
    }    else {
      parse_err("parse_program","expecting \'?\'","<program> ::= <expr>?");
      return NIL;
    }
  }
  /*NOTREACHED*/
}


pointer parse()
{
  if (lex_looking_at(tok_eof))
    return NIL; /*was: make_reset() */
  
  return parse_program();
}

