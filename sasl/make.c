#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "common.h"

#include "lex.h"
#include "store.h"
#include "abstract.h"

#include "make.h"

/*
 * maker - the code generator - make...
 */

static pointer make_err(char *f, char *msg1, int i);
pointer make_reset(void);

/* parse stack */

/* stack of program elements under construction */
/* when parse/make fails, simple refc_delete() every stack item to clean out memory */

#define STACK_SIZE (10000)

static pointer stack[STACK_SIZE];
static pointer *sp = stack;

#define Depth (sp-stack)
#define Push(x) (sp++, *sp = (x))
#define Pop(n)  (sp -= (n), sp[n])

#ifdef unused
/* debug: make_show - display maker stack items  */
static void make_show(void)
{
  pointer *spp;

  for (spp = sp; spp > stack; spp--) {
    fprintf(stderr, "make[%ld]: ", sp-stack); out_debug(*spp); 
  }
  return;
}
#endif

/* make_err() encountered a bad problem */
static pointer make_err(char *f, char *msg1, int i)
{
  (void) make_reset();
  (void) err_make(f, msg1, i);
  /*NOTREACHED*/
  return NIL;
}


/* make_reset delete partially-built code fragments */
pointer make_reset()
{
  no_code = 1; /* prevent all further code generation */
  /* todo reenable code generation at some stage? eg next '?' symbol? */ 
  
  for (/**/; sp > stack; sp--) {
    if (debug)
      fprintf(stderr, "make_reset[%ld]: ", sp-stack); out_debug(*sp); 
    refc_delete(sp);
  }
  return NIL;
}

/* various contructors */
pointer make_constant_string()
{
  char c;
  MAKE_DEBUG("make_constant_string\n");

  if (!lex_looking_at(tok_constant)) {
    (void) make_err("make_constant_string","lexer string error",0);
    return NIL;
    /*NOTREACHED*/
  }

  /* add a character to the string */
  switch(lex_tc)
    {
    case tok_const_string_end:
      return NIL;
    case tok_const_string_start_nested:	/* TODO make nested strings as nested objects?! */
    case tok_const_string_end_nested:
    case tok_const_string_char:
      c = *yytext;;
      break;
    default:
        c = *yytext;
        (void) make_err("make_constant: looking for \"\'\" at end of string, found",yytext, 0);
      /*NOTREACHED*/
      break;
    }

  return new_cons(new_char(c), make_constant_string());
}

pointer make_constant()
{
  extern tok_const lex_tc;	/* set by the lexer iff tok_constant found */

  
  MAKE_DEBUG("make_constant\n");

  switch (lex_tc) {
  case tok_const_nil:		return NIL;  /*???*/
  case tok_const_integer:	return new_int(atoi(yytext));
  case tok_const_floating:	return new_double(atof(yytext));
  case tok_const_true:		return new_bool(1);
  case tok_const_false:		return new_bool(0);
  case tok_const_char:		return new_char(yytext[1]); /* "%a" second char in token */
  case tok_const_special_char_NL:return new_char('\n');
  case tok_const_special_char_NP:return new_char('\f');
  case tok_const_special_char_SP:return new_char(' ');
  case tok_const_special_char_TAB:return new_char('\t');
  case tok_const_string_start:	return make_constant_string();

  case tok_const_string_start_nested:	/* TODO nested strings ! */
  case tok_const_string_end_nested:
  case tok_const_string_end:
  case tok_const_string_char:
    (void) make_err("make_constant","parser error! parser string error", 0); /* this never happens */
  }
  return NIL;/*NOTREACHED*/
}

pointer make_name()
{
  return new_name(yytext);
}
/* make_oper() */
pointer make_oper()
{
  switch (lex_oper_fix[0]) { /*xxx does this correctly deal with lex_oper_fix == "ip" - either infix or postfix??? */

  case 'p':
    switch(lex_oper) {
    case op_plus:		return new_oper(unary_plus_op);
    case op_minus:		return new_oper(unary_minus_op);
    case op_unary_not:		return new_oper(unary_not_op);
    case op_unary_count:	return new_oper(unary_count_op);
    default: make_err("maker: opexp", "unexpected prefix operator", lex_oper);
    }
    
  case 'i':
    switch (lex_oper) {
    case op_colon:		return new_oper(colon_op);
    case op_plusplus:		return new_oper(plusplus_op);
    case op_minusminus:		return new_oper(minusminus_op);
    case op_range:		return new_oper(range_op);
    case op_range_unbounded:	make_err("maker: opexp", "unexpected unary postfix operator", lex_oper);
    case op_or:			return new_oper(or_op);
    case op_and:		return new_oper(and_op);
    case op_unary_not:		make_err("maker: opexp", "unexpected unary infix operator", lex_oper);
    case op_much_greater:	return new_oper(much_greater_op);
    case op_greater:		return new_oper(greater_op);
    case op_greater_equal:	return new_oper(greater_equal_op);
    case op_equal:		return new_oper(equal_op);
    case op_not_equal:		return new_oper(not_equal_op);
    case op_less_equal:		return new_oper(less_equal_op);
    case op_less:		return new_oper(less_op);
    case op_much_less:		return new_oper(much_less_op);
    case op_plus:		return new_oper(plus_op);
    case op_minus:		return new_oper(minus_op);
    case op_times:		return new_oper(times_op);
    case op_divide:		return new_oper(divide_op);
    case op_int_divide:		return new_oper(int_divide_op);
    case op_rem:		return new_oper(rem_op);
    case op_power:		return new_oper(power_op);
    case op_period:		return new_comb_name(B_comb, new_name(".")); /* f.g == B f g; f.g x ==f (g x) */
    case op_unary_count:	make_err("maker: opexp", "unexpected unary infix operator", lex_oper);
    }

  case 'o':
    switch (lex_oper) {
    case op_range_unbounded:	return new_oper(range_unbounded_op);
    default: make_err("maker: opexp", "unexpected postfix operator", lex_oper);    }

  default: 	make_err("maker: opexp", "unexpected operator fix", lex_oper_fix[0] - 'a');
  }
  return NIL; /*NOTREACHED*/
}

/* helper finctions to make defs */

/* search def for definition of a name, return NIL is if not found 
   defs: (listof-names).(listof-clauses) */
pointer make_lookup_name(pointer name, pointer def)
{
  pointer defs, n, d;

  if (IsNil(def))
    return NIL;

  /* Hd(def) == name-of-def list */
  /* Tl(def) == (listof-names . listof-clauses) */
  defs = Tl(def);
  
  for (n = Hd(defs), d = Tl(defs); IsSet(n); n = Tl(n), d = Tl(d)) {
    if ( !EqName(name, Hd(n)))
      return Hd(d);
  }
  return NIL;
}

/* search expr for unbound names and substitute from definitions in defs */
pointer make_bind(pointer defs, pointer expr)
{
  if (IsNil(expr))
    return NIL;
  
  if (IsStruct(expr)) {
    /*new update(expr, make_bind(defs, H), make_bind(defs, T))*/
    Hd(expr) = make_bind(defs, Hd(expr));
    Tl(expr) = make_bind(defs, Tl(expr));
  } else {
    if (IsName(expr)) {
      pointer temp = make_lookup_name(expr, defs); /*WIPWIP*/

      if (debug) {
	fprintf(stderr, "make_bind: ");
	out_debug1(expr);
	fprintf(stderr, "==");
	out_debug(temp);
      }
      
      if (IsSet(temp)) {
	/* replace name by it's definition */
	refc_delete(&expr);
	return refc_copy_make_cyclic(temp); /*recursive??*/
      }
    }
  }
  return expr;
}


/* helper function */
/*
 * make_append - adds new cons cell to end of non-NIL list containing tl and NIL terminator; does nothing  if list is NIl
 *	NB no checking on list is done
 * return top of the list
 */
pointer make_append(pointer list, pointer tl)
{
  pointer l = list;
  
  while (IsSet(l)) {
    if (IsSet(Tl(l))) {
      l = Tl(l);
    } else {
      Assert(IsNil(Tl(l)));
      Tl(l) = tl; /* was new_cons(tl,NIL);c*/
      break;
    }
  }
  return list;
}

/* [Turner 1979]
   Mutual recursion following a where is handled by combining all the definitions follow- ing the where into a single definition with a complex left hand side and then proceeding as above. So for example 

   E where f x = ...g... ; g y = ...f...

   is first transformed to

   E where f= [x] (...g ...) , g = [y] (... f ...)

   eliminating the variables x and y. Now the mutually recursive pair of definitions can be converted into a single recursive definition as follows

   E where (f,g)= ([x] (...g ...) , [y] (...f...) which can be compiled as

   ([f, g] E) (Y([f,g] ([x] (...g ...) , [y] (... f ...))))

   using the rules already given.
*/

/* make_abstract() - make ([name] def) - abstract name from def */
/* here we either carry out the abstraction (reduce_abstract) or leave a placeholder for delayed abstraction (new_abstract) */
pointer make_abstract(pointer formal, pointer def, int r)
{
  pointer n;
  
  if (debug>1) {
    fprintf(stderr, "%s[", (r?"make_recursive_abstract":"make_abstract"));
    out_debug1(formal);
    fprintf(stderr, "] ");
    out_debug(def);
  }

  if (partial_compile)
    n = new_abstract(formal, def, r);
  else
    n = reduce_abstract(formal, def, r);
  
  if (debug)
    fprintf(stderr, "%s--> ", (r?"make_recursive_abstract":"make_abstract")); out_debug(n);
  
  return n;
}

/* NB for "f a b c = E" formals are reverese order (c,b,a) to ensure innermost is abstracted first */

/* parameter definition: abstract formal-names from the expr defining them */


/* condexp is the expression to be reduced, using
   defs which is pair of lists of names and expressions
 (name ...).((expr:list-of formals) ...)
   result is
 ([name ...] condexp) (expr:list-of formals ...)
*/
pointer make_where(pointer condexp, pointer defs)
{
  MAKE_DEBUG("make_where ...\n");
  
  /*new ???*/
  condexp = new_apply(make_abstract(Hd(defs), condexp, 0 /*non-recursive*/),
                      Tl(defs));
  Hd(defs) = NIL; /* move */
  Tl(defs) = NIL; /* move */
  refc_delete(&defs); /* cons no longer required */
  
  /* todo: copy and save defs list Hd/Tl pointers for debugging/ Module definition? */
  
  return condexp;
}

/* make an additional definition def with same name as previous
   Sp TRY def prev			|| one formal
   Sp (Sp TRY)) def prev	 	|| two formals
   Sp (Sp (... (Sp TRY))) def prev 	|| three etc ...

*/

pointer make_multi_clause(pointer def, pointer prev, int n)
{
  MAKE_DEBUG("make_multi_clause ...");

  def = new_apply(
		  new_apply(
			    new_apply(new_comb(Sp_comb), new_comb(TRY_comb)),
			    def),
		  prev);

  for (/**/; n > 1; n--)
    HH(def) = new_apply(new_comb(Sp_comb), HH(def));
      
  return def;
}

pointer make_multi_clauseOLD(pointer def, pointer prev, int n)
{
  MAKE_DEBUG("make_multi_clause ...");

  def = new_apply(def, prev);
  
  for (/**/; n > 0; n--) {
    def = new_apply(
		    new_apply(new_comb(Sp_comb), new_comb(TRY_comb)),
		    def);
  }
  
  return def;
}

#ifdef obsolete
/* for each clause on the stack:
   clause <= namelist:expr:0  |  name:expr

   result is pair of lists:
   (list-of {name|namelist}) : list-of defs
*/
pointer make_defs(int howmany, pointer *sp)
{
  int i;
  pointer n1;

  Assert(howmany >= 1);
  
  n1 = sp[howmany--]; /* first def */

  /* re-write n1 as a pair of lists */
  H(n1) = new_cons(H(n1), NIL);
  T(n1) = new_cons(T(n1), NIL);
  
  /* multi-def - two clauses with same simple names (not namelist) */
  /* else straightforward case, simply add to the pair of lists */
  for (i = howmany; i > 0; i--) {
    pointer n2 = sp[i];

    if (IsName(H(n2)) && IsName(HH(n1)) &&
	EqName(H(n2),           HH(n1))) {

      HT(n1) = make_multi_clause(T(n2), HT(n1), 42);	T(n2) = NIL; /* move expr */
      
    } else {

      HH(n1) = new_cons(H(n2), HH(n1)); H(n2) = NIL; /* move name */
      HT(n1) = new_cons(T(n2), HT(n1));	T(n2) = NIL; /* move expr */

    } 
    refc_delete(&n2);
  }
  return n1;
}
#endif


/*maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker*/



/*
 * (1)	<formal> ::= <name> | <constant> | (<namelist>)
 * (2)	<struct> ::= <formal>:<struct> | (<formal>)
 * (3)	<namelist> ::= <struct> , . . . ,<struct> | <struct>, | <struct>
 * (4)	<condexp> ::= <opexp> → <condexp>; <condexp> | <opexp>, . . . ,<opexp> | <opexp>, | <opexp>
 * (5)	<rhs> ::= <formal>* = <expr>	* means 0 or more
 * (6)	<clause> ::= <name><rhs> | <namelist> = <expr>
 * (7)	<defs> ::= <clause> [; <clause>]*	* means 0 or more
 * (8)	<expr> ::= <condexp> [where <defs>]*	* means 0 or more
 * (9)	<comb> ::= <simple>+	+=one or more
 * (10)	<simple ::= <name> | <constant> | ( <expr> ) [[| <zfexpr> ]]
 * (11)	<opexp> ::= <prefix><opexp> | <opexp>[<infix><comb>]*	where * means optional 0...
 * (12)	<opxN>	::= <prefixN> <opxN>| <comb> [<infixN><opxN>]* [<postfixN>]
 * (13) <deflist> ::= <clause> [; <clause>]*
 * (14)	<program> ::= <expr>? | def <deflist>?
 */


pointer maker_do(int howmany, char *ruledef, int rule, int subrule, int info, pointer *sp)
{
  pointer n1, n2;  /* shortcut covers most cases below */
  
  if (no_code)
    return NIL;
  
  n1 = sp[1];
  n2 = sp[2];
  
  if (debug >2)
    fprintf(stderr, "%s %d,%d\n", ruledef, rule, subrule);
  
  switch (rule) {
    case 1:
      /* (1)	<formal> ::= <name> | <constant> | (<namelist>) */
      /*		formal <= name | constant | namelist */
      switch (subrule)
    {
      case 1: return make_name();
      case 2: return make_constant();
      case 3: return n1;
    }
      break;
      
    case 2:
      /* (2) <struct> ::= (<formal>) | <formal>:<struct>
       *	                     1           2        3
       
       struct <= formal | listof-struct
       
       formal may be "()" ie NIL ...
       */
      switch (subrule) {
        case 1: return n1;
        case 2: return n1;
        case 3: return new_cons(n1,n2);
      }
      break;
      
    case 3:
      /*
       * (3)	<namelist> ::= <struct> | <struct>, | <struct> , . . . ,<struct>
       *   *                            1          2                              3
       
       namelist <= struct | listof-struct
       */
      switch (subrule) {
        case 1: return n1;
        case 2: return new_cons(n1,NIL);
        case 3: return new_cons(n1,n2);
      }
      break;
      
    case 4:
      /* (4)	<condexp> ::= <opexp> → <condexp>; <condexp> | <opexp>, . . . ,<opexp> | <opexp>, | <opexp> */
      /*	re-re-written to:
       * (4)	<condexp> ::= <opexp> → <condexp>; <condexp> | <opexp> | <opexp>, | <opexp>, <opexp> [, <opexp>]* where * means 0 or more
       *                           1          2          3          4        5                   6          7
       
       srhs     condexp <= cond_op opexp condexp condexp | opexp1 | listof-opexp
       */
      
      
      switch (subrule) {
        case 1: return n1; /*nop*/
        case 2: return new_apply(new_apply(new_oper(cond_op), n1), n2);
        case 3: return new_apply(n1, n2);
        case 4: return n1; /*nop*/
        case 5: return new_cons(n1, NIL);
        case 6: return make_append(n1, new_cons(n2,NIL));
        case 7: return make_append(n1, new_cons(n2,NIL));
      }
      break;
      
    case 5:
      /*
       *	<rhs> ::= <formal><rhs> | <formal> = <expr>
       * rewritten as
       * (5)	<rhs> ::= = <formal>* = <expr> | = <expr> + means 1 or more
       *                                    N1
       rhs <= [f1 ...] expr | expr
       
       !! ?consider check name is not present in Lhs.formal - its an error isn't it eg sasl "f a f = a?"!!
       */
      switch (subrule) {
          
        case 1: {
          int i;
          pointer expr, formals = NIL;
          /*WIP WIP WIP */
          expr = sp[howmany--];
          
          if (howmany > 0) {
            formals = sp[0];
            for (i = 1; i < howmany; i++) {
              formals = new_apply(sp[i], formals);
            }
            expr = make_abstract(formals, expr, 0);
          }
          
          return expr;
        }
      }
      
      break;
      
    case 6:
      /*
       * (6)	<clause> ::= <namelist> = <expr> | <name><rhs>
       *                           1          2        3    4
       clause <= name|namelist : expr
       */
      switch (subrule) {
        case 1: return n1; /* NB n1 is a listof-names here */
        case 2: {
          n2 = make_abstract(n1, n2, 0); /*non-recursive*/ /*xxx refc issue here is make_abstract copies incoming */
          return new_cons(n1,  n2);
        }
        case 3: return n1; /* was make_name(); */
        case 4: return new_cons(n1, n2);
      }
      break;
      
    case 7:
      /* (7)	<defs> ::= <clause> [; <clause>]*	* means 0 or more
       *                     1        2 or 3		"2" when adjacent clauses are part of a multi-clause definition of the same name, otherwise "3"
       defs <= (list-of name|namelist).(list-of expr)
       */
      switch (subrule) {
        case 1:
          /* first def - re-write n1 as a pair of lists */
          H(n1) = new_cons(H(n1), NIL);
          T(n1) = new_cons(T(n1), NIL);
          return n1;
          
          /* multi-def - two clauses with same simple names (ie not namelist) - update the single definition */
        case 2:
          HT(n1) = make_multi_clause(T(n2), HT(n1), info);	T(n2) = NIL; /* move expr */
          refc_delete(&n2);
          return n1;
          
          /* else straightforward case, simply add each of the pair of lists */
        case 3:
          HH(n1) = new_cons(H(n2), HH(n1));
          HT(n1) = new_cons(T(n2), HT(n1));
          
          H(n2) = NIL; /* move name */
          T(n2) = NIL; /* move expr */
          refc_delete(&n2);
          return n1;
          
      }
      
      break;
      
    case 8:
      /* (8)	<expr> ::= <condexp> [where <defs>]*	* means 0 or more
       1            2
       *                      condexp | [(name ...)] condexp (Y [(name ...)] (expr ...)
       
       expr <=	make_where(condexp, defs) <= ([hd(defs)] condexp) (tl defs) <= ([listof-clause-names] condexp) (listof-clauses) | expr
       
       *		condexp | (foreach clause.name in defs - (Y[clause.name]) condexp) defs ... repeating for each [where defs]; clear the list
       */
      switch (subrule) {
        case 1: return n1;
        case 2: return make_where(n1, n2);
      }
      break;
      
    case 9:
      /* (9)	<comb> ::= <simple>+	+=one or more */
      /* 		simple | ((simple1 simple2) simple3)+ */
      switch (subrule) {
        case 1: return n1;
        case 2: return new_apply(n1, n2); /* bind to left */
      }
      break;
      
    case 10:
      /* (10)	<simple ::= <name> | <constant> | ( <expr> ) [[| <zfexpr> ]] */
      /* 		simple or expr */
      /* 		simple | ((simple1 simple2) simple3)+ */
      switch (subrule)
    {
      case 1: return make_name();
      case 2: return make_constant();
      case 3: return n1;
    }
      break;
      
    case 11:
      /* (11)	<opexp> ::= <opx0> [<infix> <opx0>]+ where + means one or more */
      switch (subrule) {
        case 1: return n1;
        case 2: return new_apply(n1,n2);
      }
      break;
      
    case 12:
      /*
       * (12)	<opxN>	::= <prefixN> <opxN>| <comb> [<infixN><opxN>]* [<postfixN>]
       *                          1        2       3      4      5             6
       */
      switch (subrule) {
        case 1: return make_oper();
        case 2: return new_apply(n1, n2);
        case 3: return n1;
        case 4: return new_apply(make_oper(), n1);
        case 5: return new_apply(n1, n2);
        case 6: return new_apply(make_oper(), n1);
      }
      break;
      
    case 13:
      /*
       * (13) <deflist> ::= <clause> [; <clause>]*
       defs <= (listof-clause-names).(listof-clauses)
       *
       * retain listof-clauses for individual use
       */
      switch (subrule) {
        case 3:
          return n1;/*todo xxx BROKEN HERE make this like case: 7 defs, but updating Global DEF list */
          
          
      }
      break;
      
    case 14:
      /* (14)	<program> ::= <expr>? | def <deflist>?
       *                          1        2     3
       reduce expr | save defs
       */
      
      switch (subrule) {
          
        case 1:
          /* substitute for known DEFs; check for unbound names; return pointer to-be-reduced */
          /* n1 = make_bind(theDefs, n1);
           if (make_check_free(n1))
           return (make_reset())
           else
           return n1;
           */
          
#ifdef notyet      
          n1 = make_where(n1, defs);
#endif
          return n1;
        case 2: return n1;
        case 3: {
          /* check for unbound names; save defs */
          /*todo check for unbound ... */
          /*todo successive "def deflist?" adds to/updates "defs" */
          n1 = new_def(new_name("<TopDefs>"), n1);
          
          return n1; /*todo record the defs */
          
        }
          break;
          
        default:
          break;
      }
  }
  
  (void) make_err("got unrecognised rule", ruledef, rule);
  /*NOTREACHED*/
  
  return(make_reset());
}

#define Limit 24

int maker(int howmany, char *ruledef, int rule, int subrule, int info)
{
  pointer n;
  
  Pop(howmany);
  
  if (debug > 2) {
    int i;
    for (i=1; i<=howmany; i++)
      fprintf(stderr, "n%d: ", i); out_debug_limit(sp[i], Limit);
  }
  
  n = maker_do(howmany, ruledef, rule, subrule, info, sp);
  
  Push(n);
  
  if(debug){
    fprintf(stderr, "%s %d,%d,%d,%d (Depth=%ld) <= ", ruledef, rule, subrule, info, howmany, Depth);
    out_debug_limit(sp[0], Limit);
  }
  
  return 1;
}

pointer make_result()
{
  if (Depth == 1)
    return Pop(1);
  (void) make_err("program", "stack alignment problem", (int)Depth);
  return NIL;
}


/*maker*maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker*/
