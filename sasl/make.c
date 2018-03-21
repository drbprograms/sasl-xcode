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
static pointer *sp = stack; /* sp points to top-of-stack (note: stack[0] never used) */

#define Depth (sp-stack)    /* >=0 */
#define Push(x)  (*++sp=(x))
#define Pop(n)  (sp -= (n), sp[n])
#define Top   (*sp)


/* debug: make_show - display maker stack items  */
#define Limit 24

static void make_show(void)
{
  pointer *spp;
  
  for (spp = sp; spp > stack; spp--) {
    fprintf(stderr, "make[%ld]: ", sp-stack); out_debug_limit(*spp, Limit);
  }
  return;
}


/* make_err() encountered a bad problem */
static pointer make_err(char *f, char *msg1, int i)
{
  (void) make_reset();
  (void) err_make(f, msg1, i);
  /*NOTREACHED*/
  return NIL;
}

static pointer make_err1(char *f)
{
  (void) make_reset();
  (void) err_make1(f);
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
      fprintf(stderr, "make_reset[%ld]: ", Depth); out_debug(Top);
    refc_delete(sp);
  }
  
  refc_delete(&root);
  
  return root;
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
        case op_divide:		/*xxx todo floating ops return new_oper(divide_op);*/
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

/* helper functions to make defs */

/* search expr for unbound names and substitute from definitions in def */
/* if an error message is provided, treat unbound anmes as errors */
/*
 * bind def ()    = ()
 * bind def (a:x) = bind def a : bind def x
 * bind def NAME  = def_lookup def NAME  || when def=() will return () and trigger possionble warning
 * bind def const = const
 */
pointer make_bind(pointer def, pointer expr, char *msg)
{
  if (IsNil(expr))
    return expr;
  
  if (IsStruct(expr)) {    /*new update(expr, make_bind(def, H), make_bind(def, T))*/
    H(expr) = make_bind(def, H(expr), msg);
    T(expr) = make_bind(def, T(expr), msg);
  } else {
    if (IsName(expr)) {
      pointer d = def_lookup(def, expr, msg);
      
      if (IsSet(d)) {     /* name - replace by it's definition */
        if (debug) {
          fprintf(stderr, "make_bind: "); out_debug1(expr); fprintf(stderr, "=>"); out_debug(d);
        }
        
        refc_delete(&expr);
        if (1 /*xxx todo wip wip*/)
          return refc_copy(d);
        else
          return refc_copy_make_cyclic(d);
      } else {        /* name - no definition found */
        if (msg) {
          fprintf(stderr, "%s %s\n", msg, Name(expr));
//          make_err1("program"); /* perhaps should be a parameter, but really ... */
        }
      }
    }
  } /* else { nothing to do } */
  return expr;
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
  
  if (debug > 1) {
    fprintf(stderr, "%s[", (r?"make_recursive_abstract":"make_abstract"));
    out_debug1(formal);
    fprintf(stderr, "] ");
    out_debug(def);
  }
  
  if (partial_compile)
    n = new_abstract(formal, def, r);
  else
    n = reduce_abstract(formal, def, r);
  
  if (debug > 1)
    fprintf(stderr, "%s--> ", (r?"make_recursive_abstract":"make_abstract")); out_debug(n);
  
  return n;
}

/* condexp is the expression to be reduced, using
 defs which is pair of lists of names and expressions
 (name ...).((expr:list-of formals) ...)
 result is
 ([name ...] condexp) (Y expr)
 formals are no longer required
 */
pointer make_where(pointer condexp, pointer defs)
{
  MAKE_DEBUG("make_where ...\n");
  
  if (IsNil(defs))
    return condexp;
  
  pointer a1 = make_abstract(Hd(defs), condexp, 0/*non-recursive*/);
  pointer a2 = make_abstract(Hd(defs), Tl(defs), 1/*recursive*/);
//  pointer a1 = make_abstract(refc_copy(Hd(defs)), condexp, 0/*non-recursive*/);
//  pointer a2 = make_abstract(refc_copy(Hd(defs)), refc_copy(Tl(defs)), 1/*recursive*/);

  condexp = new_apply(a1, a2);
  
/*  Hd(defs) = NIL; / * move *//*xxx*/
//    xrefc: no need to set NIL; DO NEED THE DELETE
  Tl(defs) = NIL; /* move *//*defs are copied when used in abstract() */
  refc_delete(&defs); /* surplus cons node */
  
  /* todo: copy and save defs list Hd/Tl pointers for debugging/ Module definition? */
  
  return condexp;
}

/* add new defs to existing defs in old
   new defintions supersede old (which are deleted)
  (1) delete superseded old definitions
  (2) add new definitions to front of what remains
 */
pointer make_defs(pointer new, pointer old)
{
   
  if (IsNil(old))
    return new;

  /*WIP xxx*/
  return new;
}
/* make a multi-cluase definition */
/* multi def1 def2 n <= TRYn n def1 def2 */
pointer make_multi_clause(pointer def1, pointer def2, int n)
{
  MAKE_DEBUG("make_multi_clause ...");
    
  return new_apply4(new_comb(TRYn_comb), new_int(n), def1, def2);
}

#ifdef notdef
/* looks for name in p, matches textually the name itself *excluding* (MATCH name) instances
 returns count of times name present in p
 */
static int count_name(pointer name, pointer p)
{
  /*temp*/
  fprintf(stderr,"count_name: "); out_debug1(name); out_debug(p);
  
  Assert(IsName(name));
  if (IsSameName(name, p))
    return 1;
  
  if (IsStruct(p) && !IsMatchName(p))
    return count_name(name, Hd(p)) + count_name(name, Tl(p));
  
  return 0;
}
#endif

/* worker for de_dup() */
/* de-duplicate a single given "atom", possibly a name */
/* de_dup x ()        = x
 * de_dup x (MATCH y) = (MATCH y)
 * de_dup x (b:y)     = de_dup b : de_dup y
 * de_dup x x         = MATCH x
 * de_dup x y         = y
 */
pointer de_dup1(pointer name, pointer old)
{
   if (IsNil(old))
    return old;
  
  if (IsMatchName(old))
    return old;
  
  if (IsStruct(old)) {
    H(old) = de_dup1(name, H(old));
    T(old) = de_dup1(name, T(old));
    return old;
  }
  
  if (IsSameName(name, old))
    return new_apply(new_comb(MATCH_comb), old);
  
  return old;
}

/* de-duplicate new names
 for every new name "n", replace instances of that name in old by (MATCH "n")
 */

pointer de_dup(pointer new, pointer old)
{
 
  if (IsNil(new))
    return old;
  
  if (IsMatchName(new))
    return old;
  
  if (IsStruct(new)) {
    old = de_dup(H(new), old);
    old = de_dup(T(new), old);
    return old;
  }
    
  return de_dup1(new, old);
}

/*maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker*/


/*
 * (1)	<formal> ::= <name> | <constant> | (<namelist>)
 * (2)	<struct> ::= <formal>:<struct> | (<formal>)
 * (3)  <namelist> ::= <struct> , . . . ,<struct> | <struct>, | <struct>
 * (4)	<condexp> ::= <opexp> → <condexp>; <condexp> | <opexp>, . . . ,<opexp> | <opexp>, | <opexp>
 * (5)  <rhs> ::= = <formal>+ = <expr> | <expr>    * means 1 or more
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
  
  /* sp[1] ... sp[howmany] contains items to be processed */
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
        case 3: return new_cons(n1, de_dup(n1,n2));
      }
      break;
      
    case 3:
      /*
       * (3)  <namelist> ::= <struct> | <struct>, | <struct> [, <struct>]+ where * means 1 or more
       *   *                     1          2                        3N
       
       namelist <= struct|listof-struct
       */
      switch (subrule) {
        case 1: return n1;
        case 2: return new_cons(n1,NIL);
        case 3:  {
          /* use stack N reverse order of SASL text - comma binds "rightmost"  first */
          /* "w,x,y" compiles to w:(x:(y:NIL)) */
          /* this ensures correct de-dup'ing */
          pointer s = sp[howmany];
          int i;
          Assert(howmany >= 1);
          s = new_cons(s, NIL);
          for (i = howmany - 1; i > 0; i--)
            s = new_cons(sp[i], de_dup(sp[i], s));
          return s;
        }
       }
      break;
      
    case 4:
      /* (4)	<condexp> ::= <opexp> → <condexp>; <condexp> | <opexp>, . . . ,<opexp> | <opexp>, | <opexp> */
      /*	re-re-written to:
       * (4)	<condexp> ::= <opexp> | <opexp>, | <opexp> [, <opexp>]+ | <opexp> → <condexp>; <condexp> where + means 0 or more
       *                         1           2                   3N                   4            5
       
       srhs     condexp <= cond_op opexp condexp condexp | opexp1 | listof-opexp
       */
      
      
      switch (subrule) {
        case 1: return n1; /*nop*/
        case 2: return new_cons(n1, NIL);
        case 3: {
          /* use stack N reverse order of SASL text - comma binds "rightmost"  first */
          /* "w,x,y" compiles to w:(x:(y:NIL)) */
          /* this ensures correct de-dup'ing */
          pointer s = sp[howmany];
          int i;
          Assert(howmany >= 1);
          s = new_cons(s, NIL);
          for (i = howmany - 1; i > 0; i--)
            s = new_cons(sp[i], s);
          return s;
          
        }
        case 4: return new_apply3(new_oper(cond_op), n1, n2);
        case 5: return new_apply(n1, n2);
      }
      break;
      
    case 5:
      /*
       *	<rhs> ::= <formal><rhs> | <formal> = <expr>
       * rewritten as
       * (5)  <rhs> ::= = <formal>+ = <expr> | = <expr>    + means 1 or more
       *                      1          2        3
       *    || rhs <= expr
       !! ToDo consider check "name" is not present in Lhs.formal - its an error isn't it eg sasl "f a f = a?"!!
       */
      switch (subrule) {
        case 1: {
          /* use stack N reverse order of SASL text - abstract "rightmost" formal first */
          /* "f x y = E compiles to [x] ([y] E) where the innermost abstraction is performed first" [Turner] */
          /* this ensures correct de-dup'ing */
          pointer s = sp[howmany];
          int i;
          Assert(howmany >= 1);
          for (i = howmany - 1; i > 0; i--)
            s = new_apply(sp[i], de_dup(sp[i], s));
          return s;
        }
        case 2: {
          pointer result = make_abstract(n1, n2, 0/*nonrecursive*/);
          refc_delete(&n1); /* xrefc assume not needed xxx tidy whether to delete in make_abstract (code/no-code)) */
          return result;
        }
        case 3: return n1;
      }
      break;
      
    case 6:
      /*
       * (6)	<clause> ::= <namelist> = <expr> | <name><rhs>
       *                          1          2        3    4
       clause == names:expr  || names may be simple name (rhs) or list-of names (namelist)
       */
      switch (subrule) {
        case 1: return n1;
        case 2: return new_cons(n1, n2);
        case 3: return n1;
        case 4: return new_cons(n1, n2);
      }
    break;
    
    case 7:
    case 13:
    /* (7)	<defs> ::= <clause> [; <clause>]*	* means 0 or more
     *                      1           2      3
     defs <= (list-of (name|namelist)).(list-of expr)
     || defs == (list-of names):(list-of expr)
     */
    /*
     * (13) <deflist> ::= <clause> [; <clause>]*
     *                      1           2      3
     defs <= (list-of (name|namelist)).(list-of expr)
     || defs == (list-of names):(list-of expr)
     */
    
    switch (subrule) {
      
      case 1:
      /* first def - re-write n1 as a pair of lists */
#ifdef defdef
      n1 = add_to_def(new_def(""), H(n1), T(n1));
#else
      H(n1) = new_cons(H(n1), NIL);
      T(n1) = new_cons(T(n1), NIL);
#endif
      return n1;
      
      case 2: {
        /* || clause == names:expr */
#ifdef lastest
        if (IsSameName(HH(n1), H(n2))) {
          /*multi-clause definition */
          HT(n1) = make_multi_clause(HT(n1), refc_copy(T(n2)), info);
          /* H(n2) is surplus to requirements, and is deleted below */
        } else {
          /* single clause definition, so far */
          H(n1) = new_cons(refc_copy(H(n2)), H(n1));
          T(n1) = new_cons(refc_copy(T(n2)), T(n1));
        }
        refc_delete(&n2); /* surplus cons node, possibly with contents */
#else
#ifdef defdef
        if (IsSameName(H(Defnames(n1)), H(n2))) {
          /*multi-clause definition */
          H(DefExprs(n1)) = make_multi_clause(H(DefExprs(n1)), T(n2), info);
          T(n2) = NIL /*move*/
        } else {
          /* single clause definition, so far */
          n1 = add_to_def(n1, H(n2), T(n2)); /* ?? add_to_def(n1, n2) */
          H(n2) = T(n2) = NIL; /*move*/
        }
#else
        if (IsSameName(HH(n1), H(n2))) {
          /*multi-clause definition */
          HT(n1) = make_multi_clause(HT(n1), T(n2), info); T(n2) = NIL; /*move*/
          /* H(n2) is surplus to requirements, and is deleted below */
        } else {
          /* single clause definition, so far */
          H(n1) = new_cons(H(n2), H(n1)); H(n2) = NIL; /*move*/
          T(n1) = new_cons(T(n2), T(n1)); T(n2) = NIL; /*move*/
        }
#endif
        refc_delete(&n2); /* surplus cons node, possibly with contents */
#endif
        return n1;
      }
      
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
        case 2: n1 = make_where(n1, n2); /*was refc_delete(&n2);*/ return n1;
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
    
    case 14:
      /* (14)	<program> ::= <expr>? | def <deflist>?
       *                          1        2     3
       reduce expr | save defs
       */
      
      switch (subrule) {
          
        case 1:
          /* substitute for known DEFs; check for unbound names; return pointer to-be-reduced */

          Assert(IsNil(defs) || IsDef(defs));
          Assert(IsDef(builtin));

#ifdef def
          n1 = make_bind(builtin, n1, NULL);
          root = make_bind(defs, n1, "undefined name: ");
#else
          n1 = make_where(n1, refc_copy(DefDefs(builtin)));
          if (IsSet(defs))
            n1 = make_where(n1, refc_copy(DefDefs(defs)));
          refc_delete(&root);
          root = n1;
#endif
          
          return root;
          
        case 2: return n1;
          
        case 3: {
          /*  Assert(Depth == 0);*/
          /*
           || defs == (list-of names):(list-of expr)
           */
          
          refc_delete(&root);
//          refc_delete(&defs);

#ifdef def
          T(n1) = make_bind(builtin, T(n1), NULL);
#else
          T(n1) = make_where(T(n1), refc_copy(DefDefs(builtin)));
//          T(n1) = make_where(T(n1), n1); /* mutual recursion in the defs */
#endif

          /*WIP WIP 2nd adn subsequent DEF */
          if (IsNil(defs))
            defs = new_def(new_name("<Top>"), n1);
//          else
//            defs = add_deflist_to_def(defs, n1);

#ifdef def
          DefExprs(defs) = make_bind(builtin, DefExprs(defs), NULL); /* resolve builtin references */
          DefExprs(defs) = make_bind(defs,    DefExprs(defs), "in DEF: undefined name: "); /* resolve internal references */
          /* XXX todo Y comb here?? */
#endif
          return defs;
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

/*
 stack map: there are "howmany" items on the stack for maker_do()
 first these are popped, so they are now at
 sp[1] ... sp[howmany]
 the result of maker_do() is then Pushed onto the stack
 */
int maker(int howmany, char *ruledef, int rule, int subrule, int info)
{
  pointer n;
  
  Pop(howmany);
  
  if (debug > 2)
    make_show();
  
  n = maker_do(howmany, ruledef, rule, subrule, info, sp);
  
  Push(n);
  
  if(debug){
    fprintf(stderr, "%s %d,%d,%d,%d (Depth=%ld) <= ", ruledef, rule, subrule, info, howmany, Depth);
    out_debug_limit(sp[0], Limit);
  }
  
  return 1;
}

/*maker*maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker*/
