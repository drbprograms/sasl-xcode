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
#define Push(x) (sp++, *sp = (x))
#define Pop(n)  (sp -= (n), sp[n])


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
  /* Tl(def) == (listof names . listof-clauses) */
  defs = Tl(def);
  
  for (n = Hd(defs), d = Tl(defs); IsSet(n); n = Tl(n), d = Tl(d)) {
    /*WIP TODO xxx BUG needs to recurse over names == list-of (name|namelist) */
    if (IsName(Hd(n))) {
      if ( !EqName(name, Hd(n)))
        return Hd(d);
    } else {
      pointer h, t;
      h = make_lookup_name(Hd(n), def);
      if (IsSet(h))
        return h;
      t = make_lookup_name(Tl(n), def);
      if (IsSet(t))
        return t;
    }
  }
  return NIL;
}

#ifdef deprecated
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
#endif

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
  
  pointer a1 = make_abstract(Hd(defs), condexp, 0/*non-recursive*/);
  pointer a2 = make_abstract(Hd(defs), Tl(defs), 1/*recursive*/);

  condexp = new_apply(a1, a2);
  
/*  Hd(defs) = NIL; / * move *//*xxx*/
  Tl(defs) = NIL; /* move *//*defs are copied when used in abstract() */
  refc_delete(&defs); /* no longer required */
  
  /* todo: copy and save defs list Hd/Tl pointers for debugging/ Module definition? */
  
  return condexp;
}

/* get the definition for a given name in a defslist */
pointer def_for(pointer name, pointer defs)
{
  pointer n, d;

  if (IsNil(defs) || IsNil(name))
    return NIL;

  for (n = H(defs), d = T(defs); IsSet(n)/* && IsSet(d)*/; n = T(n), d = T(d)) {
    if (IsSameName(name, H(d)))
      return (Tl(d));
  }
  
  return NIL;
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
    
  return new_apply(
                  new_apply(
                            new_apply(new_comb(TRYn_comb), new_int(n)),
                            def1),
                  def2);
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
       * (3)	<namelist> ::= <struct> | <struct>, | <struct> , . . . ,<struct>
       *   *                            1          2                              3
       
       namelist <= struct|listof-struct
       */
      switch (subrule) {
        case 1: return n1;
        case 2: return new_cons(n1,NIL);
        case 3: return new_cons(n1, de_dup(n1,n2));
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
       * (5)  <rhs> ::= = <formal>+ = <expr> | <expr>    + means 1 or more
       *                      1          2        3
       *    || rhs <= expr
       !! ToDo consider check "name" is not present in Lhs.formal - its an error isn't it eg sasl "f a f = a?"!!
       */
      switch (subrule) {
        case 1: {
          /* use stack N reverse order of SASL text - abstract "rightmost" formal first */
          /* "f x y = E compiles to [x] ([y] E) where the innermost abstraction is performed first" [Turner] */
          /* this ensures correct de-dup'ing */
          pointer formals = sp[howmany];
          int i;
          for (i = howmany - 1; i > 0; i--)
            formals = new_apply(sp[i], de_dup(sp[i], formals));
          return formals;
        }
        case 2: {
          pointer result = make_abstract(n1, n2, 0/*nonrecursive*/);
          refc_delete(&n1); /* assume not needed xxx tidy whether to delete in make_abstract (code/no-code)) */
          return result;
        }
          /* TODO free formals */
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
      /* (7)	<defs> ::= <clause> [; <clause>]*	* means 0 or more
       *                      1           2      3
       defs <= (list-of (name|namelist)).(list-of expr
       || defs == (list-of names):(list-of expr)
       */
      switch (subrule) {
        case 1:
          /* first def - re-write n1 as a pair of lists */
          H(n1) = new_cons(H(n1), NIL);
          T(n1) = new_cons(T(n1), NIL);
          return n1;
          
        case 2: {
          /* || clause == names:expr */
          if (IsSameName(HH(n1), H(n2))) {
            /*multi-clause definition */
            HT(n1) = make_multi_clause(HT(n1), T(n2), info);
            T(n2) = NIL; /*move*/ /* H(n2) is surplus to requirements, delete below */
          } else {
            /* single clause definition, so far */
            H(n1) = new_cons(H(n2), H(n1));
            T(n1) = new_cons(T(n2), T(n1));
            H(n2) = T(n2) = NIL; /*move*/
          }
          refc_delete(&n2); /* surplus cons node, possibly with contents */
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
        case 5:
          /* consify - elide colon_op re-write (: a b) as (a:b) */
          if (IsApply(n1) && IsSet(Hd(n1)) && (Tag(Hd(n1)) == colon_op)) {
            refc_delete(&Hd(n1));
            Tag(n1) = cons_t;
            Hd(n1) = Tl(n1);
            Tl(n1) = n2;
            return n1;
          } else {
            return new_apply(n1, n2);
          }
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
          
          /* ||defs == name:deflist */
          
          if (IsSet(root))
            refc_delete(&root);

          if (IsSet(defs))
            n1 = make_where(n1, refc_copy(Tl(defs)));

          root = n1;
          
          return n1;

        case 2: return n1;
        case 3: {
          /* check for unbound names; save defs */
             if (IsSet(defs)) /* todo make_defs(n1, defs) merging previous definitions with new */
            refc_delete(&defs);
          
          if (IsSet(root))
            refc_delete(&root); /* housekeeping for safety */
          
          defs = n1 = new_def(new_name("<Top>"), n1);
          return n1;
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

pointer make_result()
{
  if (Depth == 1)
    return Pop(1);
  (void) make_err("program", "stack alignment problem", (int)Depth);
  return NIL;
}


/*maker*maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker**maker*/
