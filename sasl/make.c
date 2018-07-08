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
#define Push(x)  (*++sp=(x)) /* xxx 'x' must not reference sp!! */
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

static pointer make_err2(char *f, char *msg)
{
  (void) make_reset();
  (void) err_make2(f, msg);
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
pointer make_abstract(pointer formal, pointer def, tag t)
{
  pointer n;
  
  if (debug > 1) {
    fprintf(stderr, "%s%d[", "make_abstract", (int)t);
    out_debug1(formal);
    fprintf(stderr, "] ");
    out_debug(def);
  }
  
  if (partial_compile)
    n = new_abstract(formal, def, t);
  else
    n = reduce_abstract(formal, def, t);
  
  if (debug > 1) {
    fprintf(stderr, "%s%d--> ", "make_abstract", (int)t);
    out_debug(n);
  }
  
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
  
  pointer a1 = make_abstract(refc_copy(Hd(defs)), condexp, abstract_condexp_t);
  pointer a2 = make_abstract(refc_copy(Hd(defs)), refc_copy(Tl(defs)), abstract_defs_t);

  condexp = new_apply(a1, a2);
  
  /* todo: copy and save defs list Hd/Tl pointers for debugging/ Module definition? */
  refc_delete(&defs); /* surplus cons node */
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
  if (IsNil(old) || IsMatchName(old))
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
 for every name in new, replace instances of that name n in old by (MATCH n)
 */

pointer de_dup(pointer new, pointer old)
{
  MAKE_DEBUG("de_dup: "); out_debug1(new); out_debug(old);

  if (IsNil(new) || IsMatchName(new))
    return old;
  
  if (IsStruct(new)) {
    old = de_dup(H(new), old);
    old = de_dup(T(new), old);
    return old;
  }
    
  return de_dup1(new, old);
}

/*
 * flatten a clause definition so that namelists appear as list of names in the def
 *    from: name:expr
 *    to: (name,):(expr,)
 * and
 *    from: namelist:expr
 *    to: list-of name : list-of expr   || taking care of matchnames
 
 * flatten ((MATCH:x):y):Expr = flatten (y:((MATCH:x) Expr))
 * flatten (a:x):Expr = (flatten a (H Expr)) ++ (flatten x (T Expr))
 * flatten n:Expr = (n,):(Expr,)
 * flatten ()     = ()
 
 */
/*
 * list-of (name|namelist) . list-of exp --> list-of name . list-of exp
 */
// deprecated - only used in unit test.
pointer make_flatten(pointer p)
{
  if (IsNil(p))
    return p;
  
  if (IsCons(H(p))){
    /* namelist */
    if (IsMatchName(HH(p))) {
      pointer n;
      n = new_apply(refc_copy(HH(p)), refc_copy(T(p)));
      n = new_cons( refc_copy(TH(p)), n);
      
      refc_delete(&p);
      
      return make_flatten(n);
    } else {
      pointer h = new_cons(refc_copy(HH(p)), new_apply(new_comb(H_comb), refc_copy(T(p))));
      pointer t = new_cons(refc_copy(TH(p)), new_apply(new_comb(T_comb), refc_copy(T(p))));
      
      refc_delete(&p);
      
      return make_append(make_flatten(h), make_flatten(t));
    }
  } else {
    
    /* name */
    H(p) = new_cons(H(p), NIL);
    T(p) = new_cons(T(p), NIL);
    
    return p;
  }
  
}
/*
 * flatten (a:x):e = (flatten a:(T e))
 * flatten n:e     = n:e
 */
void make_flatten0(pointer *n, pointer *e)
{
  if (IsName(*n))
    return;
  
  Assert(IsCons(*n));
  
  if (IsMatchName(H(*n))) {
    
  } else {
    make_flatten0(&H(*n), e);
    make_flatten0(&T(*n), e);
  }
  return;
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
       
       condexp <= cond_op opexp condexp condexp | opexp1 | listof-opexp
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
       * (5)  <rhs> ::= <formal>* = <expr>     * means 0 or more
       *                               1
	       *    || rhs <= expr
       !! ToDo consider check "name" is not present in Lhs.formal - its an error isn't it eg sasl "f a f = a?"!!
       */
      switch (subrule) {
        case 1: {
          /* use stack N reverse order of SASL text - abstract "rightmost" formal first */
          /* "f x y = E compiles to [x] ([y] E) where the innermost abstraction is performed first" [Turner] */
          /* this ensures correct de-dup'ing */
          pointer s;
          int i;
          Assert(howmany >= 1); /* expr */
          
          if (howmany > 1) {
            /* formal+ expr */
            s = sp[howmany - 1];  /* last/only formal */
            for (i = howmany - 2; i > 0; i--) /* rest of formals */
              s = new_apply(sp[i], de_dup(sp[i], s));
            
            return make_abstract(s, sp[howmany], abstract_formals_t);  /* [formal+] expr */
          } else {
            return n1;/*sp[1]*//* expr */
          }
        }
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
//          matchtag
//          something cleverer needed
//          x WHERE 1, x, x = 1,42,42? ==> 42
//          *one* new variable introduced 'x'
          
//          just like f 1 2 2 WHERE f 1 x x = 42
//          the only name to be abstracted from "f 1 x x = 42" is 'x' ALSO.
//
//          how to arrange this?
//
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
        pointer dup;
        /* || clause == names:expr */
        /* "info" gives the number of arguments */
        if (info > 0 && IsSameName(HH(n1), H(n2))) {
          /* multi-clause definition - not allowed with zero formals */
          HT(n1) = make_multi_clause(HT(n1), refc_copy(T(n2)), info);
        } else {
          /* single clause definition, so far */
          H(n1) = new_cons(refc_copy(H(n2)), H(n1));/*new update(H(n1), H(n2), me)*//* Assert(IsARoot(n1) || IsARoot(n2)) */
          T(n1) = new_cons(refc_copy(T(n2)), T(n1));/*new update(T(n1), T(n2), me)*//* Assert(IsARoot(n1) || IsARoot(n2)) */
        }
        
        dup = def_any_for2(TH(n1), TT(n1), H(n2)); /*search for latest name(s) in previous lists */
        if (IsSet(dup))
          (void) make_err2("clause: duplicate definition of the name: ", Name(dup));
        
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
        case 2: n1 = make_where(n1, n2); return n1;
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

          Assert(IsNil(root));
          Assert(IsDef(builtin));
          Assert(IsNil(defs) || IsDef(defs));

          /* scope of names: "((expr) WHERE defs) WHERE builtin" */
          if (IsDef(defs))
            n1 = make_where(n1, refc_copy(DefDefs(defs)));

          n1 = make_where(n1, refc_copy(DefDefs(builtin)));

          refc_delete(&root);
          root = n1;
          
          return root;
          
        case 2: return n1;
          
        case 3: {
          Assert(IsNil(root));
          Assert(IsDef(builtin));
          Assert(IsNil(defs) || IsDef(defs));
          
          /* scope of names: "defs WHERE builtin" */
//fault xxx          DefExprs(n1) = make_where(DefExprs(n1), refc_copy(DefDefs(builtin)));/*xxx is this right?*/
          
          /* record defs for future use*/
          if (IsNil(defs))
            defs = new_def(new_name("<Top>"), n1);
          else
            defs = add_deflist_to_def(defs, n1);

          /*TODO 2nd and subsequent DEF */
 /*
          if (IsNil(defs)) {
            n1 = make_where(n1, refc_copy(DefDefs(builtin)));
            defs = new_def(new_name("<Top>"), n1);
          } else {
            DefDefs(defs) = make_where(n1, refc_copy(DefDefs(defs)));
            DefDefs(defs) = make_where(DefDefs(defs), refc_copy(DefDefs(builtin)));
          }
  */
          
          //          DefDefs(defs) = make_where(DefDefs(defs), refc_copy(DefDefs(defs)));
          
          /* update defs */
          //          DefDefs(defs) = make_where(n1, refc_copy(DefDefs(defs)));
          
//          if (IsNil(defs)) {
//            defs = new_def(new_name("<Top>"), n1);
//          }
//          else {
//            n1 = make_where(n1, refc_copy(DefDefs(defs)));
//          }
          
//            T(defs) = make_where(n1, refc_copy(DefDefs(defs)));
//          }
//          else
//            defs = add_deflist_to_def(defs, n1);

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
 these are popped, so they are now at
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
