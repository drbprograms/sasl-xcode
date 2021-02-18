#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

#include "common.h"

/*
  tag names 
*/
/* 
   NB tag name array - **needs to be kept exactly in line with enum tag in common.h**
*/
		      
static char *tag_names[] = {
  "zero_t",
  "free_t",
  "deleting_t",
  /* Constants - node contains the value of the constant and never points to anything else */
  "int_t",
  "floating_t",
  "char_t",
  "bool_t",
  "fail_t",

  /* Apply/Cons - these have a hl and tl (aka car and cdr) which can pont to any other node, or be NIL */
  "apply_t",
  "cons_t",
  
  /* Name/Abstract tags used for partially compiled code - todo decide whether these are prohibited post-compilation */
  "name_t",
  "abstract_condexp_t",
  "abstract_formals_t",
  "abstract_where_t",
  "abstract_defs_t",
  "def_t",
  
  /* Operators - tl contains first argument (or NIL) */
  "->", //cond_op",

  ":", //colon_op",
  "++", //plusplus_op",
  "--", //minusminus_op",

  "..", //range_op",

  "...", //range_unbounded_op",

  "|", //or_op",

  "&", //and_op",

  ">>", //much_greater_op",
  ">", //greater_op",
  ">=", //greater_equal_op",
  "=", //equal_op",
  "~=", //not_equal_op",
  "<=", //less_equal_op",
  "<", //less_op",
  "<<", //much_less_op",

  "+", //"plus_op",
  "-", //"minus_op",

  "*", //"times_op",
  "/", //divide_op",
  "DIV", //"int_divide_op",
  "REM ", // "rem_op",

  "**", //power_op",

  "~", //unary_not_op",

  "+", //unary_plus_op",
  "-", //unary_minus_op",

  "#", //unary_count_op",

  /* Combinators */
  "I_comb",
  "K_comb",
  "K_nil_comb",
  "S_comb",
  "B_comb",
  "C_comb",
  "Sp_comb",
  "Bp_comb",
  "Cp_comb",
  "Y_comb",
  
  "Sc_comb",
  "Bc_comb",
  "Cc_comb",
  "Spc_comb",
  "Bpc_comb",
  "Cpc_comb",
  "Yc_comb",
  
  "U_comb",
  "TRY_comb",
  "TRYn_comb",
  "MATCH_comb",
  "MATCH_TAG_comb",

  "PAIR_comb",
  "H_comb",
  "T_comb",
  
  "unary_strict",
  "unary_nonstrict"
  

};

static char *tag_name_unknown = "<bad tag value>";

char *err_tag_name(tag t)
{
  if ((int) t < TagCount)
    return tag_names[t];	/* cons instead of apply */ 
  else
    return tag_name_unknown;
}

/* helper - return printable (%s) string for any combinator */
char *comb_label(pointer p)
{
  return IsName(H(p)) ? Name(H(p)) : "";
}


/****
 */
/*
 * taginfo - private container for information about tags
 */

static struct taginfo {
  tag t;     /* the tag inquestion */
  kind k;/* tag grouping, constant, combinator etc */
  char *name;    /* printable name "+" "S" etc */
  char nargs;    /* (>=0) how many apply nodes need to be stacked for a reduction: 0 for constant, + nargs 2, S nargs 3 */
  char strict;    /* (>=0) how many arguments must be reduced (assumes first only, first+second only etc.) */
  char new;    /* (>=0) how many apply nodes after the reduction: 0 for constant  or Indirection I node, + needs 0, S needs 3 */
  // etc.
} taginfo[TagCount];

/*
 * add_tag - set up taginfo
 */
static void add_tag(tag t, kind k, char *name, char nargs, char strict, char new /* etc. */)
{
  taginfo[t] = (struct taginfo) {t, k, name, nargs, strict, new /* etc. */};
}

inline char *tag_name(pointer p)    { return taginfo[Tag(p)].name; }
inline kind tag_kind (pointer p)    { return taginfo[Tag(p)].k; }
/*inline*/char tag_nargs(pointer p)    { return taginfo[Tag(p)].nargs; }
inline char tag_strict(pointer p)   { return taginfo[Tag(p)].strict; }
inline char tag_needs(pointer p)    { return taginfo[Tag(p)].new; }

inline char is_valid_tag(tag t)   { return (t > zero_t && t < _LastTag); }

inline int is_tag(tag t, pointer p)       { return IsSet(p) && t ==         Tag(p); }
inline int is(kind t, pointer p)      { return IsSet(p) && t == taginfo[Tag(p)].k; }

inline int is_struct    (pointer p)    { return is(apply, p) || is(cons, p);  }

inline int is_op    (pointer p)    { return is(operator, p); }
inline int is_unary_op    (pointer p)    { return is_op(p) && tag_nargs(p) == 1; }
inline int is_binary_op    (pointer p)    { return is_op(p) && tag_nargs(p) == 2; }
inline int is_ternary_op(pointer p)    { return is_op(p) && tag_nargs(p) == 3; }

/*
 * tag_init call once to initialise taginfo - a bit like microcode load
 */
int tag_init()
{
  add_tag(zero_t,    special,    "zero_t ",    0, 0, 0);    /* Special - unset tag - should never be found */
  add_tag(free_t,    special,    "free_t",    0, 0, 0);     /* Special - node that is on the free list */
  add_tag(deleting_t,special,    "deleting_t",    0, 0, 0); /* Special - node that is going to become free when refc_delete() has complted recursive delEtions */
  
  
  
  /* Constants - node contains the value of the constant and neveer points to anything else */
  add_tag(int_t,    constant,    "int_t",    0, 0, 0);
  add_tag(floating_t,    constant,    "floating_t",    0, 0, 0);
  add_tag(char_t,    constant,    "char_t",    0, 0, 0);
  add_tag(bool_t,    constant,    "bool_t",    0, 0, 0);
  
  add_tag(fail_t,    fail,        "fail_t",    0, 0, 0);    /* Special constant - to indicate pattern match has failed */
  
  
  /* Apply/Cons - these have a hl and tl (aka car and cdr) which can point to any other node, or be NIL */
  add_tag(apply_t,    apply,    "apply_t",    0, 0, 0);
  add_tag(cons_t,    apply,    "cons_t",    0, 0, 0);    /* (list n) => nth-item - when applied - apply list to number ...*/
  
  /* Name/Abstract tags used for partially compiled code - todo decide whether these are prohibited post-compilation */
  add_tag(name_t,        name,         "name_t",        0, 0, 0);
  
  /* abstract_x_t tags are irregular, their 2 args reside in the node hd tl itself, so nargs == 0 */
  add_tag(abstract_condexp_t,    abstract,    "abstract_condexp_t",    0, 0, 0);
  add_tag(abstract_formals_t,    abstract,    "abstract_formals_t",    0, 0, 0);
  add_tag(abstract_where_t,    abstract,    "abstract_where_t",    0, 0, 0);
  add_tag(abstract_defs_t,    abstract,    "abstract_defs_t",    0, 0, 0);
  
  add_tag(def_t,        def,         "def_t",        0, 0, 0);//???
  
  /* TODO when there's time to conduct before/aafter testing
   # define HasPointers(t) ((t) >= 0  ) */
  
  /* Operators */
  add_tag(cond_op,        operator, "->",    3, 1, 0);
  
  /* binary operators */
  add_tag(colon_op,        operator,    ":",    2, 0, 0);    /* ((: a) b) => (a:b) */
  add_tag(plusplus_op,        operator,    "++",    2, 3 /*XXX or 0*/, 1);    /* ++ () x => x */
  /* ++ list1 list2 =>  (hd-list1 : ((++ tl-list1) list2))  */
  // add_tag(minusminus_op,    operator,    "--",    2, 0, 0);
  
  // add_tag(range_op,        operator,    "..",    2, 0, 0);
  
  add_tag(or_op,        operator,    "|",    2, 1, 0);
  add_tag(and_op,        operator,    "&",    2, 2, 0);
  
  add_tag(much_greater_op,    operator,    ">>",    2, 2, 0);
  add_tag(greater_op,        operator,    ">",    2, 2, 0);
  add_tag(greater_equal_op,    operator,    ">=",    2, 2, 0);
  add_tag(equal_op,        operator,    "=",    2, 2, 0);
  add_tag(not_equal_op,        operator,    "~=",    2, 2, 0);
  add_tag(less_equal_op,    operator,    "<=",    2, 2, 0);
  add_tag(less_op,        operator,    "<",    2, 2, 0);
  add_tag(much_less_op,        operator,    "<<",    2, 2, 0);
  
  add_tag(plus_op,        operator,    "+",    2, 2, 0);
  add_tag(minus_op,        operator,    "-",    2, 2, 0);
  
  add_tag(times_op,        operator,    "*",    2, 2, 0);
  add_tag(divide_op,        operator,    "/",    2, 2, 0);
  add_tag(int_divide_op,    operator,    "DIV",    2, 2, 0);
  add_tag(rem_op,        operator,    "REM",    2, 2, 0);
  
  add_tag(power_op,        operator,    "**",    2, 2, 0);
  
  /* unary operators */
  add_tag(unary_strict,        builtin_op,    "unary",1, 1, 0);    /* Uname(p) contains name built in one-argument test eg "function" */
  add_tag(unary_nonstrict,     builtin_op,    "unary",1, 0, 0);     /* Uname(p) contains name built-in one argument maths eg "sin" */
  
  add_tag(unary_not_op,        operator,    "~",    1, 1, 0);

  add_tag(unary_plus_op,    operator,        "+",    1, 1, 0);
  add_tag(unary_minus_op,    operator,        "-",    1, 1, 0);
  
  // add_tag(range_unbounded_op,    operator,    "...",    1, 1, 0);
  // add_tag(unary_count_op,    operator,    "#",    1, 1, 0);
  
  /* Combinators - tl contains first argument.  Optionally hd points to name_t name of variable being abstracted,    for debugging purposes */
  add_tag(I_comb,        combinator,"I_comb",    1, 0, 0);
  add_tag(K_comb,    combinator,    "K_comb",    2, 0, 0);
  add_tag(K_nil_comb,    combinator,"K_nil_comb",2, 0, 0);    /* checks 2nd arg is NIL */
  add_tag(S_comb,    combinator,    "S_comb",    3, 0, 3);    /* S f g x => f x (g x)     == ((f x) (g x)) */
  add_tag(Sc_comb,    combinator,   "Sc_comb",   3, 0, 3);    /* Sp - S with cons instead of apply */
  add_tag(B_comb,    combinator,    "B_comb",    3, 0, 2);    /* B f g x => f (g x)        == (f (g x)) */
  add_tag(Bc_comb,    combinator,   "Bc_comb",   3, 0, 2);
  add_tag(C_comb,    combinator,    "C_comb",    3, 0, 2);    /* C f g x => f x g        == ((f x) g) */
  add_tag(Cc_comb,    combinator,   "Cc_comb",   3, 0, 2);
  
  add_tag(Y_comb,     combinator,   "Y_comb",    1, 0, 1);    /* Y f => (f <self>) */
  add_tag(Yc_comb,    combinator,    "Yc_comb",    1, 0, 1);
  
  add_tag(Sp_comb,    combinator,    "Sp_comb",  4, 0, 4);    /* Sp f g h x => f (g x) (h x)    == ((f (g x)) (h x)) */
  add_tag(Spc_comb,   combinator,    "Spc_comb", 4, 0, 4);
  add_tag(Bp_comb,    combinator,    "Bp_comb",  4, 0, 3);    /* Bp f g h x => f g (h x)    == ((f g) (h x))*/
  add_tag(Bpc_comb,   combinator,    "Bpc_comb", 4, 0, 3);
  add_tag(Cp_comb,    combinator,    "Cp_comb",  4, 0, 4);    /* Cp f g h x => f (g x) (h x)    == ((f (g x)) (h x))*/
  add_tag(Cpc_comb,   combinator,    "Cpc_comb", 4, 0, 4);
  
  add_tag(U_comb,     combinator,     "U_comb",   2, 0, 4);    /* U f g => f (H g) (T g)    == ((f (H g)) (T g)) */    /* U f(x:y)=f x (f y) [Turner79] */
  add_tag(TRY_comb,   combinator,     "TRY_comb", 2, 0, 0);    /* TRY FAIL y => y; TRY x y => x */
  add_tag(TRYn_comb,  combinator,     "TRYn_comb",4, /*xxx FIX IT 4 or */5 , 0);    /* TRYn 1 f g x => TRY (f x) (g x) == ((TRY (f x)) (g x)) */
  add_tag(MATCH_comb, combinator,     "MATCH_comb",3, 0, 0);    /* MATCH const E x => const = x -> E; FAIL*/
  add_tag(MATCH_TAG_comb,combinator,  "MATCH_TAG_comb",3, 0, 0);    /* MATCH test E x => (test x)= FALSE -> FAIL; E */
  
  // add_tag(PAIR_comb,    combinator,    "PAIR_comb",    2, 0, 0);    /* PAIR x y => 'x:y" notused */
  add_tag(H_comb,     combinator,     "H_comb",   1, 1, 0);    /* (H x:y) => x */
  add_tag(T_comb,     combinator,     "T_comb",   1, 1, 0);    /* (T x:y) => y */
  
  // _LastTag      /* Never appears in a node, used to calculate size of the array-of tag values */
  
  return 0;
}

/*
 ****/

/* looks for name in p, matches textually the name itself
 return 1 if name is present, otherwise 0
 */
int got_name(pointer name, pointer p)
{
  int res = 0;
  
  Assert(IsName(name));
  if (is_same_name(name, p))
    res = 1;
  else if (IsStruct(p))
    res = got_name(name, Hd(p)) || got_name(name, Tl(p));
  
  if (res)
    Debug1("got_name:%s\n", Name(name));
  
  return res;
}

/* Returns 1 if n1 and n2 are name nodes with same Name() strings, otherwise 0 */
int is_same_name(pointer n1, pointer n2)
{
  return
    IsName(n1) &&
    IsName(n2) &&
    Name(n1) &&
    Name(n2) &&
    (! strcmp(Name(n1), Name(n2)));
}
/*
 * error handling - err...
 */



jmp_buf jmpbuffer;	/* for longjmp() in case of errors */

int err_lex(char *msg1, char *msg2)
{
  extern char *yytext;
  extern int yylineno;
  extern char *lex_filename;
  
  Error5("reset: reading input: %s %s \"%s\" at %s line:%d\n", msg1, msg2, yytext, lex_filename, yylineno);
  longjmp(jmpbuffer, 1);
  return 0; /*NOTEREACHED*/
}

int err_parse(char *f, char *msg1, char *msg2)
{
  extern char *yytext;
  extern int yylineno;
  extern char *lex_filename;

  Error6("reset: parsing: %s (%s in rule %s) got \"%s\" at %s line:%d\n", msg1, f, msg2, yytext, lex_filename, yylineno);
  longjmp(jmpbuffer, 1);
  return 0; /*NOTEREACHED*/
}

int err_make(char *f, char *msg1, int i)
{
  Error3("reset: making: %s (%s %d)\n", f, msg1, i);
  longjmp(jmpbuffer, 2);
  return 0; /*NOTEREACHED*/
}

int err_make2(char *f, char *msg1)
{
  Error2("reset: making: %s%s\n", f, msg1);
  longjmp(jmpbuffer, 2);
  return 0; /*NOTEREACHED*/
}

int err_make1(char *f)
{
  Error1("reset: making: %s\n", f);
  longjmp(jmpbuffer, 2);
  return 0; /*NOTEREACHED*/
}

int err_out(char *f, char *msg1, char *msg2, int n)
{
  Error3("reset: out: %s (%s %s)\n", msg1, f, msg2);
  longjmp(jmpbuffer, 3);
  return 0; /*NOTEREACHED*/
}

int err_reduce2(char *msg1, char *msg2)
{
  Error2("reset: reduce: %s %s\n", msg1, msg2);
  longjmp(jmpbuffer, 4);
  return 0; /*NOTEREACHED*/
}

int err_reduce(char *msg1)
{
  return err_reduce2(msg1, "");
}

int err_refc(char *msg1)
{
  Error1("reset: refc: %s\n", msg1);
  longjmp(jmpbuffer, 5);
  return 0; /*NOTEREACHED*/
}

int err_refc1(char *msg1, unsigned u)
{
  Error2("reset: refc: %s%u\n", msg1, u);
  longjmp(jmpbuffer, 5);
  return 0; /*NOTEREACHED*/
}

int err_refc2(char *msg1, char *msg2)
{
  Error2("reset: refc: %s %s\n", msg1, msg2);
  longjmp(jmpbuffer, 5);
  return 0; /*NOTEREACHED*/
}

int err_zone(char *msg1)
{
  Error1("reset: zone: %s\n", msg1);
  longjmp(jmpbuffer, 5);
  return 0; /*NOTREACHED*/
}

int err_zone1(char *msg1, char *msg2)
{
  Error2("reset: zone: %s%s\n", msg1, msg2);
  longjmp(jmpbuffer, 5);
  return 0; /*NOTREACHED*/
}

pointer err_store(char *msg1)
{
    Error1("reset: store: %s\n", msg1);
    longjmp(jmpbuffer, 5);
    return NIL; /*NOTEREACHED*/
}

/*
 * outputter - out...
 */

static int out_out(FILE *where, pointer n);

/* change to use "*" which some text editors will display a italic */
int out_comb_name(FILE *where, pointer n)
{
  if (IsSet(Hd(n))) {
    fprintf(where, "*");
    (void) out_out(where, Hd(n));
    fprintf(where, "*");
  }
  
  return 0;
}

/* out_out 
 pretty-print pointer n on channel s, global "budget" used to control monster outputs -1 is infinite!
 used by out() and out_debug() only
 */

static int budget = -1;

#define STACK_SIZE 10000
#define STACK_SIZE 50000 /xx

//#define Stacked (sp-stack)    /* >=0 */
#define Depth  (sp-stack)
#define Stacked (sp-base)

//#define Push(x) (sp++, *sp = (x))
//#define Pop(n)  (sp -= (n), sp[n])
#define Pop(n)  (Assert(Depth >= (n)), sp -= (n), Top) /* assert(sp>=base) value is Top of stack */
#define Push(n) (Assert(Depth < STACK_SIZE), sp[1] = (n), sp++) /* sequencing to ensure Push(*sp) works correctly */
#define Top   (*sp)
#define Max 10

static int out_out(FILE *where, pointer n)
{
  pointer stack[Max+1];
  pointer *sp = stack;
  
  /* loop avoidance */
  if (budget == 0) {
    fprintf(where, "... ");
    return 0;
  }
  if (IsNil(n))
    return fprintf(where, "()");
//  else
//    if (IsWeak(n))
//      return fprintf(where, "<loop>");  /* can we identify the variable in question? */
    else    {
      char *sep = " ";
      int i;
      
      char *refc_pointer_info(pointer p); /* for debugging */

      if (IsWeak(n))
        (void) fprintf(where, "@");	/* hint for weak loops */
      
      switch (Tag(n)) {	/* (tag) cast ensures compler warnings for any tags not covered */
        case zero_t:
        case free_t:
        case deleting_t:
          /* Should Never Happen - so do NOT print "being deleted" node "*/
          return fprintf(where, "\n!!%s%s\n.... ", err_tag_name(Tag(n)), refc_pointer_info(n));

        case int_t:
          return fprintf(where, "%d",Num(n));
        case floating_t:
          return fprintf(where, "%e",Dbl(n));
        case char_t:	/* need to be smarter to print strings 'qwerty" */ /* todo decide whether to print special chars as 'TAB' or flatten? */
          return fprintf(where, "%%%c",Char(n));
        case bool_t:
          return fprintf(where, "%s", Bool(n) ? "TRUE" : "FALSE");
        case fail_t:
          return fprintf(where, "FAIL");

        case cons_t:
          sep = ":";
          if (budget > 0)
            budget--;
          
          for (i = 0; i < Max && IsSet(n) && IsCons(n); i++) {
            Push(n);
            n = Hd(n);
          }
          
          (void) fprintf(where, "(");
          (void) out_out(where, n);
          /* assert(i > 0) */
          for (/**/; i > 0; i--) {
            (void) fprintf(where, "%s", sep);
            (void) out_out(where, Tl(Top));
            Pop(1);
          }
          return fprintf(where, ")");
          
        case apply_t:
          sep = " ";
          
          if (budget > 0)
            budget--;
          
          for (i = 0; i < Max && IsSet(n) && IsApply(n); i++) {
            Push(n);
            n = Hd(n);
          }
          
          /* assert(i > 0) */
          (void) fprintf(where, "(");
          (void) out_out(where, n);
          for (/**/; i > 0; i--) {
            (void) fprintf(where, "%s", sep);
            (void) out_out(where, Tl(Top));
            Pop(1);
          }
          
          return fprintf(where, ")");
          
        case name_t:
          return fprintf(where, "%s",Name(n));/* todo handle names */
        case abstract_defs_t:
          (void) fprintf(where,"*");
        case abstract_where_t:
          (void) fprintf(where,"*");
        case abstract_condexp_t:
          (void) fprintf(where,"*");
        case abstract_formals_t:
          (void) fprintf(where, "[");
          (void) out_comb_name(where, n);
          (void) fprintf(where, "]");
          return out_out(where, Tl(n));
          
        case def_t:
          (void) fprintf(where, "{");
          (void) out_comb_name(where, n);
          (void) fprintf(where, ":");
          (void) out_out(where, Tl(n));
          return fprintf(where, "}");
          
        case cond_op:
          return fprintf(where, "->");
        case colon_op:
          return fprintf(where, ":");
        case plusplus_op:
          return fprintf(where, "++");
        case minusminus_op:
          return fprintf(where, "--");
          
        case range_op:
          return fprintf(where, "..");
          
        case range_unbounded_op:
          return fprintf(where, "...");
          
        case or_op:
          return fprintf(where, "|");
          
        case and_op:
          return fprintf(where, "&");
          
        case much_greater_op:
          return fprintf(where, ">>");
        case greater_op:
          return fprintf(where, ">");
        case greater_equal_op:
          return fprintf(where, ">=");
        case equal_op:
          return fprintf(where, "=");
        case not_equal_op:
          return fprintf(where, "~=");
        case less_equal_op:
          return fprintf(where, "<=");
        case less_op:
          return fprintf(where, "<");
        case much_less_op:
          return fprintf(where, "<<");
          
        case plus_op:
          return fprintf(where, "+");
        case minus_op:
          return fprintf(where, "-");
          
        case times_op:
          return fprintf(where, "*");
        case divide_op:
          return fprintf(where, "/");
        case int_divide_op:
          return fprintf(where, "div");
        case rem_op:
          return fprintf(where, "rem");
          
        case power_op:
          return fprintf(where, "**");
          
        case unary_not_op:
          return fprintf(where, "~");
          
        case unary_plus_op:
          return fprintf(where, "unary+");/* no different binary plus */
        case unary_minus_op:
          return fprintf(where, "unary-"); /* no different binary minus */
          
        case unary_count_op:
          return fprintf(where, "#");
          
          /* hd contains debug name ie orginal variable */
        case I_comb:
          (void) fprintf(where, "I");
          return out_comb_name(where, n);
        case K_comb:
          (void) fprintf(where, "K");
          return out_comb_name(where, n);
        case K_nil_comb:
          (void) fprintf(where, "K_nil");
          return out_comb_name(where, n);
        case Y_comb:
          (void) fprintf(where, "Y");
          return out_comb_name(where, n);
        case Yc_comb:
          (void) fprintf(where, "Yc");
          return out_comb_name(where, n);
        case U_comb:
          (void) fprintf(where, "U");
          return out_comb_name(where, n);
        case S_comb:
          (void) fprintf(where, "S");
          return out_comb_name(where, n);
        case Sc_comb:
          (void) fprintf(where, "Sc");
          return out_comb_name(where, n);
        case Sp_comb:
          (void) fprintf(where, "Sp");
          return out_comb_name(where, n);
        case Spc_comb:
          (void) fprintf(where, "Spc");
          return out_comb_name(where, n);
        case B_comb:
          (void) fprintf(where, "B");
          return out_comb_name(where, n);
        case Bc_comb:
          (void) fprintf(where, "Bc");
          return out_comb_name(where, n);
        case Bp_comb:
          (void) fprintf(where, "Bp");
          return out_comb_name(where, n);
        case Bpc_comb:
          (void) fprintf(where, "Bpc");
          return out_comb_name(where, n);
        case C_comb:
          (void) fprintf(where, "C");
          return out_comb_name(where, n);
        case Cc_comb:
          (void) fprintf(where, "Cc");
          return out_comb_name(where, n);
        case Cp_comb:
          (void) fprintf(where, "Cp");
          return out_comb_name(where, n);
        case Cpc_comb:
          (void) fprintf(where, "Cpc");
          return out_comb_name(where, n);
        case TRY_comb:
          (void) fprintf(where, "TRY");
          return out_comb_name(where, n);
        case TRYn_comb:
          (void) fprintf(where, "TRYn");
          return out_comb_name(where, n);
        case MATCH_comb:
          (void) fprintf(where, "MATCH");
          return out_comb_name(where, n);
        case MATCH_TAG_comb:
          (void) fprintf(where, "MATCH_TAG");
          return out_comb_name(where, n);
        case PAIR_comb:
          (void) fprintf(where, "PAIR");
          return out_comb_name(where, n);
        case H_comb:
          (void) fprintf(where, "H");
          return out_comb_name(where, n);
        case T_comb:
          (void) fprintf(where, "T");
          return out_comb_name(where, n);
          
        case unary_strict:
          return fprintf(where, "strict-%s", Uname(n));
        case unary_nonstrict:
          return fprintf(where, "non-strict-:%s", Uname(n));
        case _LastTag:
          return err_zone("invalid tag: _LastTag");
      }
    }
  return 0; /*NOTEREACHED*/
}

pointer out(pointer n)
{
  budget = -1; /* unlimited output */

  (void) out_out(stdout, n);
  (void) User("\n");
  return n;
}

#define Limit 16  /* arbitrary limit to output */
pointer out_debug(pointer n)
{
 return out_debug_limit(n, Limit);
}


pointer out_debug_limit(pointer n, int limit)
{
  if (!debug)
    return n;
  
  budget = limit;
  (void) out_out(stderr, n);
  Debug("\n");
  return n;
}

pointer out_debug1(pointer n)
{
 return out_debug_limit1(n, Limit);
}

/* out_debug_limit1() - NO newline, otherwise same as out_deb ug_limit() */
pointer out_debug_limit1(pointer n, int limit)
{
  if (!debug)
    return n;

  budget = limit;
  (void) out_out(stderr, n);
  /* NO newline */
  return n;
}

/*
 * pretty_print - show an expression as SASL
 */
/* rules from parse */

/*
 (6)  <clause> ::= <name><rhs> | <namelist> = <expr>
 
 (1) <formal> ::= <name> | <constant> | (<namelist>)
 (2)  <struct> ::= <formal>:<struct> | (<formal>)
 (3)  <namelist> ::= <struct> , . . . ,<struct> | <struct>, | <struct>

 (6.1) <clause> ::= <namelist> = <expr>
 clause <= names:expr
 (7)  <defs> ::= <clause> [; <clause>]*  * means 0 or more
 defs <= (list-of (name|namelist)).(list-of expr)
 (8)  <expr> ::= <condexp> [where <defs>]*  * means 0 or more
 expr <= make_where(defs, condexp)
 expr <= make_apply(abstract_condexp(H(defs), condexp),  abstract+where(H(defs), T(defs))
 expr <= make_apply(abstract_condexp(list-of (name|namelist)), condexp),
                            abstract+where(list-of (name|namelist)), list-of-expr)
 expr <= make_apply( abstract_condexp(list-of defs/names, condexp),
                            abstract+where(list-of defs/names, list-of defs/expr) )
 expr <= make_apply( abstract_condexp(list-of defs/names, condexp), abstract_where(list-of defs/names, list-of defs/expr) )
 
 abstract.c:
 expr <= make_apply( abstract_condexp(list-of defs/names, condexp), Y(abstract_defs(list-of defs/names, list-of defs/expr)) )
 expr <= make_apply( abstract1(U defs/names1 (U defs/names2 .... (Udefs/namesN K_nil, condexp)))), Y(abstract_defs(list-of defs/names, list-of defs/expr)) )
  expr <= make_apply( abstract1(U defs/names1 (U defs/names2 .... (Udefs/namesN K_nil, condexp)))), Y(abstract_defs(list-of defs/names, list-of defs/expr)) )
 


 */
/* rules from make */

/* make_where()
  expr ::= <condexp> where <defs> <= ([H(defs)]c condexp) ([H(defs)]w T(defs))
<= ([H(defs)]c condexp) Y_comb ([H(defs)]d T(defs))
<= ([H(defs)]c condexp) Y_comb (U ([HH(defs)]d T(defs))
                                  ([TH(defs)]d T(defs)))
<= U*d1* ([HH(defs)]c condexp)([TH(defs)]c condexp)
         Y_comb (U ([HH(defs)]d T(defs))
                                  ([TH(defs)]d T(defs)))

                                                          where c=abstract_condexp_t
                                                                w=abstract_where_t
                                                                d=abstract_defs_t
                                                                d1=Name(HH(defs)   //name of first clause **may also be a namelist with no name!
 */
/* maker()
  (5)  <rhs> ::= <formal>+ = <expr> <=  [(formal1 ... (formalN-1 de_dup(formalN-1, formalN)))]f expr
                                                           where +=one or more f=abstract_formals_t
                                                           where de_dup(n, exp)= replace name "n" by (MATCH "n") in "exp"
 
 (6)  <clause> ::= <namelist> = <expr> | <name><rhs>
 => <namelist> . <expr> | <name> . <rhs>      // defines a list of names or a name (with possible formals) :-)
 
 
 (7)  <defs> ::= <clause> [; <clause>]*  * means 0 or more
 (13) <deflist> ::= <clause> [; <clause>]*
 defs <= (list-of (name|namelist)).(list-of expr)


  */

/* rules from abstract */

/* reduce_abstract() */
/* [const] E   => MATCH const E       {abstract_formals_t} ==> const*/
/* [NIL]   E   => MATCH NIL   E       {abstract_formals_t} ==> "()"*/
/* [name] E    => abstract1(name, E)  */
/* [MATCH n] E => MATCH n E           {abstract_formals_t} ==> n */
/* [x:NIL] E   => MATCH_tag CONS U ([x] (K_nil E))         ==> pp(x) " = "  */
/* [x:y] E     => MATCH_tag CONS U ([x] ([y] E))           ==> pp(x) ":" pp(y) */
/* [a b] E     => [a] ([b] E) {!abstract_formals_t} */

/* reduce_abstrct1() - abstract one name only */
/* [x] E => K abstract_do(x, E)   || if x is *not* present in E (got == 1)*/
/* [x] E =>   abstract_do(x, E)   || otherwise */
/* deprecated: if "r then recursive abstract Y ... */

/* reduce_abstract_do() */
/* [x] x ==> I  ||  *got++ */

/* [x] E1 E2 ==>  S E1 E2   || if x occurs in E1 and E2     (hgot&&tgot)*/
/* [x] E1 E2 ==>  C E1 E2   || if x occurs in E1 and not E2 (hgot&&!tgot) */
/* [x] E1 E2 ==>  B E1 E2   || if x occurs in E2 and not E1 (!hgot&&tgot)*/
/* [x] E1 E2 ==>    E1 E2   || if x occurs in neither E1 nor E2 (!hgot&&!tgot) or (got==0) */

/* [x] E1:E2 ==>  Sp E1 E2  || if x occurs in E1 and E2 */
/* [x] E1:E2 ==>  Cp E1 E2  || if x occurs in E1 and not E2 */
/* [x] E1:E2 ==>  Bp E1 E2  || if x occurs in E2 and not E1 */
/* [x] E1:E2 ==>     E1 E2  || if x occurs in neither E1 nor E2 (got==0) */

/* [x] other ==> other      || (got==0) */

/* rules from make */

/*
 */

/* make_where() */
/*
 (name ...).((expr:list-of formals) ...)
 result is
 ([name ...] condexp) (Y expr)
 */

/* make_multi_clause*() */
/* multi def1 def2 n <= TRYn n def1 def2 */


/* helper */

/* is_nil_terminated - return 1 if last list element is NIL (or p is NIL) */
static int is_nil_terminated(pointer p)
{
  while (IsCons(p) && IsStrong(p))
    p = T(p);
  
  return IsNil(p);
}
/*
 (1) <formal> ::= <name> | <constant> | (<namelist>)
 (2)  <struct> ::= <formal>:<struct> | (<formal>)
 (3)  <namelist> ::= <struct> , . . . ,<struct> | <struct>, | <struct>
 */
extern int pretty_print_formal(FILE *where, pointer n); /*forward reference */

static pointer stack[STACK_SIZE+1];
static pointer *sp = stack;

/*
 * reduce_show - print a constant - unstructured results of reduction
 */
int pretty_print_const(FILE *where, pointer p)
{
  if (IsNil(p))
    return 0;
  
  if (IsNum(p))
    fprintf(where, "%d", Num(p));
  else if (IsDbl(p))
    fprintf(where, "%g", Dbl(p));
  else if (IsChar(p))
    fprintf(where, "%c", Char(p));
  else if (IsBool(p))
    fprintf(where, "%s", Bool(p) ? "TRUE" : "FALSE");
  else {
    err_out(err_tag_name(Tag(p)), "pretty_print_const: fail", "\n", 0);
    return 1;
  }
  
  return 0;
}

int pretty_print_name(FILE *where, pointer p)
{
  if (IsName(p))
    fprintf(where, "%s", Name(p));
  else
    return 1;
  
  return 0;
}


int pretty_print_namelist(FILE *where, pointer n)
{
  char sep;
 
  if (! IsCons(n))
    return 1; /* error - arrived here from formal for (namelist) and not name or constant - ignore "f ((x)) = 1" */

  pretty_print_formal(where, H(n));

  n = T(n);
  
  if (IsNil(n))
    return fprintf(where, ",");
  
  sep = is_nil_terminated(n) ? ',' : ':';
  for (/**/; IsCons(n); n = T(n)) { //ToDo IsStrong() ...
    fprintf(where, "%c", sep);
    pretty_print_formal(where, H(n));
  }
  
  if (IsSet(n)) {
    fprintf(where, "%c", sep); // Assert(sep==':')
    pretty_print_formal(where, n);
  }

//  do {
//    fprintf(where, "%c", sep);
//    pretty_print_namelist(where, n);
//    n = T(n);
//  } while (IsCons(n));

  return 0;
}

int pretty_print_list(FILE *where, pointer p)
{
  if (!IsCons(p))
    return 1;
  
//  Debug2("pretty_print_list%s (Depth=%d)\n", refc_pointer_info(p), Depth);

  if (IsWeak(p)) {
    fprintf(where, "<loop/list>");
    return 0;
  }

  /* 1-list "a,"
   * 2-list "a,b"
   * n-list "a,b ....n"
   * list not terminated by nil "...:n"  */
  (void) pretty_print(where, H(p));
  if (IsNil(T(p))) {
    /* 1-list */
    fprintf(where, ",");
  } else {
    /* n-list */
    pointer c;
    for (c = T(p); IsSet(c) ; c = T(c)) {
      if (IsWeak(c)) {
        fprintf(where, ":<loop/list>");
        break; /* done */
      } else if (IsCons(c)) { /* normal case 1,2,3 */
        fprintf(where, ",");
        pretty_print(where, H(c));
      } else { /* non-nil last element 1,2:3 */
        fprintf(where, ":");
        pretty_print(where, c);
        break; /* done */
      }
    }
  }
  return 0;
}
int pretty_print_formal(FILE *where, pointer n)
{
  /* (1ab) <formal> ::= <name> | <constant> | (<namelist>) */
  if (IsName(n) || IsConst(n))
    return pretty_print(where, n);
  
  /* (c) <formal> ::= (<namelist>) */
  fprintf(where, "(");
  pretty_print_namelist(where, n);
  fprintf(where, ")");

  return 0;
}

/* (5)  <rhs> ::= <formal>* = <expr>    * means 0 or more */
int pretty_print_clause(FILE *where, pointer name, pointer n)
{
  
  pretty_print_namelist(where, name);

  return 0;
}

int pretty_print_def(FILE *where, pointer n)
{
  /* def_t (list of lhs) (list of rhs)    lists ought to be same (length and finite) */
  pointer lhs, rhs;
 fprintf(where, "|| %s\n", comb_label(n));
  
  for (lhs = HT(n),   rhs = TT(n);
       !IsNil(lhs);  /* nb rhs can validly be NIL */
       lhs = T(lhs), rhs = T(rhs)) {
    pretty_print_clause(where, H(lhs), H(rhs));
    fprintf(where, "; "); // Todo offside etc
  }

  return 0;
}

int pretty_print(FILE *where, pointer n)
{
  pointer *base = sp;  /* remember Top on entry - used to calculate Stacked */
  extern char *refc_pointer_info(pointer p); /* for debugging */
  
//  Debug2("pretty_print%s (Depth=%ld)\n", refc_pointer_info(n), Depth);
  if (! debug)
    return 0;
  
  if (IsNil(n))
    return fprintf(where, "()");
  
  if (IsWeak(n))
    return fprintf(where, "<loop>");
  
  // (void) fprintf(where, "(");
  Push(n);
  
  /* travel down the 'spine' */
  // todo check for IsWeak()
  while (IsApply(Top)) {
    if (IsWeak(H(Top))) {
      Pop(Stacked);
      return fprintf(where, "<loop/spine>");
    }
//    Debug1("pretty_print/spine%s\n", refc_pointer_info(Top));
    Push(H(Top));
  }
//    Debug1("pretty_print/spine%s\n", refc_pointer_info(Top));

  {/*start: inner loop*/
    const int MAXARG = 4; /* max number of apply nodes in spine for a reduction */
    pointer arg[MAXARG+1];
    
    pointer it = Top; /* "end" of the spine */
    int nargs = tag_nargs(it);
    
    int i;
    
//    Debug3("pretty_print/inner%s (%d/%ld nargs/Stacked\n", refc_pointer_info(it), nargs, Stacked);
    
    /*Pop "it" */
    Pop(1);
    
    /* locate args, numbered arg[1], arg[2], ... */
    if (nargs > Stacked) {
      //      err_reduce2("problem printing ", err_tag_name(it));
      Pop(Stacked);
      return 1;
    }
    
    for (i = 1; i <= nargs;  i++) {
      arg[i] = Tl(Top);
//      Debug3("pretty_print arg[%d/%d]%s\n", i, nargs, refc_pointer_info(arg[i]));
      Pop(1);
    }
    
    if (IsConst(it))
      pretty_print_const(where, it);
    else if (IsName(it))
      pretty_print_name(where, it);
    else if (IsCons(it)) {
      pretty_print_list(where, it);
      if (Stacked > 1) {
        /* list applied to a number */
        fprintf(where, "%s", " ");
        Pop(1);
        pretty_print(where, T(Top));
      }
    } else if (IsUnaryOp(it)) {
      /* prefix only */
      fprintf(where, "%s", err_tag_name(Tag(it)));
      pretty_print(where, Tl(arg[1]));
    } else if (Tag(it) == unary_strict || Tag(it) == unary_nonstrict) {
      Assert(nargs == 1);//xx
      fprintf(where, "%s", Uname(it));
      pretty_print(where, arg[1]);
    } else if (IsBinaryOp(it)) {
      Assert(nargs == 2);
      pretty_print(where, arg[1]);
      fprintf(where, " %s ", err_tag_name(Tag(it)));
      pretty_print(where, arg[2]);
    } else if (Tag(it) == cond_op) {
      Assert(nargs == 3);//xx
      pretty_print(where, arg[1]);
      fprintf(where, " -> ");
      pretty_print(where, arg[2]);
      fprintf(where, "; ");
      pretty_print(where, arg[3]);
    } else {
      switch (Tag(it)) {
        case zero_t:
        case free_t:
        case deleting_t:
          /* Should Never Happen - so do NOT print "being deleted" node "*/
          Debug1("pretty_print: should never see%s\n",refc_pointer_info(it));
          return fprintf(where, "\n!!%s\n.... ", err_tag_name(Tag(it)));

        case _LastTag:
          return err_zone("invalid tag: _LastTag");

        case abstract_condexp_t:
          (void) pretty_print_formal(where, T(it)); // irregular
          break;
        case abstract_where_t:
        case abstract_defs_t:
        case abstract_formals_t:
          (void) pretty_print_formal(where, H(it)); // irregular
          (void) fprintf(where, " = ");
          (void) pretty_print_formal(where, T(it)); // irregular
          break;
          
        case def_t:
          pretty_print_def(where, it);
          break;
          
          /* hd contains debug name ie orginal variable */
        case I_comb: /* I*x* E => x;  I E => E */
          //ToDo I*x* {} => x; I*x* E => E
          if (IsSet(H(it)))
            fprintf(where, "%s", comb_label(it));
          else
            pretty_print(where, arg[1]);
          break;

        case K_comb:  /* K x y => x */
        case K_nil_comb:
            pretty_print(where, arg[1]);
          break;

        case S_comb: /* S f g x => f x (g x) */
          //ToDo "share" rather than printing twice: arg1 x (arg2 x) where x = arg3
//          pretty_print(where, arg[1]);
//          fprintf(where, "%s", comb_label(it));
//          fprintf(where, "%s", "(");
//          pretty_print(where, arg[2]);
//          fprintf(where, "%s", comb_label(it));
//          fprintf(where, "%s", ")");
        {
          char *x = comb_label(it);
          pretty_print(where, arg[1]);
          fprintf(where, "%s", x);
          fprintf(where, "%s", "(");
          pretty_print(where, arg[2]);
          fprintf(where, " %s) where %s = ", x, x);
          pretty_print(where, arg[3]);
          break;
        }
        case B_comb: /* B f g x => f (g x) or f . g x*/
          pretty_print(where, arg[1]);
          fprintf(where, "%s", " . ");
          pretty_print(where, arg[2]);
          pretty_print(where, arg[3]);
          break;
        case C_comb: /* C f g x => f x g */
          pretty_print(where, arg[1]);
          pretty_print(where, arg[3]);
          pretty_print(where, arg[2]);
          break;

        case Yc_comb:
        case Y_comb:
            (void) fprintf(where, " Where ");
            pretty_print(where, arg[1]);
          break;

        case U_comb:
          /* [x:NIL] E => U ([x] (K_nil E)) */
          /* [x:y] E => U ([x] ([y] E)) */
          /* E */
 // BUG fails with prelude-test.sasl
//          pretty_print(where, T(T(arg[1]))); // simplistic
          break;
          
        case TRYn_comb:
//          pretty_print(where, arg[1]);
////////what?
//          pretty_print(where, arg[2]);
          break;

          /* problematic ones */
        case PAIR_comb:
          /* not used */
          break;
          
          
        case H_comb:
          fprintf(where, " hd (");
          pretty_print(where, arg[1]);
          fprintf(where, ")");
          break;
        case T_comb:
          fprintf(where, " tl (");
          pretty_print(where, arg[1]);
          fprintf(where, ")");
          break;
        default:
          break;
      }
    }

  }

  /* print remaining unexpected args */
  while (Stacked > 0) {
    pretty_print(where, T(Top));
    Pop(1);
  }
  
  /* Pop remaining unexpected args */
  Pop(Stacked);
  return 0;
}

/*
  helper functions
*/



/*
 * Tables to store variable-sized objects
 * new_table creates an array of count object of given size (exeactly)
 */

void *new_table(size_t count, size_t size)
{
  /* to do add loggin/validation? */
  void *t =  calloc(size, count);
  if (t == NULL) {
    (void) err_zone("new_table: calloc out of space");
    return 0; /*NOTREACHED*/
  }
  return t;
}

void free_table(void *t)
{
  /* to do add loggin/validation? */
  free(t);
}

