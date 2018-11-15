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
  "abstract_defs_t",
  "def_t",
  
  /* Operators - tl contains first argument (or NIL) */
  "cond_op",

  "colon_op",
  "plusplus_op",
  "minusminus_op",

  "range_op",

  "range_unbounded_op",

  "or_op",

  "and_op",

  "much_greater_op",
  "greater_op",
  "greater_equal_op",
  "equal_op",
  "not_equal_op",
  "less_equal_op",
  "less_op",
  "much_less_op",

  "plus_op",
  "minus_op",

  "times_op",
  "divide_op",
  "int_divide_op",
  "rem_op",

  "power_op",

  "unary_not_op",

  "unary_plus_op",
  "unary_minus_op",

  "unary_count_op",

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

/* looks for name in p, matches textually the name itself
 return 1 if name is present, otherwise 0
 */
int got_name(pointer name, pointer p)
{
    /*temp*/
    fprintf(stderr,"got_name: "); out_debug1(name); out_debug(p);
    
    Assert(IsName(name));
    if (IsSet(p) && IsName(p) && EqName(p, name))
        return 1;
    if (IsStruct(p)) {
        return got_name(name, Hd(p)) || got_name(name, Tl(p));
    }
    return 0;
}


/*
 * error handling - err...
 */



jmp_buf jmpbuffer;	/* for longjmp() in case of errors */

int err_parse(char *f, char *msg1, char *msg2)
{
  extern char *yytext;

  (void) fprintf(stderr, "reset: parsing: %s (%s in rule %s) got \"%s\"\n", msg1, f, msg2, yytext);
  longjmp(jmpbuffer, 1);
  return 0; /*NOTEREACHED*/
}

int err_make(char *f, char *msg1, int i)
{
  (void) fprintf(stderr, "reset: making: %s (%s %d)\n", f, msg1, i);
  longjmp(jmpbuffer, 2);
  return 0; /*NOTEREACHED*/
}

int err_make2(char *f, char *msg1)
{
  (void) fprintf(stderr, "reset: making: %s%s\n", f, msg1);
  longjmp(jmpbuffer, 2);
  return 0; /*NOTEREACHED*/
}

int err_make1(char *f)
{
  (void) fprintf(stderr, "reset: making: %s\n", f);
  longjmp(jmpbuffer, 2);
  return 0; /*NOTEREACHED*/
}

int err_out(char *f, char *msg1, char *msg2, int n)
{
  (void) fprintf(stderr, "reset: out: %s (%s %s)\n", msg1, f, msg2);
  longjmp(jmpbuffer, 3);
  return 0; /*NOTEREACHED*/
}

int err_reduce2(char *msg1, char *msg2)
{
  (void) fprintf(stderr, "reset: reduce: %s %s\n", msg1, msg2);
  longjmp(jmpbuffer, 4);
  return 0; /*NOTEREACHED*/
}

int err_reduce(char *msg1)
{
  return err_reduce2(msg1, "");
}

int err_refc(char *msg1)
{
  (void) fprintf(stderr, "reset: refc: %s\n", msg1);
  longjmp(jmpbuffer, 5);
  return 0; /*NOTEREACHED*/
}

int err_refc1(char *msg1, unsigned u)
{
  (void) fprintf(stderr, "reset: refc: %s%u\n", msg1, u);
  longjmp(jmpbuffer, 5);
  return 0; /*NOTEREACHED*/
}

int err_zone(char *msg1)
{
  (void) fprintf(stderr, "reset: zone: %s\n", msg1);
  longjmp(jmpbuffer, 5);
  return 0; /*NOTEREACHED*/
}

int err_store(char *msg1)
{
    (void) fprintf(stderr, "reset: store: %s\n", msg1);
    longjmp(jmpbuffer, 5);
    return 0; /*NOTEREACHED*/
}

/*
 * outputter - out...
 */

int out_tag(tag t)
{
  if (debug)
    return fprintf(stderr, "reset: %s", err_tag_name(t));

  return 0;
}

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

#define Max 10

#define Push(x) (sp++, *sp = (x))
#define Pop(n)  (sp -= (n), sp[n])

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
      
      if (Wrefc(n) > 0)
        (void) fprintf(where, "@");	/* hint for weak loops */
      
      switch (Tag(n)) {	/* (tag) cast ensures compler warnings for any tags not covered */
        case zero_t:
          /* Should Never Happen */
          return fprintf(where, "\n!!zero.... "); /* Hd should be NIL; Tl should be rest of feelist of any */
        case free_t: {
          /* Should Never Happen - so do NOT print freelist*/
          return fprintf(where, "\n!!free.... "); /* Hd should be NIL; Tl should be rest of feelist of any */
        }
        case deleting_t: {
          /* Should Never Happen - so do NOT print freelist*/
          return fprintf(where, "\n!!deleting.... "); /* Hd should be NIL; Tl should be rest of feelist of any */
        }
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
            (void) out_out(where, Tl(Pop(1)));
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
            (void) out_out(where, Tl(Pop(1)));
          }
          
          return fprintf(where, ")");
          
        case name_t:
          return fprintf(where, "%s",Name(n));/* todo handle names */
        case abstract_defs_t:
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
          return fprintf(where, "U+");/* "U" to be different binary plus */
        case unary_minus_op:
          return fprintf(where, "U-"); /* "U" to be different binary minus */
          
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
          return err_store("invalid tag: _LastTag");
      }
    }
  return 0; /*NOTEREACHED*/
}

pointer out(pointer n)
{
  budget = -1; /* unlimited output */

  (void) out_out(stdout, n);
  (void) printf("\n");
  return n;
}

#define Limit 64  /* arbitrary limit to output */
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
  (void) fprintf(stderr, "\n");
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
  helper function
*/

/* list_length list
   len () = 0
   len (a:x) = 1 + len x

   NO checks made for infinite (looping) list (todo could return "infinity"?
*/

int list_length(pointer p)
{
  if (IsNil(p))
    return 0;
  
  Assert(IsCons(p));

  return 1 + list_length(Tl(p));
}

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

