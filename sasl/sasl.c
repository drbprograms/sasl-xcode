#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

#include "common.h"
#include "parse.h"
#include "reduce.h"
#include "store.h"

/*
 * graph globals
 */
const pointer NIL = {(node *)0,0};
pointer root = {(node *)0,0};
pointer defs = {(node *)0,0};

/* 
 * defaults and environment variables
 */
int partial_compile;
int reduce_optimise;
int debug;
int no_code;

static int no_reset;
/*
 * getenv_int - get int value from named environment variable
 * 	var - environment variable name
 *	def - default value to return if Var is not set, or not an integer
 * returns value of var as int, or else 0
 */
static int getenv_int(char *var, int def)
{
  int res;
  char *val = getenv(var);
  if (!val || sscanf(val, "%d", &res) != 1)
    res = def;
  if (debug)
    fprintf(stderr, "%s=%d\n", var, res);
  return res;
}
#ifdef getnenv_bool
/*UNUSED*/
static int getenv_bool(char *var)
{
  int res = getenv("no_reset") ? 1 : 0;
  if (res && debug)
    fprintf(stderr, "%s\n", var);
  return res;
}
#endif

static int main_init()
{
  debug 		= getenv_int("debug", 1);	/* default on and low */
  partial_compile 	= getenv_int("partial_compile", 0); /* default off */
  reduce_optimise 	= getenv_int("reduce_optimise", 0); /* default off */
  no_code		= getenv_int("no_code", 0); /* default off */

  no_reset		= getenv_int("no_reset", 1); /* default off - todo change for interactive use */

  if (IsSet(root))
    refc_delete(&root);
  else
    root = NIL;
  
  if (IsSet(defs))
    refc_delete(&defs);
  else
    defs = NIL;
  
  return 0;
}

/* *** test *** test *** test *** test *** test *** test *** test *** test */
/* *** test *** test *** test *** test *** test *** test *** test *** test */

/*
 * main
 */


int main(int argc, char **argv)
{
  int err;
  int resetting = 0; /* loop avoidance in longjmp() */
  pointer p;

  (void) main_init();
  
  if(debug) {
    fprintf(stderr, "sizeof(node) %lu\n", sizeof(node));
    fprintf(stderr, "_LastTag %d\n", _LastTag);
    fprintf(stderr, "_TagCount %d\n", _LastTag);
  }

  (void) fprintf(stderr, "hello from %s\n", argv[0]);

#ifdef unitest
  /* *** test *** test *** test *** test *** test *** test *** test *** test */
#define name(n) new_name(n)
#define cons(h,t) new_cons((h),(t))
#define ap(h,t) new_apply((h),(t))

#define def(n, expr) ap(name(n), name(expr))

#define add_def(name, expr, list) \
    cons(cons(name, H(list)), \
         cons(expr, T(list)))
    
#define list1 add_def(name("firstname"), name("firstdef"),cons(NIL,NIL))
#define list2 add_def(name("2name"), name("2def"),list1)
#define list3 add_def(name("firstname"), name("redfined"),cons(NIL,NIL))

  
#define x name("x")
#define y name("y")
#define z name("z")
#define yx cons(cons(y,x),NIL)
#define zyx cons(z,yx)
#define new1 NIL
#define old ap(yx,zyx)
#ifdef test_de_dup
  extern pointer de_dup(pointer n, pointer o);
#define test(n,o) {out_debug1(n);out_debug1(o);out_debug(cons(n,de_dup(n,o)));}
#endif
  
#define test(new, old)  {out_debug(new);out_debug(old);out_debug(reduce(new));}
  
#define Itest ap(new_comb(I_comb), NIL)
  
  {
    extern pointer make_defs(pointer n, pointer o);
    fprintf(stderr, "*test start*\n");
    test(Itest, NIL);
    fprintf(stderr, "*test done*\n");
  }
  /* *** test *** test *** test *** test *** test *** test *** test *** test */
#endif
  
  err = setjmp(jmpbuffer);
  if (err != 0) {
    /* arrived here from a longjmp() call */

    /* todo print more useful diagnostics and skip to sensible place eg token in col 1? ie offside=0? */
    int i;

    if (no_reset)
      exit(1);
    
    if (!resetting) {
      resetting++;
      (void) main_init();
      (void) parse_reset();
      if (debug)
        (void) reduce_log_report(stderr);
      
      fprintf(stderr, "reset ");
      for (i=1; i<=err; i++) /* indicate discreetely which reset occurred */
        fprintf(stderr, ". ");
    }
    resetting = 0;
  }
  
  for (p = parse_program(); IsSet(p); p = parse_program()) {

    if (IsSet(p)) {
      
      if (debug) {
        fprintf(stderr, "program ==> "); out_debug(IsSet(root) ? root : defs);/*todo be more selective printing *new* defs*/
        (void) reduce_log_report(stderr);
      }
      
      if (IsSet(root)) {
        root = reduce_print(root);
        printf("\n"); /* in deference to Unix */
        refc_delete(&root);
      }
      
      if (debug)
        (void) reduce_log_report(stderr);
    }
    fprintf(stderr, "what next?\n");
  }
  return(0);
}
