#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

#include "common.h"
#include "lex.h"
#include "parse.h"
#include "reduce.h"
#include "store.h"

/*
 * graph globals
 */
const pointer NIL = {(node *)0,0};
pointer root = {(node *)0,0};
pointer defs = {(node *)0,0};
pointer builtin = {(node *)0,0};

/* 
 * defaults and environment variables
 */
int partial_compile;
int reduce_optimise;
int debug;
int no_code;
int no_prelude = 0;

int unit_test;

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

/* parse SASL programs contained in a file */
static int read_file(char *filename)
{
  pointer p;

  lex_do_get(filename);

  for (p = parse(); IsSet(p); p = parse()) {
    
    if (debug) {
      fprintf(stderr, "program ==> "); out_debug(IsSet(root) ? root : defs);/*todo be more selective printing *new* defs*/
      (void) reduce_log_report(stderr);
    }
    
    if (IsSet(root)) {
      root = reduce_print(&root);
      printf("\n"); /* in deference to Unix */
      refc_delete(&root);
    }
    
    if (debug)
      (void) reduce_log_report(stderr);
  }
  
  return 0;
}

/* startup */
static int main_init()
{
  debug 		    = getenv_int("debug", 1);	    /* default on and low */
  partial_compile 	= getenv_int("partial_compile", 0); /* default off */
  reduce_optimise 	= getenv_int("reduce_optimise", 0); /* default off */
  no_code		    = getenv_int("no_code", 0);     /* default off */
  no_prelude        = getenv_int("no_prelude", 0);  /* default prelude unless "-p" in argv[] */
  no_reset		    = getenv_int("no_reset", 1);    /* default off - todo change for interactive use */
  
  unit_test         = getenv_int("unit_test", 0);   /* default off */

 return store_init() || reduce_init();
}

/* closedown */
static int main_done()
{
  store_done();
  (void) reduce_log_report(stderr);
  return 0;
}

/* *** test *** test *** test *** test *** test *** test *** test *** test */

#define N(n) new_name(n)
#define C(x,y) new_cons(x,y)
#define I(i) new_int(i)
#define D(x,i) C(N(x)),I(i))

void unit_test_do002(void)
{
  extern pointer make_flatten(pointer p);
  root = new_def(N("test defs"), NIL);
  
  root = add_to_def(root, N("a"), I(1));
//  root = add_to_def(root, N("b"), I(22));
//  root = add_to_def(root, N("c"), I(333));
  out(root);

  out(make_flatten(DefDefs(root)));
  return;
}


void unit_test_do/*001*/(void)
{
#define DF(x) {pointer *dp = def_for(root, N(x)); printf("%s = ", (x)); if (dp) out(*dp); else printf("<<undefined>>\n");}
  root = new_def(N("test defs"), NIL);
  
  root = add_to_def(root, N("a"), I(1));
  root = add_to_def(root, N("b"), I(22));
  root = add_to_def(root, N("c"), I(333));
  out(root);

  DF("b");
  DF("xxx");
  
  root = add_to_def(root, C(N("h"), C(N("t"), NIL)), C(I(111),C(I(222), NIL)));
  out(root);

  DF("c");
  DF("h");
  DF("yyy");
  DF("t");

  return;
}
/* *** test *** test *** test *** test *** test *** test *** test *** test */

/*
 * main
 */


int main(int argc, char **argv)
{
  int err;
  int resetting = 0; /* loop avoidance in longjmp() */
  int i;
 
  if(debug > 1) {
    fprintf(stderr, "sizeof(node) %lu\n", sizeof(node));
    fprintf(stderr, "_LastTag %d\n", _LastTag);
    fprintf(stderr, "_TagCount %d\n", _LastTag);
  }

  (void) main_init();
  
  /*
   * error handling
   */
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
  
  /* read file args, or else stdin */

  /* process command line: sasl [-p] [file..] */
  
  /* option: -p  suprsess prelude - overrides no_prelude environment variable */
  i = 1;
  if (argc > 1 && ! strcmp(argv[1], "-p")) {
    no_prelude = 1;
    i++;
  }
  
  if (! no_prelude) {
    /* read prelude file first */
    read_file("prelude");
  }
  
  if (unit_test) {
    unit_test_do();
  } else
    if (argc > i) {
      /* read file... */
      for (/**/; argc > i; i++)
        read_file(argv[i]);
    } else {
      /* no files - interactive */
      (void) fprintf(stderr, "hello from %s\n", argv[0]);
      read_file("/dev/stdin"); /*todo not quite right - need to say what next after ever program */
      fprintf(stderr, "what next?\n");
    }
  
  /*
   * all done - wrap up
   */
  main_done();
  
  if (debug)
    (void) reduce_final_report(stderr);
  
  return(0);
}
