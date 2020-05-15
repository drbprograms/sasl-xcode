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
int logging;
int no_code;
int Y_loop;
int prelude = 0;
int check;
int loop_check;

int unit_test;
int mem_dump;

static int no_reset;

int weak_path;
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

  Debug2("%s=%d\n", var, res);
  return res;
}
#ifdef getnenv_bool
/*UNUSED*/
static int getenv_bool(char *var)
{
  int res = getenv("no_reset") ? 1 : 0;
  if (res && debug)
    Debug("%s\n", var);
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
      Debug("program ==> "); out_debug(IsSet(root) ? root : defs);/*todo be more selective printing *new* defs*/
    }
    (void) reduce_log_report(stderr);

    if (IsSet(root)) {
      root = reduce_print(&root);
      User("\n"); /* in deference to Unix */
      
      Log("Start extra\n"); (void) reduce_log_report(stderr); Log("End extra\n");

      refc_delete(&root);
    }
    
    (void) reduce_log_report(stderr);
  }
  
  return 0;
}

/* startup */
static int main_init()
{
  debug 		    = getenv_int("debug", 1);	        /* default on and low */
  partial_compile 	= getenv_int("partial_compile", 0); /* default off */
  reduce_optimise 	= getenv_int("reduce_optimise", 0); /* default off */
  no_code		    = getenv_int("no_code", 0);         /* default off */
  prelude           = getenv_int("prelude", 1);         /* default prelude - "-p" in argv[] overrides */
  no_reset          = getenv_int("no_reset", 1);        /* default off - todo change for interactive use */

  /* deep debugging */
  unit_test         = getenv_int("unit_test", 0);       /* default off */
  logging           = getenv_int("logging", 1);         /* default on */
  mem_dump          = getenv_int("mem_dump", 0);        /* default off */
  check             = getenv_int("check", 1);           /* default ON - should always be 1 to validate refc algorithm */
  Y_loop            = getenv_int("Y_loop", 1);          /* default 1 - "knot-tying" implementation of Y combinator */
  loop_check        = getenv_int("loop_check", 1);      /* default ON xxx could be off when tests fininshed? */

  /* deep changes */
  weak_path         = getenv_int("weak_path", 1);       /* default on = examine weakness of pointers when copying *//*XXX to be implemented as switchable */

 return store_init() || tag_init() || reduce_init();
}

/* closedown */
static int main_done()
{
  store_done();
  reduce_log_report(stderr);
  reduce_final_report(stderr);

  return 0;
}

/* *** test *** test *** test *** test *** test *** test *** test *** test */

#define N(n) new_name(n)
#define C(x,y) new_cons(x,y)
#define I(i) new_int(i)
#define D(x,i) C(N(x)),I(i))

#ifdef deprecated
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
#endif

void unit_test_do(void)
{
  /* nothing */
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
    Debug1("sizeof(node) %lu\n", sizeof(node));
    Debug1("_TagCount %d\n", TagCount);
  }

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
      (void) reduce_log_report(stderr);
      
      Error("reset ");
      for (i=1; i<=err; i++) /* indicate discreetely which reset occurred */
        Error(". ");
    }
    resetting = 0;
  }
  
  /* initialise */
  (void) main_init(); //todo check return value
    
  /* read file args, or else stdin */

  /* process command line: sasl [-p] [file..] */
  
  /* option: -p  supress prelude - overrides prelude environment variable */
  i = 1;
  if (argc > 1 && ! strcmp(argv[1], "-p")) {
    prelude = 0;
    i++;
  }
  
  if (prelude) {
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
      Error1("hello from %s\n", argv[0]);
      read_file(NULL); /* defaults to stdin */
      Error("what next?\n");
    }
  
  /*
   * all done - wrap up
   */
  main_done();
  
  return(0);
}
