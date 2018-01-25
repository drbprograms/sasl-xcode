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

static int getenv_bool(char *var)
{
  int res = getenv("no_reset") ? 1 : 0;
  if (res && debug)
    fprintf(stderr, "%s\n", var);
  return res;
}

static int main_init()
{
  debug 		= getenv_int("debug", 2);	/* default on and high */
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


/*
 * main
 */


int main(int argc, char **argv)
{
  int err;
  int resetting = 0; /* loop avoidance in longjmp() */
  
  if(debug)
    fprintf(stderr, "sizeof(node) %lu\n", sizeof(node));

  (void) main_init();
  
  (void) fprintf(stderr, "hello from %s\n", argv[0]);

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
    for (i=1; i<=err; i++) /* indicate discreetely which jump occurred */
      fprintf(stderr, ". ");
    }
    resetting = 0;
  }

  while (1) {
    root = parse();
    if (IsSet(root)) {
      
      if (debug) {
	out_debug(root);
	(void) reduce_log_report(stderr);
	fprintf(stderr, " ==>\n");
      }

      if (IsDef(root)) {
	defs = root; root = NIL; /*move*/
      } else {
	reduce_print(root);
	printf("\n");
	refc_delete(&root);
      }
      
      if (debug)
	(void) reduce_log_report(stderr);
    }      
    fprintf(stderr, "\nwhat next?\n");
  } 
  return(0);
}
