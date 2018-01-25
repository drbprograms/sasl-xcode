/*
 * abstract... - handle name bindings and generate combinators
 */

#include <stdio.h>
#include <string.h>
#include "common.h"

#include "store.h"

#include "reduce.h"

/* helper functions to deal with SASL repeated formals eg "same x x = TRUE; same x y = FALSE" */
/* count occurences of name in p which are NOT of form (MATCH anything) */

#define Matched(p) (IsSet(p) && IsSet(Hd(p)) && (Tag(Hd(p)) == MATCH_comb))


/* 
   compile(formal1, formal2, ….formaln E) →

  (MATCH test-for-formal1) compile(formal2, …. [formal1] E) →
    (MATCH test-for-formal1 [formal1]E) 
      (MATCH test-for-formal2 [formal2][formal1]E)
         … (MATCH test-for-formaln […][formaln]E)
*/



/* looks for name in p 
   return 1 if name is present, otherwise 0
*/
static int got_name(pointer name, pointer p)
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
  [Turner 1979]
  combinator
  U f (x:y) = f x y

  [x : y] E to mean U ([x] ([y] E)


  "E1 WHERE a:b = E2"
  compiles to
  ([a:b] E1) E2
  =>
  U ([a] ([b] E1)) E2

  "DEF f (x,y,z) = E"
  compiles to - definition of ","
  DEF f = [x, y, z] E
  <= [x:(y:(z:NIL))] E
  <= (U [z] (U [y] (U [z] (K+ E))))

  combinator
  K+ x NIL => x
  K+ x y  => <fail>

  Local recursion

  E1 where x=...x... 

  this is transformed to

  ([x] E1)(Y([x](... x ...)))

  where Y is the fixpoint combinator, defined by the equation Y f=f(Y f)

*/

/* [x] E - helper function, with optimisation - only to be called from reduce_abstract1() below */
/* got - how nmany 'free' instances of name have been found */
/* Delays placing 'cancellator' combinators K, B, C etc */
static pointer reduce_abstract_do(pointer name, pointer n, int *got)
{
  pointer h, t;
  int hgot = 0, tgot = 0;
  int ap = 0;
    
  if (IsNil(n))
    return n;	/* do[x] NIL => NIL */ /*assert((*got)==0)*/

  if (IsName(n) && EqName(name, n)) {
    		/* do[x] x => Ix */
    /* update in place */
    Tag(n) = I_comb;
    Hd(n) = name; /*was refc_copy(name);*/
    Tl(n) = NIL;	/* safety */
    (*got)++;
    return n;
  }

  if (IsApply(n))
    ap = 1;
  else if ( !IsCons(n)) 
    return n;	/* do[x] other => other */ /*assert((*got)==0)*/
  
  /*new update(n, reduce_abstract_do(name, H, &hgot), reduce_abstract_do(name, Tl, &tgot))*/
  h = reduce_abstract_do(name, Hd(n), &hgot);  
  t = reduce_abstract_do(name, Tl(n), &tgot);      
  *got += hgot + tgot;

  if (*got && !ap)
    Tag(n) = apply_t; /* we are going apply a combinator */
  
  /*new update(n, new_apply(new_comb(ap?S_comb:Sc_comb), H), T) */


  if (hgot) {
    if (tgot) {	/* do[x] f g			=> S f g x	=> (f x) (g x)*/
      		/* do[x] f:g			=> Sc f g x	=> (f x):(g x)*/
      Hd(n) = new_apply(new_comb(ap ? S_comb : Sc_comb), h);
    } else {	/* do[x] f g0	=> S f (K g)	=> C f g x	=> f x g */
      Hd(n) = new_apply(new_comb(ap ? C_comb : Cc_comb), h);
    }
  } else {
    if (tgot) {	/* do[x] f0 g	=> S (K f) g	=> B f g x	=> f (g x) */
      		/* todo
		   do[x] f0 x	=> S (K f) I	=> x */
      Hd(n) = new_apply(new_comb(ap ? B_comb : Bc_comb), h);
    } else { 	/* do[x] f0 g0	=> S (K f)(K g)	=> f g */
      Hd(n) = h;/* nop */
    }
  }
  Tl(n) = t;

  return n;
}


/* abstract [name] exp - a single name */
/* if "r then recursive abstract Y ... */
static pointer reduce_abstract1(pointer name, pointer exp, int r)
{
  int got = 0;

  if (debug)
    fprintf(stderr, "%s[%s] ", (r?"recursive_abstract1":"abstract1"), IsNil(name)?"NIL":Name(name)); out_debug(exp);

  exp = reduce_abstract_do(name, exp, &got);      /* [x] exp */

  if (got == 0)
    exp = new_apply(new_comb(K_comb), exp);
  
  if (r)
    exp = new_apply(new_comb(Y_comb), exp);
  
#ifdef possiblytooclever
  if (r == 0) {
    /* Let */
    if (got == 0)
      exp = new_apply(new_comb(K_comb), exp);
  }  else {
    /* Letrec */
    /* Note: Y K exp == exp, so only add Y_comb when recursive use of name is 'got' */
    if (got > 0)
      exp = new_apply(new_comb(Y_comb), exp);
  }
#endif
  
  if (debug)
    fprintf(stderr, "%s[%s] --> ", (r?"recursive_abstract1":"abstract1"), IsNil(name)?"NIL":Name(name)); out_debug(exp);
    
  return exp;
}

/* abstract a name or pattern (a possibly recursive list of names) from an exp */
/* if r then recursive abstract Y ... */
/* [pattern1] ([pattern2] ([pattern3] .... exp))*/


pointer reduce_abstract(pointer pattern, pointer exp, int r)
{
  /* [const] E => MATCH const E */
  /* [NIL]   E => MATCH NIL   E */
  /* [x ... x ...] E => [ ... x ...] MATCH (eq x) E */
  /* [x : ... x ...] E => [ ... x ...] MATCH (eq x) E */  /* xxx TO DO TO DO */

  if (debug > 1) {
    fprintf(stderr, "%s[", (r?"recursive_abstract":"abstract"));
    out_debug1(pattern);
    fprintf(stderr, "] ");
    out_debug(exp);
  }

  if (IsNil(pattern) || IsConst(pattern)) {
    exp = new_apply(new_apply(new_comb(MATCH_comb),
			      new_apply(new_comb(equal_op), pattern)), /*was refc_copy(pattern)*/
		    exp);
  } else if (IsName(pattern)) {
    /* [name] E => abstract1(name, E) */
    exp = reduce_abstract1(pattern, exp, 0); /* nb bug was 1; todo remove "r" parameter from abstract1() */
  } else if (IsApply(pattern)) {
    /* [a ... a...] E => [... a ...] MATCH (eq x) E */
    /* [a b] E => [a] ([b] E) */
    if (IsName(Hd(pattern)) && got_name(Hd(pattern), Tl(pattern))) {
      exp = new_apply(new_apply(new_comb(MATCH_comb),
				new_apply(new_comb(equal_op), Hd(pattern))),
		      exp);
      exp = reduce_abstract(T(pattern), exp, 0/*nb*/);
    } else
      exp = reduce_abstract(H(pattern),
			    reduce_abstract(T(pattern), exp, 0/*nb*/), 0/*nb*/);
  } else if (IsNil(Tl(pattern))) {
    Assert(IsCons(pattern));
    /* [x:NIL] E => U ([x] (K_nil E)) */	
    exp = new_apply(new_comb(U_comb),
		    reduce_abstract(Hd(pattern),
				    new_apply(new_comb(K_nil_comb), exp),
				    0/*nb*/));
  }
  else {
    /* [x:y] E => U ([x] ([y] E)) */

    /* TO DO TO DO */
    /*
      [x : ... x ...] E => ??? ???
     */
    
    exp = new_apply(new_comb(U_comb),
		    reduce_abstract(Hd(pattern),
				    reduce_abstract(Tl(pattern), exp, 0/*nb*/),
				    0/*nb*/));
  } 
  
  if (r)
    exp = new_apply(new_comb(Y_comb), exp);
  
  if (debug) {
    fprintf(stderr, "%s[", (r?"recursive_abstract":"abstract"));
    out_debug1(pattern);
    fprintf(stderr, "] --> ");
    out_debug(exp);
  }
  
  return exp;
}

