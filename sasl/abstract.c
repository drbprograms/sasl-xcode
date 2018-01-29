/*
 * abstract... - handle name bindings and generate combinators
 */

#include <stdio.h>
#include <string.h>
#include "common.h"

#include "store.h"

#include "reduce.h"

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
/* this is the engine room of the abstrsaction process */
/* got - how nmany 'free' instances of name have been found */
/* Delays placing 'cancellator' combinators K, B, C etc */

/* Abstraction rules */
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


/* implementation: */

/* do[x] x => I x */
/* do[x] simple ==> simple */

/* do[x] f g      => S f g x                {=> (f x) (g x)} */
/* do[x] f:g      => Sc f g x               {=> (f x):(g x)} */

/* do[x] f g0     => S f (K g)  => C f g x  {=> f x g} */
/* do[x] f:g0     => Sp f (K g) => Cp f g x {=> f x:g} */

/* do[x] f0 g     => S (K f) g  => B f g x  {=> f (g x)} */
/* do[x] f0 g     => S (K f) g  => Bp f g x {=> f:(g x)} */

/* do[x] f0 g0    => f0 g0 (got==0) */
/* do[x] f0:g0    => f0:g0 (got==0) */

static pointer reduce_abstract_do(pointer name, pointer n, int *got)
{
  int hgot = 0, tgot = 0; /* counts how many occurences of x in Hd(n), Tl(n) */
  int ap; /* IsApply(n) - used to determine S vs Sp etc */
  
  if (debug > 1) {
    fprintf(stderr, "%s[%s] (got==%d)", "recursive_abstract_do", IsNil(name)?"NIL":Name(name), *got);
    out_debug(n);
  }
  
  if (IsNil(n))
    return n;	/* do[x] NIL => NIL */ /*assert((*got)==0)*/
  
  if (IsSameName(name, n))   {
    /* do[x] x => I x */
    (*got)++;
    return new_apply(new_comb(I_comb), n);
  }
  
  if (IsAtom(n)) {
    /* do[x] simple ==> simple */
    return n;
  }
  
  /*new update(n, reduce_abstract_do(name, H, &hgot), reduce_abstract_do(name, Tl, &tgot))*/
  Hd(n) = reduce_abstract_do(name, Hd(n), &hgot);
  Tl(n) = reduce_abstract_do(name, Tl(n), &tgot);
  
  *got += hgot + tgot;
  
  if (IsApply(n)) {
    ap = 1;
  } else {
    Assert(IsCons(n));
    ap = 0;
    
    /* if we are about to apply a combinator (got!=0), n must be apply (not cons) */
    if (*got > 0)
      Tag(n) = apply_t;
  }
  
  /*new update(n, new_apply(new_comb(ap?S_comb:Sc_comb), H), T) */
  
  if (hgot) {
    if (tgot) {
      Hd(n) = new_apply(new_comb(ap ? S_comb : Sc_comb), Hd(n));
    } else {
      Hd(n) = new_apply(new_comb(ap ? C_comb : Cc_comb), Hd(n));
    }
  } else {
    if (tgot) {
      /* todo
       do[x] f0 x	=> S (K f) I	=> x */
      Hd(n) = new_apply(new_comb(ap ? B_comb : Bc_comb), Hd(n));
    } else {
      /* do[x] f0 g0	=> S (K f)(K g)	=> f g */
      /* nop */
    }
  }
  
  return n;
}


/* abstract1 [name] exp - worker functions to abstract a single name (or NIL) */

/* [x] E => K abstract_do(x, E)   || if x is *not* present in E (got == 1)*/
/* [x] E =>   abstract_do(x, E)   || otherwise */

/* deprecated: if "r then recursive abstract Y ... */
static pointer reduce_abstract1(pointer name, pointer exp, int r)
{
  int got = 0; /* counts how many occurences of name in exp */
  Assert(IsName(name) || IsNil(name));
  
  if (debug)
    fprintf(stderr, "%s[%s] ", (r?"recursive_abstract1":"abstract1"), IsNil(name)?"NIL":Name(name)); out_debug(exp);

  exp = reduce_abstract_do(name, exp, &got);

#ifndef possiblytooclever
  if (got == 0)
    exp = new_apply(new_comb(K_comb), exp);
  
  if (r)
    exp = new_apply(new_comb(Y_comb), exp);
#else
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

/* top level abstraction function */
/* abstract a name or pattern (a possibly recursive list of names) from an exp */
/* if r then recursive abstract Y ... */

/* [const] E   => MATCH const E */
/* [NIL]   E   => MATCH NIL   E */
/* [name] E    => abstract1(name, E) */
/* [MATCH n] E => MATCH n E */
/* [a b] E     => [a] ([b] E) */
/* [x:NIL] E   => U ([x] (K_nil E)) */
/* [x:y] E     => U ([x] ([y] E)) */

pointer reduce_abstract(pointer pattern, pointer exp, int r)
{
  if (debug > 1) {
    fprintf(stderr, "%s[", (r?"recursive_abstract":"abstract"));
    out_debug1(pattern);
    fprintf(stderr, "] ");
    out_debug(exp);
  }
  
  if (IsNil(pattern) || IsConst(pattern)) {
    /* [const] E => MATCH const E */
    /* [NIL]   E => MATCH NIL   E */
    exp = new_apply(new_apply(new_comb(MATCH_comb), pattern), /*was refc_copy(pattern)*/
                    exp);
  } else if (IsName(pattern)) {
    /* [name] E => abstract1(name, E) */
    exp = reduce_abstract1(pattern, exp, 0); /* nb bug was 1; todo remove "r" parameter from abstract1() */
  } else if (IsMatchName(pattern)) {
    exp = new_apply(pattern, exp);
  } else if (IsApply(pattern)) {
      /* [a b] E => [a] ([b] E) */
      exp = reduce_abstract(Hd(pattern),
                            reduce_abstract(Tl(pattern),
                                            exp, 0/*nb*/), 0/*nb*/);
  } else if (IsNil(Tl(pattern))) {
    Assert(IsCons(pattern));
    /* [x:NIL] E => U ([x] (K_nil E)) */
    exp = new_apply(new_comb(U_comb),
                    reduce_abstract(Hd(pattern),
                                    new_apply(new_comb(K_nil_comb),
                                              exp),
                                    0/*nb*/));
  } else {
    Assert(IsCons(pattern));
    /* [x:y] E => U ([x] ([y] E)) */
    
    /* xxx TO DO TO DO */
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

