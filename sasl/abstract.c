/*
 * abstract... - handle name bindings and generate combinators
 */

#include <stdio.h>
#include <string.h>
#include "common.h"

#include "abstract.h"

#include "store.h"

#include "reduce.h"

/* meta-rule */

/* [any] ([something] E) ==> [any] E' ... where E' = reduce_abstract(something, E) */
pointer abstract_meta_rule(pointer n)
{
  pointer new;
  
  if (!IsAbstract(n))
    return n; /* nothing needed here */
  
  /* this never happens */
  if (!partial_compile)
    err_make2("unexpected abstraction: %s", err_tag_name(Tag(n)));
  
  /* innermost first for abstraction - could be made less strict but alpha-conversion is an implementation issue (todo) */
  Debug1("abstract_meta_rule: %s:", err_tag_name(Tag(n))); out_debug(n);

  new =  reduce_abstract(refc_copy(H(n)), refc_copy(T(n)), Tag(n));

  Debug1("abstract_meta_rule: %s --> ", err_tag_name(Tag(n))); out_debug(new);

  refc_delete(&n);
  /* this refc stuff is all too complicated (todo) */
  
  return new;
}
//  ToDo - implement alpha-conversion to stop meta-abastraction being strict
// 2020-03-17
/* [any] ([... any ...] E) ==> ([... any ...] E)  otherwise [ ... others ... ] ([any] E)  */

//  /* abstract over an "inner" abstraction - taking care to avoid aliasing "inner" names */
//  if (IsAbstract(n)) { // nb abstract is not an atom!
//    if (got_name(name, H(n)))
//      return n; /* do nothing */
//    else
//      Tl(n) = reduce_abstract_do(name, Tl(n), &tgot);
//  } else

/* end meta-rules */


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
    Debug3("%s[%s] (got==%d) ", "reduce_abstract_do", IsNil(name)?"NIL":Name(name), *got);
    out_debug(n);
  }
  
  if (IsNil(n))
    return n;	/* do[x] NIL => NIL */ /*assert((*got)==0)*/

  n = abstract_meta_rule(n); /* nested abstractions */

  if (is_same_name(name, n)) {
    /* combinator "labelling" do[x] x => Ix */
    /* was - update in place */
    (*got)++;
    return new_comb_label(I_comb, n);
  }

  if (IsAtom(n)) {
    /* do[x] simple ==> simple */
    return n;
  } else {
    /*new update(n, reduce_abstract_do(name, H, &hgot), reduce_abstract_do(name, Tl, &tgot))*/
    Hd(n) = reduce_abstract_do(name, Hd(n), &hgot); //stack: push H; push name;new_int(hgot); reduce_abstract_do
    Tl(n) = reduce_abstract_do(name, Tl(n), &tgot);
  }
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
//2020-04-25      Hd(n) = new_apply(new_comb(ap ? S_comb : Sc_comb), Hd(n));  //stack: new_comb(S_comb||Sc_comb);push H; make_apply
      Hd(n) = new_apply(new_comb_label(ap ? S_comb : Sc_comb, refc_copy(name)), Hd(n));  //stack: new_comb(S_comb||Sc_comb);push H; make_apply
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


/* abstract1 [name] exp - worker function to abstract a single name (or NIL) */

/* [x] E => K abstract_do(x, E)   || if x is *not* present in E (got == 1)*/
/* [x] E =>   abstract_do(x, E)   || otherwise */
/* deprecated: if "r then recursive abstract Y ... */
static pointer reduce_abstract1(pointer name, pointer exp)
{
  int got = 0; /* counts how many occurences of name in exp */
  Assert(IsName(name) || IsNil(name));
  
  if (debug /*> 1 better to see actual abstractiosn rather than mechanations of reduce_abstract_do() */)
    Debug2("%s[%s]\t", "abstract1", IsNil(name)?"NIL":Name(name)); out_debug(exp);

  exp = reduce_abstract_do(name, exp, &got);

#ifndef possiblytooclever
  if (got == 0)
    exp = new_apply(new_comb_label(K_comb, refc_copy(name)), exp);
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
  
  if (debug /*> 1*/)
    Debug2("%s[%s]-->\t", "abstract1", IsNil(name)?"NIL":Name(name)); out_debug(exp);
    
  return exp;
}

/* top level abstraction function */
/* abstract a name or pattern (a possibly recursive list of names) from an exp */
/* if r then recursive abstract Y ... */

/* [const] E   => MATCH const E */
/* [NIL]   E   => MATCH NIL   E */
/* [name] E    => abstract1(name, E) */
/* [MATCH n] E => MATCH n E */
/* [x:NIL] E   => MATCH_tag CONS U ([x] (K_nil E)) */
/* [x:y] E     => MATCH_tag CONS U ([x] ([y] E)) */
/* [a b] E     => [a] ([b] E) */

/* meta-rules */
/* [any] ([other] E) => [other] ([any'] E)  where any' = "any - namesof(other)"  - avoid alpha-rule renaming

/* "pattern" is deleted before returning; so every recursive use of pattern needs to be "refc_copy(pattern)" */

pointer reduce_abstract(pointer pattern, pointer exp, tag t)
{
  Debug1("%s[", err_tag_name(t)); out_debug1(pattern); Debug("] ");
  out_debug(exp);

  Assert(IsAbstractTag(t));
  Assert(!IsComb(pattern));

  exp = abstract_meta_rule(exp); /* nested abstractions */

  if (t == abstract_where_t) {
    /* only apply "Y" at outermost level, so "downgrade" t then wrap Y round the result */
    return new_apply(new_comb(Y_comb),
                     reduce_abstract(pattern, exp, abstract_defs_t)
                     ); /* "pattern" already deleted in inner call */
  } else if (IsNil(pattern) || IsConst(pattern)) {
    Assert(t == abstract_formals_t);/*temporary xxx*/
    /* [const] E => MATCH const E */
    /* [NIL]   E => MATCH NIL   E */
    exp = new_apply3(new_comb(MATCH_comb), refc_copy(pattern), exp);
  } else if (IsName(pattern)) {
    /* [name] E => abstract1(name, E) */
    exp = reduce_abstract1(pattern, exp);
  } else if (IsMatchName(pattern)) {
    Assert(t == abstract_formals_t);/*temporary xxx*/
    /* [MATCH name] E => MATCH name E */
    exp = new_apply(refc_copy(pattern), exp);
  } else {
    Assert(IsStruct(pattern));
    if (IsApply(pattern)) {
      /* [a b] E => [a] ([b] E) */
      exp = reduce_abstract(refc_copy(Hd(pattern)),
                            reduce_abstract(refc_copy(Tl(pattern)), exp, t),
                            t);
    } else {
      pointer comb;
      Assert(IsCons(pattern));

      if (IsName(Hd(pattern)) && (t != abstract_condexp_t)) {
        comb = new_comb_label(U_comb, refc_copy(Hd(pattern)));
      }
      else
        comb = new_comb(U_comb);

      if (IsNil(Tl(pattern))) {
        /* [x:NIL] E => U ([x] (K_nil E)) */
        //        exp = new_apply(new_comb(U_comb),
        //        exp = new_apply(IsName(Hd(pattern)) ? new_comb_label(U_comb,  refc_copy(Hd(pattern))) : new_comb(U_comb),
        exp = new_apply(comb,
                        reduce_abstract(refc_copy(Hd(pattern)),
                                        new_apply(new_comb(K_nil_comb), exp),
                                        t));
      } else {
        /* [x:y] E => U ([x] ([y] E)) */
        //        exp = new_apply(new_comb(U_comb),
        //        exp = new_apply(IsName(Hd(pattern)) ? new_comb_label(U_comb,  refc_copy(Hd(pattern))) : new_comb(U_comb),
        exp = new_apply(comb,
                        reduce_abstract(refc_copy(Hd(pattern)),
                                        reduce_abstract(refc_copy(Tl(pattern)), exp, t),
                                        t));
      }
      
      if (t == abstract_formals_t) /* match a list (cons_t) argument */
        exp = new_apply3(new_comb(MATCH_TAG_comb), new_cons(NIL,NIL), exp);
    }
  }
  
  Debug1("%s[", err_tag_name(t)); out_debug1(pattern); Debug("] --> "); out_debug(exp);
  
  refc_delete(&pattern);

  return exp;
}

