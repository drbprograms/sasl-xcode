#include <stdio.h>
#include <stdlib.h> /* for malloc() */
#include <string.h>

#include "common.h"

#include "zone.h"

#include "store.h"

#define Limit 6 /* keep low for brevity */



/* helper function */
/*
 * make_append - adds new cons cell containing tl and NIL terminator to end of non-NIL list; does nothing  if list is NIL
 * || append (a:x) tl = a:append x tl
 * || append () tl    = tl
 * return top of the list
 */
pointer make_append(pointer list, pointer tl)
{
  if (IsNil(list))
    return tl;

  /* was Assert(IsCons(list)); but wrong as that is strict */
  Assert(IsStruct(list));
  T(list) = make_append(T(list), tl);
  return list;
}

/*
 * logging
 */
void store_log(char *s, pointer p)
{
  Log2("%s%s ", s, zone_pointer_info(p));
  out_debug_limit(p, Limit);
}

/*
 * error reporting
 */
static int refc_err(char *msg, pointer p)
{
  if (debug) {
    Log2("refc_err: %s: %s\n", msg, zone_pointer_info(p));
    refc_check();
  }
  return err_refc(msg);
}

void store_log_new(pointer p)
{
  store_log("new", p);
}
/* new node containing a copy of tag and contents of Node(p) */
pointer new_copy(pointer p)
{
  pointer n = new_node(Tag(p));
  Val(n) = Val(p);
  store_log_new(n);
  return n;
}

/* TODO
change all these to add make_xx and redefine new_xx as make_xx(NIL,  ...)

TODO
*/
pointer new_int(int i)
{
  pointer n = new_node(int_t);
  Num(n) = i;
  store_log_new(n);
  return n;
}
pointer new_double(double d)
{
  pointer n = new_node(floating_t);
  Dbl(n) = d;
  store_log_new(n);
  return n;
}
pointer new_char(char c)
{
  pointer n = new_node(char_t);
  Char(n) = c;
  store_log_new(n);
  return n;
}
pointer new_bool(char b)
{
  pointer n = new_node(bool_t);
  Bool(n) = b;
  store_log_new(n);
  return n;
}
pointer new_fail()
{
  pointer n = new_node(fail_t);
  store_log_new(n);
  return n;
}

pointer new_apply(pointer hd, pointer tl)
{
  pointer n = new_node(apply_t);
  Hd(n) = hd;
  Tl(n) = tl;
  store_log_new(n);
  return n;
}

pointer new_apply3(pointer hh, pointer th, pointer t)
{
  return new_apply(new_apply(hh,th), t);
}

pointer new_apply4(pointer hhh, pointer thh, pointer th, pointer t)
{
  return new_apply(new_apply3(hhh, thh, th), t);
}

pointer new_cons(pointer hd, pointer tl)
{
  pointer n = new_node(cons_t);
  Hd(n) = hd;
  Tl(n) = tl;
  store_log_new(n);
  return n;
}

pointer new_abstract(pointer name, pointer def, tag t)
{
  pointer n;
  
  switch (t) {
  case abstract_condexp_t:
  case abstract_formals_t:
  case abstract_defs_t:
    n = new_node(t);
    break;
  default:
      err_refc1("new_abstract: bad tag: ", (int) t);
    /*NOTREACHED*/
  }
  Hd(n) = name;
  Tl(n) = def;
  store_log_new(n);
  return n;
}

/* def: (defname:(listof name|namelist):(listof expr)) */
pointer new_def(pointer name, pointer def)
{
  pointer n = new_node(def_t);
  Hd(n) = name;
  Tl(n) = def;
  store_log_new(n);
  return n;
}

/* add_def - add a name|namelist definition to defs list (may be a simple name or a namelist) */
pointer add_to_def(pointer def, pointer name, pointer expr)
{
  Assert(IsDef(def));
  /*TODO XXX use and test this XXX */
  if (IsNil(DefDefs(def)))
    DefDefs(def) = new_cons(NIL,NIL);
  
  DefNames(def) = new_cons(name, DefNames(def));
  DefExprs(def) = new_cons(expr, DefExprs(def));

  return def;
}


/*
 * add (namelist:def) any                  = || recurse over namelist
 *
 * add (name:newdef)  ((name:y): (def:z))  = (name:y):(newdef:z)      || replace def with newdef
 * add (name:newdef)  ((other:y):(def:z))  = (other:y_add):(def:z_add)
 *                                                   WHERE y_add:z_add = add (name:newdef) (y:z)  || recurse
 * add (name:newdef)  (():())              = (name:()):(newdef:())    || add_deflist_to_def
 * add ()             any                  = any
 *
 * to allow name *or* namelist, need to introduce 'nameMatch'
 * nameMatch (a:b) (a:b) = TRUE
 * nameMatch (a:b) (x:y) = any (member (a:b)) (x:y) -> throw error; FALSE || error case
 * nameMatch name  (x:y) = member name (x:y)
 * nameMatch ()     any  = FALSE
 *
 */


/*
 * def_for2 - lookup name n in (list-of names, list-of defs) - returns 0 if not found, else pointer to expression
 */
pointer *def_for2(pointer names, pointer defs, pointer n)
{
  if (IsNil(names))
    return (pointer *)0;
  
  Assert(IsCons(names));

  if (IsAtom(H(names))) {
    /* name */
    if (IsSameName(H(names), n)) {
      if (debug >1)
        fprintf(stderr, "def_for: \"%s\"\n", Name(n) );
      return &H(defs);
    }
  } else {
    /* namelist */
    pointer *dp = def_for2(H(names), H(defs), n);
    Assert(IsCons(H(names)));
    if (dp)
      return dp;
  }
  
  return def_for2(T(names), T(defs), n);
}

/* look for duplicated of name/namelist - return first matching name, otherwise 0 */
pointer def_any_for2(pointer names, pointer defs, pointer n)
{
  if (IsName(n)) {
    /* name */
    if (def_for2(names, defs, n))
      return n;
  } else {
    /* namelist */
    Assert(IsCons(n));
    for ( /**/; IsSet(n); n = T(n)) {
      pointer dup = def_any_for2(names, defs, H(n));
      if (IsSet(dup))
        return (dup);
    }
  }
  
  return NIL;
}

/*
 * def_for - lookup name n in def - returns 0 if not found, else pointer to expression
 */
pointer *def_for(pointer def, pointer n)
{
  if (IsNil(def))
    return (pointer *)0;
  
  Assert(IsDef(def));
  
  return def_for2(DefNames(def), DefExprs(def) ,n);
}
#ifdef notdef
/* search def for definition of a name, return NIL is if not found */
pointer *def_for0(pointer deflist, pointer name)
{
  pointer n, d;
  
  if (IsNil(deflist))
    return (pointer *)0;
  
  /* deflist == (list-of (name|namelist) . list-of clauses) */
  for (n = H(deflist), d = T(deflist); IsSet(n); n = T(n), d = T(d)) {
    if (IsAtom(H(n))) {
      /* name */
      if (IsSameName(H(n), name)) {
        if (debug >1)
          fprintf(stderr, "def_for: \"%s\"\n", Name(Hd(n)) );
        return &H(d);
      }
      continue; /*loop*/
    } else {
      /* namelist */
      pointer *h, *t;
      Assert(IsCons(H(n))); /* namelist */
      Assert(IsCons(H(d)));

      h = def_for0(HH(d), name);
      if (h)
        return h;
      
      t = def_for0(HH(d), name);
      if (t)
        return t;
    }
  }
  
  return (pointer *)0;
}

pointer def_lookup(pointer def, pointer name, char *msg)
{
  pointer *dp = def_for0(def, name);
  
  if (dp)
    return *dp;
  else
    return NIL;
}
#endif

/* add_deflist_to_deflist - add new defs to old */
pointer add_deflist_to_def(pointer def, pointer deflist)
{
  pointer n, d;
  Assert(0);// XXX XXX XXX
  if (IsNil(def) || IsNil(deflist))
    return def;
  
  Assert(IsDef(def));
  Assert(IsCons(deflist));
  
  for (n = H(deflist), d = T(deflist); IsSet(n); n = T(n), d = T(d)) {
    if (IsAtom(H(n))) {
      /* name */
      pointer *dp = def_for2(H(def), T(def), H(n));
      if (dp)
        refc_delete(dp); /* delete old definition *//* todo - remove old name in deflist - complicated as it may be part of namelist; instead rely on def_for() to always find *most recent* definition */
      def = add_to_def(def, H(n), H(d));
    } else {
      /* namelist */
      def = add_deflist_to_def(def, HH(n));
      def = add_deflist_to_def(def, TH(n));
    }
  }
  return def;
}

/* names - list-of name|namelist, defs = list-of defs, to be added: deflist (list-of name|namelist . list-of defs) */
pointer add_deflist_to_defNEW2(pointer names, pointer defs, pointer deflist)
{
  if (IsNil(deflist))
    return NIL;
 
  
  
  return NIL;
}
pointer add_deflist_to_defNEW(pointer def, pointer deflist)
{
  if (IsNil(def) || IsNil(deflist))
    return def;
  
  Assert(IsDef(def));
  Assert(IsCons(deflist));
  
  
  
  (void) add_deflist_to_defNEW2(DefNames(def), DefExprs(def), deflist);

  return def;
}

pointer new_name(char *s)
{
  pointer n = new_node(name_t);
  
  if (!s)
    (void) err_store("new_name: null string pointer");
  
  Name(n) = (char *)malloc(strlen(s)+1);
  
  if (!Name(n))
    (void) err_store("new_name: malloc out of space");
  
  if (!strcpy(Name(n), s))
    (void) err_store("new_name: strcpy failed");
  
  store_log_new(n);
  return n;
}

pointer new_oper(tag oper)
{
  pointer n = new_node(oper);	/* oper is unchecked */
  store_log_new(n);
  return n;
}

static pointer new_unary(tag t, char *name, pointer (*fun)(pointer p))
{
  pointer n = new_node(t);
  
  Uname(n) = (char *)malloc(strlen(name)+1);
  
  if (!Uname(n))
    (void) err_store("new_unary: malloc out of space");
  
  if (!strcpy(Uname(n), name))
    (void) err_store("new_unary: strcpy failed");

  Ufun(n) = fun;
  
  return n;
}

pointer new_unary_strict(char *name, pointer (*fun)(pointer p))
{
  return new_unary(unary_strict, name, fun);
}

pointer new_unary_nonstrict(char *name, pointer (*fun)(pointer p))
{
  return new_unary(unary_nonstrict, name, fun);
}

/*
 * refc_changeto_TYPE - change anything to given type
 */

/* special tag to indicate cell is being freed - no change to any pointers already there */
static pointer refc_change_to_deleting(pointer p)
{
  Log1("refc_change_to_deleting%s\n", refc_pointer_info(p));

  Assert(HasPointers(p));
  Tag(p) = deleting_t;

  return p;
}

/*
 * storage management - reference counting ref...
 */

#ifdef new
/*xxx incomplete - what is Tag(n)!= t ?*/
pointer make_node(pointer n, tag t)
{
  if (IsNil(n))
    n = new_node(t);

  return n;
}

pointer make_comb_name(pointer n, tag t, pointer name)
{
  n = make_node(n, t);
  Hd(n) = name;
  Tl(n) = NIL; /*WIP is this needed?*/
  store_log_new(n);
  return n;
}

pointer new_comb_name(tag t, pointer name)
{
  return make_comb_name(NIL, t, name);
}

pointer make_apply(pointer n, pointer hd, pointer tl)
{
  make_node(n, apply_t);
  Hd(n) = hd;
  Tl(n) = tl;
  store_log_new(n);
  return n;
}
#else

/* hide the sasl variable name in hd of the combinator */
pointer new_comb_name(tag t, pointer name)
{
  pointer n = new_node(t);
  Hd(n) = name;
  store_log_new(n);
  return n;
}

#endif
/* vanilla version */
pointer new_comb(tag t)
{
  return new_comb_name(t, NIL);
}

/* ... ... */

static int refc_copy_NILcount = 0;	/* NIL pointers copied (not) */
static int refc_copy_Scount = 0;	/* strong pointers copied */
static int refc_copy_Wcount = 0;	/* weak pointers copied */
static int refc_copy_make_cyclic_NILcount = 0;	/* NIL pointers weakened (not) */
static int refc_copy_make_cyclic_Scount = 0; /* strong ponters made weak */
static int refc_copy_make_cyclic_Wcount = 0; /* vv. should be always 0 */

static int refc_delete_flip_count = 0;	/* nodes changed from red to blue (NodeBit inverted) */

static int refc_search_start_count = 0;	/* start pointers made weak by search */
static int refc_search_strong_count = 0;	/* strong pointers made weak by seach */


void refc_search_log(pointer start, pointer p)
{
  /*NB zone_pointer_info() returns pointer to a *fixed string* */
  Log2("refc_search%s%s\t", zone_pointer_info(p), (Node(p) == Node(start) ? "*" : ""));
  Log1("start%s\n", zone_pointer_info(start));
  
  if (Node(p) == Node(start))
    refc_search_start_count++;
  else
    refc_search_strong_count++;
  return;
}

void refc_search_flip_log(pointer start, pointer p)
{
  /*NB zone_pointer_info() returns pointer to a *fixed string* */
  Log2("refc_search_flip%s%s\t", zone_pointer_info(p), (Node(p) == Node(start) ? "$" : ""));
  Log1("start%s\n", zone_pointer_info(start));

  if (Node(p) == Node(start))
    refc_search_start_count++;
  else
    refc_search_strong_count++;
  return;
}

/* run a check as well as logging */
void refc_delete_log(pointer p, unsigned depth)
{
  Log2("refc_delete%s (depth=%u)\n", zone_pointer_info(p), depth);
  return;
}
void refc_delete_flip_log(pointer p, refc_pair ext, unsigned depth)
{
  refc_delete_flip_count++;
  Log5("refc_delete_flip%s (s+/w+) (%u/%u) (depth=%u) %s\n", zone_pointer_info(p), ext.s, ext.w, depth,
       HasRefs(ext) ? "" : " is free");
  return;
}

void refc_delete_post_search_log(pointer p, refc_pair ext, unsigned depth)
{
  Log2("refc_delete_post_search%s (depth=%u)\n", zone_pointer_info(p), depth);

//  not and error of somewhere "below" on the loop has become strong somehow
//  if (HasRefs(ext) && Srefc(p) == 0) {
//    Log4("refc_delete: loop search unexpectedly weak%s (s+/w+) (%u/%u) (depth=%u)\n", zone_pointer_info(p), ext.s, ext.w, depth);
//
//  }
  // not an error if last weak pointers can become strong
//  if (! HasRefs(ext) && Srefc(p) != 0) {
//    Log4("refc_delete: loop search unexpectedly strong%s (s+/w+) (%u/%u) (depth=%u)\n", zone_pointer_info(p), ext.s, ext.w, depth);
//
//  }
  
  return;
}

void refc_delete_post_deleteHd_log(pointer p, refc_pair ext, unsigned depth)
{
  Log4("refc_delete_post_deleteHd%s (s+/w+) (%u/%u) (depth=%u)\n", zone_pointer_info(p), ext.s, ext.w, depth);
  return;
}

/* Log errors */
void refc_delete_post_delete_log(pointer p, refc_pair ext, unsigned depth)
{
  Log2("refc_delete_post_delete%s (depth=%u)\n", zone_pointer_info(p), depth);

  if (ALLrefc(p) > 0) {
    /* (NOT) assume it's freed elsewhere by a recursive application of delete ... */
    Log3("delete: not freed: %s (s+/w+) %u/%u)\n", zone_pointer_info(p), ext.s, ext.w);
  } else if (HasRefs(ext) /*&& IsFree(p)*/) {
    /* there were external pointers but everything is freed!? */
    Log3("delete: unexpectedly freed: %s (s+/w+) %u/%u)\n", zone_pointer_info(p), ext.s, ext.w);
  }

  return;
}

void refc_copyN_log(pointer p, int n)
{
  Log2("refc_copyN%s(N=%d)\n", zone_pointer_info(p), n);
  
  if (IsNil(p))
    refc_copy_NILcount += n;
  else {
    if (IsStrong(p))
      refc_copy_Scount += n;
    else
      refc_copy_Wcount += n;
  }
  return;
}

static void refc_make_copy_log(pointer p, unsigned weak)
{
  Log2("refc_make_copy(weak=%u)%s \n", weak, zone_pointer_info(p));
  
  if (IsNil(p))
    refc_copy_NILcount++;
    else {
      if (IsStrong(p))
        refc_copy_Scount++;
      else
        refc_copy_Wcount++;
    }
}

void refc_copyS_log(pointer p, const char *s)
{
  Log2("refc_copyS%s \"%s\"\n", zone_pointer_info(p), s);
  return;
}

void refc_copy_pointerS_log(pointer p, const char *s)
{
  Log2("refc_copy_pointerS%s \"%s\"\n", zone_pointer_info(p), s);
  return;
}

void refc_copyNth_log(pointer p, unsigned n)
{
  Log2("refc_copy_Nth%s n==%u\n", zone_pointer_info(p), n);
  return;
}

void refc_copy_make_cyclic_log(pointer p)
{
  if (IsNil(p))
    refc_copy_NILcount++;
  else {
    if (IsStrong(p))
      refc_copy_make_cyclic_Scount++;
    else
      refc_copy_make_cyclic_Wcount++;
  }
  return;
}

/*
 * refc_make_weak
 * ensure a pointer is weak (ok if it is weak already)
 */
/* ToDo does this belong in zone.c? */
static void refc_make_weak(pointer *pp)
{
  if (IsSet(*pp) && IsStrong(*pp)) {
    Wrefc(*pp)++;
    Srefc(*pp)--;
    PtrBit(*pp) = !NodeBit(*pp); /* Was: PtrBit(p) = !PtrBit(p); */
  }
  return;
}

/*
  refc_search = helper function for refc_delete
 
  start - start of the search
  pp - current pointer - passed By Reference

  search(start,<R,S>)
  if <R,S> is a strong pointer then
    if S==start then make <R,S> into a weak pointer
  else
    if (Srefc(s) > 1) then make <R,S> into a weak pointer
  else
    search(start, <S,hd(s)>); search(start, <S,tl(S)>)
*/

static void refc_search(pointer start, pointer *pp)
{
  if (IsNil(*pp))
    return;

  if (! HasPointers(*pp))
    return;
  
  refc_search_log(start, *pp);
  
  /* weak pointer to start === BUG  search "/w .... *" */

#if 1/**2018-11-09**/
  if (IsStrong(*pp) && HasPointers(*pp)) {  /* never make weak pointers to constants */
#else
  if (IsStrong(*pp) && HasPointers(*pp) && (! IsComb(*pp))) {  /* never make weak pointers to constants *or combinators* */
#endif
#ifdef notdef
//    if (SameNode(start, *pp) /*|| Srefc(*pp) > 1*/) { the second condition is vital for @(I ... loop ...) */
#endif
      if (SameNode(start, *pp) || Srefc(*pp) > 1) {
      /* make a strong pointer to a non-constant weak, end of search */
      refc_search_flip_log(start, *pp);
      refc_make_weak(pp);
    } else {
      /* strong pointer stays strong: recurse (node contains pointers) */
      /* Assert(Srefc(*pp) == 1); Assert(HasPointers(*pp)); */
      refc_search(start, &Hd(*pp));
      refc_search(start, &Tl(*pp));
    }
  }
#if fix0
  /*XXX xxx todo*/
  else {
    if (ALLrefc(*pp) == Wrefc(*pp)) {
      /* unable to weaken a pointer as they are all weak already ==> deletion is underway at *pp */
      Wrefc(*pp)--;
      *pp = NIL;
      //      refc_search(start, &Hd(*pp));
//      refc_search(start, &Tl(*pp));
    }
  }
#endif

  /* else if p points to constant or is weak, do nothing */
  
  return;
}


/*
 * refc_delete -  DELETE(<R,S>) destroy pointer from from R to S
 *
 * delete pointer - passed By Reference
 *
 * returns NIL
 *
 * usage: refc_delete(&Hd(p));
 */

void refc_delete(pointer *pp)
{
  pointer p = *pp; /* copy */
  refc_pair ext = zero_refc_pair;  /* checking info - used in refc_*.log() functions */
  static unsigned depth = 0;
  
  if (IsNil(*pp))
    return;
  
  depth++;
  refc_delete_log(p, depth);

  *pp = NIL;  /* really delete the pointer in situ */

  /* adjust reference counts, free the node if appropriate */
  if (IsWeak(p)) {
    if (Wrefc(p) == 0) {
      (void) err_refc("delete: pointer is weak but Wrefc==0");
      return; /*NOTREACHED*/
    }
    
    Wrefc(p)--;
  }
  else {
    if (Srefc(p) == 0) {
      (void) err_refc("delete: pointer is strong but Srefc==0");
      return; /*NOTREACHED*/
    }
    
    Srefc(p)--;
  }

  if (IsFree(p) && ALLrefc(p) > 0)/*2018-11-11*/
    return; /* important - don't chase our tail - fix:2018-10-12 */

  if (Srefc(p) == 0) {
    
    if (Wrefc(p) == 0) { /* not in a loop */
      if (HasPointers(p) && ! IsDeleting(p)){
        refc_change_to_deleting(p);
        refc_delete(&Hd(p));
        refc_delete(&Tl(p));
      }
      
      /* assert(ALLRefc(p) == 0) after this deletion */
      refc_delete_post_delete_log(p, ext, depth);

      free_node(p); /* pointed-to node is free for re-use */
    }
    else { /* is in a loop */
      /* Assert(Srefc(p) == 0 && Wrefc(p) > 0) */
      /*TODO
       * finalise the "rules" for aomts and stuct in refc operations
       * N.b. when copying apointers
       */
      
      if (! HasPointers(p)) {
          refc_err("constant has only weak references", p);
        /*NOTREACHED*/
      }
#ifdef notdef /*WIP WIP*/
      if (IsStruct(p)) {
#endif
        Assert(HasPointers(p));
      ext = zone_check_island(p);
      
      /* invert pointers - by changing pointed-to node */
      NodeBit(p) = !NodeBit(p);	/* "an essential implementation trick" */
      Srefc(p) = Wrefc(p);	/* !!! was this done correctly in 1985? */
      Wrefc(p) = 0;		/* !!! was this done correctly in 1985? */

      refc_delete_flip_log(p, ext, depth);

      /* recursive search */
      refc_search(p, &Hd(p));
      refc_search(p, &Tl(p));
      
      refc_delete_post_search_log(p, ext, depth);
      
      /* Srefc(p) and/or Wrefc(p) may be changed by the searches */
      if (Srefc(p) == 0  && ! IsDeleting(p)) {
        /* really delete everything now */

        refc_change_to_deleting(p);

//        if (HasRefs(ext))
//          refc_err("!!!delete: deleting a loop which still has external references", p);

        refc_delete(&Hd(p));
        refc_delete_post_deleteHd_log(p, ext, depth);
        refc_delete(&Tl(p));
        refc_delete_post_delete_log(p, ext, depth);

        /*fix:2018-10-12*/
        /*fix:2018-11-10*/
        if (ALLrefc(p) == 0)/*WIP*/
          free_node(p);
        else/*WIP*/
          Log1("refc_delete: not freeing free node%s\n", refc_pointer_info(p));/*WIP*/
      }
    }
  } /* else SRefc(p) > 0 so do nothing */

  depth--;
  return;
}

pointer delete_hd_only(pointer p)
{
  (void)err_refc("delete_hd_only: not implemented");
  return NIL; /*NOTREACHED*/
}

int refc_check()
{
  return zone_check();
}

char *refc_pointer_info(pointer p)
{
  return zone_pointer_info(p);
}

/*
 * refc_log_report(where)
 *  (1) call new_log_report() to report new storage consistency and new nodes created
 *  (2) print tab-separated listing of pointer operations performed, appending a TOTAL, checking that total tallys and printing and ERROR line if not
 */
void refc_log_report(FILE *where)
{
  /* first report on storage */
  new_log_report(where);
  
  (void) fprintf(where,"%s\t%s\n","What","count");
  
  (void) fprintf(where,"%s\t%d\n","free nodes", refc_free());
  (void) fprintf(where,"%s\t%d\n","nodes flipped", refc_delete_flip_count);
  (void) fprintf(where,"%s\t%d\n","NIL pointers requested to be copied", refc_copy_NILcount);
  (void) fprintf(where,"%s\t%d\n","strong pointers copied", refc_copy_Scount);
  (void) fprintf(where,"%s\t%d\n","weak pointers copied", refc_copy_Wcount);
  (void) fprintf(where,"%s\t%d\n","NIL pointers requested to be made cyclic", refc_copy_make_cyclic_NILcount);
  (void) fprintf(where,"%s\t%d\n","strong made cyclic", refc_copy_make_cyclic_Scount);
  (void) fprintf(where,"%s\t%d\n","weak made cyclic", refc_copy_make_cyclic_Wcount);
  (void) fprintf(where,"%s\t%d\n","start pointers weakened", refc_search_start_count);
  (void) fprintf(where,"%s\t%d\n","search pointers weakened", refc_search_strong_count);
  
  /* TODO count and verify the stack! */
  return;
}

void refc_final_report(FILE *where)
{
  if (refc_inuse() >0) {
    err_refc1("!!final report but number of pointers in use==", refc_inuse());
  }
  else
    (void) fprintf(where, "final report ok\n");
}

/*
 * ref_update_to_TYPE - where TYPE is a constant
 * change apply node to a constant type and set value of that type.
 * Deletes any pointers in the node
 * Error if node not an apply node - intended for use only in reduce()
 */
pointer refc_update_to_int(pointer n, int i)
{
  if ( !IsApply(n)) {
    (void) err_refc("refc_update_to_int");
    return NIL; /*NOTREACHED*/
  }
  
  refc_delete(&Hd(n));
  refc_delete(&Tl(n));
  
  Tag(n) = int_t;
  Num(n) = i;
  
  return n;
}

pointer refc_update_to_bool(pointer n, char b)
{
  if ( !IsApply(n)) {
    (void) err_refc("refc_update_to_bool");
    return NIL; /*NOTREACHED*/
  }
  
  refc_delete(&Hd(n));
  refc_delete(&Tl(n));
  
  Tag(n) = bool_t;
  Bool(n) = b;
  
  return n;
}

pointer refc_update_to_char(pointer n, char c)
{
  if ( !IsApply(n)) {
    (void) err_refc("refc_update_to_char");
    return NIL; /*NOTREACHED*/
  }
  refc_delete(&Hd(n));
  refc_delete(&Tl(n));
  
  Tag(n) = char_t;
  Char(n) = c;
  
  return n;
}

pointer refc_update_to_double(pointer n, double d)
{
  if ( !IsApply(n)) {
    (void) err_refc("refc_update_to_double");
    return NIL; /*NOTREACHED*/
  }
  
  refc_delete(&Hd(n));
  refc_delete(&Tl(n));
  
  Tag(n) = floating_t;
  Dbl(n) = d;
  
  return n;
}

/* special tag to indicate pattern match has failed */
pointer refc_update_to_fail(pointer n)
{
  if ( !IsApply(n)) {
    (void) err_refc("refc_update_to_fail");
    return NIL; /*NOTREACHED*/
  }
  
  refc_delete(&Hd(n));
  refc_delete(&Tl(n));
  
  Tag(n) = fail_t;
  
  return n;
  
}


/* Copy a pointer at from copying to */
/*
 * from is a pointer passed by reference
 * to is unchanged
 * either to or *from can be NIL
 */

/*
 * refc_copy - original COPY(R,<S,T>,isweak(<S,T)) written as a function to be used "R = COPY(<S,T>)"
 *  Copy a pointer, NOT creating a loop.
 */
#ifdef old
pointer refc_copy(pointer p)
{
  refc_copy_log(p);
  
  if (IsNil(p))
    return NIL;
  
  if (IsStrong(p))
    Srefc(p)++;
  else
    Wrefc(p)++;
  
  return p;
}
#endif
/* copy a pointer n times */
pointer refc_copyN(pointer p, int n)
{
  refc_copyN_log(p, n);
  
  if (IsNil(p))
    return p;
  
  if (IsStrong(p))
    Srefc(p) += n;
  else
    Wrefc(p) += n;
  
  return p;
}

pointer refc_copy(pointer p)
{
  return refc_copyN(p, 1);
}

/* copy the H of a pointer, propagating weakness
 * result is strong if p is strong and H(p) is strong, otherwise weak
 */
pointer refc_copyH(pointer p)
{
  pointer c;
  
  refc_copyN_log(p, -1/**/);
  
  if (IsNil(p))
    return NIL;
  
  c = H(p);
  
  if (IsNil(c))
    return NIL;
  
  /* IsStrong(p) && IsStrong(c) => Strong */
  /* IsWeak(p)   && IsWeak(c)   => IsWeak */
  /* IsStrong(p) && IsWeak(c)   => IsWeak */
  /* IsWeak(p) && IsWeak(c)     => IsWeak */
  
  if (IsStrong(p) && IsStrong(c)) {
    Srefc(c) += 1;
  } else {
    PtrBit(c) = !NodeBit(c); /* make weak, might be already */
    Wrefc(c) += 1;
  }
  
  return c;
}

pointer refc_copyT(pointer p)
{
  pointer c;
  
  refc_copyN_log(p, -2/**/);
  
  if (IsNil(p))
    return NIL;
  
  c = T(p);
  
  if (IsNil(c))
    return NIL;
  
  /* IsStrong(p) && IsStrong(c) => Strong */
  /* IsWeak(p)   && IsWeak(c)   => IsWeak */
  /* IsStrong(p) && IsWeak(c)   => IsWeak */
  /* IsWeak(p) && IsWeak(c)     => IsWeak */
  
  if (IsStrong(p) && IsStrong(c)) {
    Srefc(c) += 1;
  } else {
    PtrBit(c) = !NodeBit(c); /* make weak */
    Wrefc(c) += 1;
  }
  
  return c;
}

/*
 * refc_make_copy() copy a pointer, making sure that there are never weak pointers to constants
 * "weak" is the number >=0, of "intermediate" pointers which have been visited before "p"
 */
static pointer refc_make_copy(pointer p, unsigned weak)
{
  if (! IsNil(p)) {
    if ((IsWeak(p) || weak) && HasPointers(p)) {
      /* force pointer to be weak - eg when there have been weak pointers "en route" */
      PtrBit(p) = !NodeBit(p);
      Wrefc(p) += 1;
    } else {
      /* force pointer to be strong - including when a weak pointer leads to a constant */
      PtrBit(p) = NodeBit(p);
      Srefc(p) += 1;
    }
  }

  /* log the newly-created pointer */
  refc_make_copy_log(p, weak);

  return p;
}


/*
 copy NIL () = NIL
 copy anyStrong () = strong++
 copy anyWeak ()   = weak+
 
 copy (a:x) (H:y) = copy a y
 copy (a:x) (T:y) = copy x y
 
 copy NIL y = Err selecting on NIL
 copy atom y = Err selecting on atom
 
 */

/*
 * copy p xyz...
 *  p or x or y or z ... weak -> make weak; make strong (unless target is constant)
 *
 */
static pointer refc_copyS_do(pointer p, char *s, unsigned weak)
{
  Assert(s);

  /* selector encodes "path" to pointed-to node, visit each path item in turn, counting weaks */
  for ( /**/ ; *s; s++) {
    if (! HasPointers(p))
      err_refc("refc_copyS: node does not contain pointers");
    
    if (*s == 'H')
      p = H(p);
    else if (*s == 'T')
      p = T(p);
    else
      err_refc1("refc_copyS: bad selector", *s);
    
    if (IsSet(p) && IsWeak(p))
      weak++;
  }
  
  return refc_make_copy(p, weak);
}

/* copy ignoring weakness of p itself - use where p is Hd/Tl of something being updated*/
pointer refc_copyS(pointer p, char *s)
{
  Assert( s);
  Assert(*s);
  refc_copyS_log(p, s);
  return refc_copyS_do(p, s, 0);
}

/* copy taking into account strength of p - to update p itself*/
pointer refc_copy_pointerS(pointer p, char *s)
{
  unsigned weak = 0;
  if (IsSet(p) && IsWeak(p))
    weak++;

  refc_copy_pointerS_log(p, s);
  return refc_copyS_do(p, s, weak);
}
/*
 * refc_copy_nth - execute:
 *        'H' then (n-1) 'T' then 'H'
 * specifically used to implement (list int) eg ('Hello" 5) => 'o"
 * returns FAIL on error, rather then calling err_refc(), and reduce() handles the problem
 * NB p in this example must point to the *whole* application not just the list!
 */
pointer refc_copyNth(pointer p, unsigned n)
{
  unsigned weak = 0;
  
  refc_copyNth_log(p, n);

  if (! IsApply(p))
    return new_fail();
  p = H(p); /* H..... */
  if (IsSet(p) && IsWeak(p))
    weak++;
  
  while (n-- > 1) {   /*was while (--n >0) which is bad news for unsigned when n==0!*/
    if (! IsCons(p))
      return new_fail();
    p = T(p); /* HT*.... */
    if (IsSet(p) && IsWeak(p))
      weak++;
  }
  
  if (! IsCons(p))
    return new_fail();
  p = H(p); /* HT*...H */
  if (IsSet(p) && IsWeak(p))
    weak++;

  return refc_make_copy(p, weak);
}


#ifdef notdef
/* refc_adjust - change refc for a pointer by"delta" (>= -1)
 if delta  > 0 increase refc
 if delta == 0 no change
 if delta  < 0 delete pointer
 */
void refc_adjust(pointer n, int delta)
{
  if (delta < 0 )
    refc_delete(&n);
  else if (delta > 0)
    (void) refc_copyN(n , delta);
  
  return;
}
#endif

/*
 * refc_copy_make_cyclic - original COPY(R,<S,T>, TRUE) written as a function to be used "R = COPY(<S,T>)"
 *  Copy a pointer, creating a loop - only for use with Y combinator!.
 */
pointer refc_copy_make_cyclic(pointer p)
{
  refc_copy_make_cyclic_log(p); /* log original state of p strong/weak */
  
  if (IsNil(p))
    return NIL;
  
  PtrBit(p) = !NodeBit(p);  /* assert IsWeak(p) */
  Wrefc(p)++;
  
  return p;
}

#ifdef deprecated
/* refc_update - update-in-place overwriting contents.
 * NB old contents are deleted before installing new, so refc are incremented to be correct!
 * If new value is NIL, delete and return NIL.
 */
pointer refc_update(pointer n, pointer new)
{
  tag t;
  
  Assert (!IsNil(n));
  
  if (!HasPointers(n)) {
    (void) err_refc("trying to update a constant");
    return NIL; /*NOTREACHED*/
  }
  
  if (IsNil(new)) {
    refc_delete(&n);
    Assert(IsNil(n));
    return n;
  }
  
  t = Tag(new);
  
  if (HasPointers(new)) {
    n = refc_update_hdtl(n, refc_copyH(new), refc_copyT(new));
  } else {
    union val v = Val(new);
    n = refc_update_hdtl(n, NIL, NIL);
    Val(n) = v;
  }
  
  Tag(n) = t;
  
  return n;
}
#endif

/* insert new contents in an apply node, free old, new node can be any kind */
/* copy hd, tl from new into old; delete hd to */

/*
 * refc_update_hd
 *  replace the hd pointer of a node, with a new pointer (if different), deleting the old hd
 */
pointer refc_update_hd(pointer n, pointer new)
{
  if (!HasPointers(n)) {
    (void) err_refc("trying to update_hd a constant");
    return NIL; /*NOTREACHED*/
  }
  /*was
   if (!EqPtr(Hd(n), new)) {
   refc_delete(&Hd(n));
   Hd(n) = new;
   }
   */
  
  refc_delete(&Hd(n));
  Hd(n) = new;
  
  return n;
}
/*
 * refc_update_tl
 *  replace the tl pointer of a node, with a new pointer (if different), deleting the old tl
 */
pointer refc_update_tl(pointer n, pointer new)
{
  if (!HasPointers(n)) {
    (void) err_refc("trying to update_tl a constant");
    return NIL; /*NOTREACHED*/
  }
  
  /*was
   if (!EqPtr(Tl(n), new)) {
   refc_delete(&Tl(n));
   Tl(n) = new;
   }
   */
  
  return n;
}

/*
 * refc_updateSS - update hd and tl to nodes pointed-to by selectors
 * NB to prevent loops, selector strings must not be empty!
 */
void refc_updateSS(pointer *pp, char *h, char *t)
{
  Assert( h &&  t);
  Assert(*h && *t);
  *pp = refc_update_hdtl(*pp, refc_copyS(*pp, h), refc_copyS(*pp, t));
}

/*
 * refc_updateIS - update hd to be I combinator and tl to node pointed-to by selectors
 * NB to prevent loops, selector strings must not be empty!
 */

void refc_updateIS(pointer *pp, char *t)
{
  Assert( t);
  Assert(*t);
  *pp = refc_update_hdtl(*pp, new_comb(I_comb), refc_copyS(*pp, t));
}

/*
 * refc_update_hdS - update hd to node pointed to by selectors; tl is unchanged
 */
pointer refc_update_hdS(pointer *pp, char *h)
{
  Assert( h);
  Assert(*h);
  return refc_update_hd(*pp, refc_copyS(*pp, h));
}



/*
 * refc_update_Itl
 *  hd gets I_comb always
 */
pointer refc_update_Itl(pointer n, pointer newtl)
{
  return refc_update_hdtl(n, new_comb(I_comb), newtl);
}

/*
 * replace a pointer with something it points to
 *
 */
pointer /*so can be used in an expression*/ refc_update_pointerS(pointer *pp, char *s)
{
  Assert( s);
  Assert(*s);
  
  if (IsNil(*pp)) {
    refc_err("attempting to update a NIL pointer", *pp);
    /*NOTREACHED*/
  }
  
  {
    pointer newp = refc_copy_pointerS(*pp, s);
    refc_delete(pp);
    *pp = newp;
  }
  
  return *pp;
}
/*
 * refc_update_hdtl
 *  update in place - replace hd and tl with new contents; requires n to be a pointer node already; content of n are deleted
 *  does *not* modify tag (cons/apply).
 */
pointer refc_update_hdtl(pointer n, pointer newhd, pointer newtl)
{
  if (!HasPointers(n)) {
    (void) err_refc("trying to update_hdtl a constant");
    return NIL; /*NOTREACHED*/
  }
  
  refc_delete(&Hd(n));
  Hd(n) = newhd;
  
  refc_delete(&Tl(n));
  Tl(n) = newtl;
  
  /*WIP*/
  if (!HasPointers(n) && Wrefc(n) > 0)
    (void) fprintf(stderr,"refc_update: making constant with Wrefc>0: %s\n", zone_pointer_info(n));
  
  return n;
  
}

/*
 * startup/closedown
 */
int store_init()
{
  Log("store_init\n");
  refc_delete(&root);
  refc_delete(&defs);
  refc_delete(&builtin);
  
  return 0;
}

int store_done()
{
  refc_delete(&root);
  refc_delete(&defs);
  refc_delete(&builtin);
  
  Log("store_done\n");
  
  return 0;
}
