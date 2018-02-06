#include <stdio.h>
#include <stdlib.h> /* for malloc() */
#include <string.h>

#include "common.h"

#include "zone.h"

#include "store.h"

#define Limit 6 /* keep low for brevity */

void store_log(char *s, pointer p)
{
  if (debug) {
    (void) fprintf(stderr, "%s%s ", s, zone_pointer_info(p));
    out_debug_limit(p, Limit);
  }
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

pointer new_cons(pointer hd, pointer tl)
{
  pointer n = new_node(cons_t);
  Hd(n) = hd;
  Tl(n) = tl;
  store_log_new(n);
  return n;
}

pointer new_abstract(pointer name, pointer def, int r)
{
  pointer n = new_node(r ? recursive_abstract_t : abstract_t);
  Hd(n) = name;
  Tl(n) = def;
  store_log_new(n);
  return n;
}

pointer new_def(pointer name, pointer def)
{
  pointer n = new_node(def_t);
  Hd(n) = name;
  Tl(n) = def;
  store_log_new(n);
  return n;
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

/*
 * storage management - reference counting ref...
 */

#ifdef old
/* hide the sasl variable name in hd of the combinator */
pointer new_comb_name(tag t, pointer name)
{
  pointer n = new_node(t);
  Hd(n) = name;
  store_log_new(n);
  return n;
}
#endif

/*new*/
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

pointer new_apply2(pointer hd, pointer tl)
{
  return make_apply(NIL, hd, tl);
}

/*end new*/
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
  if (debug)
    (void) fprintf(stderr,"refc_search%s%s\n", zone_pointer_info(p), (Node(p) == Node(start) ? "*" : ""));
  
  if (Node(p) == Node(start))
    refc_search_start_count++;
  else
    refc_search_strong_count++;
  return;
}

void refc_delete_post_search_log(pointer p)
{
  if (debug)
    (void) fprintf(stderr,"refc_delete_post_search%s\n", zone_pointer_info(p));

  return;
}
  
void refc_delete_post_delete_log(pointer p)
{
  if (debug)
    (void) fprintf(stderr,"refc_delete_post_delete%s\n", zone_pointer_info(p));

  return;
}

void refc_copyN_log(pointer p, int n)
{
  if (debug)
    (void) fprintf(stderr,"refc_copyN%s(N=%d)\n", zone_pointer_info(p), n);
  
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
  
  refc_search_log(start, *pp);
  
  /* weak pointer to start === BUG  search "/w .... *" */
  
  if (IsStrong(*pp) && HasPointers(*pp)) {
    if (Node(*pp) == Node(start) || Srefc(*pp) > 1) {
      /* make strong pointer weak */
      
      if (Node(*pp) == Node(start))
        refc_search_start_count++;
      else
        refc_search_strong_count++;
      
      Wrefc(*pp)++;
      Srefc(*pp)--;
      PtrBit(*pp) = !NodeBit(*pp); /* Was: PtrBit(p) = !PtrBit(p); */
    } else {
      /* strong pointer stays strong: recurse (if node contains pointers) */
      Assert(Srefc(*pp) == 1);
      refc_search(start, &Hd(*pp));
      refc_search(start, &Tl(*pp));
    }
  }
  /* if p points to constant or is weak, do nothing */
  
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
  
  if (IsNil(*pp))
    return;
  
  *pp = NIL;	/* really delete the pointer in situ */
  
  if (debug)
    (void) fprintf(stderr,"refc_delete%s\n", zone_pointer_info(p));
  
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
  
  if (Srefc(p) == 0) {
    
    if (Wrefc(p) == 0) { /* not in a loop */
      
      if (HasPointers(p)) {
        refc_delete(&Hd(p));
        refc_delete(&Tl(p));
      }
      
      refc_delete_post_delete_log(p); /* assert(ALLRefc(p) == 0) after this deletion */
      
      free_node(p); /* pointed-to node is free for re-use */
    }
    else { /* is in a loop */
      
      /* Assert(HasPointer(p)); */
      if (!HasPointers(p)) {
        if (IsStrong(p))
          (void) err_refc("constant has weak references (deleting strong pointer)");
        else
          (void) err_refc("constant has weak references (deleting weak pointer)");
        return; /*NOTREACHED*/
      }
      
      /* invert pointers */
      refc_delete_flip_count++;
      
      NodeBit(p) = !NodeBit(p);	/* "an essential implementation trick" */
      Srefc(p) = Wrefc(p);	/* !!! was this done correctly in 1985? */
      Wrefc(p) = 0;		/* !!! was this done correctly in 1985? */
      
      /* recursive search */
      refc_search(p, &Hd(p));
      refc_search(p, &Tl(p));
      
      refc_delete_post_search_log(p);
      
      /* Srefc(p) and/or Wrefc(p) may be changed by the searches */
      if (Srefc(p) == 0) {
        refc_delete(&Hd(p));
        refc_delete(&Tl(p));
        
        
        refc_delete_post_delete_log(p);
#if toocautious
        /* assert(IsFree(p)) - has been freed above */
        if ( !refc_isfree(p))
          (void) err_refc("loop not freed");
#else
        store_log("loop not freed", p);
#endif
      }
    }
  }
  
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
 *	(1) call new_log_report() to report new storage consistency and new nodes created
 *	(2) print tab-separated listing of pointer operations performed, appending a TOTAL, checking that total tallys and printing and ERROR line if not
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

/* 
 * ref_update_to_TYPE
 * change apply node to a constant type and set value of that type.
 * Deltes any pointers in the node
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
 *	Copy a pointer, NOT creating a loop.
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
    return NIL;

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

/*
 * refc_copy_make_cyclic - original COPY(R,<S,T>, TRUE) written as a function to be used "R = COPY(<S,T>)"
 *	Copy a pointer, creating a loop - only for use with Y combinator!.
 */
pointer refc_copy_make_cyclic(pointer p)
{ 
  refc_copy_make_cyclic_log(p); /* log original state of p strong/weak */

  if (IsNil(p))
    return NIL;

  PtrBit(p) = !NodeBit(p);	/* assert IsWeak(p) */
  Wrefc(p)++;
  
  return p;
}

/* refc_update - update-in-place overwriting contents.  
 * NB old contents are deleted before installing new, so refc are incremented to be correct! 
 */
pointer refc_update(pointer n, pointer new)
{
  tag t;

  if (!HasPointers(n)) {
    (void) err_refc("trying to update a constant");
    return NIL; /*NOTREACHED*/
  }

  t = Tag(new);
  
  if (HasPointers(new)) {
    n = refc_update_hdtl(n, refc_copy(Hd(new)), refc_copy(Tl(new)));
  } else {
    union val v = Val(new);
    n = refc_update_hdtl(n, NIL, NIL);
    Val(n) = v;
  }
  
  Tag(n) = t;

  return n;
}

/* insert new contents in an apply node, free old, new node can be any kind */
/* copy hd, tl from new into old; delete hd to */

/*
 * refc_update_hd
 *	replace the hd pointer of a node, with a new pointer (if different), deleting the old hd
 */
pointer refc_update_hd(pointer n, pointer new)
{
  if (!HasPointers(n)) {
    (void) err_refc("trying to update_hd a constant");
    return NIL; /*NOTREACHED*/
  }
  
  if (!EqPtr(Hd(n), new)) {
    refc_delete(&Hd(n));
    Hd(n) = new;
  }

  return n;
}
/*
 * refc_update_tl
 *	replace the tl pointer of a node, with a new pointer (if different), deleting the old tl
 */
pointer refc_update_tl(pointer n, pointer new)
{
  if (!HasPointers(n)) {
    (void) err_refc("trying to update_tl a constant");
    return NIL; /*NOTREACHED*/
  }
 
  if (!EqPtr(Tl(n), new)) {
    refc_delete(&Tl(n));
    Tl(n) = new;
  }
  
  return n;
}

/*
 * refc_update_hdtl
 *	update in place - replace hd and tl with new contents; requires n to be a pointer node already; content of n are deleted
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

/* refc_moveh2h(from, to) move pointer: Hd(to) becomes Hd(from), setting original Hd(to) to NIL */
pointer refc_move_h2h(pointer to, pointer from)
{
  Hd(to) = Hd(from);
  Hd(from) = NIL;
  return to;
}
