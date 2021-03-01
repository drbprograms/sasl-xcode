/*
Storage allocation - bare metal
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>  /* isprint() */

#include "common.h"

#include "zone.h"

enum zone_t_type {
  zone_t_standard,
  zone_t_file,
  zone_t_url
} zone_t_type;

typedef struct zone_header {
  long unsigned size;  /* number of nodes available in the zone */
  unsigned used;  /* number of nodes used by new_node().  assert(used <= size) */
  node *nodes;  /* pointer to first node to be used - fixed at the time zone is created by new_zone() */
  node *check_nodes;  /* if check is set, pointer to first check node - fixed at the time zone is created by new_zone() else 0 */
  enum zone_t_type type;  /* type of zone - may alter the significance of the nodes therein - fixed at the time zone is created by new_zone() */
  unsigned seq;
  struct zone_header *previous, *next;  /* linked structure of zones - we avoid a master list to keep things flexible */
} zone_header;
static zone_header zero_zone_header = {0};

typedef struct {
  zone_header *z; /* zone of node */
  long off;         /* offset within that zone - Assert(offset < size) */
} zone_address;
zone_address zero_zone_address = {&zero_zone_header , 0};

static unsigned zone_new_count = 0;  /* total number of zones created by new_zone() */
static long unsigned zone_total_size = 0; /* sum of zone->size for all zones created by new_zone() */
static unsigned zone_seq = 1; /* sequence number counting how many new zones hav been made. Acts as unique id for zone_pointer_info() */

/* zone_header points to the most recently created zone */
static zone_header *zone_current = 0;  /* zone currently being used by new_node() to create nodes */

static long unsigned zone_size_default = 1024L;  /* to be updated elsewhere */

static unsigned new_count = 0; /* how many new nodes */
static unsigned new_tags[TagCount]; /* how mant new nodes, by tag */

/*
 * Logging - record info during a run - less verbose than debugging.
 */
static void new_log(pointer p)
{
  int t = Tag(p);
  Assert(t < TagCount);
  new_count++;
  new_tags[t]++;
  return;
}

static void zone_free_log(pointer p)
{
  Log1("zone_free%s\n", zone_pointer_info(p));
  return;
}

static void zone_new_log(zone_header *z)
{
  Log3("zone_new: seq: %u\tsize:\t%lu\t%s\n", z->seq, z->size, z->check_nodes ? " with checking" : "") ;
  return;
}

static void zone_log(char *s, pointer p)
{
  if (logging)
    Log2("%s%s\n", s, zone_pointer_info(p));
}

static void zone_log_weaken(pointer p)     { zone_log("weaken", p); }
static void zone_log_strengthen(pointer p) { zone_log("strengthen", p); }
static void zone_log_flip(pointer p)       { zone_log("flip", p); }
static void zone_log_erase(pointer p)      { zone_log("erase", p); }

/*
create a new zone of storage suffcient for 'size' nodes
return C-pointer to first node which is a zone_t node:
Hd=chain of information items
 Num: number of node that can still be created created in zone
 Num: size of zone
 Num: enum zone_t_type
   0 - standard
...
Tl=pointer to next zone created, initially NIL
new_node() below initialises the rest of the zone incrementally; wish to avoid scanning the whole zone at once to avoid peromance impact akin to makr-scan.
<guaranteed> to return usable zone - no need for caller to check.
*/

/*
 zone_new - allocate storage for 'size' nodes; point back to 'parent' (if any).  `
 Set up zone_header at start of zone for use by zone_xxx() and new_XXX() only.
 If 'check', then allocate double space at 'check_nodes' for use in consistency checking.
 for any given (node *)n, check info resides at (node *)(n + z->size) where z is the zone in question
 
 Always returns a valid zone, caller does not need to check (or takes err()).
 Always creates zone_t_standard (for now).
 */
static zone_header *zone_new(long unsigned size, struct zone_header *parent)
{
  // bug fixed 2020-05-16 https://tree.taiga.io/project/northgate91-project-one/issue/57
  zone_header *z = malloc(sizeof(zone_header));
  node *n        = calloc(size, sizeof(node));
  node *cn = check?calloc(size, sizeof(node)) : 0;  /* cn could be in one large alloc with n, but chose not, for flexibility */
  
  if (!z || !n || (check && !cn)) {
    (void) err_zone("out of space");
    return (zone_header *) 0; /*NOTREACHED*/
  }

  z->size = size;
  z->nodes =  n;
  z->check_nodes = cn;
  z->used = 0;
  z->type = zone_t_standard;
  z->seq = zone_seq++;
  z->previous = parent;
  if (parent)
    parent->next = z;
  z->next = 0;
  
  zone_new_count++;
  zone_total_size += z->size;
  
  /* count different kinds of zone when the they are introduced */
  
  zone_new_log(z);
  
  return z;
}


/*
 * freelist - linked list of nodes, linked in Hd() by strong pointer, end of list Hd==NIL
 */
static pointer zone_freelist = {(node *)0,0};/* "NIL" in a way that satisfies cc() */

static unsigned zone_free_count = 0;  /* how many nodes in the free list */
static unsigned zone_inuse_count = 0;  /* nodes in use == reachable from 'root' or 'defs' or 'builtin' */

/* info - how many nodes are in use? */
unsigned zone_inuse()
{
  return zone_inuse_count;
}

/* info - how many nodes on the free list */
unsigned zone_free()
{
  return zone_free_count;
}

/*
 node_new() return pointer to a new node
 contents un-initialised
 <guaranteed> to return usable node - no need for caller to check.
 */
//ToDo replace with bombproof new_struct_node() new_atom_node()
static pointer new_node(tag t)
{
  node *n;
  pointer p;
  
  /* TODO use freelist if any - simpler not to do this yet as makes no of nodes used clear - remember to include zone_freecount-- */
  
  /* make sure there is free space to be used */
  if (!zone_current || (zone_current->used >= zone_current->size))
    zone_current = zone_new(zone_size_default, zone_current);
  
  /* assert(zone_zurrent && zone_current->used < zone_current_size) so there is space for one node in the zone  */
  zone_inuse_count++;
  n = zone_current->nodes + ((zone_current->used)++);  /* first node is (nodes+0) */
  
  p.p = n;
  n->bit = p.bit = 0;   /* Strong */
  n->s_refc = 1;
  n->w_refc = 0;
  
  Tag(p) = t;
  
  H(p) = T(p) = NIL; /* safety */
  
  new_log(p);
  
  return p;
}

/*
 * new_atom/new_hdtl return pointer to a new node
 * contents un-initialised if atom, td tl set to hdtl (may be NIL)
 * <guaranteed> to return a usable node - no need for caller to check
 * error if tag is not appropriate
 */
pointer new_atom(tag t)
{
  if (HasPointersTag(t))
    err_zone1("new atom: incorrect tag:", err_tag_name(t));
  
  return new_node(t);
}

pointer new_hdtl(tag t, pointer hd, pointer tl)
{
  pointer p;
  
  if (!HasPointersTag(t))
    err_zone1("new hdtl: incorrect tag:", err_tag_name(t));
  
  p = new_node(t);
  H(p) = hd;
  T(p) = tl;

  return p;
}

/*
 * change_tag - set tag of existing node; returns p
 * hdtl must have only NIL pointers if being made into an atom
 * when atom is changed to hdtl, it is given NIL pointers
 */

pointer change_tag(pointer p, tag t)
{
  if (IsNil(p))
    return p;
  
  if (HasPointers(p)) {
    if (!HasPointersTag(t)) { /* becoming an atom */
      if (!IsNil(H(p)))
        err_zone("change tag: Hd isn't NIL");
      if (!IsNil(T(p)))
        err_zone("change tag: Tl isn't NIL");
    }
    Tag(p) = t;
  } else {
    Tag(p) = t;
    if (HasPointersTag(t)) { /* formerly atom */
      H(p) = T(p) = NIL;
    }
  }
  return p;
}

/*
 * set_hd/set_tl/set_hdtl
 * WIP what checking / restructuring can be enforced here xxx
 * eg set_hd(pointer p, pointer function pf) { Hd(p) = pf(Hd(p)) }
 * REPLACE with update_hd/tl/=hdtl which first deletes pointers in place and adds new ones per store.c
 */
pointer set_hd(pointer p, pointer hd)
{
  if (!HasPointers(p))
    err_zone("set_hd of atom");
  Hd(p) = hd;
  return p;
}

pointer set_tl(pointer p, pointer tl)
{
  if (!HasPointers(p))
    err_zone("set_tl of atom");
  Tl(p) = tl;
  return p;
}

pointer set_hdtl(pointer p, pointer hd, pointer tl)
{
  if (!HasPointers(p))
    err_zone("set_hdtl of atom");
  Hd(p) = hd;
  Tl(p) = tl;
  return p;
}

/* update-in-place with use-before-loose ;-) */
/* requires *pp is a valid Hd/Tl location - no checking */
/* new may be NIL */
pointer zone_update(pointer *pp, pointer new)
{
  extern void refc_delete(pointer *pp);
  pointer old = *pp;
  *pp = new;
  refc_delete(&old);
  return *pp;
}


/*
 * free_node - deallocate the node p points to, adds to front of zone_freelist
 * pointer - passed By Reference
 *  freelist is composed of apply nodes
 *   ONLY to be called from refc_delete()
 */
void free_node(pointer p)
{
  zone_free_log(p);
  /* validation */
  if (IsNil(p)) {
    err_zone("free: pointer is NIL");
    return;
  }
  if (Srefc(p) > 0)
    err_zone("free: Srefc not zero");
  if (Wrefc(p) > 0)
    err_zone("free: Wrefc not zero");
  if(IsFree(p))
    err_zone("free: node is already free");
  if (HasPointers(p)) {
    if (IsSet(Hd(p)))
      err_zone("free: Hd not NIL");
    if (IsSet(Tl(p)))
      err_zone("free: Tl not NIL");
  }

  /* free storage created with strdup() */
  if (IsName(p)) {
    Assert(Name(p));  /* should always point to something */
    free(Name(p));
  }
  if (IsFun(p)) {
    Assert(Uname(p));  /* should always point to something */
    free(Uname(p));
  }
    
  /* add to freelist - zone_frelist is Hd linked list of strong pointers (possibly NIL) */
  change_tag(p, free_t);  /* was cons_t); */
  set_hd(p,  NIL); // not needed
  set_tl(p,  zone_freelist);
  
  PtrBit(p)= NodeBit(p) = PtrBit(zone_freelist);  /* link with strong pointers */
  Srefc(p) = 1;
  Wrefc(p) = 0;
  
  zone_freelist = p;
  
  zone_free_count++; zone_inuse_count--;
  
  return;
}


/* increment/decrement reference counts - works for check nodes as well */
//XXX temp
static unsigned Srefc_decr(pointer p) { return Srefc(p) == 0 ?   (void) Debug1("!!Srefc_decr%s\n", zone_pointer_info(p)), (void) Assert(0), err_zone("srefc_decr but Srefc==0") : Srefc(p)--; }
static unsigned Wrefc_decr(pointer p) { return Wrefc(p) == 0 ?   (void) Assert(0), err_zone("wrefc_decr but Wrefc==0") : Wrefc(p)--; }
static unsigned Srefc_incr(pointer p) { return Srefc(p)++; }
static unsigned Wrefc_incr(pointer p) { return Wrefc(p)++; }

void zone_weaken(pointer *pp)
{
  const pointer p = *pp;
  if (IsSet(p) && IsStrong(p))  {
    Wrefc_incr(p);
    Srefc_decr(p);
    PtrBit(*pp) = !NodeBit(p);
    zone_log_weaken(*pp);
  }
}

void zone_strengthen(pointer *pp)
{
  const pointer p = *pp;
  if (IsSet(p) && IsWeak(p))  {
    Wrefc_decr(p);
    Srefc_incr(p);
    PtrBit(*pp) =  NodeBit(p);
    zone_log_strengthen(*pp);
  }
}


/* flip node - reverse all strong/weak pointers to the node */
void zone_flip(pointer p)
{
  unsigned s;
  
  if (IsNil(p)) // || ! HasPointers(p))
     return;
   
  NodeBit(p) = !NodeBit(p);  /* "an essential implementation trick" */
  s = Srefc(p);
  Srefc(p) = Wrefc(p);    /* !!! was this done correctly in 1985? */
  Wrefc(p) = s;      /* !!! was this done correctly in 1985? */

  zone_log_flip(p);
  return;
}

void zone_erase(pointer *pp)
{
  pointer p = *pp;
  
  *pp = NIL;
  
  if (IsStrong(p)) {
    if (Srefc(p) == 0)
      err_refc("erase: pointer is strong but Srefc==0");
    Srefc_decr(p);
  } else {
    if (Wrefc(p) == 0)
      err_refc("erase: pointer is weak but Wrefc==0");
    Wrefc_decr(p);
  }
  zone_log_erase(p);
  return;
}



/*********************************************************************************************************
 * Storage checking
 */
static int check_log(char *msg, int result)
{
  Debug1("zone_check: %s\n", msg);
  return result;
}

static int check_log2(char *msg, int result, long unsigned i, long unsigned j)
{
  Debug2(msg, i, j);
  return result;
}

static int check_log5(char *msg, int result, int zone_no, int node_no, int i, int j)
{
  Debug5("%s [%u.%u] (node/check node %s/%s)\n", msg, zone_no, node_no, err_tag_name((tag) i), err_tag_name((tag) j));
  return result;
}

static int check_log6(char *msg, int result, int zone_no, int node_no, int i, int j)
{
  Debug5("%s [%u.%u] (%u/%u)\n", msg,  zone_no, node_no, i, j);
  return result;
}

typedef struct { unsigned s; unsigned w;} pair;
static const pair zero_pair = {0,0};

/*
 * used to accumulate refc stored in (non-debug) nodes and
 *  deficit == missing pointers found only in debug nodes
 *  excess  == extra pointers found only in debug nodes
 */
//deprecated
typedef enum{unknown = 0, island, strong_root, strong, weak, strong_weak} check_node_data_status;  // 2020-06-08 currently unused but in place for future

/* number of s/w pointers, with excess/deficit wrt refc - set per-node by */
typedef struct {check_node_data_status status; pair total, excess, deficit;} check_node_data;

static const check_node_data zero_data = {unknown, 0};

/* pointer counts - set per-pointer by check_get_pointer_counts() */
typedef struct {
  unsigned
  nil,    /* number of nil pointers, including root, zone_free */
  strong, /* number of strong pointers, including root, zone_freelist if they are non-nil */
  weak,   /* number of weak pointers */
  hdtl,   /* number of HasPointers nodes */
  atom,   /* number of !HasPointers nodes*/
  srefc,  /* sum of strong refc in nodes */
  wrefc;  /* sum of wesk refc in nodes */
} check_counts;
const check_counts zero_zone_check_counts = { 0 };

/* Helper - "a = b; add counts add counts from second arg to counts in first arg */
static void check_counts_add(check_counts *a, check_counts *b)
{
  a->nil   += b->nil;
  a->strong+= b->strong;
  a->weak  += b->weak;
  a->hdtl  += b->hdtl;
  a->atom  += b->atom;
  a->srefc += b->srefc;
  a->wrefc += b->wrefc;
  return;
}

/* Check tags.  Here we abuse tag range of enum tag - assume C compiler does not range check. Strictly only for check nodes in zone.c */
static const tag visited_t     = _LastTag + 1;
static const tag fordeletion_t = _LastTag + 2;
static const tag inuse_t       = _LastTag + 3;

static const tag willfree_t    = _LastTag + 4; // deprecated

/*
 zone_of_node - look-up the zone in which a given node resides - works for check nodes also
 NB linear search, zone by zone - "slow" when lots of zone
 */
static zone_header *zone_of_node(node *n)
{
  zone_header *z;
  
  for (z = zone_current; z; z = z->previous) {
    if ((n >= z->nodes) && (n < (z->nodes + z->used)))
      return z;
    
    if (z->check_nodes &&
        (n >= z->check_nodes) && (n < (z->check_nodes + z->used)))
      return z;
    
  }
//  err_zone("can't get pointer zone info");
    Assert(0);
  /*NOTREACHED*/
  return (zone_header *)0;
}

#define Label(x) ((IsSet(x) && IsWeak(x)) ? "@" : "")
#define MAX 256 /*!!*/
static char s[MAX];

/*
 * zone_opinter_zone_detail - get non-NIL pointer's zone and offset in zone
 * returns valid zone_address or else calls err_zone() to reset
 * if chk then check node info is returned
 */
#define Limit 24 /* restrict output to this level, unless debug>1 */
static zone_address zone_pointer_address(pointer p)
{
  zone_address z;
  
  if (IsNil(p))
    return zero_zone_address;
  
  z.z = zone_of_node(Node(p));
  z.off = Node(p) - z.z->nodes;
  Assert(z.off < z.z->size);
  return z;
}

/* return the check pointer corresponding to pointer p (which is not a check pointer) */
static pointer zone_check_pointer_of(pointer p)
{
  zone_header *zh;
  pointer p_check;

  if (IsNil(p))
    return NIL;
  
  zh = zone_of_node(Node(p));
  Assert((Node(p) - zh->nodes) < zh->size);   /* range check - require (p is not a pointer to a check node) */

  Node(p_check) = zh->check_nodes + (Node(p) - zh->nodes);
  PtrBit(p_check) = PtrBit(p);

  return p_check;
}

char *zone_address_info(zone_address z, char *suffix, int chk)
{
  node *np;
  int c;
  
  np = (chk ? z.z->check_nodes : z.z->nodes) + z.off;
  
  c = snprintf(s, MAX, "%s (s/w %u/%u) [%u.%ld] %s",
               suffix,
               np->s_refc,
               np->w_refc,
               z.z->seq,
               z.off,
               err_tag_name(np->t)
               );
  
  if (!chk && debug) {
    /* debugging: add detail of pointed-to non-check nodes when debug > 0 */
    //extra-start
    if (check) {
      node *np_check = z.z->check_nodes + z.off;
      c += snprintf(s + c, MAX - c, " (s-check/w-check %u/%u)",
                    np_check->s_refc,
                    np_check->w_refc);
    }
    //extra-end
    if (HasPointersTag(np->t)) {
      zone_address z_hd = zone_pointer_address(np->val.pointers.hd);
      zone_address z_tl = zone_pointer_address(np->val.pointers.tl);
      
      snprintf(s + c, MAX - c, " [%u.%ld]%s.[%u.%ld]%s",
               z_hd.z->seq, z_hd.off, Label(np->val.pointers.hd),
               z_tl.z->seq, z_tl.off, Label(np->val.pointers.tl)
               );
    } else {
      switch (np->t) {
        case name_t:  snprintf(s + c, MAX - c, " \"%s\"", np->val.n); break;
        case int_t:   snprintf(s + c, MAX - c, " %d",     np->val.i); break;
        case char_t:
          if (isprint(np->val.c))
            snprintf(s + c, MAX - c, " \'%c\'", np->val.c);
          else
            snprintf(s + c, MAX - c, " 0x%02x", np->val.c);
          break;
        case bool_t:  snprintf(s + c, MAX - c, " %s",     np->val.b ? "TRUE" : "FALSE"); break;
        default:  break;
      }
    }
  }
  return s;
}
#undef Label

// xxx broken for check nodes
static char *zone_pointer_info_do(pointer p, int chk)
{
  Assert(chk ? check : 1);  /* chk only set if checking */

  if (IsNil(p))
    return "/NIL";  /* '/' separator from previous part of message */
  
  return zone_address_info(zone_pointer_address(p), (IsStrong(p)?"/s":"/w"), chk);  /* Note: IsStrong works correctly for check nodes as well as nodes. */
}

/*NB zone_pointer_info_do() returns pointer to a *fixed string* */
char *zone_pointer_info(pointer p)
{
  return zone_pointer_info_do(p, 0);
}

static char i[MAX];

/* return printable string for logging node */
/* NB zone_node_info() returns pointer to a *fixed string* */
/* Not reentrant */
//depracated
static char *zone_node_info(check_node_data data)
{
    char *msg = "";
    
    switch (data.status) {
      case unknown:     msg = " !unknown island check status"; break;
      case island:      msg = " is an island"; break;
      case strong_root: msg = " case (a) root protected by strong pointers"; break;
      case strong:      msg = " case (b) protected only by non-root strong pointers"; break;
      case weak:        msg = " case (c) protected only by non-root weak pointers"; break;
      case strong_weak: msg = " cases (b+c) protected by non-root strong and weak pointers"; break;
    }

  snprintf(i, MAX, "(s/w %u/%u) (s+/w+ %u/%u) (s-/w- %u/%u) status:%s",
           data.total.s,    data.total.w,
           data.excess.s,   data.excess.w,
           data.deficit.s,  data.deficit.w,
           msg);
  return i;
}

/*
 * strngthen/weaken/erae pointer pp, adjust check node pp_check and associated counts
 */
static void zone_strengthen_withcounts(pointer *pp, check_counts *counts)
{
  if (IsWeak(*pp)) {
    const pointer p_check = zone_check_pointer_of(*pp);
    Assert(Wrefc(p_check)); //xx 2021-02-19 to be sure - was causing problems
    Wrefc(p_check)--;
    Srefc(p_check)++;
    counts->strong--;
    counts->weak++;
    zone_strengthen(pp); /* do this last so that zone_log_weaken reflects check changes */
  }
}

static void zone_weaken_withcounts(pointer *pp, check_counts *counts)
{
  if (IsStrong(*pp)) {
    pointer p_check;
    p_check = zone_check_pointer_of(*pp);
//xx
//    if (!Srefc(p_check))
//      Debug3("!!zone_weaken_withcounts%s (s_check/w_check %u/%u)\n", zone_pointer_info(*pp), Srefc(p_check), Wrefc(p_check));
//xx
//xx    Assert(Srefc(p_check)); //xx 2021-02-19 to be sure - was causing problems
    Wrefc(p_check)++;
    Srefc(p_check)--;
    counts->strong++;
    counts->weak--;
    zone_weaken(pp); /* do this last so that zone_log_weaken reflects check changes */
  }
}

static void zone_erase_withcounts(pointer *pp, check_counts *counts)
{
  const pointer p = *pp, p_check = zone_check_pointer_of(p);

  if (IsStrong(p)) {
    if (Srefc(p) == 0)
      err_refc("erase: pointer is strong but Srefc==0");
    Srefc_decr(p); Srefc(p_check)--;
    counts->strong--;
  } else {
    if (Wrefc(p) == 0)
      err_refc("erase: pointer is weak but Wrefc==0");
    Wrefc_decr(p); Wrefc(p_check)--;
    counts->weak--;
  }
  zone_erase(pp);  /* do this last so that zone_log reflects check changes */
  return;
}

void zone_flip_withcounts(pointer n, check_counts *counts)
{
  unsigned s;
  pointer n_check;

  if (IsNil(n))  // || ! HasPointers(p))
    return;
  n_check = zone_check_pointer_of(n);
  
  zone_flip(n);

  /* note: NodeBit(n_ check) not in use */
  s = Srefc(n_check);
  counts->strong += Wrefc(n_check) - s;
  counts->weak   -= Wrefc(n_check) - s;

  Srefc(n_check) = Wrefc(n_check);
  Wrefc(n_check) = s;
  

  return;
}


/*
 zone-debug_reset - reset the check_nodes in every zone - must be used prior to a check
 
 NB time taken is proportional to total zone aize, so performance penality incurred
 */
static void check_reset()
{
  zone_header *z;

  /* visit zones setting check nodes to zero */
  for (z = zone_current; z; z = z->previous) {
    if (z->check_nodes) {
      memset(z->check_nodes, 0, (size_t) (z->size * sizeof(node)));
    }
  }
  return;
}


/*
 * Iterative check, not recursive - MacOS stack is limited to depth 512 then aborts
 */
static pointer *stack;
static pointer *sp;
static unsigned stack_size = 0;

#define Stacked (sp-stack)
#define Push(p) (Assert(Stacked < stack_size), *sp++ = (p))
#define Pop     (Assert(Stacked > 0),         (*--sp))


/*
 * report when loops found
 * intention is to only print as many nodes are required to get back to "p"
 */
#define Limit 24 /* restrict output to this level, unless debug>1 */
/* worker */
static int zone_loop_report_do(const pointer p, const pointer start, const unsigned s_limit, unsigned *strong_count)
{
  
  if (IsNil(p) ||
      IsWeak(p) ||
      ! HasPointers(p))
    return 0;
  
  (*strong_count)++;
  
  Log2("zone_loop_report_do%s strong_count=%u ", zone_pointer_info(p), *strong_count);
  out_debug_limit(p, Limit);
  
  if (Node(p) == Node(start)) {
    Log2("zone_loop_report_do%s loop complete here, size %u\n", zone_pointer_info(p), *strong_count);
    return 1;
  }
  
  if ((*strong_count) > s_limit) {
    Log2("!!zone_loop_report_do%s loop NOT complete at size %u\n", zone_pointer_info(p), *strong_count);
    return 1;
  }
  
  return (
          zone_loop_report_do(H(p), start, s_limit, strong_count) ||
          zone_loop_report_do(T(p), start, s_limit, strong_count));
}

/* wrapper */
static int zone_loop_report(pointer p, unsigned s_limit)
{
  unsigned strong_count = 0;

  Debug1("zone_loop_report%s ", zone_pointer_info(p));
  out_debug_limit(p, (debug > 1 ? s_limit : Limit));

  return (! HasPointers(p) ||
          zone_loop_report_do(H(p), p, s_limit, &strong_count) ||
          zone_loop_report_do(T(p), p, s_limit, &strong_count));
}

/*
 * update check info for a pointer at the pointed to node
 * s_limit is max number of strong pointers expected - strong loop avoidance
 * returns 0 when node visited for first time, otherwise non zero.
 */
static int get_pointer_counts(pointer p, unsigned s_limit, check_counts *counts)
{
  zone_header *z;
  long int node_no;  /* offset of Node(p) within the zone */

  /* updates "counts" for the pointer */
  if (IsNil(p)) {
    (counts->nil)++;
    return 1;
  }
  
  if (IsStrong(p)) {
    (counts->strong)++;
    
    /* loop avoidance */
    if ((s_limit > 0) && (counts->strong > s_limit)) {
      if (debug > 1) {
        /* continue regardless - DANGER!*/
        Log2("!!check_get_pointer_counts: strong pointer loop detected%s (traversing pointers) (limit=%u)\n", zone_pointer_info(p), s_limit);
        zone_loop_report(p, s_limit); // NEW
        return 1;
      } else {
        char s[MAX];
        Log2("!!%s%s\n","check_get_pointer_counts: strong pointer loop detected (traversing pointers)",  zone_pointer_info(p));
        (void) snprintf(s, MAX, "strong pointer loop detected (traversing pointers) (limit=%u )\n", s_limit);
        zone_loop_report(p, s_limit); // NEW
        (void) err_zone(s);
        /*NOTREACHED*/
      }
    }
  }
  else {
    (counts->weak)++;
  }
  
  z = zone_of_node(Node(p));
  node_no = Node(p) - z->nodes; /*offset within zone nodes*/
  
  Assert(z->check_nodes);
// deprecated Assert(z->nodes[node_no].t);  // see check with warning below

  if (IsStrong(p))
    (z->check_nodes[node_no].s_refc)++;
  else
    (z->check_nodes[node_no].w_refc)++;
  
  /* after first visit, check tag will be set */
  if (z->check_nodes[node_no].t) {
    if (mem_dump > 1)
      Debug1("mem_dump+%s\n",zone_pointer_info(p));
    return 1;
  }

  /* first visit: update "count" and check_node for node */
  /* set check tag to indicate "node have been visited" */
  /* tag sanity check - wise here and also prerequesite for zone_deletion_check() */
  {
    tag t = z->nodes[node_no].t;
    if (t <= zero_t || t >= _LastTag) {
    Log2("!!check_get_pointer_counts%s\tbad tag: %d\n", zone_pointer_info(p), t);
      return 1;
    }
  }

  z->check_nodes[node_no].t = visited_t;

  /* sum refc at node */
  counts->srefc += Srefc(p);
  counts->wrefc += Wrefc(p);
  
  /* count node */
  if (HasPointers(p))
    (counts->hdtl)++;
  else
    (counts->atom)++;
  
  if (mem_dump) {
    Debug1("mem_dump%s\n",zone_pointer_info(p));
  }
  return 0;
}

/*
 check_traverse_pointers - traverse pointers, populating check_nodes with Tag/Srefc/Wrefc
 recursively traverse from p counting pointers and nodes and updating Error
 
 s_limit - max number of strong pointers before a loop is certain
 
 counts - passed by reference, updated here
 
 returns 0 if all ok, otherwise >0
 */
// 2021-01-21 removed stop_node, instead use zero_t tag to indicate whether node already visited
// 2021-02-05 added start node!
static int check_traverse_pointers_do(pointer p, unsigned s_limit,  check_counts *counts, const pointer *localroot)
{
  while (1) {
    while (HasPointers(p)) {
      /* pointer nodes */
      int seen_before = get_pointer_counts(p, s_limit, counts);
            
      if (seen_before)
        break; /* not first visit to p */
      
#define zone_weaken_withcounts(p, c) zone_weaken(p)
      if (localroot) {
        /* if set, weaken pointers to localroot and adjust counts */
        if (SameNode(*localroot, Hd(p))) zone_weaken_withcounts(&Hd(p), counts);
        if (SameNode(*localroot, Tl(p))) zone_weaken_withcounts(&Tl(p), counts);
      }
#undef zone_weaken_withcounts
      /* first visit: visit descendants  */
      Push(p);
      p = H(p);
    }
    
    /* non-pointer node - or already-visited H(node) */
    if (!HasPointers(p))
      (void) get_pointer_counts(p, s_limit, counts);
    
    if (Stacked == 0)
      return 0; /* all done */
    
    p = Pop;
    Assert(HasPointers(p));
    p = T(p);
  }
}


/*
* check_traverse_pointers - accumulate counts for Reach*(p); at same time weaken any pointers to localroot (optional)
*/

static int check_traverse_pointers(pointer p, unsigned s_limit, check_counts *counts, const pointer *localroot)
{
  int res;
  
  /* Assert(is_tree(n)) - NO loops! */
  
  stack_size = s_limit;
  sp = stack = new_table(stack_size, sizeof(pointer));
  
  res = check_traverse_pointers_do(p, s_limit, counts, localroot);
  
  free_table(stack);
  
  return res;
}


/*
 zone_check_travserse_nodes - traverse all nodes in a zone and compare to pointer traverse results
 visit all nodes in all zones; compare node tag and refc to debug_node tag and refc
 
 for each node:
 accumulate strong and weak refc, as found in the node and store into the check node
 check(Tag   == check Tag)
 check(Srefc == check SRefc)
 check(Wrefc == check WRefc)
 check(! (Wrefc >0 && Srefc==0))
 
 returns 0 if all ok, otherwise >0
 usage if (zone_check_traverse_valid(root)) bad; ok;
 
 */

/*
 * zone_check_data - add excess/deficit for given node to "data"
 * returns 0 if there are no discreperancies
 */

static int check_get_node_counts(zone_address z, check_node_data *data)
{
  int res = 0;
  char *suffix = "";
  
  const zone_header *zh = z.z;
  const long off = z.off;
  
        node *np       = zh->nodes       + off;  /* no const to allow ue as arg below (C weirdness) */
  const node *np_check = zh->check_nodes + off;

  const unsigned
  s =       np      ->s_refc,
  s_check = np_check->s_refc,
  w =       np      ->w_refc,
  w_check = np_check->w_refc;
  
  /* accumulate node data, tally mismatch in res_i (taking care not to generate "negative" unsigneds) */
  
  /* refc */
  /* excess: refc without pointer */
  /* deficit: error uncounted pointer */

  /* strong */
  data->total.s += s;
  if (  s !=s_check) {
    res++;
    Log2("!!check_node%s\tstrong refc wrong: check==%u\n", zone_address_info(z, suffix, 0), s_check);
    if (s > s_check)
      data->excess.s  += s - s_check;
    else
      data->deficit.s += s_check - s; /* Assert(s_check > s) */
  }
  
  /* weak */
  data->total.w += w;
  if (  w !=w_check) {
    res++;
    Log2("!!check_node%s\tweak refc wrong: check==%u\n", zone_address_info(z, suffix, 0), w_check);
    if (w > w_check)
      data->excess.w  += w - w_check;
    else
      data->deficit.w += w_check - w; /* Assert(w_check > w) */
  }

  return res;
}

static int check_scan(check_node_data *data)
{
  int res = 0;
  zone_header *z;
  
  /* for a zone, compare any check nodes to the corresponding nodes */
  for (z = zone_current; z; z = z->previous) {
    long off;
    Assert(z->check_nodes);

    /* accumulate node data */
    for (off = 0; off < z->size; off++)
        res += check_get_node_counts((zone_address){z, off}, data);
  }
  return res;
}

/* check_loop
 * follow all strong (and only strong) pointers, counting nodes visited (in *strong_count).
 * iff there is a strong loop then strong_count will exceed s_limit -> report error
 * This function is needed because zone_check_traverse_pointers() only visits Hd/Tl first time through, preventing loop detection in all but pathological cases!
 *
 * usage if (check_loop_do(p, limit)) bad; ok;
 */
/* worker */
static int check_loop_do(pointer p, unsigned s_limit, unsigned strong_count)
{
  
  if (IsNil(p) ||
      IsWeak(p) ||
      ! HasPointers(p))
    return 0;
  
  if ((strong_count++) > s_limit) {
    Debug2("!!zone_loop_check_do: found strong loop size %u: at: %s: \n", strong_count, zone_pointer_info(p));
    zone_loop_report(p, s_limit); // NEW
    return 1;
  }
  
  return (
          check_loop_do(H(p), s_limit, strong_count) ||
          check_loop_do(T(p), s_limit, strong_count));
}

/* wrapper */
#define Limit 24 /* restrict output to this level, unless debug>1 */
static int check_loop(pointer p, unsigned s_limit)
{
  if (check_loop_do(p, s_limit, 0)) {
    out_debug_limit(p, (debug > 1 ? s_limit : Limit));
    return 1;
  }
  return 0;
}

static int check_one_root(pointer root, unsigned s_limit, check_counts *counts, char *info)
{
  int res = 0;

  if (IsNil(root))
    return res;

  if (delete == 1 /* 1985 algorithm */ && IsWeak(root))
      Debug1("!!%s pointer is weak - unexpected\n", info);
    
  res += check_traverse_pointers(root, s_limit, counts, 0/*no localroot*/);
  res += check_loop(root, s_limit);

  Debug2("check_one_root(\"%s\")%s", info, zone_pointer_info(root));
  Debug6(" refc-sum:(s/w %u/%u) pointer-count:(sp/wp %u/%u) refc-excess:(s+/w+ %u/%u)",
         counts->srefc,
         counts->wrefc,
         counts->strong,
         counts->weak,
         counts->srefc - counts->strong,
         counts->wrefc - counts->weak
         );
  Debug3(" (inuse=hdtl+atom %u=%u+%u)\n",
         counts->hdtl + counts->atom,
         counts->hdtl,
         counts->atom
         );

  return res;
}


/*
 zone_check_do - check storage consistency - 'root' is start of the graph; 'freelist' is the start of freelist.
 0. zone check:
 (zone_current != 0)
 check(count of zones == zone_new_count)
 check(sum of size of zones == zone_total_size)
 
 check(zone_inuse_count <= zone_total_size)
 check(zone_free_count <= zone_total_size)
 check(zone_free_count + zone_inuse_count <= zone_total_size)
 check(zone_free_count + zone_inuse_count == (sum of used of zones))
 
 1. check(count of nodes reachable from root and defs == zone_inuse_count)
 2. check(count of nodes reachable from freelist == zone_free_count
 3. check(count of all pointers == sum ALLrefc)
 4. check(count of weak pointers == sum Wrefc)
 5. check(count of strong pointers == sum Srefc)
 
 6. reachability check, recalculate expected reference counts and compare to actual
 
 returns 0 if all ok, otherwise >0
 usage if (zone_check_do(root, freelist)) bad; else ok;
 
 */
static int check_do(pointer root, pointer defs, pointer freelist)
{
#ifdef notyet
  int free_count = 0;
  int inuse_count = 0;
#endif
  
  if (!check)
    return 0; /* ok */
  
  /* zone check */
  {
    unsigned new_count = 0;  /* count of zones */
    long unsigned total_size = 0;  /* sum of size of zones */
    unsigned total_used = 0;  /* sum of used of zones */
    
    zone_header *z;
    
    /* report zone info */
    Debug2("%s\t%u\n", "zones in use", zone_new_count);
    Debug2("%s\t%lu\n", "total zone size", zone_total_size);
    Debug2("%s\t%u\n", "nodes used", zone_inuse_count);
    Debug2("%s\t%u\n", "nodes free", zone_free_count);
    
    /* report and return if no zones in use */
    if (!zone_current) {
      check_log("no zones in use", 1);
      return 0; /* ok */
    }
    
    /* visit zones calculating check values */
    for (z = zone_current; z; z = z->previous) {
      new_count++;
      total_size += z->size;
      total_used += z->used;
    }
    
    /* report and return if variances found */
    if (zone_new_count != new_count)
      return check_log2("!!zone_new_count:%u but found %u\n", 2, zone_new_count, new_count);
    
    if (zone_total_size != total_size)
      return check_log2("!!zone_total_size:%lu but found %lu\n", 3, zone_total_size, total_size);
    
    if (zone_inuse_count > zone_total_size)
      return check_log2("!!zone_inuse_count:%u but zone total size %lu\n", 4, zone_inuse_count, total_size);
    
    if ((zone_inuse_count + zone_free_count) > zone_total_size)
      return check_log2("!!(zone_inuse_count + zone_free_count) > zone_total_size: %u > %u", 5, (zone_inuse_count + zone_free_count), zone_total_size);
    
    if ((zone_inuse_count + zone_free_count) != total_used)
      return check_log2("!!zone_inuse_count + zone_free_count) != (zone_total_used): %u > %u", 5, (zone_inuse_count + zone_free_count), total_used);
    
    /* otherwise report ok and continue */
    Debug4("%s\t%u==%u+%u\n", "zone check ok", total_used, zone_inuse_count, zone_free_count);
    
  }
  
  /* pointer check */
  {
    /* count nodes visited; count strong pointers followed; count weak pointers (not followed) */
    /* uses zone->check_nodes to store working data; for and given (node *)n, check info resides at (node *)(n + z->size) where z is the zone in question */
    /* TODO could loop forever is there is a cycle of strong pointers so limit at zone_total_size nodes */
    /* lemma: with N nodes, any chain of pointers longer than N is a loop */
    
    check_counts theCounts = zero_zone_check_counts, *counts = &theCounts;
    
    int res = 0;
    int is_tree = 0;
    
    check_reset();  /* clear all check_nodes for use */
    
    /* check the counts and populate check in use nodes */
    res += check_one_root(root, zone_inuse_count*2, counts, "root"); /* root - the program graph */
    res += check_one_root(defs, zone_inuse_count*2, counts, "defs"); /* defs - saved definitions */
    res += check_one_root(builtin, zone_inuse_count*2, counts, "builtin"); /* sasl - builtin definitions */
    {
      pointer *sp;
      for (sp = make_sp; sp > make_stack; sp--) {
        res += check_one_root(*sp, zone_inuse_count*2, counts, "make_stack");
      }
    }
        
    Debug2("%s\t%u\n", "nil pointers",    counts->nil );
    Debug2("%s\t%u\n", "strong pointers", counts->strong );
    Debug2("%s\t%u\n", "strong refc sum",     counts->srefc );
    Debug2("%s\t%u\n", "weak pointers",   counts->weak );
    Debug2("%s\t%u\n", "weak refc sum",       counts->wrefc );
    Debug2("%s\t%u\n", "struct nodes",    counts->hdtl );
    Debug2("%s\t%u\n", "atom nodes",      counts->atom );

    /* 1. check(count of nodes reachable from {root,defs,builtin} == zone_inuse_count) */
    if (zone_inuse_count != (counts->hdtl + counts->atom))
      Debug4( "!!inuse count: %u but found %u==(%u+%u)\n", zone_inuse_count, (counts->hdtl + counts->atom), counts->hdtl, counts->atom);
    
    /* 1a. check for tree structure: strong_count == zone_inuse_count */
    if (zone_inuse_count == counts->strong)
      is_tree = 1;
    
    /* traverse freelist adding to free_counts, check them , then add to total */
    /* freelist - list of available nodes not used by the program graph */
    {
      check_counts theCounts = zero_zone_check_counts, *free_counts = &theCounts;
      res += check_one_root(freelist, (zone_inuse_count + zone_free_count)*2, free_counts, "freelist"); /* freelist - chain of free nodes */
      
      /* 2. check(count of nodes reachable from freelist == zone_free_count */
      /*
       assert(strong_count == zone_free_count)
       assert(struct_count == zone_free_count)
       assert(nil_count == zone_free_count) - each node - has one NIL pointer - last has two
       assert(weak_count == 0)
       assert(atom_count == 0)
       */
      if (free_counts->strong != zone_free_count)
        Debug2("!!free:%u but free count:%u\n", free_counts->strong, zone_free_count);
      
      if (free_counts->hdtl != zone_free_count)
        Debug2("!!free structures:%u but free count:%u\n", free_counts->hdtl, zone_free_count);
      
      if(IsSet(freelist) &&
         (free_counts->nil != zone_free_count +1))
        Debug2("!!free nil count:%u but expecting:%u\n", free_counts->nil, zone_free_count +1);
      
      if (free_counts->weak != 0)
        Debug1("!!freelist contains %u weak pointers\n", free_counts->weak);
      
      if (free_counts->atom != 0)
        Debug1("!!freelist contains %u atoms\n", free_counts->atom);
      
      /* increment totals for whole-store checks below "counts += free_counts;"*/
      check_counts_add(counts, free_counts);
    }
    
    if (res)
      return check_log2("!!check pointer scan failed\n", 66, res, 0);
    
    if ((counts->strong + counts->weak)  == (counts->hdtl + counts->atom))
      Debug6("%s\t(%u+%u)==(%u+%u)==%u\n", (is_tree ? "pointer check ok (tree)" : "pointer check ok"),
                     counts->strong, counts->weak, counts->hdtl, counts->atom, (counts->strong + counts->weak));
    if (counts->strong != counts->srefc)
      Debug2("!!strong pointers:%u but srefc %u\n", counts->strong, counts->srefc);
    if (counts->weak != counts->wrefc)
      Debug2("!!weak pointers:%u but wrefc %u\n", counts->weak, counts->wrefc);

    /* inspect the check nodes in every zone and report discreprancies  */
    {
      check_node_data data = zero_data;
      
      res += check_scan(&data);
      
      Debug2("%s\t%u\n", "strong ref counts",  counts->strong );
      Debug2("%s\t%u\n", "weak ref counts",  counts->weak );
      
      /* 3. check(count of all pointers == sum ALLrefc) */
      if ((counts->strong + counts->weak) != (data.total.s + data.total.w))
        Debug2("!!pointers:%u but ALLrefc %u\n", (counts->strong + counts->weak), (data.total.s + data.total.w));
      
      /* 4. check(count of weak pointers == sum Wrefc) */
      if (counts->strong != data.total.s)
        Debug2("!!strong pointers:%u but Srefc %u\n", counts->strong, data.total.s);

      /* 5. check(count of strong pointers == sum Srefc) */
      if (counts->weak != data.total.w)
        Debug2("!!weak pointers:%u but Wrefc %u\n", counts->weak, data.total.w);

      /* 6. reachability check, recalculate expected reference counts and compare to actual  */
      if (data.deficit.s)        Debug1("!!deficit of %u strong pointers\n",  data.deficit.s);
      if (data.deficit.w)        Debug1("!!deficit of %u weak pointers\n",    data.deficit.w);
      if (data.excess.s)         Debug1("!!excess of %u strong pointers\n",   data.excess.s);
      if (data.excess.w)         Debug1("!!excess of %u weak pointers\n",     data.excess.w);

      if ((counts->strong + counts->weak) == (data.total.s + data.total.w))
        Debug6("%s\t(%u+%u)==(%u+%u)==%u\n",
               (res ?"reference count check failed" : "reference count check ok"),
                       counts->strong,
                       counts->weak,
                       data.total.s,
                       data.total.w,
                       (counts->strong + counts->weak));
      if (res)
        return check_log2("!!check traverse nodes found errors\n", 66, res, 0);
    }
  }
  
  return 0; /* ok */
}

/*
 * zone_check_deletion(pointer n) - check from "first principals" whether n is free
 *
 * returns number of nodes free in "Reach*(n)"
 *
 * NB resets the check nodes
 *
 * Note - after this everything to be deleted is marked fordeletion_t check-tag;  no further "recursive" refc_delete() would ve required to reclaim all storage freed - this is an advantage vs conventional refc.
 *
 * Insight: free nodes are not reachable from any x in Reach*(n) which is protected by external pointers, and external pointers can be detected by testing (ALLRefc(x) < ALLRefc(check_x))
 *
 * Method:
 *  examine nodes x in Reach*(n)
 *    populate check nodes refc and tag pointers within reach n; making a note of "count(x) and sum(ALLrefc(x) and count(pointer_reach)"
 *
 *  re-examine nodes x in Reach*(n)
 *    at node x:
 *    if ALLRefc(x) < ALLRefc(x_check)
 *      then setReachable(x) and setReachable(Reach(x)) -- stopping at atoms, or when already-reacahble found
 *      else visit(Reach(x)
 *      mark "reachable" if ALLRefc(x) < ALLRefc(x_check) where x_check is the check node for x populated in previous step
 *  when done, non-reachable check nodes are the ones to be deleted; make a note of "count(Reach(n)|not reachable)"
 *
 * Implementation - logic:
 *  examine Reach*(n) == counts = check_traverse_pointers(n):
 *    (a) count(x)          == counts.atom   + counts.hdtl
 *    (b) sum(ALLrefc(x))   == counts.srefc  + counts.wrefC
 *    (c) count(ptr_reach)  == counts.strong + counts.weak
 *
 *    reach = (a)
 *    external pointers = (b) - (c)
 
 *  re-examine x in Reach*(n) == seach from n
 *      [Assert(Tag(n_check)) a priori not {deletion, inuse} for all Reach*(n))]
 *
 *  if Tag(x_check == (inuse, deletion)) done. * already visited * else
 *  if ALLRefc(x > ALLrefc(x_check) {recursively mark all y in Reach*(x): change_tag(y, inuse_t) else
 *  mark fordeletion_t; continue to examine H(x), T(x)
 *
 * n_check has three "labels"
 * 1. visited by get_pointer_counts
 * 2. visited by check inuse and candidate for deletion
 * 3. visited by check inuse and inuse
 *
 * Implementation - data:
 * all Reach*(n), Tag(n_check) is set to "1" by "get_pointer_counts()"
 * in check_count_inuse() changes allowed are
 * 1 -> 2   visited -> fordeletion_t
 * 1 -> 3   visited -> inuse_t
 * 2 -> 3   fordeletion_t -> inuse_t
 * After check_count_inuse(), all Reach*(n) is in either 1 or 2 - reachable node are partitiioned into "free" and "inuse".
 *
 * Implementation - check tags - only for use in checking here, not for use on non-check nodes.
 * 1 == visited_t
 * 2 == fordeletion_t
 * 3 == inuse_t
 * 4 == willdelete_t - confirmation that fordeletion will be deleted - deprecated
 *
 * Additional method - double check which nodes are to be freed
 * all Reach*(n) where Tag(n_check) == "2", set to "1" and count
 *  "2" -> "4"
 * afterwards all *directly reachable* "2" have become "1"
 * Lemma Nodes which are free, are in chain of free nodes from n, none other.
 *
 * Implememtation:
 *  recursive scan from n setting fordeletion_t to willfree_t and counting total
 */

unsigned check_set_inuse(pointer n)
{
  unsigned inuse;  /* count of unique nodes set to inuse in Reach*(n) (1->3 or 2->3 in "label" terminology) */
  pointer n_check;
  
  if (IsNil(n))
    return 0;
  
  n_check = zone_check_pointer_of(n);
  
  if (Tag(n_check) == inuse_t)
    return 0; /* reachable and already visited */
  
  Tag(n_check) = inuse_t;
  inuse = 1; /* mark as reachable */
  if (debug > 1)  Debug1("check_set_inuse:%s\n", zone_pointer_info(n));

  if (HasPointers(n)) {
    inuse += check_set_inuse(Hd(n));
    inuse += check_set_inuse(Tl(n));
  }
  return inuse;
}

/* visit recursively, weakening any pointers to start; returns of pointers still to be weakened (<=todo) */
/* todo is number of pointers required to be weakened - stop at zero */
unsigned check_weaken_loop(pointer start, pointer *pp, check_counts *counts, unsigned todo)
{
  const pointer p = *pp;
  if (IsNil(p)
      || !todo
//
//|| IsWeak(p) //XX added to prevent inifinite loops - but does it get the job done??? NO NO NO!
//
      )
    return 0;
  
  if (SameNode(start, p)) {
    zone_weaken_withcounts(pp, counts);
    return --todo;
  }

  if (HasPointers(p) &&
      Tag(zone_check_pointer_of(p)) != inuse_t //XX added to prevent inifinite loops - but does it get the job done??? NO NO NO!
      ) {
    todo -= check_weaken_loop(start, &Hd(p), counts, todo);
    if (todo)
      todo -= check_weaken_loop(start, &Tl(p), counts, todo);
  }
  return todo;
}

/* xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx */
/* traverse pointers staring with *pp - if pointed-to node already in use weaken *pp, otherwise set it inuse, strengthen *pp and recurse, return: how many inuse */
unsigned set_inuse_2021algorithm(pointer *pp, check_counts *counts)
{
  unsigned inuse = 0;  /* count of unique nodes set to inuse in Reach*(n) (1->3 or 2->3 in "label" terminology) */
  pointer p_check, p = *pp;
  
  if (IsNil(p))
    return 0;
  
  p_check = zone_check_pointer_of(p);
  
  if (Tag(p_check) == inuse_t) {
    zone_weaken_withcounts(pp, counts);
    return 0; /* reachable and already visited */
  }
  
  Tag(p_check) = inuse_t; /* mark as reachable */
  inuse = 1;
  zone_strengthen_withcounts(pp, counts);
  if (debug > 1)  Debug1("set_inuse_2021algorithm:%s\n", zone_pointer_info(*pp));

  if (HasPointers(p)) {
    inuse += set_inuse_2021algorithm(&Hd(p), counts);
    inuse += set_inuse_2021algorithm(&Tl(p), counts);
  }
  return inuse;
}


unsigned free_2021algorithm(pointer *pp, tag t)
{
  unsigned free = 0;
  pointer p = *pp, p_check;
  
  if (IsNil(p))
    return 0;
  
  zone_erase(pp);
  
  p_check = zone_check_pointer_of(p);
  if (Tag(p_check) == t)
    return 0;
  
  if (debug > 1)  Debug1("free_2021algorithm:%s\n", zone_pointer_info(p));
  if (HasPointers(p)) {
    free += free_2021algorithm(&Hd(p), t);
    free += free_2021algorithm(&Tl(p), t);
  }
  if (ALLrefc(p) == 0) { /* erasing last pointer frees node*/
    free++;
    free_node(p);
  }
  return free;
}


/*
 * zone_recycle() erase pointer pp and when tag t, recurse and free node when last pointer deleted; return number of nodes freed
 * only to be called *after* setting check tags via set_inuse_2021algorithm() or check_set_inuse().
 */
unsigned zone_recycle(pointer *pp, tag t)
{
  pointer p = *pp, p_check;
  unsigned free = 0;

  if (IsNil(p) || Tag(p) == free_t)//*xxWHY is this needed for 99.2 etc */)
    return 0;

  zone_erase(pp);
  
  p_check = zone_check_pointer_of(p);
  if (Tag(p_check) != t)
    return 0;
  
  if (HasPointers(p)) {
    free  = zone_recycle(&Hd(p), t);
    free += zone_recycle(&Tl(p), t);
  }
  
  if (ALLrefc(p) == 0 && Tag(p) != free_t) { /* last pointer and not already freed eg by a following loops */
    free_node(p);
    free++;
  }
  return free;
}



/* xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx */

/* extra check - visit fordeletion_t nodes in Reach*(n) - only - setting to willfree_t (only *directly* reachable from n) */
static unsigned check_set_willfree(pointer n)
{
  unsigned willfree = 0;
  pointer n_check;
  
  if (IsNil(n))
    return 0;

  n_check =  zone_check_pointer_of(n);

  if (Tag(n_check) != fordeletion_t)
    return 0;
  
  willfree = 1;
  Tag(n_check) = willfree_t;
  if (debug > 1)  Debug1("check_set_willfree:%s\n", zone_pointer_info(n));
  
  if (HasPointers(n)) {
    willfree += check_set_willfree(Hd(n));
    willfree += check_set_willfree(Tl(n));
  }
  return willfree;
}


// NEEDS to be made ITERATIVE!
static unsigned check_count_inuse(pointer n)
{
  pointer n_check;
  
  if (IsNil(n))
    return 0;
  
  n_check = zone_check_pointer_of(n);

  switch (Tag(n_check)) {
    case fordeletion_t:
    case inuse_t:
      return 0;
    default:
      Tag(n_check) = fordeletion_t;  /* can change later to inuse_t (2->3 in above terminology) */
  }
  
  if (ALLrefc(n) > ALLrefc(n_check)) { /* protected by external pointers */
    unsigned inuse = check_set_inuse(n);
    Log4("check_count_inuse%s (s+/w+ %u/%u) inuse=%u\n", zone_pointer_info(n), Srefc(n) - Srefc(n_check),
     Wrefc(n) - Wrefc(n_check), inuse);
    return inuse; /* Reach*(n) inuse */
  }
  if (HasPointers(n))
    return check_count_inuse(Hd(n)) + check_count_inuse(Tl(n));
  return 0;
}

/*
  9. Visit Reach*(n) again,
    1. examine noteboook for externals s
    2. at externals s
        1. add s to notebook inuse ‘list’. Examine pointers from s
            1. if they point to node x already in notebook inuse weaken
            2. if they don’t point node x already to notebook inuse, strengthen pointer and add x to noteboook inuse, recursively visit x
*/
static unsigned find_inuse_2021algorithm(pointer *pp, check_counts *counts)
{
  pointer p, p_check;
  p = *pp;
  if (IsNil(p))
    return 0;
  p_check = zone_check_pointer_of(p);
  if (debug > 1)
    Debug4("find_inuse_2021algorithm%s refc-excess:(s+/w+ %u/%u) %d\n", zone_pointer_info(p), Srefc(p) - Srefc(p_check), Wrefc(p) - Wrefc(p_check), Tag(p_check));

  switch (Tag(p_check)) {
    case fordeletion_t:
    case inuse_t:
      return 0;
    case visited_t:
    default:
      Tag(p_check) = fordeletion_t;  /* may change later to inuse_t (2->3 in above terminology) */
  }
  if (ALLrefc(p) > ALLrefc(p_check)) { /* protected by external pointers */
    unsigned inuse = 1;
    Debug4("localroot-start%s refc-excess:(s+/w+ %u/%u) inuse=%u\n", zone_pointer_info(p), Srefc(p) - Srefc(p_check), Wrefc(p) - Wrefc(p_check), inuse);
    
    Tag(p_check) = inuse_t;
    
    if (HasPointers(p)) {
      if (Srefc(p) == Srefc(p_check) /* && (Wrefc(p) > Wrefc(p_check))*/) { /* protected only by weak exxternal pointers */
        zone_flip_withcounts(p, counts);
      }
      zone_weaken_withcounts(pp, counts); /* strong loop avoidance */
      
      inuse += set_inuse_2021algorithm(&Hd(p), counts);
      inuse += set_inuse_2021algorithm(&Tl(p), counts);
    }
    Debug4("localroot-done%s refc-excess:(s+/w+ %u/%u) inuse=%u\n", zone_pointer_info(p), Srefc(p) - Srefc(p_check), Wrefc(p) - Wrefc(p_check), inuse);
    return inuse; /* Reach*(p) inuse */
  }
//  if (ALLrefc(p) > ALLrefc(p_check)) { /* protected by external pointers */
//    unsigned inuse = 1;
//    Tag(p_check) = inuse_t;
//    if (HasPointers(p)) {
//      unsigned s = Srefc(p) - Srefc(p_check);
//      unsigned w = Wrefc(p) - Wrefc(p_check);
//      Debug3("localroot%s refc-excess:(s+/w+ %u/%u)\n", zone_pointer_info(p), s, w);
//      //      WIP WIP-start
//      if (s == 0) {
//        /* >>> loop protected only by weak pointer <<< */
//        unsigned w_check = Wrefc(p_check);
//        Assert(w);
//        zone_flip(p);
//        if (w_check) { /* n has weak "internal" as well as being protected by weak "external" pointer(s) */
//          unsigned w2;
//          Log1("2021 algorithm - potential issue %u internal weak references\n", w_check);
//          w2 = check_weaken_loop(p, &Hd(p), counts, w_check);
//          w2 = check_weaken_loop(p, &Tl(p), counts, w2);
//          if (w2)
//            Log1("!!localroot problem %u internal weak not weakened ", w2);
//          Log3("localroot-weak%s weakened:(w2/w_check %u/%u)\n",zone_pointer_info(p), w2, w_check);
//        }
//      }
//      //      WIP WIP-end
//      inuse += set_inuse_2021algorithm(&Hd(p), counts);
//      inuse += set_inuse_2021algorithm(&Tl(p), counts);
//      Debug3("localroot-done%s refc-excess:(s+/w+ %u/%u)\n", zone_pointer_info(p), Srefc(p) - Srefc(p_check), Wrefc(p) - Wrefc(p_check));
//    }
//    Log4("find_inuse_2021algorithm%s (s+/w+ %u/%u) inuse=%u\n", zone_pointer_info(p), Srefc(p) - Srefc(p_check),
//         Wrefc(p) - Wrefc(p_check), inuse);
//    return inuse; /* Reach*(p) inuse */
//  }

  
  if (HasPointers(p))
    return find_inuse_2021algorithm(&Hd(p), counts) + find_inuse_2021algorithm(&Tl(p), counts);
  return 0;
}



/* +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ */
static unsigned check_count_inuse_do(pointer n)
{
  pointer n_check, start = NIL; /* stop on RE-reaching initial "n" node */
  unsigned inuse = 0;   /* count of nodes in use - node protected by external pointers *and their Reach* */
  Debug1("check_count_inuse_do: stack_size %u\n", stack_size);//xx
  while (1) {
    while (HasPointers(n)) {
      /* pointer nodes */
      if (SameNode(n, start))
        break;
      if (IsNil(start)) /* first time through */
        start = n;
      
      /* visit pointer node */
      n_check = zone_check_pointer_of(n);
      if (ALLrefc(n) > ALLrefc(n_check)) {
        inuse += check_set_inuse(n);
        break;
      }
      Debug1("%lu ", (sp-stack)); //xx
      Push(n);
      n = H(n);
    }
    
    if (IsSet(n) && !HasPointers(n)) {
      /* visit non-pointer node */
      n_check = zone_check_pointer_of(n);
      if (ALLrefc(n) > ALLrefc(n_check))
        inuse += check_set_inuse(n);
    }
    
    if (Stacked == 0)
      return inuse; /* all done */
    
    n = Pop;
    Assert(HasPointers(n));
    n = T(n);
  }
}

static unsigned check_count_inuseNEW(pointer n)
{
  unsigned inuse;
  
  stack_size = zone_inuse_count*2;
  sp = stack = new_table(stack_size, sizeof(pointer));
  
  inuse = check_count_inuse_do(n);

  free_table(stack);
  
  return inuse;
}

#undef Stacked
#undef Push
#undef Pop

/* +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ +++ */

unsigned zone_check_deletion(pointer n)
{
  unsigned
    reach,  /* count of nodes reachable from n, including n */
    externals, /* sum of "external" references */
    free,   /* count of nodes which are free */
    inuse;  /* count of nodes which are not free - reach == free + inuse */
  check_counts theCounts = zero_zone_check_counts; /* counts set by check_one_root() */
  pointer n_check;
  
  if (!check_deletions ||IsNil(n))
    return 0;

  n_check = zone_check_pointer_of(n);
  
  /* populate check nodes in Reach*(n) */
  // NB weak ptr n will generate !!error mesg in here; but should never get here with weak pointer anyway due to way algorithm works
  check_reset();
  if (check_one_root(n, zone_inuse_count*2, &theCounts, "deletion check"))
    Log1("deletion-check problem%s", zone_pointer_info(n));  // temporary strong look during pointer "move" update eg for I_comb stacked==2
//    err_zone("zone_check_deletion: problem traversing deletion nodes");
  
  /* NB "n" has been counted by check_one_root() as a root pointer, which is it not, so need to offset pointer count and refc sum by 1 */
  if (IsStrong(n)) {
    (theCounts.strong)--; Srefc(n_check)--;
  } else {
    (theCounts.weak)--;   Wrefc(n_check)--;
  }
  
  Debug7("zone_check_deletion%s refc-sum:(s/w %u/%u) pointer-count:(sp/wp %u/%u) refc-excess:(s+/w+ %u/%u)",
         zone_pointer_info(n),
         theCounts.srefc,
         theCounts.wrefc,
         theCounts.strong,
         theCounts.weak,
         theCounts.srefc - theCounts.strong,
         theCounts.wrefc - theCounts.weak
         );
  Debug5(" root-excess:(s+/w+ %u/%u) (inuse=hdtl+atom %u=%u+%u)\n",
         Srefc(n) - Srefc(n_check),
         Wrefc(n) - Wrefc(n_check),
         theCounts.hdtl + theCounts.atom,
         theCounts.hdtl,
         theCounts.atom
         );
  
  if (theCounts.srefc < theCounts.strong)
    err_zone("zone_check_deletion: more strong pointers than counts");
  if (theCounts.wrefc < theCounts.weak)
    err_zone("zone_check_deletion: more weak pointers than counts");
  
  externals = (theCounts.srefc + theCounts.wrefc) - (theCounts.strong + theCounts.weak); /* sum of counted references - count of pointers */
  reach = (theCounts.atom  + theCounts.hdtl); /* count of nodes, including n */

  /* short cut 1 - if there are no external references, Reach*(n) must be free */
  if (externals == 0)
    return reach;
  
  /* short cut 2 - if there are external references to n, then n *and Reach(n)* is inuse */
  if (ALLrefc(n) > ALLrefc(n_check))
    return 0; //NEW 2021-02-01
  
#define check_count_inuse(n,x) check_count_inuseNEW(n)//xxxxxxxxxxxxxxxxxxxxx
#undef check_count_inuse
  /* there are external pointers, but not at n; how many inuse nodes do they protect? */
  inuse = check_count_inuse(n);          /* count of nodes protected by external pointer */
  if (inuse > reach)
    err_zone("zone_check_deletion: problem traversing deletion pointers");
  
  free = reach - inuse; /* count of nodes that are free */
  if (free && Tag(n_check) != fordeletion_t) /* if any nodes are free, then n is one of them */
    err_zone("zone_check_deletion: local root deletion problem");
  
  {
    /* extra check - count the fordeletion_t nodes directly reachable from n (ie no intervening inuse_t)*/
    unsigned willfree = check_set_willfree(n);
    if (willfree != free)
      Log2("!!zone_check_deletion: expected %u willfree but got %u\n", free, willfree);
  }

  Log3("zone_check_deletion%s %u nodes out of %u will be freed\n",
       zone_pointer_info(n),
       free,
       reach);

  return free;
}

/*
 
 When last strong pointer has been deleted, but there are still weak references:

 2021 algorithm()  <<xx needs updating for reality as tested below ....>>
 
 Implementation - when deletion gives Wrefc>0 Srefc=0
 1. Flip n making all Weak into Strong (so zero Wrefc)
 2. Search from n
     1. summing refc
     2. counting pointers
     3. weakening pointers to n
 3. If n has Strong pointers, all done - they protect n and reach n.  n in use nothing else to do.
 4. If n has no Strong pointers, and the number of pointers found is equal to the sum of refc, then n and Reach*(n) are all free.   No need for recursive deletion, they can simply be recycled from the notebook.
 5. Otherwise, n has no Strong pointers and so has no external pointers, but there are external pointers, they must be elsewhere.
 6. Clear the notebook
 7. Visit Reach*(n) [this could be done as part of 2 to save visits at cost of earlier use of notebook]
     1. in the notebook count times x is visited via Strong and via Weak local refc and status mayfree
 8. After Protected nodes will have refc > local refc
 9. Visit Reach*(n) again,
     1. examine noteboook  for externals
     2. at externals s - ensure at least one external is strong
         1. add s to notebook inuse ‘list’. Examine pointers from s
             1. if they point to node x already in notebook inuse weaken
             2. if they don’t point node x already to notebook inuse, strengthen pointer and add x to noteboook inuse, recursively visit x
 10. If n has Strong pointers, n inuse and all done.
 11. If n has no Strong pointers, some part of Reach*(n) is free - recursively delete pointers from n recycling nodes where refc==0. Done.  No need for recusive deletion they can simply be freed.
     1. Nodes to be recycled will be have notebook status mayfree - which may help implementation, but not necessary.
 12. Discard notebook
 */

void zone_delete2021(pointer n)
{
  //  unsigned inuse;
  unsigned
  reach,  /* count of nodes reachable from n, including n */
  externals, /* sum of "external" references */
  free,   /* count of nodes which are free */
  inuse;  /* count of nodes which are not free - reach == free + inuse */
  check_counts theCounts = zero_zone_check_counts; /* counts set by check_one_root() */
  pointer n_check;
  pointer *shortcut = 0; /* search shortcut - weakens pointers during search */
  
  if (Srefc(n) > 0) /* n is not free */
    return;
  
  if (Wrefc(n) == 0) { /* n is free - Allrefc(p) == 0 */
    if (HasPointers(n)) {
      extern void refc_delete(pointer *pp);
      refc_delete(&H(n));
      refc_delete(&T(n));
    }
    free_node(n);
    return;
  }
  
  /* Srefc == 0 && Wrefc > 0 more to do */
  /*  1. Flip n making all Weak into Strong (so zero Wrefc) */
  zone_flip(n);  /* Now; Srefc > 0 && Wrefc == 0 */
  
  /* 2. Search  from n - and populate local refc
   *   1. summing refc
   *   2. counting pointers
   *   3. weakening pointers to n
   */
    check_reset();
  {
    int s = IsStrong(n);
//    PtrBit(n) = !NodeBit(n);//xx
//    Assert(IsWeak(n));//xx
    
//xx    shortcut = n;//xx 2021-02-27 TEMP no weakening  in check_traverse_pointers()
    if (check_traverse_pointers(n, zone_inuse_count*2, &theCounts, shortcut))
      err_zone("zone_delete2021: problem traversing deletion nodes");
    
    /* NB "n" has been counted (but not weakened) by check_traverse_pointers() as a root pointer, which is it not, so need to offset check pointer count and check refc sum by 1 */
    n_check = zone_check_pointer_of(n);
    if (s) {
      (theCounts.strong)--;
      Srefc(n_check)--;
    } else {
      (theCounts.weak)--;
      Wrefc(n_check)--;
    }
  }//xx sometimes root-excess is shown incorrect after this ... problem with "stale" n pointer?
  
  Debug7("zone_delete2021%s refc-sum:(s/w %u/%u) pointer-count:(sp/wp %u/%u) refc-excess:(s+/w+ %u/%u)",
         zone_pointer_info(n),
         theCounts.srefc,
         theCounts.wrefc,
         theCounts.strong,
         theCounts.weak,
         theCounts.srefc - theCounts.strong,
         theCounts.wrefc - theCounts.weak
         );
  Debug5(" root-excess:(s+/w+ %u/%u) (inuse=hdtl+atom %u=%u+%u)",
         Srefc(n) - Srefc(n_check),
         Wrefc(n) - Wrefc(n_check),
         theCounts.hdtl + theCounts.atom,
         theCounts.hdtl,
         theCounts.atom
         );
  Debug4("(s/w %u/%u) (s_check/w_check %u/%u)\n", Srefc(n), Wrefc(n), Srefc(n_check), Wrefc(n_check));
  
  if (theCounts.srefc < theCounts.strong)
    err_zone("zone_check_deletion: more strong pointers than strong counts");
  if (theCounts.wrefc < theCounts.weak)
    err_zone("zone_check_deletion: more weak pointers than weak counts");
  
//xx  { extern unsigned refc_delete_post_search_log(pointer p, unsigned free); refc_delete_post_search_log(n , log); }//xx deprecated
  
  /*  3. If n has Strong pointers, all done - they protect n and reach n.  n in use nothing else to do. */

  if (shortcut && Srefc(n) > 0) /* pointers-to n have been weakend, and Srefc > 0, so nothing free*/
    return;
  
  /* 4. If n has no Strong pointers, and the number of pointers found is equal to the sum of refc, then n and Reach*(n) are all free.   No need for recursive deletion, they can simply be recycled. */
  externals = (theCounts.srefc + theCounts.wrefc) - (theCounts.strong + theCounts.weak);
  reach = (theCounts.atom  + theCounts.hdtl); /* count of nodes, including n */
  
  if (externals == 0) {
    Log1("zone_delete2021%s no externals, so all free\n", zone_pointer_info(n));
    if (HasPointers(n)) { /* recycle all visited nodes */
      zone_recycle(&Hd(n), visited_t);
      zone_recycle(&Tl(n), visited_t);
    }
    if (Tag(n) != free_t)
      free_node(n);
    return;
  }
  
  /*  5. Otherwise, n has no Strong pointers and so has no external pointers, but there are external pointers, they must be elsewhere.
   6. Clear the notebook
   7. Visit Reach*(n) [this could be done as part of 2 to save visits at cost of earlier use of notebook]
   1. in the notebook count times x is visited via Strong and via Weak local refc and status mayfree
   8. After Protected nodes will have refc > local refc
   9. Visit Reach*(n) again,
   1. examine noteboook for externals s
   2. at externals s
   1. add s to notebook inuse ‘list’. Examine pointers from s
   1. if they point to node x already in notebook inuse weaken
   2. if they don’t point node x already to notebook inuse, strengthen pointer and add x to noteboook inuse, recursively visit x
   */
  
  inuse  = find_inuse_2021algorithm(&n, &theCounts); /* theCounts invalidated now˚ */
  free = reach - inuse; /* count of nodes that are free */
  //xx start extra
//  if (check_loop(root, zone_inuse_count*2))
//    err_zone("delete - loop");
//
//  //  if (zone_check()) // destroys x_check info
//  //    err_zone("delete - check");
//  Debug("delete - no loop\n");
  //xx end extra
#if xx2021algorithm_fail
//  /*  10. If n has Strong pointers, n inuse and all done. */ //XXX ALogorithm fail - NO
//  if (Srefc(n) > 0) {
//    if (inuse != reach)
//      Debug2("delete - problem setting in use vs reach %u %u\n",  inuse, reach);
//    //      err_zone("delete - problem setting in use");
//    Log1("zone_delete2021%s in use - protected by localroot elsewhere\n", zone_pointer_info(n));
//    return;
//  }
#endif
  /* 10. If nothing is free, all done */
  if (!free) {
      if (inuse != reach)
         Log2("!!delete - none free, problem setting in use vs reach %u %u\n",  inuse, reach);
       //      err_zone("delete - problem setting in use");
       Log1("zone_delete2021%s in use - protected by localroot elsewhere\n", zone_pointer_info(n));
       return;
  }

//  /*  11. If n has no Strong pointers, some part of Reach*(n) is free - recursively delete pointers from n recycling nodes where refc==0. Done.  No need for recusive deletion they can simply be freed.
  /*  11. If there are free, then n and some part of Reach*(n) is free - recursively delete pointers from n recycling nodes where refc==0. Done.  No need for recusive deletion they can simply be freed.
   1. Nodes to be recycled will be have notebook status mayfree - which may help implementation, but not necessary.
   */
  if (HasPointers(n)) {
    unsigned freed = 0;
    Log2("zone_delete2021%s no local externals, %u free\n", zone_pointer_info(n), free);
    freed += zone_recycle(&Hd(n), fordeletion_t);
    freed += zone_recycle(&Tl(n), fordeletion_t);
    if (freed != free)
      Log3("!!zone_delete2021%s error freed %u out of %u free\n", zone_pointer_info(n), freed, free);
  }
  if (Tag(n) != free_t)
    free_node(n);
  
  return;
  /* 12. Discard notebook - nothing to do */
}


/* externally usable version */
int zone_check()
{
  return check_do(root, defs, zone_freelist);
}

/*
 * +reporting++reporting++reporting++reporting++reporting++reporting++reporting+
 */

void new_zone_log_report(FILE *where)
{
  /* ToDo */
  return;
}

/*
 * zone_log_report(where)
 *  print tab separated listing of new nodes created, appending a TOTAL, checking tat the total tallies, and printing an ERROR line if not
 */
void zone_log_report(FILE *where)
{
  int i, check=0;
  
  if (! logging)
    return;
  
  /* first check storage for consistency */
  if (zone_check())
    Error("zone_check error - continuing anyway\n");/*deprecated*/
  
  (void) fprintf(where,"operator\tnew\n");
  
  for (i=0; i<TagCount; i++)
    if (new_tags[i] > 0) {
      (void) fprintf(where,"%s\t%u\n", err_tag_name((tag) i), new_tags[i]);
      check += new_tags[i];
    }
  
  (void) fprintf(where,"Total\t%u\n", new_count);
  
  if (check != new_count)
    (void) fprintf(where, "ERROR -->\t%u\n",check);
}

