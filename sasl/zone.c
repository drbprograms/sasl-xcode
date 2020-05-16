/*
 storage allocation - bare metal
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"

#include "zone.h"

const refc_pair zero_refc_pair = {0,0};
const zone_check_node_data zero_data = {unknown, 0};

typedef struct {
  unsigned
  nil,  /* number of nil pointers, including root, refc_free */
  strong,    /* number of strong pointers, including root, refc_freelist if they are non-nil */
  weak,    /* number of weak pointers */
  hdtl,
  atom;
} zone_check_counts;
const zone_check_counts zero_zone_check_counts = { 0 };

/* Helper - "a = b; add counts add counts from second arg to counts in first arg */
void zone_check_counts_add(zone_check_counts *a, zone_check_counts *b)
{
  a->nil   += b->nil;
  a->strong+= b->strong;
  a->weak  += b->weak;
  a->hdtl  += b->hdtl;
  a->atom  += b->atom;
  return;
}

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
zone_header zero_zone_header = {0};

typedef struct {
  zone_header *z; /* zone of node */
  long off;         /* offset within that zone */
} zone_address;
zone_address zero_zone_address = {&zero_zone_header , 0};

static unsigned zone_new_count = 0;  /* total number of zones created by new_zone() */
static long unsigned zone_total_size = 0; /* sum of zone->size for all zones created by new_zone() */
static unsigned zone_seq = 1; /* sequence number counting how many new zones hav been made. Acts as unique id for zone_pointer_info() */

/* zone_header points to the most recently created zone */
static zone_header *zone_current = 0;  /* zone currently being used by new_node() to create nodes */

static long unsigned zone_size_default = 1024L;  /* to be updated elsewhere */

void zone_new_log(zone_header *z)
{
  Debug3("zone_new: seq: %u\tsize:\t%lu\t%s\n", z->seq, z->size, z->check_nodes ? " with checking" : "") ;
  return;
}

void new_zone_log_report(FILE *where)
{
  /* ToDo */
  return;
}

/*
 zone_of_node - look-up the zone in which a given node resides - works for check nodes also
 */
zone_header *zone_of_node(node *n)
{
  zone_header *z;
  
  for (z = zone_current; z; z = z->previous) {
    if ((n >= z->nodes) && (n < (z->nodes + z->used)))
      return z;
    
    if (z->check_nodes &&
        (n >= z->check_nodes) && (n < (z->check_nodes + z->used)))
      return z;
    
  }
  return (zone_header *)0;
}

/*
 * zone_id - return unique string identifying pointed-to node, prepend '*' for weak pointers only
 *
 * NB this is no re-entrant so this will fail to produce correct results:
 *   printf("%s %s", zone_id(p1), zone_id(p2);
 */

#define Label(x) ((IsSet(x) && IsWeak(x)) ? "@" : "")
#define MAX 256 /*!!*/
static char s[MAX];

/*
 * zone_opinter_zone_detail - get non-NIL pointer's zone, zone seq and offset in zone
 * returns 0 for success
 * if chk then check node info is returned
 */
#define Limit 24 /* restrict output to this level, unless debug>1 */
static int zone_pointer_detail(pointer p, zone_address *zp)
{
  *zp = zero_zone_address;
  
  if (IsNil(p))
    return 0;
  
  zp->z = zone_of_node(Node(p));
  if (! zp->z) {
    Log1("zone_pointer_zone_detail failed  (p:0x%p): ", Node(p));
    out_debug_limit(p, Limit);
    return 1; /*fail*/
  }
  
  zp->off = Node(p) - zp->z->nodes;
  return 0;
}

static char *zone_pointer_info_do(pointer p, int chk)
{
  zone_address theZone, *zp = &theZone;
  int c;

  Assert(chk ? check : 1);  /* chk only set if checking */

  if (IsNil(p))
    return "/NIL";  /* '/' separator from previous part of message */
  
  if (zone_pointer_detail(p, zp)) {
    err_refc("can't get pointer zone info");
  }
  
  if (chk) {
    c = snprintf(s, MAX, "/%c (s/w %u/%u) [%u.%ld] %s",
                 (IsStrong(p)?'s':'w'),
                 zp->z->check_nodes[zp->off].s_refc,
                 zp->z->check_nodes[zp->off].w_refc,
                 zp->z->seq,
                 zp->off,
                 err_tag_name(zp->z->check_nodes[zp->off].t)
                 );  }else {
    c = snprintf(s, MAX, "/%c (s/w %u/%u) [%u.%ld] %s",
              (IsStrong(p)?'s':'w'),
              zp->z->nodes[zp->off].s_refc,
              zp->z->nodes[zp->off].w_refc,
              zp->z->seq,
              zp->off,
              err_tag_name(zp->z->nodes[zp->off].t)
              );
  }
  
  if (!chk && debug) {
    /* debugging: add detail of pointed-to non-check nodes when debug > 0 */
    if (HasPointers(p)) {
      zone_address theHd, *zp_hd = &theHd;
      zone_address theTl, *zp_tl = &theTl;

      if (zone_pointer_detail(Hd(p), zp_hd))
        err_refc("can't get pointer hd info");
      
      if (zone_pointer_detail(Tl(p), zp_tl))
        err_refc("can't get pointer tl info");
      
      snprintf(s + c, MAX - c, " [%u.%ld]%s.[%u.%ld]%s",
               zp_hd->z->seq,
               zp_hd->off,
              Label(Hd(p)),
               zp_tl->z->seq,
               zp_tl->off,
              Label(Tl(p))
              );
    } else if (IsName(p))
      snprintf(s + c, MAX - c, " \"%s\"", Name(p));
    else if (IsNum(p))
      snprintf(s + c, MAX - c, " %u", Num(p));
    else if (IsChar(p))
      snprintf(s + c, MAX - c, " %c", Char(p));
    else if (IsBool(p))
      snprintf(s + c, MAX - c, " %s", Bool(p) ? "TRUE" : "FALSE");
  }
  return s;
}
#undef Label

/*NB zone_pointer_info_do() returns pointer to a *fixed string* */
char *zone_pointer_info(pointer p)
{
  return zone_pointer_info_do(p, 0);
}

/*NB zone_pointer_info_do() returns pointer to a *fixed string* */
char *zone_pointer_check_info(pointer p)
{
  return zone_pointer_info_do(p, 1);
}

static char i[MAX];

/* return printable string for logging node */
/* NB zone_node_info() returns pointer to a *fixed string* */
char *zone_node_info(zone_check_node_data data)
{
    char *msg = "";
    
    switch (data.status) {
      case unknown:     msg = " !unknown island check status"; break;
      case strong_root: msg = " case (a) root protected by strong pointers"; break;
      case strong:      msg = " case (b) protected by non-root strong pointers"; break;
      case weak:        msg = " case (c) protected only by non-root weak pointers"; break;
      case island:      msg = " is an island"; break;
    }

  snprintf(i, MAX, "(s/w %u/%u) (s+/w+ %u/%u) (s-/w- %u/%u) status:%s",
           data.total.s,    data.total.w,
           data.excess.s,   data.excess.w,
           data.deficit.s,  data.deficit.w,
           msg);
  return i;
}
/*
 zone_new - allocate storage for 'size' nodes; point back to 'parent' (if any).  `
 Set up zone_header at start of zone for use by zone_xxx() and new_XXX() only.
 If 'check', then allocate double space at 'check_nodes' for use in consistency checking.
 for any given (node *)n, check info resides at (node *)(n + z->size) where z is the zone in question
 
 Always returns a valid zone, caller does not need to check (or takes err()).
 */
zone_header *zone_new(long unsigned size, struct zone_header *parent)
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
 zone-debug_reset - reset the check_nodes in every zone - must be used prior to a check
 
 NB time taken is proportional to total zone aize, so performance penality incurred
 */
void zone_debug_reset()
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
 storage management - new... node...
 */

static unsigned new_count = 0; /* how many new nodes */
static unsigned new_tags[TagCount]; /* how mant new nodes, by tag */

void new_log(pointer p)
{
  new_count++;
  new_tags[Tag(p)]++;
  
  return;
}

void refc_free_log(pointer p)
{
  Debug1("refc_free%s\n", zone_pointer_info(p));
  
  return;
}

/*
 * freelist - linked list of nodes, linked in Hd() by strong pointer, end of list Hd==NIL
 */
static pointer refc_freelist = {(node *)0,0};/* "NIL" in a way that satisfies cc() */

static unsigned refc_free_count = 0;  /* how many nodes in the free list */
static unsigned refc_inuse_count = 0;  /* nodes in use == reachable from 'root' or 'defs' or 'builtin' */

/* how many nodes are in use? */
unsigned refc_inuse()
{
  return refc_inuse_count;
}

/* how many nodes have been freed */
unsigned refc_free()
{
  return refc_free_count;
}

/* is given node on the freelist? */
/* DEPRECATED due to linear earch */
static int refc_isfree(pointer p)
{
  pointer f;

  for (f = refc_freelist; IsSet(f); f = Tl(f)) {
    if (SameNode(p, f)) {
      Assert(IsFree(p)); /* double=check the tag - could check whole list every time but that's excessive */
      return 1;
    }
  }
  return 0;
}

/*
 node_new() return pointer to a new node
 contents un-initialised
 <guaranteed> to return usable node - no need for caller to check.
 */
pointer new_node(tag t)
{
  node *n;
  pointer p;
  
  /* TODO use freelist if any - simpler not to do this yet as makes no of nodes used clear - remember to include refc_freecount-- */
  
  /* make sure there is free space to be used */
  if (!zone_current || (zone_current->used >= zone_current->size))
    zone_current = zone_new(zone_size_default, zone_current);
  
  /* assert(zone_zurrent && zone_current->used < zone_current_size) so there is space for one node in the zone  */
  refc_inuse_count++;
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
 * free_node - helper function to deallocate the node p points to, adds to front of refc_freelist
 * pointer - passed By Reference
 *  freelist is composed of apply nodes
 *   ONLY to be called from refc_delete
 */
void free_node(pointer p)
{
  refc_free_log(p);
  /* validation */
  if (IsNil(p)) {
    err_refc("free: pointer is NIL");
    return;
  }
  if (Srefc(p) > 0)
    err_refc("free: Srefc not zero");
  if (Wrefc(p) > 0)
    err_refc("free: Wrefc not zero");
  if(IsFree(p))
    err_refc("free: node is already free");
  if(refc_isfree(p))
    err_refc("free: node is already free, and on the freelist");
  if (HasPointers(p)) {
    if (IsSet(Hd(p)))
      err_refc("free: Hd not NIL");
    if (IsSet(Tl(p)))
      err_refc("free: Tl not NIL");
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
    
  /* add to freelist - refc_frelist is Hd linked list of strong pointers (possibly NIL) */
  Tag(p) = free_t;  /* was cons_t; */
  Hd(p) = NIL; // not needed
  Tl(p) = refc_freelist;
  
  PtrBit(p)= NodeBit(p) = PtrBit(refc_freelist);  /* link with strong pointers */
  Srefc(p) = 1;
  Wrefc(p) = 0;
  
  refc_freelist = p;
  
  refc_free_count++; refc_inuse_count--;
  
  return;
}

/* +checking++checking++checking++checking++checking++checking++checking+ */

int refc_check_log(char *msg, int result)
{
  Debug1("refc_check: %s\n", msg);
  return result;
}

int refc_check_log2(char *msg, int result, long unsigned i, long unsigned j)
{
  Debug2(msg, i, j);
  return result;
}

int refc_check_log5(char *msg, int result, int zone_no, int node_no, int i, int j)
{
  Debug5("%s [%u.%u] (node/check node %s/%s)\n", msg, zone_no, node_no, err_tag_name((tag) i), err_tag_name((tag) j));
  return result;
}

int refc_check_log6(char *msg, int result, int zone_no, int node_no, int i, int j)
{
  Debug5("%s [%u.%u] (%u/%u)\n", msg,  zone_no, node_no, i, j);
  return result;
}

/*
 * Iterative check - MacOS stack is limited to depth 512 then aborts
 */
static pointer *stack;
static pointer *sp;
static unsigned stack_size = 0;

#define Stacked (sp-stack)
#define Push(p) (Assert(Stacked < stack_size), *sp++ = (p))
#define Pop     (Assert(Stacked > 0),         (*--sp))


/*
 * report on loops found
 * intention is to only print as many nodes are required to get back to "p"
 */
#define Limit 24 /* restrict output to this level, unless debug>1 */
/* worker */
int zone_loop_report_do(const pointer p, const pointer start, const unsigned s_limit, unsigned *strong_count)
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
int zone_loop_report(pointer p, unsigned s_limit)
{
  unsigned strong_count = 0;

  Debug1("zone_loop_report%s ", zone_pointer_info(p));
  out_debug_limit(p, (debug > 1 ? s_limit : Limit));

  return (! HasPointers(p) ||
          zone_loop_report_do(H(p), p, s_limit, &strong_count) ||
          zone_loop_report_do(T(p), p, s_limit, &strong_count));
}

/*
 * update check info for a pointer and pointed to node
 * returns ALLrefc of check node
 */
static int refc_check_visit_node(pointer p, unsigned s_limit, zone_check_counts *counts)
{
  zone_header *z;
  long int node_no;  /* offset of Node(p) within the zone */

  /* updates "counts" for the pointer */
  if (IsStrong(p)) {
    (counts->strong)++;
    
    if ((s_limit > 0) && (counts->strong > s_limit)) {
      if (debug > 1) {
        /* continue regardless - DANGER!*/
        Debug2("strong pointer loop detected%s (traversing pointers) (limit=%u)\n", zone_pointer_info(p), s_limit);
        zone_loop_report(p, s_limit); // NEW
        return 1;
      } else {
        char s[MAX];
          Debug2("%s%s\n","refc_check_traverse_pointers0: strong pointer loop detected (traversing pointers)",  zone_pointer_info(p));
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
  if (!z)
    return refc_check_log("traverse pointers: can't find zone for pointer ", 99);
  node_no = Node(p) - z->nodes; /*offset within zone nodes*/
  
  /* update check refc */
  Assert(z->check_nodes);
  if (IsStrong(p))
    (z->check_nodes[node_no].s_refc)++;
  else
    (z->check_nodes[node_no].w_refc)++;
  
  z->check_nodes[node_no].t = z->nodes[node_no].t;
  
  return((z->check_nodes[node_no].s_refc) +
         (z->check_nodes[node_no].w_refc));
}

static int refc_check_traverse_pointers_do(pointer p, unsigned s_limit, node *stop_node, zone_check_counts *counts)
{
  while (1) {
    /* pointer nodes, not previously visited */
    while (HasPointers(p)) {
      if ((refc_check_visit_node(p, s_limit, counts) == 1)
          || (stop_node && (Node(p) != stop_node))) {
        /* (AllRefc(check node) == 1 or not reached stop_node ) ==> so this is first visit  ==> so also visit descendants  */
        (counts->hdtl)++;
        if (mem_dump)
          Debug1("mem_dump%s\n",zone_pointer_info(p));
      }
      else
        break; /* only visit/count pointed-to node first time through */
      
      Push(p);
      p = H(p);
    }
    
    if (IsNil(p)) {
      (counts->nil)++;
    } else if (!HasPointers(p)) {
      if ((refc_check_visit_node(p, s_limit, counts) == 1)
          || (stop_node && (Node(p) !=stop_node))) {
        (counts->atom)++;
        if (mem_dump)
          Debug1("mem_dump%s\n",zone_pointer_info(p));
      }
    }
    
    if (Stacked == 0)
      return 0; /* all done */
    
    p = Pop;
    Assert(HasPointers(p));
    p = T(p);
  }
}

int refc_check_traverse_pointers0(pointer p, unsigned s_limit, node *stop_node, zone_check_counts *counts)
{
  int res;
  
  /* Assert(is_tree(n)) - NO loops! */
  
  stack_size = s_limit;
  sp = stack = new_table(stack_size, sizeof(pointer));
  
  res = refc_check_traverse_pointers_do(p, s_limit, stop_node, counts);
  
  free_table(stack);
  
  return res;
}

#undef Stacked
#undef Push
#undef Pop

#ifdef notdef
/*DEPRECATED
 refc_check_traverse_pointers - traverse pointers, populating check_nodes with Tag/Srefc/Wrefc
 recursively traverse from p counting pointers and nodes and updating Error
 
 s_limit - max umber of strong pointers before a loop is certain
 nil_count count of NIL pointers  - passed by reference
 strong_count count of strong pointers - passed by reference
 weak_count count of strong pointers  - passed by reference
 struct_count count of struct nodes  - passed by reference
 atom_count count of non-struct nodes  - passed by reference
 
 for each pointer: increment the strong or weak reference count of the pointed-to- node's debug_node
 
 returns 0 if all ok, otherwise >0
 
 */
int refc_check_traverse_pointers(pointer p, int s_limit, zone_check_counts *counts)
{
  zone_header *z;
  long int node_no;  /* offset of Node(p) within the zeon */
  
  /* pointer counts */
  if (IsNil(p)) {
    (counts->nil_count)++;
    return 0;  /* nothing else to do */
  }
  
  if (IsStrong(p)) {
    (counts->strong_count)++;
    
    if ((s_limit > 0) && (counts->strong_count > s_limit)) {
      if (debug > 1) {         /* continue regardless - DANGER!*/
        Debug("strong pointer loop detected (limit=%u)\n", s_limit);
        return 1;
      } else {
        char s[MAX];
        (void) snprintf(s, MAX, "strong pointer loop detected (limit=%u) ", s_limit);
        (void) err_zone(s);
        /*NOTREACHED*/
      }
    }
    
  }
  else {
    (counts->weak_count)++;
  }
  
  /* copy details into check_nodes */
  z = zone_of_node(Node(p));
  if (!z)
    return refc_check_log("traverse pointers: can't find zone for pointer ", 99);
  node_no = Node(p) - z->nodes;
  z->check_nodes[node_no].t = z->nodes[node_no].t;
  if (IsStrong(p))
    (z->check_nodes[node_no].s_refc)++;
  else
    (z->check_nodes[node_no].w_refc)++;
  
  /* only follow record node counts and follow sub-pointers once */
  if (((z->check_nodes[node_no].s_refc) +
       (z->check_nodes[node_no].w_refc)) > 1) {
    return 0;
  }
  
  /* traverse remainder of a structure  - combinators have pointers to embedded names so search them also */
  if (HasPointers(p)) {
    int i;
    (*struct_count)++;
    
    i =  refc_check_traverse_pointers(Hd(p), s_limit, nil_count, strong_count, weak_count, struct_count, atom_count);
    i += refc_check_traverse_pointers(Tl(p), s_limit, nil_count, strong_count, weak_count, struct_count, atom_count);
    return i;
  }
  else {
    (*atom_count)++;
    return 0;
  }
  /*NOTREACHED*/
  return 0;
}
#endif

/*
 refc_check_travserse_nodes - traverse all nodes in a zone and compare to pointer traverse results
 visit all nodes in all zones; compare node tag and refc to debug_node tag and refc
 
 for each node:
 accumulate strong and weak refc, as found in the node and store into the check node
 check(Tag   == check Tag)
 check(Srefc == check SRefc)
 check(Wrefc == check WRefc)
 check(! (Wrefc >0 && Srefc==0))
 
 returns 0 if all ok, otherwise >0
 usage if (refc_traverse2(root)) bad; ok;
 
 */

//start-depracated
/* Reporting - called when discreperancy found.  */
static int refc_check_traverse_valid(int res_i, pointer p, pointer debug_p)
{
  if (ALLrefc(p) >  0 && ALLrefc(debug_p) == 0) {
    
    /* leak - not pointed to but ALLrefc >  zero */
    Debug2("!!%s%s\n", "leaked", zone_pointer_info(p));
    
  } else
  if (ALLrefc(p) ==  0 && ALLrefc(debug_p) >   0) {
    
    /* orphan - is pointed-to but ALLrefc == zero */
    Debug2("!!%s%s\n", "orphan" , zone_pointer_info(p));
    
  }
#if 0
  else if (Srefc(p) == 0) {
    /* weakling - is pointed-to but refc == zero */
    Debug2("!!%s%s\n", "weakling" , zone_pointer_info(p));
  }
#endif

  else {
    
    /* other differences */
    Debug2("!!%s%s\n", "store", zone_pointer_info(p));
    Debug2("!!%s%s\n", "check", zone_pointer_check_info(debug_p));
    
  }
  return res_i;
}
//end-depracated

/* inspect check_node_info: is an island if there are not excess strong or weak pointers */
int zone_is_island(zone_check_node_data data)
{
  /* "excess" is evidence of "external" pointers to a node - the refc is greater than the number of pointers found in the scan. */
  return ! (HasRefs(data.excess));
}


/* Reporting - called when island discreperancy found. This is detailed. */
static int refc_check_traverse_valid_island(int res_i, pointer p, pointer debug_p)
{
//  if (logging && res_i)
  {
    Log5("zone_check_island: refc_check_traverse_valid_island%s\t(s+/w+) (%u/%u)\t(s-/w-) (%u/%u)",
         zone_pointer_info(p),
         (Srefc(p) > Srefc(debug_p) ? Srefc(p) - Srefc(debug_p): 0),
         (Wrefc(p) > Wrefc(debug_p) ? Wrefc(p) - Wrefc(debug_p): 0),
         (Srefc(debug_p) > Srefc(p) ? Srefc(debug_p) - Srefc(p): 0),
         (Wrefc(debug_p) > Wrefc(p) ? Wrefc(debug_p) - Wrefc(p): 0));
    if (debug) {
      if (Srefc(p) > Srefc(debug_p))
      Log(" loop protected by strong pointer");
    if (Wrefc(p) > Wrefc(debug_p))
      Log(" loop protected by weak pointer");
      if (Srefc(debug_p) > Srefc(p)) {
      Log2(" strong pointer deficit %u > %u", Srefc(debug_p), Srefc(p));
//        /* This Cancels the current check_island by overwriting checknodes - so must call err */
//        zone_check();
//        err_zone("strong pointer deficit");
      }
    if (Wrefc(debug_p) > Wrefc(p))
      Log(" weak pointer deficit");
    }
    Log("\n");
  }
  return 0; /* NO failures here */
}

#if 0
//DEPRECATED - renamed ..._island() below
static int refc_check_traverse_nodes(zone_header *z, int zone_no, refc_check_node_info *info, int (check(int res_i, pointer p, pointer debug_p)))
{
  int i, res = 0;
  
  /* for a zone, compare nodes to the check_nodes */
  for (i = 0; i< z->size; i++) {
    int res_i = 0;
    
    /* accumulate node info, noting mismatch in res_i*/
    info->total.s += z->nodes[i].s_refc;
    if (z->nodes[i].s_refc  != z->check_nodes[i].s_refc) {
      res_i++;
      if (z->nodes[i].s_refc > z->check_nodes[i].s_refc)
        info->excess.s +=
          z->nodes[i].s_refc - z->check_nodes[i].s_refc;
      else
      if (z->check_nodes[i].s_refc > z->nodes[i].s_refc)
        info->deficit.s +=
          z->check_nodes[i].s_refc - z->nodes[i].s_refc;
    }
    
    info->total.w   += z->nodes[i].w_refc;
    if (z->nodes[i].w_refc  != z->check_nodes[i].w_refc) {
      res_i++;
      if (z->nodes[i].w_refc > z->check_nodes[i].w_refc)
        info->excess.w +=
          z->nodes[i].w_refc - z->check_nodes[i].w_refc;
      else
      if (z->check_nodes[i].w_refc > z->nodes[i].w_refc)
        info->deficit.w +=
          z->check_nodes[i].w_refc - z->nodes[i].w_refc;
    }
    
    /* further validation - nodes in use always have a strong pointer */
    if (z->nodes[i].s_refc == 0 && z->nodes[i].w_refc > 0)
      res_i ++;

    if (z->nodes[i].t != z->check_nodes[i].t)
      res_i++;
    
    if (res_i) {
      pointer p, debug_p;
      node *np;
      
      np = (z->nodes)+i;
      Node(p)= np;
      PtrBit(p) = np->bit; /*always use strong for the "fake' pointer*/
      
      np = (z->check_nodes)+i;
      Node(debug_p)= np;
      PtrBit(debug_p) = np->bit; /*always use strong for the "fake' pointer*/
      
      res += check(res_i, p, debug_p);
    }
  }
  return res;
}
#endif

/* refc_check_node_data - add excess/deficit for give node to "data"
 * returns 0 if there are no discreperancies
 */

static int zone_check_data(zone_address *zp, zone_check_node_data *data)
{
  int res = 0;

  const zone_header *z = zp->z;
  const long off = zp->off;

  const unsigned
    s =       z->nodes      [off].s_refc,
    s_check = z->check_nodes[off].s_refc,
    w =       z->nodes      [off].w_refc,
    w_check = z->check_nodes[off].w_refc;
  tag
    t =       z->nodes      [off].t,
    t_check = z->check_nodes[off].t;
  
  /* accumulate node data, tally mismatch in res_i (taking care not to generate "negative" unsigneds) */
  
  /* strong reference counts */
  data->total.s += s;
  if (  s !=s_check) {
    res++;
    if (s > s_check) {
      data->excess.s +=s - s_check;
    } else
      if (s_check > s) {
        data->deficit.s += s_check - s;
      }
  }
  
  /* weak reference counts */
  data->total.w += w;
  if (  w !=w_check) {
    res++;
    if (w > w_check) {
      data->excess.w += w - w_check;
    }    else
      if (w_check > w) {
        data->deficit.w += w_check - w;
      }
  }
  
  /* tags */
  if (t != t_check) {
    res++;
  }
  return res;
}

static int refc_check_traverse_nodes(zone_header *z, zone_check_node_data *data, int (check(int res_i, pointer p, pointer debug_p)), int ignore_unpopulated)
{
  int res = 0;
  zone_address theZone = {z, 0}, *zp = &theZone;
  
  Assert(z->check_nodes);

  /* for a zone, compare any check nodes to the corresponding nodes */
  for (/***/; zp->off < z->size; zp->off++) {
    int res_i = 0;
    
    if (ignore_unpopulated && z->check_nodes[zp->off].t == zero_t) /* ignore unpopulated nodes */
      continue;
    
    /* accumulate node data */
    res_i += zone_check_data(zp, data);

    /* further validation - nodes in use always have a strong pointer */
    /* ... *except* during refc_delete() as indicated by deleting_t  */
    if (z->nodes[zp->off].s_refc == 0 && z->nodes[zp->off].w_refc > 0 && z->nodes[zp->off].t != deleting_t)
      res_i ++;
    
    /* is something amiss, report using "check" */
    if (res_i || check == refc_check_traverse_valid_island) {//xxx temp second part!
      pointer p, debug_p;
      node *np;
      
      np = (z->nodes)+(zp->off);
      Node(p)= np;
      PtrBit(p) = np->bit; /*always use strong for the "fake' pointer*/
      
      np = (z->check_nodes)+(zp->off);
      Node(debug_p)= np;
      PtrBit(debug_p) = np->bit; /*always use strong for the "fake' pointer*/
      
      res += check(res_i, p, debug_p);
    }
  }
  return res;
}

/* refc_check_loop
 * follow all strong (and only strong) pointers, counting nodes visited (in *strong_count).
 * iff there is a strong loop then strong_count will exceed s_limit -> report error
 * This function is needed because refc_check_traverse_pointers() only visits Hd/Tl first time through, preventing loop detection in all but pathological cases!
 *
 * usage if (refc_loop_check(p, limit)) bad; ok;
 */
/* worker */
int refc_check_loop_do(pointer p, unsigned s_limit, unsigned strong_count)
{
  
  if (IsNil(p) ||
      IsWeak(p) ||
      ! HasPointers(p))
    return 0;
  
  if ((strong_count++) > s_limit) {
    Debug2("!!refc_loop_check_do: found loop size %u: at: %s: ", strong_count, zone_pointer_info(p));
    zone_loop_report(p, s_limit); // NEW
    return 1;
  }
  
  return (
          refc_check_loop_do(H(p), s_limit, strong_count) ||
          refc_check_loop_do(T(p), s_limit, strong_count));
}

/* wrapper */
#define Limit 24 /* restrict output to this level, unless debug>1 */
int refc_check_loop(pointer p, unsigned s_limit)
{
  if (refc_check_loop_do(p, s_limit, 0)) {
    out_debug_limit(p, (debug > 1 ? s_limit : Limit));
    return 1;
  }
  return 0;
}


/* refc_find_roots
 * search for nodes such that ALLrefc(p) > (count of pointers-to p)
 *
 * this is achieved simply by comparing ALLrefc(p) and ALLrefc(check(p))
 */
pointer refc_find_roots()
{
  return NIL;
}


static int zone_check_one_root(pointer root, unsigned s_limit, zone_check_counts *counts, char *info)
{
  int res = 0;

  if (IsNil(root))
    return res;

  if (debug
//   xxx   > 1
      )
    Debug2("zone_check_one_root(\"%s\")%s\n", info, zone_pointer_info(root));
  
  if (IsWeak(root))
      Debug1("!!%s pointer is weak - unexpected\n", info);
    
  res += refc_check_traverse_pointers0(root, s_limit, 0/*non-stop*/, counts);
  res += refc_check_loop(root, s_limit);

  return res;
}

/*
 zone_check_do - check storage consistency - 'root' is start of the graph; 'freelist' is the start of freelist.
 0. zone check:
 (zone_current != 0)
 check(count of zones == zone_new_count)
 check(sum of size of zones == zone_total_size)
 
 check(refc_inuse_count <= zone_total_size)
 check(refc_free_count <= zone_total_size)
 check(refc_free_count + refc_inuse_count <= zone_total_size)
 check(refc_free_count + refc_inuse_count == (sum of used of zones))
 
 1. check(count of nodes reachable from root and defs == refc_inuse_count)
 2. check(count of nodes reachable from freelist == refc_free_count
 3. check(count of all pointers == sum ALLrefc)
 4. check(count of weak pointers == sum Wrefc)
 5. check(count of strong pointers == sum Srefc)
 
 6. reachability check, recalculate expected reference counts and compare to actual, using check_nodes.
 
 returns 0 if all ok, otherwise >0
 usage if (zone_check_do(root, freelist)) bad; else ok;
 
 */
int zone_check_do(pointer root, pointer defs, pointer freelist)
{
#ifdef notyet
  int free_count = 0;
  int inuse_count = 0;
#endif
  
  if (!check)
    return 0;
  
  /* zone check */
  {
    unsigned new_count = 0;  /* count of zones */
    long unsigned total_size = 0;  /* sum of size of zones */
    unsigned total_used = 0;  /* sum of used of zones */
    
    zone_header *z;
    
    /* report zone info */
    Debug2("%s\t%u\n", "zones in use", zone_new_count);
    Debug2("%s\t%lu\n", "total zone size", zone_total_size);
    Debug2("%s\t%u\n", "nodes used", refc_inuse_count);
    Debug2("%s\t%u\n", "nodes free", refc_free_count);
    
    /* report and return if no zones in use */
    if (zone_current == 0) {
      refc_check_log("no zones in use", 1);
      return 1;
    }
    
    /* visit zones calculating check values */
    for (z = zone_current; z; z = z->previous) {
      new_count++;
      total_size += z->size;
      total_used += z->used;
    }
    
    /* report and return if variances found */
    if (zone_new_count != new_count)
      return refc_check_log2("!!zone_new_count:%u but found %u\n", 2, zone_new_count, new_count);
    
    if (zone_total_size != total_size)
      return refc_check_log2("!!zone_total_size:%lu but found %lu\n", 3, zone_total_size, total_size);
    
    if (refc_inuse_count > zone_total_size)
      return refc_check_log2("!!refc_inuse_count:%u but zone total size %lu\n", 4, refc_inuse_count, total_size);
    
    if ((refc_inuse_count + refc_free_count) > zone_total_size)
      return refc_check_log2("!!(refc_inuse_count + refc_free_count) > zone_total_size: %u > %u", 5, (refc_inuse_count + refc_free_count), zone_total_size);
    
    if ((refc_inuse_count + refc_free_count) != total_used)
      return refc_check_log2("!!refc_inuse_count + refc_free_count) != (zone_total_used): %u > %u", 5, (refc_inuse_count + refc_free_count), total_used);
    
    /* otherwise report ok and continue */
    Debug4("%s\t%u==%u+%u\n", "zone check ok", total_used, refc_inuse_count, refc_free_count);
    
  }
  
  /* pointer check */
  {
    /* count nodes visited; count strong pointers followed; count weak pointers (not followed) */
    /* uses zone->check_nodes to store working data; for and given (node *)n, check info resides at (node *)(n + z->size) where z is the zone in question */
    /* TODO could loop forever is there is a cycle of strong pointers so limit at zone_total_size nodes */
    /* lemma: with N nodes, any chain of pointers longer than N is a loop */
    
    zone_check_counts theCounts = zero_zone_check_counts, *counts = &theCounts;
    
    int res = 0;
    int is_tree = 0;
    
    zone_header *z;
    
    zone_debug_reset();  /* clear all check_nodes for use */
    
    /* check the counts and populate check nodes */
    res += zone_check_one_root(root, refc_inuse_count*2, counts, "root"); /* root - the program graph */
    res += zone_check_one_root(defs, refc_inuse_count*2, counts, "defs"); /* defs - saved definitions */
    res += zone_check_one_root(builtin, refc_inuse_count*2, counts, "builtin"); /* sasl - builtin definitions */
//    NEW 2020-01-13
//    if (/*XXX*/ IsNil(root)) {
//      extern pointer *root_sp, root_stack[]; /* ToDo - modularise these variables properly */
//      pointer *spp;
//      for (spp = root_stack + 1; spp <= root_sp; spp++) {
//        res += zone_check_one_root(*spp, (refc_inuse_count + refc_free_count)*2, counts, "root_stack"); /* root_stack - bottom up */
//      }
//    }
//    END-NEW
    
    //ToDo add checks to the stack
    
    Debug2("%s\t%u\n", "nil pointers",    counts->nil );
    Debug2("%s\t%u\n", "strong pointers",  counts->strong );
    Debug2("%s\t%u\n", "weak pointers",    counts->weak );
    Debug2("%s\t%u\n", "struct nodes",    counts->hdtl );
    Debug2("%s\t%u\n", "atom nodes",    counts->atom );
    
    /* 1. check(count of nodes reachable from {root,defs,builtin} == refc_inuse_count) */
    if (refc_inuse_count != (counts->hdtl + counts->atom))
      Debug4( "!!inuse count: %u but found %u==(%u+%u)\n", refc_inuse_count, (counts->hdtl + counts->atom), counts->hdtl, counts->atom);
    
    /* 1a. check for tree structure: strong_count == refc_inuse_count */
    if (refc_inuse_count == counts->strong)
      is_tree = 1;
    
    /* traverse freelist adding to free_counts, check them , then add to total */
    /* freelist - list of available nodes not used by the program graph */
    {
      zone_check_counts theCounts = zero_zone_check_counts, *free_counts = &theCounts;
      res += zone_check_one_root(freelist, (refc_inuse_count + refc_free_count)*2, free_counts, "freelist"); /* freelist - chain of free nodes */
      
      /* 2. check(count of nodes reachable from freelist == refc_free_count */
      /*
       assert(strong_count == refc_free_count)
       assert(struct_count == refc_free_count)
       assert(nil_count == refc_free_count) - each node - has one NIL pointer - last has two
       assert(weak_count == 0)
       assert(atom_count == 0)
       */
      if (free_counts->strong != refc_free_count)
        Debug2("!!free:%u but free count:%u\n", free_counts->strong, refc_free_count);
      
      if (free_counts->hdtl != refc_free_count)
        Debug2("!!free structures:%u but free count:%u\n", free_counts->hdtl, refc_free_count);
      
      if(IsSet(freelist) &&
         (free_counts->nil != refc_free_count +1))
        Debug2("!!free nil count:%u but expecting:%u\n", free_counts->nil, refc_free_count +1);
      
      if (free_counts->weak != 0)
        Debug1("!!freelist contains %u weak pointers\n", free_counts->weak);
      
      if (free_counts->atom != 0)
        Debug1("!!freelist contains %u atoms\n", free_counts->atom);
      
      /* increment totals for whole-store checks below "counts += free_counts;"*/
      zone_check_counts_add(counts, free_counts);
    }
    
    if (res)
      return refc_check_log2("!!check pointer traverse failed\n", 66, res, 0);
    
    if ((counts->strong + counts->weak)  == (counts->hdtl + counts->atom))
      Debug6("%s\t(%u+%u)==(%u+%u)==%u\n", (is_tree ? "pointer check ok (tree)" : "pointer check ok"),
                     counts->strong, counts->weak, counts->hdtl, counts->atom, (counts->strong + counts->weak));
    
    /* inspect the check nodes in every zone and report discreprancies  */
    {
      zone_check_node_data data = zero_data;
      unsigned i;
      
      for (z = zone_current, i = 0;
           z;
           z = z->previous, i++)
#if 0
        res += refc_check_traverse_nodes(z, &data, refc_check_traverse_valid);
#else
        res += refc_check_traverse_nodes(z, &data, refc_check_traverse_valid, 0 /*do not ignore empty*/);
#endif
      
      Debug2("%s\t%u\n", "strong ref counts",  counts->strong );
      Debug2("%s\t%u\n", "weak ref counts",  counts->weak );
      
      /* 3. check(count of all pointers == sum ALLrefc) */
      if ((counts->strong + counts->weak) != (data.total.s + data.total.w))
        Debug2("!!pointers:%u but ALLrefc %u\n ", (counts->strong + counts->weak), (data.total.s + data.total.w));
      
      /* 4. check(count of weak pointers == sum Wrefc) */
      if (counts->strong != data.total.s)
        Debug2("!!strong pointers:%u but Srefc %u\n ", counts->strong, data.total.s);
      
      /* 5. check(count of strong pointers == sum Srefc) */
      if (counts->weak != data.total.w)
        Debug2("!!weak pointers:%u but Wrefc %u\n ", counts->weak, data.total.w);
      
      if ((counts->strong + counts->weak) == (data.total.s + data.total.w))
        Debug6("%s\t(%u+%u)==(%u+%u)==%u\n",
                       "reference count check ok",
                       counts->strong,
                       counts->weak,
                       data.total.s,
                       data.total.w,
                       (counts->strong + counts->weak));
      if (res)
        return refc_check_log2("!!check traverse nodes failed\n", 66, res, 0);
    }
  }
  
  return 0;
}

/*
 * zone_check_island(pointer p)
 * does p point to a self-contained graph, where all the pointers to p and Reach(p) are within the graph.
 * returns number of external pointers (0 for an island)
 *
 * Method:
 *  Clear all check nodes
 *  Populate check notes for Reach*(p)  (ie p and all nodes reachable from p)
 *  Compare nodes to check nodes for Reach*(p)
 *    Any "excess" pointers are counted and the total returned.  Similarly erroneous "deficit" pointers are counted too.
 *    If there are no excess pointers, the node is on an island and should be freed.
 *    Otherwise
 *       case (a) root protected by strong pointers
 *       case (b) protected by non-root strong pointers
 *       case (c) protected only by non-root weak pointers - and possibly non-root weak pointers also
 *
 * WARNING overwrites all check nodes
 * WARNING not reentrant
 */
zone_check_node_data zone_check_island(pointer p, unsigned depth)
{
  int res; //depracated - value not used
  zone_check_counts theCounts = zero_zone_check_counts;
  
  zone_debug_reset();
  
//  Assert(HasPointers(p));
  if (! HasPointers(p))
    return zero_data;
    
#if 1 //2020-01-06 re-enabled: p indicates which node might be deleted, having just deleted a pointer to it.
  //2020-01-04 disabled again  //2019-01-16 reinstated /*XXX XXX 2018-11-09*/
  //
  Debug2("%s%s\n","refc_check_traverse_pointers0:",  zone_pointer_info(H(p)));
  //
  res = refc_check_traverse_pointers0(H(p), refc_inuse_count*2, Node(p)/*stop here*/, &theCounts);
  //
  Debug2("%s%s\n","refc_check_traverse_pointers0:",  zone_pointer_info(T(p)));
  //
  res = refc_check_traverse_pointers0(T(p), refc_inuse_count*2,  Node(p)/*stop here*/, &theCounts);
#else
  Debug2("%s%s\n","refc_check_traverse_pointers0:",  zone_pointer_info(p));
  res = refc_check_traverse_pointers0(p, refc_inuse_count*2, &theCounts);
#endif
  
  /* inspect node "p" and note it's status */
  {
    zone_address theAddr, *zp = &theAddr;
    zone_check_node_data root_data = zero_data;
    
    zone_pointer_detail(p, zp);
    zone_check_data(zp, &root_data);
    
    if (root_data.excess.s >= 1)
      root_data.status = strong_root;

//    start-extra
    Log2("zone_check_island2%s root: %s ",
         zone_pointer_info(p), /*not re-entrant*/
         zone_node_info(root_data));
    //start extra
    Log2("\nzone_check_island2%s debg: %s\nzone_check_island ",
         zone_pointer_check_info(p), /*not re-entrant*/
         zone_node_info(root_data));
//end-extra
    
    /* inspect the check nodes in every zone and report discreprancies  */
    {
      zone_header *z;
      zone_check_node_data data = zero_data;
      unsigned i;
      
      for (z = zone_current, i = 0;
           z;
           z = z->previous, i++) {
        
        (void) refc_check_traverse_nodes(z, &data, refc_check_traverse_valid_island, 1);
      }
      
      data.status = (root_data.status == strong_root    ? strong_root :
                     data.excess.s > root_data.excess.s ? strong :
                     data.excess.w > root_data.excess.w ? weak :
                     island);
      
      Log2("zone_check_island%s root: %s ",
           zone_pointer_info(p), /*not re-entrant*/
           zone_node_info(root_data));
//start extra
      Log2("\nzone_check_island%s debg: %sq\nzone_check_island ",
           zone_pointer_check_info(p), /*not re-entrant*/
           zone_node_info(root_data));
//end extra
      Log3(" %s (depth=%u)%s\n",
           zone_node_info(data),
           depth,
           HasRefs(data.deficit) ? " insufficient refc - should only occur *during* a reduction" : "");
      
      return data;
    }
  }
  /*NOTREACHED*/
}

/* externally usable version */
int zone_check()
{
  return zone_check_do(root, defs, refc_freelist);
}

/*
 * new_report(where)
 *  print tab separated listing of new nodes created, appending a TOTAL, checking tat the total tallies, and printing an ERROR line if not
 */
void new_log_report(FILE *where)
{
  int i, check=0;
  
  if (! logging)
    return;
  
  /* first check storage for consistency */
  if (zone_check())
    Error("refc_check error - continuing anyway\n");/*deprecated*/
  
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

