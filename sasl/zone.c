/*
 storage allocation - bare metal
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"

#include "zone.h"

typedef struct { unsigned s; unsigned w;} pair;
const pair zero_pair = {0,0};

/* used to accumulate refc stored in (non-debug) nodes and
 *  deficit == missing pointers found only in debug nodes
 *  excess  == extra pointers found only in debug nodes
 */
typedef enum{unknown = 0, island, strong_root, strong, weak, strong_weak} zone_check_node_data_status;  // 2020-06-08 currently unused but in place for future
typedef struct {zone_check_node_data_status status; pair total, excess, deficit;} zone_check_node_data;

const zone_check_node_data zero_data = {unknown, 0};

typedef struct {
  unsigned
  nil,  /* number of nil pointers, including root, zone_free */
  strong,    /* number of strong pointers, including root, zone_freelist if they are non-nil */
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
  long off;         /* offset within that zone - Assert(offset < size) */
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
 NB linear search, zone by zone - "slow" when lots of zone
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
  err_zone("can't get pointer zone info");
  /*NOTREACHED*/
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
 * zone_opinter_zone_detail - get non-NIL pointer's zone and offset in zone
 * returns valid zone_address or else calls err_zone() to reset
 * if chk then check node info is returned
 */
#define Limit 24 /* restrict output to this level, unless debug>1 */
zone_address zone_pointer_address(pointer p)
{
  zone_address z;
  
  if (IsNil(p))
    return zero_zone_address;
  
  z.z = zone_of_node(Node(p));
  z.off = Node(p) - z.z->nodes;
  Assert(z.off < z.z->size);   /* require (p is not a pointer to a check node) */
  return z;
}

static char *zone_pointer_info_do(pointer p, int chk)
{
  zone_address z;
  int c;

  Assert(chk ? check : 1);  /* chk only set if checking */

  if (IsNil(p))
    return "/NIL";  /* '/' separator from previous part of message */
  
  z = zone_pointer_address(p);
  
  if (chk) {
    c = snprintf(s, MAX, "/%c (s/w %u/%u) [%u.%ld] %s",
                 (IsStrong(p)?'s':'w'),
                 z.z->check_nodes[z.off].s_refc,
                 z.z->check_nodes[z.off].w_refc,
                 z.z->seq,
                 z.off,
                 err_tag_name(z.z->check_nodes[z.off].t)
                 );

  }else {
    c = snprintf(s, MAX, "/%c (s/w %u/%u) [%u.%ld] %s",
              (IsStrong(p)?'s':'w'),
              z.z->nodes[z.off].s_refc,
              z.z->nodes[z.off].w_refc,
              z.z->seq,
              z.off,
              err_tag_name(z.z->nodes[z.off].t)
              );
  }
  
  if (!chk && debug) {
    /* debugging: add detail of pointed-to non-check nodes when debug > 0 */
    if (HasPointers(p)) {
      zone_address z_hd = zone_pointer_address(Hd(p));
      zone_address z_tl = zone_pointer_address(Tl(p));
      
      snprintf(s + c, MAX - c, " [%u.%ld]%s.[%u.%ld]%s",
               z_hd.z->seq,
               z_hd.off,
              Label(Hd(p)),
               z_tl.z->seq,
               z_tl.off,
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
/* Not reentrant */
char *zone_node_info(zone_check_node_data data)
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

static void zone_free_log(pointer p)
{
  Debug1("zone_free%s\n", zone_pointer_info(p));
  
  return;
}

/*
 * freelist - linked list of nodes, linked in Hd() by strong pointer, end of list Hd==NIL
 */
static pointer zone_freelist = {(node *)0,0};/* "NIL" in a way that satisfies cc() */

static unsigned zone_free_count = 0;  /* how many nodes in the free list */
static unsigned zone_inuse_count = 0;  /* nodes in use == reachable from 'root' or 'defs' or 'builtin' */

/* how many nodes are in use? */
unsigned zone_inuse()
{
  return zone_inuse_count;
}

/* how many nodes have been freed */
unsigned zone_free()
{
  return zone_free_count;
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
 * free_node - helper function to deallocate the node p points to, adds to front of zone_freelist
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
  Tag(p) = free_t;  /* was cons_t; */
  Hd(p) = NIL; // not needed
  Tl(p) = zone_freelist;
  
  PtrBit(p)= NodeBit(p) = PtrBit(zone_freelist);  /* link with strong pointers */
  Srefc(p) = 1;
  Wrefc(p) = 0;
  
  zone_freelist = p;
  
  zone_free_count++; zone_inuse_count--;
  
  return;
}

/* +checking++checking++checking++checking++checking++checking++checking+ */

static int zone_check_log(char *msg, int result)
{
  Debug1("zone_check: %s\n", msg);
  return result;
}

static int zone_check_log2(char *msg, int result, long unsigned i, long unsigned j)
{
  Debug2(msg, i, j);
  return result;
}

static int zone_check_log5(char *msg, int result, int zone_no, int node_no, int i, int j)
{
  Debug5("%s [%u.%u] (node/check node %s/%s)\n", msg, zone_no, node_no, err_tag_name((tag) i), err_tag_name((tag) j));
  return result;
}

static int zone_check_log6(char *msg, int result, int zone_no, int node_no, int i, int j)
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
 * report when loops found
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
 * returns ALLrefc of check node
 */
static int zone_check_pointer(pointer p, unsigned s_limit, zone_check_counts *counts)
{
  zone_header *z;
  long int node_no;  /* offset of Node(p) within the zone */

  /* updates "counts" for the pointer */
  if (IsStrong(p)) {
    (counts->strong)++;
    
    if ((s_limit > 0) && (counts->strong > s_limit)) {
      if (debug > 1) {
        /* continue regardless - DANGER!*/
        Debug2("!!zone_check_visit_node: strong pointer loop detected%s (traversing pointers) (limit=%u)\n", zone_pointer_info(p), s_limit);
        zone_loop_report(p, s_limit); // NEW
        return 1;
      } else {
        char s[MAX];
          Debug2("!!%s%s\n","zone_check_visit_node: strong pointer loop detected (traversing pointers)",  zone_pointer_info(p));
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

/*
 zone_check_traverse_pointers - traverse pointers, populating check_nodes with Tag/Srefc/Wrefc
 recursively traverse from p counting pointers and nodes and updating Error
 
 s_limit - max number of strong pointers before a loop is certain
 stop_node - end search when this node is reached - optional, may be NULL
 
 counts - passed by reference, updated here
 
 returns 0 if all ok, otherwise >0
 */
static int zone_check_traverse_pointers_do(pointer p, unsigned s_limit, node *stop_node, zone_check_counts *counts)
{
  while (1) {
    // bug in stop_node corrected here https://tree.taiga.io/project/northgate91-project-one/issue/62
    while (HasPointers(p)) {
      /* pointer nodes */
      if ((zone_check_pointer(p, s_limit, counts) > 1)
          || (stop_node && (Node(p) == stop_node))) {
        /* not first pointer to node, or node is stop node */
        break;
      }
      
      /* count node, visit descendants  */
      (counts->hdtl)++;
      if (mem_dump)
        Debug1("mem_dump%s\n",zone_pointer_info(p));
      
      Push(p);
      p = H(p);
    }
    
    /* non-pointer nodes */
    if (IsNil(p)) {
      (counts->nil)++;
    } else if (!HasPointers(p)) {
      if ((zone_check_pointer(p, s_limit, counts) > 1)
          || (stop_node && (Node(p) == stop_node))) {
        /* not first pointer to node, or node is stop node */
      } else {
        /* count node */
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


static int zone_check_traverse_pointers(pointer p, unsigned s_limit, node *stop_node, zone_check_counts *counts)
{
  int res;
  
  /* Assert(is_tree(n)) - NO loops! */
  
  stack_size = s_limit;
  sp = stack = new_table(stack_size, sizeof(pointer));
  
  res = zone_check_traverse_pointers_do(p, s_limit, stop_node, counts);
  
  free_table(stack);
  
  return res;
}

#undef Stacked
#undef Push
#undef Pop


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

//start-depracated
/* Reporting - called when discreperancy found.  */
static int zone_check_traverse_valid(int res_i, pointer p, pointer debug_p)
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


/* Reporting - called when island discreperancy found. This is detailed. */
static int zone_check_traverse_valid_island(int res_i, pointer p, pointer debug_p)
{
  if (debug > 1) {
    Debug5("zone_check_island: zone_check_traverse_valid_island%s\t(s+/w+) (%u/%u)\t(s-/w-) (%u/%u)",
         zone_pointer_info(p),
         (Srefc(p) > Srefc(debug_p) ? Srefc(p) - Srefc(debug_p): 0),
         (Wrefc(p) > Wrefc(debug_p) ? Wrefc(p) - Wrefc(debug_p): 0),
         (Srefc(debug_p) > Srefc(p) ? Srefc(debug_p) - Srefc(p): 0),
         (Wrefc(debug_p) > Wrefc(p) ? Wrefc(debug_p) - Wrefc(p): 0));
    if (Srefc(p) > Srefc(debug_p))
      Debug(" loop protected by strong pointer");
    if (Wrefc(p) > Wrefc(debug_p))
      Debug(" loop protected by weak pointer");
    if (Srefc(debug_p) > Srefc(p)) {
      Debug2(" strong pointer deficit %u > %u", Srefc(debug_p), Srefc(p));
      //        /* This Cancels the current check_island by overwriting checknodes - so must call err */
      //        zone_check();
      //        err_zone("strong pointer deficit");
    }
    if (Wrefc(debug_p) > Wrefc(p))
      Debug2(" weak pointer deficit %u > %u", Wrefc(debug_p), Wrefc(p));
    Debug("\n");
  }
  return 0; /* NO failures here */
}


/* zone_check_data - add excess/deficit for give node to "data"
 * returns 0 if there are no discreperancies
 */

static int zone_check_data(zone_address *zp, zone_check_node_data *data)
{
  int res = 0;

  const zone_header *z = zp->z;
  const long off = zp->off;
  
  const node *np       = z->nodes       + off;
  const node *np_check = z->check_nodes + off;

  const unsigned
  s =       np      ->s_refc,
  s_check = np_check->s_refc,
  w =       np      ->w_refc,
  w_check = np_check->w_refc;
  
  tag
  t =       np      ->t,
  t_check = np_check->t;
  
  /* accumulate node data, tally mismatch in res_i (taking care not to generate "negative" unsigneds) */
  
  /* strong reference counts */
  data->total.s += s;
  if (  s !=s_check) {
    res++;
    if (s > s_check) {
      data->excess.s +=s - s_check;
      // all: err; s_check==0->orphaned
    } else
//      if (s_check > s)
      {
        data->deficit.s += s_check - s;
        // island: warn protected by strong %u (s_check - s)
        // full: err; s==0->leaked
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
    // all: err
  }
  return res;
}

static int zone_check_traverse_nodes(zone_header *z, zone_check_node_data *data, int (check(int res_i, pointer p, pointer debug_p)), int ignore_atoms)
{
  int res = 0;
  zone_address theZone = {z, 0}, *zp = &theZone;
  
  Assert(z->check_nodes);

  /* for a zone, compare any check nodes to the corresponding nodes */
  for (/***/; zp->off < z->size; zp->off++) {
    int res_i = 0;
    
    if (ignore_atoms &&
        (z->check_nodes[zp->off].t == zero_t || ! HasPointersTag(z->check_nodes[zp->off].t))
        ) /* ignore unpopulated nodes and nodes without pointers */
      continue;
    
    /* accumulate node data */
    res_i += zone_check_data(zp, data);

    /* further validation - nodes in use always have a strong pointer */
    /* ... *except* during refc_delete() as indicated by deleting_t  */
    if (z->nodes[zp->off].s_refc == 0 && z->nodes[zp->off].w_refc > 0 && z->nodes[zp->off].t != deleting_t)
      res_i ++;
    
    /* is something amiss, report using "check" */
    if (res_i || check == zone_check_traverse_valid_island) {//xxx temp second part!
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

/* zone_check_loop
 * follow all strong (and only strong) pointers, counting nodes visited (in *strong_count).
 * iff there is a strong loop then strong_count will exceed s_limit -> report error
 * This function is needed because zone_check_traverse_pointers() only visits Hd/Tl first time through, preventing loop detection in all but pathological cases!
 *
 * usage if (zone_loop_check(p, limit)) bad; ok;
 */
/* worker */
static int zone_check_loop_do(pointer p, unsigned s_limit, unsigned strong_count)
{
  
  if (IsNil(p) ||
      IsWeak(p) ||
      ! HasPointers(p))
    return 0;
  
  if ((strong_count++) > s_limit) {
    Debug2("!!zone_loop_check_do: found loop size %u: at: %s: ", strong_count, zone_pointer_info(p));
    zone_loop_report(p, s_limit); // NEW
    return 1;
  }
  
  return (
          zone_check_loop_do(H(p), s_limit, strong_count) ||
          zone_check_loop_do(T(p), s_limit, strong_count));
}

/* wrapper */
#define Limit 24 /* restrict output to this level, unless debug>1 */
int zone_check_loop(pointer p, unsigned s_limit)
{
  if (zone_check_loop_do(p, s_limit, 0)) {
    out_debug_limit(p, (debug > 1 ? s_limit : Limit));
    return 1;
  }
  return 0;
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
    
  res += zone_check_traverse_pointers(root, s_limit, 0/*non-stop*/, counts);
  res += zone_check_loop(root, s_limit);

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
 
 6. reachability check, recalculate expected reference counts and compare to actual, using check_nodes.
 
 returns 0 if all ok, otherwise >0
 usage if (zone_check_do(root, freelist)) bad; else ok;
 
 */
static int zone_check_do(pointer root, pointer defs, pointer freelist)
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
    Debug2("%s\t%u\n", "nodes used", zone_inuse_count);
    Debug2("%s\t%u\n", "nodes free", zone_free_count);
    
    /* report and return if no zones in use */
    if (zone_current == 0) {
      zone_check_log("no zones in use", 1);
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
      return zone_check_log2("!!zone_new_count:%u but found %u\n", 2, zone_new_count, new_count);
    
    if (zone_total_size != total_size)
      return zone_check_log2("!!zone_total_size:%lu but found %lu\n", 3, zone_total_size, total_size);
    
    if (zone_inuse_count > zone_total_size)
      return zone_check_log2("!!zone_inuse_count:%u but zone total size %lu\n", 4, zone_inuse_count, total_size);
    
    if ((zone_inuse_count + zone_free_count) > zone_total_size)
      return zone_check_log2("!!(zone_inuse_count + zone_free_count) > zone_total_size: %u > %u", 5, (zone_inuse_count + zone_free_count), zone_total_size);
    
    if ((zone_inuse_count + zone_free_count) != total_used)
      return zone_check_log2("!!zone_inuse_count + zone_free_count) != (zone_total_used): %u > %u", 5, (zone_inuse_count + zone_free_count), total_used);
    
    /* otherwise report ok and continue */
    Debug4("%s\t%u==%u+%u\n", "zone check ok", total_used, zone_inuse_count, zone_free_count);
    
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
    res += zone_check_one_root(root, zone_inuse_count*2, counts, "root"); /* root - the program graph */
    res += zone_check_one_root(defs, zone_inuse_count*2, counts, "defs"); /* defs - saved definitions */
    res += zone_check_one_root(builtin, zone_inuse_count*2, counts, "builtin"); /* sasl - builtin definitions */
    {
      pointer *sp;
      for (sp = make_sp; sp > make_stack; sp--) {
        res += zone_check_one_root(*sp, zone_inuse_count*2, counts, "make_stack");
      }
    }
    
//    NEW 2020-01-13
//    if (/*XXX*/ IsNil(root)) {
//      extern pointer *root_sp, root_stack[]; /* ToDo - modularise these variables properly */
//      pointer *spp;
//      for (spp = root_stack + 1; spp <= root_sp; spp++) {
//        res += zone_check_one_root(*spp, (zone_inuse_count + zone_free_count)*2, counts, "root_stack"); /* root_stack - bottom up */
//      }
//    }
//    END-NEW
    
    //ToDo add checks to the stack
    
    Debug2("%s\t%u\n", "nil pointers",    counts->nil );
    Debug2("%s\t%u\n", "strong pointers",  counts->strong );
    Debug2("%s\t%u\n", "weak pointers",    counts->weak );
    Debug2("%s\t%u\n", "struct nodes",    counts->hdtl );
    Debug2("%s\t%u\n", "atom nodes",    counts->atom );
    
    /* 1. check(count of nodes reachable from {root,defs,builtin} == zone_inuse_count) */
    if (zone_inuse_count != (counts->hdtl + counts->atom))
      Debug4( "!!inuse count: %u but found %u==(%u+%u)\n", zone_inuse_count, (counts->hdtl + counts->atom), counts->hdtl, counts->atom);
    
    /* 1a. check for tree structure: strong_count == zone_inuse_count */
    if (zone_inuse_count == counts->strong)
      is_tree = 1;
    
    /* traverse freelist adding to free_counts, check them , then add to total */
    /* freelist - list of available nodes not used by the program graph */
    {
      zone_check_counts theCounts = zero_zone_check_counts, *free_counts = &theCounts;
      res += zone_check_one_root(freelist, (zone_inuse_count + zone_free_count)*2, free_counts, "freelist"); /* freelist - chain of free nodes */
      
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
      zone_check_counts_add(counts, free_counts);
    }
    
    if (res)
      return zone_check_log2("!!check pointer traverse failed\n", 66, res, 0);
    
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
        res += zone_check_traverse_nodes(z, &data, zone_check_traverse_valid, 0 /*do not ignore empty*/);
      
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
        return zone_check_log2("!!check traverse nodes failed\n", 66, res, 0);
    }
  }
  
  return 0;
}

/*
 * zone_check_island(pointer n)
 * does node p point to a self-contained graph, where all the pointers to p and Reach(p) are within the graph?
 * returns number of external pointers in a zone_check_node_data (zero_zone_check_node for an island)
 *
 * Method:
 *  Clear all check nodes
 *  Populate check notes for Reach*(n)  (ie all nodes reachable from n, possibly including n)
 *  Compare nodes to check nodes for Reach*(n)
 *    Any "excess" pointers are counted and the total returned.
 *    Erroneous "deficit" pointers are will already have been reported
 *    If there are no excess pointers, the node is on an island and should be freed.
 *    Otherwise
 *       case (a) root protected by strong pointers
 *       case (b) protected by non-root strong pointers
 *       case (c) protected only by non-root weak pointers - and possibly non-root weak pointers also
 *
 * Returns 0 if an island, non-zero if there are external pointers.
 *
 * Usage if(zone_check_island(p)) printf("it's free");
 *
 * WARNING overwrites all check nodes
 * WARNING not reentrant
 */
int zone_check_island(pointer n, unsigned depth)
{
  int res;
  zone_check_counts theCounts;
  
  if (! HasPointers(n))
    return 0;
  
  zone_debug_reset();
  
  /* populate check node in Reach(n) */
  //2020-01-06 re-enabled: p indicates which node might be deleted, having just deleted a pointer to it.   //2020-01-04 disabled again  //2019-01-16 reinstated /*XXX XXX 2018-11-09*/
  theCounts = zero_zone_check_counts;
  res  = zone_check_traverse_pointers(H(n), zone_inuse_count*2, Node(n)/*stop here*/, &theCounts);
  res += zone_check_traverse_pointers(T(n), zone_inuse_count*2, Node(n)/*stop here*/, &theCounts);
  
  if (res)
    err_zone("zone_check_island: problem traversing pointers"); /* too drastic? */
  
  {
    zone_header *z;
    zone_check_node_data data = zero_data;
    
    /* inspect the check nodes in every zone and report discreprancies and accumulating data */
    for (z = zone_current; z; z = z->previous) {
      res += zone_check_traverse_nodes(z, &data, zone_check_traverse_valid_island, 1 /*ignore unpopulated*/);
    }
    if (res)
      err_zone("zone_check_island: problem traversing nodes"); /* too drastic? */
    
//    if (data.deficit.s)
//      err_zone1("zone_check_island: strong refc deficit: ", data.deficit.s);
//    if (data.deficit.w)
//      err_zone1("zone_check_island: weak refc deficit: ", data.deficit.w);

    /* start-logging */
    Log1("zone_check_island%s ", zone_pointer_info(n));
    if (data.excess.s)
      Log1(" protected by strong pointers: %u", data.excess.s);
    if (data.excess.w)
      Log1(" protected by weak pointers: %u",   data.excess.w);
    Log("\n");
    /* end-logging */
    
    return data.excess.s || data.excess.w;
  }
}

/* externally usable version */
int zone_check()
{
  return zone_check_do(root, defs, zone_freelist);
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

