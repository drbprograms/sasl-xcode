/*
 storage allocation - bare metal
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"

#include "zone.h"

const refc_pair zero_refc_pair = {0,0};
const zone_check_node_data zero_data = {{0,0},{0,0},{0,0}};

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
  unsigned size;  /* number of nodes available in the zone */
  unsigned used;  /* number of nodes used by new_node().  assert(used <= size) */
  node *nodes;  /* pointer to first node to be used - fixed at the time zone is created by new_zone() */
  node *check_nodes;  /* if check is set, pointer to first check node - fixed at the time zone is created by new_zone() else 0 */
  enum zone_t_type type;  /* type of zone - may alter the significance of the nodes therein - fixed at the time zone is created by new_zone() */
  unsigned seq;
  struct zone_header *previous, *next;  /* linked list of zones - we avoid a master list to keep things flexible */
} zone_header;

static unsigned zone_new_count = 0;  /* total number of zones created by new_zone() */
static unsigned zone_total_size = 0; /* sum of zone->size for all zones created by new_zone() */
static unsigned zone_seq = 1; /* sequence number counting how many new zones hav been made. Acts as unique id for zone_pointer_info() */

/* zone_header points to the most recently created zone */
static zone_header *zone_current = 0;  /* zone currently being used by new_node() to create nodes */

static unsigned zone_size_default = 1024;  /* to be updated elsewhere */

void zone_new_log(zone_header *z)
{
  Debug3("zone_new: seq: %u\tsize:\t%u\t%s\n", z->seq, z->size, z->check_nodes ? " with checking" : "") ;
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

/* helper: get non-NIL pointer's zone seq and offset in zone
 * returns 0 for success
 * if chk then check node info is returned
 */
static int zone_pointer_detail(pointer p, unsigned *seq, long *off, int chk)
{
  zone_header *z;
  
  Assert(chk ? check : 1);  /* chk only set if checking */
  
  if (IsNil(p)) {
    *seq = 0;
    *off = 0;
  } else {
    z = zone_of_node(Node(p));
    if (!z) {
      *seq = 0;
      *off = 0;
      Log1("zone_pointer_detail failed  (p:0x%p)\n", Node(p));
      return 1; /*fail*/
    }
    *seq = z->seq;
    if (chk)
      *off = Node(p) - z->check_nodes;
    else
      *off = Node(p) - z->nodes;
  }
  return 0;
}

static char *zone_pointer_info_do(pointer p, int chk)
{
  unsigned seq;
  long off;
  int c;

  Assert(chk ? check : 1);  /* chk only set if checking */

  if (IsNil(p))
    return "NIL";
  
  if (zone_pointer_detail(p, &seq, &off, chk)) {
    /* fail */
    if (chk)
      err_refc("can't get pointer check zone info");
    else
      err_refc("can't get pointer zone info");
  }
  
  c = snprintf(s, MAX, "/%c (s/w %u/%u) [%u.%ld] %s",
              (IsStrong(p)?'s':'w'),
              Srefc(p),
              Wrefc(p),
              seq,
              off,
              err_tag_name(Tag(p))
              );
  
  if (!chk && debug >1) {
    /* debugging: add detail of pointed-to non-check nodes when debug > 1 */
    if (HasPointers(p)) {
      unsigned seq_hd, seq_tl;
      long off_hd, off_tl;
      if (zone_pointer_detail(Hd(p), &seq_hd, &off_hd, chk))
        err_refc("can't get pointer hd info");
      
      if (zone_pointer_detail(Tl(p), &seq_tl, &off_tl, chk))
        err_refc("can't get pointer tl info");
      
      snprintf(s + c, MAX - c, " [%u.%ld]%s.[%u.%ld]%s",
              seq_hd,
              off_hd,
              Label(Hd(p)),
              seq_tl,
              off_tl,
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
  snprintf(i, MAX, "(s/w %u/%u) (s+/w+ %u/%u) (s-/w- %u/%u)",
           data.total.s,    data.total.w,
           data.excess.s,   data.excess.w,
           data.deficit.s,  data.deficit.w);
  return i;
}
/*
 zone_new - allocate storage for 'size' nodes; point back to 'parent' (if any).  `
 Set up zone_header at start of zone for use by zone_xxx() and new_XXX() only.
 If 'check', then allocate double space at 'check_nodes' for use in consistency checking.
 for any given (node *)n, check info resides at (node *)(n + z->size) where z is the zone in question
 
 Always returns a valid zone, caller does not need to check (or takes err()).
 */
zone_header *zone_new(unsigned size, struct zone_header *parent)
{
  long unsigned need = sizeof(zone_header) + (size * sizeof(node)) * (check ? 2 : 1);
  zone_header *z = malloc(need);

  if (z==NULL) {
    (void) err_zone("out of space");
    return 0; /*NOTREACHED*/
  }
  
  z->size = size;
  z->nodes = (node *) (z + sizeof(zone_header));
  z->check_nodes = (check ? z->nodes + (size * sizeof(node)) : NULL);
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

int refc_check_log2(char *msg, int result, int i, int j)
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
 * update check info for a pointer and pointed to node
 * returns ALLrefc of check node
 */
static int refc_check_visit_node(pointer p, unsigned s_limit, unsigned *nil_count,  unsigned *s_count, unsigned *w_count, unsigned *struct_count, unsigned *atom_count)
{
  zone_header *z;
  long int node_no;  /* offset of Node(p) within the zone */

  /* count the pointer */
  if (IsStrong(p)) {
    (*s_count)++;
    
    if ((s_limit > 0) && (*s_count > s_limit)) {
      if (debug > 1) {
        /* continue regardless - DANGER!*/
        Debug1("strong pointer loop detected (limit=%u)\n", s_limit);
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
    (*w_count)++;
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

static int refc_check_traverse_pointers_do(pointer p, unsigned s_limit, unsigned *nil_count,  unsigned *s_count, unsigned *w_count, unsigned *struct_count, unsigned *atom_count)
{
  while (1) {
    /* pointer nodes, not previously visited */
    while (HasPointers(p)) {
      if (refc_check_visit_node(p, s_limit, nil_count, s_count, w_count, struct_count, atom_count) == 1) {
        (*struct_count)++;
        if (mem_dump)
          Debug1("mem_dump%s\n",zone_pointer_info(p));
      }
      else
        break; /* only visit/count pointed-to node first time through */
      
      Push(p);
      p = H(p);
    }
    
    if (IsNil(p)) {
      (*nil_count)++;
    } else if (!HasPointers(p)) {
      if (refc_check_visit_node(p, s_limit, nil_count, s_count, w_count, struct_count, atom_count) == 1) {
        (*atom_count)++;
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

int refc_check_traverse_pointers0(pointer p, unsigned s_limit, unsigned *nil_count,  unsigned *s_count, unsigned *w_count, unsigned *struct_count, unsigned *atom_count)
{
  int res;
  
  /* Assert(is_tree(n)) - NO loops! */
  
  stack_size = s_limit;
  sp = stack = new_table(stack_size , sizeof(pointer));
  
  res = refc_check_traverse_pointers_do(p, s_limit, nil_count, s_count, w_count, struct_count, atom_count);
  
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
 s_count count of strong pointers - passed by reference
 w_count count of strong pointers  - passed by reference
 struct_count count of struct nodes  - passed by reference
 atom_count count of non-struct nodes  - passed by reference
 
 for each pointer: increment the strong or weak reference count of the pointed-to- node's debug_node
 
 returns 0 if all ok, otherwise >0
 
 */
int refc_check_traverse_pointers(pointer p, int s_limit, int *nil_count,  int *s_count, int *w_count, int *struct_count, int *atom_count)
{
  zone_header *z;
  long int node_no;  /* offset of Node(p) within the zeon */
  
  /* pointer counts */
  if (IsNil(p)) {
    (*nil_count)++;
    return 0;  /* nothing else to do */
  }
  
  if (IsStrong(p)) {
    (*s_count)++;
    
    if ((s_limit > 0) && (*s_count > s_limit)) {
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
    (*w_count)++;
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
    
    i =  refc_check_traverse_pointers(Hd(p), s_limit, nil_count, s_count, w_count, struct_count, atom_count);
    i += refc_check_traverse_pointers(Tl(p), s_limit, nil_count, s_count, w_count, struct_count, atom_count);
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

/* inspect check_node_info: is an island if there are not excess strong or weak pointers */
int zone_is_island(zone_check_node_data data)
{
  return ! (HasRefs(data.excess) || HasRefs(data.deficit)); //XXX is this really correct!??
}


static int refc_check_traverse_valid_island(int res_i, pointer p, pointer debug_p)
{
  if (logging && res_i) {
    Log5("zone_check_island: refc_check_traverse_valid_island%s\t(s+/w+) (%u/%u)\t(s-/w-) (%u/%u)",
         zone_pointer_info(p),
         (Srefc(p) > Srefc(debug_p) ? Srefc(p) - Srefc(debug_p): 0),
         (Wrefc(p) > Wrefc(debug_p) ? Wrefc(p) - Wrefc(debug_p): 0),
         (Srefc(debug_p) > Srefc(p) ? Srefc(debug_p) - Srefc(p): 0),
         (Wrefc(debug_p) > Wrefc(p) ? Wrefc(debug_p) - Wrefc(p): 0));
    if (Srefc(p) > Srefc(debug_p))
      Log(" loop protected by strong pointer");
    if (Wrefc(p) > Wrefc(debug_p))
      Log(" loop protected by weak pointer");
    if (Srefc(debug_p) > Srefc(p))
      Log(" strong pointer deficit");
    if (Wrefc(debug_p) > Wrefc(p))
      Log(" weak pointer deficit");
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

static int refc_check_traverse_nodes(zone_header *z, int zone_no, zone_check_node_data *data, int (check(int res_i, pointer p, pointer debug_p)), int ignore_unpopulated)
{
  int i, res = 0;
  
  /* for a zone, compare any check nodes to the correspondng nodes */
  for (i = 0; i < z->size; i++) {
    int res_i = 0;
 
    Assert(z->check_nodes);
    if (ignore_unpopulated && z->check_nodes[i].t == zero_t) /* ignore unpopulated nodes */
      continue;
    
    /* accumulate node info, noting mismatch in res_i (taking care not to generate "negative" unsigneds) */
    
    /* strong */
    data->total.s += z->nodes[i].s_refc;
    if (z->nodes[i].s_refc  != z->check_nodes[i].s_refc) {
      res_i++;
      if (z->nodes[i].s_refc > z->check_nodes[i].s_refc)
        data->excess.s +=
          z->nodes[i].s_refc - z->check_nodes[i].s_refc;
      else
      if (z->check_nodes[i].s_refc > z->nodes[i].s_refc)
        data->deficit.s +=
          z->check_nodes[i].s_refc - z->nodes[i].s_refc;
    }
    
    /* weak */
    data->total.w += z->nodes[i].w_refc;
    if (z->nodes[i].w_refc  != z->check_nodes[i].w_refc) {
      res_i++;
      if (z->nodes[i].w_refc > z->check_nodes[i].w_refc)
        data->excess.w +=
          z->nodes[i].w_refc - z->check_nodes[i].w_refc;
      else
      if (z->check_nodes[i].w_refc > z->nodes[i].w_refc)
        data->deficit.w +=
          z->check_nodes[i].w_refc - z->nodes[i].w_refc;
    }

    /* further validation - nodes in use always have a strong pointer */
    /* ... *except* during refc_delete() as indicated by deleting_t  */
    if (z->nodes[i].s_refc == 0 && z->nodes[i].w_refc > 0 && z->nodes[i].t != deleting_t)
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

/* refc_check_loop
 * follow all strong (and only strong) pointers, counting nodes visited (in *s_count).
 * iff there is a strong loop then s_count will exceed s_limit -> report error
 * This funciton is needed because refc_check_traverse_pointers() only visits Hd/Tl first time through, preventing loop detection in all but pathological cases!
 *
 * usage if (refc_loop_check(p, limit)) bad; ok;
 */
int refc_check_loop_do(pointer p, unsigned s_limit, unsigned s_count)
{
  
  if (IsNil(p) ||
      IsWeak(p) ||
      ! HasPointers(p))
    return 0;
  
  if ((s_count++) > s_limit) {
    Debug2("!!loop_check: found loop size %u: at: %s: ", s_count, zone_pointer_info(p));
    return 1;
  }
  
  return (
          refc_check_loop_do(H(p), s_limit, s_count) ||
          refc_check_loop_do(T(p), s_limit, s_count));
}

/* wrapper */
#define Limit 24
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
    unsigned total_size = 0;  /* sum of size of zones */
    unsigned total_used = 0;  /* sum of used of zones */
    
    zone_header *z;
    
    /* report zone info */
    Debug2("%s\t%u\n", "zones in use", zone_new_count);
    Debug2("%s\t%u\n", "total zone size", zone_total_size);
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
      return refc_check_log2("!!zone_total_size:%u but found %u\n", 3, zone_total_size, total_size);
    
    if (refc_inuse_count > zone_total_size)
      return refc_check_log2("!!refc_inuse_count:%u but zone total size %u\n", 4, refc_inuse_count, total_size);
    
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
    /* ponter counts */
    unsigned nil_count = 0;    /* number of nil pointers, including root, refc_free */
    unsigned strong_count = 0;  /* number of strong pointers, including root, refc_freelist if they are non-nil */
    unsigned weak_count = 0;    /* number of weak pointers */
    /* node counts */
    unsigned struct_count = 0;
    unsigned atom_count = 0;
    
    int res = 0;
    int is_tree = 0;
    
    zone_header *z;
    
    zone_debug_reset();  /* clear all check_nodes for use */
    
    /* check the counts and populate check nodes */

    /* root - the program graph */
    Debug1("root\t%s\n", zone_pointer_info(root));
    if (IsNil(root)) {
      Debug("root pointer is nil\n");
    } else{
      if (IsWeak(root))
        Debug("!!root pointer is weak - unexpected\n");
      
      res += refc_check_traverse_pointers0(root, refc_inuse_count*2, &nil_count, &strong_count, &weak_count, &struct_count, &atom_count);
      res += refc_check_loop(root, refc_inuse_count*2);
    }
    
    /* defs - saved definitions */
    Debug1("defs\t%s\n", zone_pointer_info(defs));
    if (IsNil(defs)) {
      Debug("defs pointer is nil\n");
    } else{
      if (IsWeak(defs))
        Debug("!!defs pointer is weak - unexpected\n");
      
      res = refc_check_traverse_pointers0(defs, refc_inuse_count*2, &nil_count, &strong_count, &weak_count, &struct_count, &atom_count);
      res += refc_check_loop(defs, refc_inuse_count*2);
    }
    
    /* sasl - builtin definitions */
    Debug1("builtin\t%s\n", zone_pointer_info(builtin));
    if (IsNil(builtin)) {
      Debug("builtin pointer is nil\n");
    } else{
      if (IsWeak(builtin))
        Debug("!!builtin pointer is weak - unexpected\n");
      
      res = refc_check_traverse_pointers0(builtin, refc_inuse_count*2, &nil_count, &strong_count, &weak_count, &struct_count, &atom_count);
      res += refc_check_loop(builtin, refc_inuse_count*2);
    }
    
    Debug2("%s\t%u\n", "nil pointers",    nil_count );
    Debug2("%s\t%u\n", "strong pointers",  strong_count );
    Debug2("%s\t%u\n", "weak pointers",    weak_count );
    Debug2("%s\t%u\n", "struct nodes",    struct_count );
    Debug2("%s\t%u\n", "atom nodes",    atom_count );
    
    /* 1. check(count of nodes reachable from root == refc_inuse_count) */
    if (refc_inuse_count != (struct_count + atom_count))
      Debug4( "!!inuse count: %u but found %u==(%u+%u)\n", refc_inuse_count, (struct_count + atom_count), struct_count, atom_count);
    
    /* 1a. check for tree structure: strong_count == refc_inuse_count */
    if (refc_inuse_count == strong_count)
      is_tree = 1;
    
    /* save previous total, traverse freelist adding to counts */
    
    
    /* freelist - list of available nodes not used by the program graph */
    Debug1("freelist\t%s\n", zone_pointer_info(freelist));
    {
      unsigned free_nil_count = 0;    /* number of nil pointers, including root, refc_free */
      unsigned free_strong_count = 0;  /* number of strong pointers, including root, refc_freelist if they are non-nil */
      unsigned free_weak_count = 0;    /* number of weak pointers */
      /* node counts */
      unsigned free_struct_count = 0;
      unsigned free_atom_count = 0;
      
      if (IsNil(freelist)) {
        Debug("freelist pointer is nil\n");
      } else{
        if (IsWeak(freelist))
          Debug("!!freelist pointer is weak - unexpected\n");
        
        res += refc_check_traverse_pointers0(freelist, (refc_inuse_count + refc_free_count)*2, &free_nil_count, &free_strong_count, &free_weak_count, &free_struct_count, &free_atom_count);
        res += refc_check_loop(freelist, free_struct_count*2);
      }
      
      Debug2("%s\t%u\n", "freelist nodes",    free_strong_count);
      
      /* 2. check(count of nodes reachable from freelist == refc_free_count */
      /*
       assert(strong_count == refc_free_count)
       assert(struct_count == refc_free_count)
       assert(nil_count == refc_free_count) - each node - has one NIL pointer - last has two
       assert(weak_count == 0)
       assert(atom_count == 0)
       */
      if (free_strong_count != refc_free_count)
        Debug2("!!free:%u but free count:%u\n", free_strong_count, refc_free_count);
      
      if (free_struct_count != refc_free_count)
        Debug2("!!free structures:%u but free count:%u\n", free_struct_count, refc_free_count);
      
      if(IsSet(freelist) &&
         (free_nil_count != refc_free_count +1))
        Debug2("!!free nil count:%u but expecting:%u\n", free_nil_count, refc_free_count +1);
      
      if (free_weak_count != 0)
        Debug1("!!freelist contains %u weak pointers\n", free_weak_count);
      
      if (free_atom_count != 0)
        Debug1("!!freelist contains %u atoms\n", free_atom_count);
      
      /* increment totals for whole-store checks below */
      nil_count    += free_nil_count;
      strong_count   += free_strong_count;
      weak_count  += free_weak_count;
      struct_count  += free_struct_count;
      atom_count  += free_atom_count;
    }
    
    if (res)
      return refc_check_log2("!!check pointer traverse failed\n", 66, res, 0);
    
    if ((strong_count + weak_count)  == (struct_count + atom_count))
      Debug6("%s\t(%u+%u)==(%u+%u)==%u\n", (is_tree ? "pointer check ok (tree)" : "pointer check ok"),
                     strong_count, weak_count, struct_count, atom_count, (strong_count + weak_count));
    
    /* inspect the check nodes in every zone and report discreprancies  */
    {
      zone_check_node_data info = zero_data;
      unsigned i;
      
      for (z = zone_current, i = 0;
           z;
           z = z->previous, i++)
#if 0
        res += refc_check_traverse_nodes(z, i, &info, refc_check_traverse_valid);
#else
        res += refc_check_traverse_nodes(z, i, &info, refc_check_traverse_valid, 0 /*do not ignore empty*/);
#endif
      
      Debug2("%s\t%u\n", "strong ref counts",  strong_count );
      Debug2("%s\t%u\n", "weak ref counts",  weak_count );
      
      /* 3. check(count of all pointers == sum ALLrefc) */
      if ((strong_count + weak_count) != (info.total.s + info.total.w))
        Debug2("!!pointers:%u but ALLrefc %u\n ", (strong_count + weak_count), (info.total.s + info.total.w));
      
      /* 4. check(count of weak pointers == sum Wrefc) */
      if (strong_count != info.total.s)
        Debug2("!!strong pointers:%u but Srefc %u\n ", strong_count, info.total.s);
      
      /* 5. check(count of strong pointers == sum Srefc) */
      if (weak_count != info.total.w)
        Debug2("!!weak pointers:%u but Wrefc %u\n ", weak_count, info.total.w);
      
      if ((strong_count + weak_count) == (info.total.s + info.total.w))
        Debug6("%s\t(%u+%u)==(%u+%u)==%u\n",
                       "reference count check ok",
                       strong_count,
                       weak_count,
                       info.total.s,
                       info.total.w,
                       (strong_count + weak_count));
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
 *    Any "extra" pointers are counted and the total returned.
 *
 * WARNING overwrites all check nodes
 * WARNING not reentrant
 */
zone_check_node_data zone_check_island(pointer p, unsigned depth)
{
  int res;
  unsigned nil_count = 0, s_count = 0, w_count = 0, struct_count = 0, atom_count = 0;
  
  zone_debug_reset();
  
//  Assert(HasPointers(p));
  if (! HasPointers(p))
    return zero_data;
    
#if 1 //2019-01-16 reinstated /*XXX XXX 2018-11-09*/
  res = refc_check_traverse_pointers0(H(p), refc_inuse_count*2, &nil_count, &s_count, &w_count, &struct_count, &atom_count);
  res = refc_check_traverse_pointers0(T(p), refc_inuse_count*2, &nil_count, &s_count, &w_count, &struct_count, &atom_count);
#else
  res = refc_check_traverse_pointers0(p, refc_inuse_count*2, &nil_count, &s_count, &w_count, &struct_count, &atom_count);
#endif
  /* inspect the check nodes in every zone and report discreprancies  */
  {
    zone_header *z;
    zone_check_node_data data = zero_data;
    unsigned i;
    
    for (z = zone_current, i = 0;
         z;
         z = z->previous, i++) {

      (void) refc_check_traverse_nodes(z, i, &data, refc_check_traverse_valid_island, 1);
    }

    Log2("zone_check_island: %s (depth=%u)\n", zone_node_info(data), depth);
#if 0
    Debug("root\t%s\n", zone_pointer_info(root));
    Debug("defs\t%s\n", zone_pointer_info(defs));
    Debug("builtin\t%s\n", zone_pointer_info(builtin));
#endif
    
    return data;
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

