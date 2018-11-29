

/* *** *** *** */
extern unsigned refc_inuse(void);
extern unsigned refc_free(void);

extern pointer new_node(tag t);
extern char *err_tag_name(tag t);
extern void free_node(pointer p);

extern void new_log_report(FILE *where);
extern char *zone_pointer_info(pointer p);
extern int zone_check(void);

typedef struct { unsigned s; unsigned w;} refc_pair;
#define HasRefs(e) ((e).s || (e).w)

extern const refc_pair zero_refc_pair;

/* used to accumulate refc stored in (non-debug) nodes and
 *  deficit == missing pointers found only in debug nodes
 *  excess  == extra pointers found only in debug nodes
 */
typedef struct { refc_pair total, excess, deficit;} zone_check_node_data;

extern zone_check_node_data zone_check_island(pointer p, unsigned depth);
extern int zone_is_island(zone_check_node_data data);

extern const zone_check_node_data zero_data;
char *zone_node_info(zone_check_node_data info);
