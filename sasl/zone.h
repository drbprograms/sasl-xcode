

/* *** *** *** */
extern unsigned refc_inuse(void);
extern unsigned refc_free(void);

extern pointer new_node(tag t);
extern char *err_tag_name(tag t);
extern void free_node(pointer p);

extern void new_log_report(FILE *where);
extern char *zone_pointer_info(pointer p);
extern int zone_check(void);

typedef struct {unsigned s; unsigned w;} refc_pair;
extern const refc_pair zero_refc_pair;

extern refc_pair zone_check_island(pointer p);

