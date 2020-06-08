/* *** *** *** */
extern unsigned zone_inuse(void);
extern unsigned zone_free(void);

extern pointer new_node(tag t);
extern char *err_tag_name(tag t);
extern void free_node(pointer p);

extern void zone_log_report(FILE *where);
extern char *zone_pointer_info(pointer p);
extern int zone_check(void);

extern int zone_check_island(pointer p, unsigned depth);

