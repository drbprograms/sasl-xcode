

/* *** *** *** */
extern int refc_inuse();
extern int refc_free();

extern pointer new_node(tag t);
extern char *err_tag_name(tag t);
extern void free_node(pointer p);

extern void new_log_report(FILE *where);
extern char *zone_pointer_info(pointer p);
extern int zone_check();
extern int refc_isfree(pointer p);
