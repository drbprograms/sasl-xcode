

/* *** *** *** */
extern int refc_inuse(void);
extern int refc_free(void);

extern pointer new_node(tag t);
extern char *err_tag_name(tag t);
extern void free_node(pointer p);

extern void new_log_report(FILE *where);
extern char *zone_pointer_info(pointer p);
extern int zone_check(void);
extern int refc_isfree(pointer p);
