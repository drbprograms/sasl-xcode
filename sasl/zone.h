/* *** *** *** */
extern unsigned zone_inuse(void);
extern unsigned zone_free(void);

extern void zone_weaken(pointer *pp);
extern void zone_strengthen(pointer *pp);
extern void zone_erase(pointer *pp);


extern pointer new_node(tag t);
extern char *err_tag_name(tag t);
extern void free_node(pointer p);

extern void zone_log_report(FILE *where);
extern char *zone_pointer_info(pointer p);
extern int zone_check(void);


unsigned check_set_inuse(pointer n);
unsigned free_2021algorithm(pointer *pp);
unsigned set_inuse_2021algorithm(pointer *pp);
extern unsigned zone_check_deletion(pointer n);
