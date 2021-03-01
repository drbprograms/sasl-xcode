/* *** *** *** */
extern unsigned zone_inuse(void);
extern unsigned zone_free(void);

extern void zone_weaken(pointer *pp);
extern void zone_strengthen(pointer *pp);
extern void zone_erase(pointer *pp);

extern pointer new_atom(tag t);
extern pointer new_hdtl(tag t, pointer hd, pointer tl);
extern pointer change_tag(pointer p, tag t);

extern pointer zone_update(pointer *pp, pointer new);

extern pointer set_hd(pointer p, pointer hd);
extern pointer set_tl(pointer p, pointer tl);

extern char *err_tag_name(tag t);
extern void free_node(pointer p);

extern void zone_log_report(FILE *where);
extern char *zone_pointer_info(pointer p);
extern int zone_check(void);

void zone_delete2021(pointer p);

unsigned check_set_inuse(pointer n);
extern unsigned zone_check_deletion(pointer n);
