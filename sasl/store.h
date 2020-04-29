extern pointer make_append(pointer list, pointer tl);

extern pointer new_int(int i);
extern pointer new_double(double d);
extern pointer new_char(char c);
extern pointer new_bool(char b);
extern pointer new_fail(void);
extern pointer new_apply(pointer hd, pointer tl);
extern pointer new_apply3(pointer hh, pointer th, pointer t);
extern pointer new_apply4(pointer hhh, pointer thh, pointer th, pointer t);

extern pointer new_cons(pointer hd, pointer tl);
extern pointer new_abstract(pointer name, pointer def, tag t);
extern pointer new_def(pointer name, pointer def);
extern pointer add_to_def(pointer def, pointer name, pointer d);
extern pointer add_deflist_to_def(pointer def, pointer deflist);
extern pointer *def_for2(pointer names, pointer defs, pointer n);
extern pointer def_any_for2(pointer names, pointer defs, pointer n);
extern pointer *def_for(pointer def, pointer n);
extern pointer new_name(char *s);
extern pointer new_oper(tag oper);
extern pointer new_comb_label(tag t, pointer name);
extern pointer new_comb(tag t);
extern pointer new_unary_strict(char *name, pointer (*fun)(pointer p));
extern pointer new_unary_nonstrict(char *name, pointer (*fun)(pointer p));

extern void refc_delete(pointer *pp);
extern pointer refc_update(pointer n, pointer new);
extern pointer refc_update_to_int(pointer n, int i);
extern pointer refc_update_to_bool(pointer n, char b);
extern pointer refc_update_to_char(pointer n, char c);
extern pointer refc_update_to_double(pointer n, double d);
extern pointer refc_update_to_fail(pointer n);

extern pointer refc_copy(pointer p);
extern pointer refc_copy_make_cyclic(pointer p);
extern pointer refc_copyH(pointer p);
extern pointer refc_copyT(pointer p);
extern pointer refc_copyS(pointer p, char *s);
extern pointer refc_copyNth(pointer p, unsigned n);

extern pointer refc_update_Itl(pointer n, pointer newtl);

extern pointer refc_update_hdS(pointer *p, char *h);
extern void refc_updateSS(pointer *p, char *h, char *t);
extern void refc_updateIS(pointer *pp, char *t);
extern pointer refc_update_pointerS(pointer *p, char *s);

extern pointer refc_update_hdtl(pointer n, pointer newhd, pointer newtl);
extern pointer refc_move_h2h(pointer from, pointer to);

extern int refc_check(void);
extern char *refc_pointer_info(pointer p);

extern void refc_log_report(FILE *where);
extern void refc_final_report(FILE *where);

extern int store_init(void);
extern int store_done(void);

