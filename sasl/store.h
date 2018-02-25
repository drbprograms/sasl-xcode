extern pointer make_append(pointer list, pointer tl);

extern pointer new_copy(pointer p);
extern pointer new_nil_list(void);
extern pointer new_int(int i);
extern pointer new_double(double d);
extern pointer new_char(char c);
extern pointer new_bool(char b);
extern pointer new_fail(void);
extern pointer new_apply(pointer hd, pointer tl);
extern pointer new_apply3(pointer hh, pointer th, pointer t);
extern pointer new_apply4(pointer hhh, pointer thh, pointer th, pointer t);


extern pointer new_cons(pointer hd, pointer tl);
extern pointer new_abstract(pointer name, pointer def, int r);
extern pointer new_def(pointer name, pointer def);
extern pointer add_to_def(pointer def, pointer name, pointer d);
extern pointer new_name(char *s);
extern pointer new_oper(tag oper);
extern pointer new_comb_name(tag t, pointer name);
extern pointer new_comb(tag t);
extern pointer new_unary_predicate(char *name, int (*fun)(pointer p));
extern pointer new_unary_maths(char *name, int (*fun)(pointer p));

extern void refc_delete(pointer *pp);
extern pointer refc_update(pointer n, pointer new);
extern pointer refc_update_to_int(pointer n, int i);
extern pointer refc_update_to_bool(pointer n, char b);
extern pointer refc_update_to_char(pointer n, char c);
extern pointer refc_update_to_double(pointer n, double d);
extern pointer refc_update_to_fail(pointer n);

extern pointer refc_copy(pointer p);
extern pointer refc_copy_make_cyclic(pointer p);
extern pointer refc_update_hd(pointer n, pointer new);
extern pointer refc_update_tl(pointer n, pointer new);
extern pointer refc_update_hdtl(pointer n, pointer newhd, pointer newtl);
extern pointer refc_move_h2h(pointer from, pointer to);

extern int refc_check(void);
extern char *refc_pointer_info(pointer p);
extern void refc_log_report(FILE *where);
