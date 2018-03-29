extern pointer reduce_print(pointer *p);
extern pointer reduce(pointer *n);
extern int reduce_init(void);

extern void reduce_log_report(FILE *where);
extern void reduce_final_report(FILE *where);

#define REDUCE_DEBUG0(where)           (debug ? fprintf(stderr, "reduce: %s\n", where) : 0)
#define REDUCE_DEBUG1(where, what)     (debug ? fprintf(stderr, "reduce: %s %d\n", where, what) : 0)
