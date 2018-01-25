extern void reduce_print(pointer p);
extern pointer reduce(pointer n);

extern void reduce_log_report(FILE *where);

#define REDUCE_DEBUG(where, what) 	(debug ? fprintf(stderr, "reduce: %s %d\n", where, what) : 0)
