extern pointer parse_program(void);
extern int parse_reset(void);

#define Parse_Debug(where)		(debug > 1 ? fprintf(stderr, "%s\n", where) : 0)
#define Parse_Debug3(where, i, j, k)	(debug > 1 ? fprintf(stderr, "%s %d %d (%d)\n", where, i, j, k) : 0)
#define Parse_Debug1(where, s)	(debug ? fprintf(stderr, "%s%s\n", where, s) : 0) 

