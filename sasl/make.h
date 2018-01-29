extern int maker(int howmany, char *ruledef, int rule, int subrule, int info);
#define Maker0(def,r,s)		maker(0,(def),(r),(s),0) /* zero items on stack */
#define Maker1(def,r,s) 	maker(1,(def),(r),(s),0) /* one item on stack */
#define Maker2(def,r,s) 	maker(2,(def),(r),(s),0) /* two items on stack */
#define MakerN(n,def,r,s)	maker(n,(def),(r),(s),0) /* n items on stack*/

#define Maker0i(def,r,s,i)  maker(0,(def),(r),(s),(i)) /* Maker0 + info */
#define Maker1i(def,r,s,i) 	maker(1,(def),(r),(s),(i))
#define Maker2i(def,r,s,i) 	maker(2,(def),(r),(s),(i))
#define MakerNi(n,def,r,s,i)	maker(n,(def),(r),(s),(i))

#define MAKE_DEBUG(where) 		(debug>1 ? fprintf(stderr, "%s", where) : 0)

extern pointer make_reset(void);
extern pointer make_result(void);
