/*
 * common data types and functions
 */

/*
 * the graph data types
 */

struct node; /* forward reference */

typedef struct pointer {
  struct node *p;
  unsigned char bit:1;
} pointer;	/* ++ sizeof(pointer) => 16 (sizeof(node *) + sizeof(unisnged char) + padding) == (16 +1 +15bytes padding!) */

extern pointer root;
extern pointer defs;
extern const pointer NIL;

/* NB numeric values here need to fit into a 'char' in tok->t below */
/* NB changes here need to be reflected in zone.c where the printable version of the tags is made */
typedef enum tag {
    /* Constants - node contains the value of the constant and neveer points to anything else */
  int_t,
#define IsNumTag(t) ((t)==int_t)
  floating_t,
#define IsDblTag(t) ((t)==floating_t)
  char_t,
#define IsCharTag(t) ((t)==char_t)
  bool_t,
#define IsBoolTag(t) ((t)==bool_t)
#define IsConstTag(t) ((t)>=int_t && (t)<=bool_t)
  /* Special constant - to indicate pattern match has failed */
  fail_t,
#define IsFailTag(t) ((t)==fail_t)  


  
  /* Apply/Cons - these have a hl and tl (aka car and cdr) which can pont to any other node, or be NIL */
  apply_t,
#define IsApplyTag(t) ((t)==apply_t)
  cons_t,
#define IsConsTag(t) ((t)==cons_t)
#define IsStructTag(t) ((t)==apply_t || (t)==cons_t)
  /* Name/Abstract tags used for partially compiled code - todo decide whether these are prohibited post-compilation */
  name_t,
#define IsNameTag(t) ((t)==name_t)
  abstract_t,
#define IsAbstractTag(t) ((t)==abstract_t)
  recursive_abstract_t,
#define IsRecursiveAbstractTag(t) ((t)==abstract_t)
  def_t,
#define IsDefTag(t) ((t)==def_t)
  
#define HasPointersTag(t) (IsStructTag(t) || IsCombTag(t) || IsAbstractTag(t) || IsRecursiveAbstractTag(t) || IsDefTag(t))
  /* TODO when there's time to conduct before/aafter testing 
     # define HasPointers(t) ((t) >= apply_t) */


  /* Operators - tl contains first argument (or NIL) */
  cond_op,
#define IsTernaryOpTag(t) ((t) == cond_op)
  /* binary operators */
  colon_op,
  plusplus_op,
  minusminus_op,

  range_op,
  
  range_unbounded_op,
  
  or_op,
  
  and_op,
  
  much_greater_op,
  greater_op,
  greater_equal_op,
  equal_op,
  not_equal_op,
  less_equal_op,
  less_op,
  much_less_op,

  plus_op,
  minus_op,
  
  times_op,
  divide_op,
  int_divide_op,
  rem_op,
  
  power_op,
#define IsBinaryOpTag(t) ((t)>=range_op && (t)<=power_op)
  
  /* unary operators */
  unary_not_op,
  
  unary_plus_op,
  unary_minus_op,
  
  unary_count_op,
#define IsUnaryOpTag(t) ((t)>=unary_not_op && (t)<=unary_count_op)
  
  /* Combinators - tl contains first argument.  Optionally hd points to name_t name of variable being abstracted, for debugging purposes */
  I_comb,
  K_comb,
  K_nil_comb, /* checks 2nd arg is NIL */
  S_comb,
  B_comb,
  C_comb,
  Sp_comb,
  Bp_comb,
  Cp_comb,
  Y_comb,
  
  Sc_comb, 	/* cons instead of apply */ 
  Bc_comb,
  Cc_comb,
  Spc_comb,
  Bpc_comb,
  Cpc_comb,
  Yc_comb,
  
  U_comb,	/* U f(x:y)=f x [Turner79] 
		   for pattern matching: E1 WHERE a:b = E2
		   compiles to ([a:b] E1) E2, and
		   DEF f (x,y,z) = E
		   compiles to
		   DEF f = [x,y,z] E
		   => [x:(y:(z:NIL))] E
		   => U ([x] (U [y] (U [z] (K E))))
		   AND: "in the actual implementation rather than K we would here use a combinator with 
		   a similar action but which checks the second argument does in fact take the value NIL."
		*/
  TRY_comb,
  TRYn_comb,
  MATCH_comb,

  PAIR_comb,
  H_comb,	/* hd / tl combinators - lazy */
  T_comb,

  FUN1_comb,	/* built in one-argument functions eg abs() sin(), cos() */
  FUN2_comb	/* ...with two arguments */
    
#define IsCombTag(t) ((t)>=I_comb)
#define TagCount (FUN2_comb+1)	/* tags are numbered from 0 */
} tag;

typedef struct node 
{
  union val {
    struct pointers {
      pointer hd, tl;	/* apply_t	sasl "apply" node, OR ...
			   cons_t	sasl "cons" node
			   ALSO NB comb_t uses hd as tag, tl as name of variable being abstracted */
    } pointers;

    int i;	/* num_t	sasl "num" */
    char c;	/* char_t 	sasl "char" */
    char b;	/* bool_t	sasl "bool" */
    double d;	/* floating_t	sasl "real" */

    char *n;	/* name_t */

  } val;
  /* ALL nodes have these fields... */
  tag t;	/* tag - exactly determines the contents, amonsgt the various possibilities */
		/* NB sizeof(char) == 1 and sizeof(tag) == sizeof(int) (== 4 locally). */
  unsigned s_refc;
  unsigned w_refc;
  unsigned bit:1;
} node;


#define IsSet(ptr)	((ptr).p)
#define IsNil(ptr)	(!IsSet(ptr))
#define PtrBit(ptr)	((ptr).bit)
#define Node(ptr)	((ptr).p)

#define EqPtr(p1, p2)	(Node((p1)) == Node((p2)) && PtrBit((p1)) == PtrBit((p2)))	/* assert(EqPtr(NIL, NIL) */

#define _GET(ptr,item)	(Node(ptr)->item)	/* GET(n,hd) = GET(n1,tl); ????? */

#define Srefc(ptr)	_GET((ptr),s_refc)
#define Wrefc(ptr)	_GET((ptr),w_refc)
#define ALLrefc(ptr)	(Srefc(ptr)+Wrefc(ptr))
#define PtrRefc(ptr)	(IsStrong(ptr) ? Srefc(ptr) : Wrefc(ptr))	/* is this needed? */

#define IsStrong(ptr)	(PtrBit(ptr)==NodeBit(ptr))
#define IsWeak(ptr)	(!IsStrong(ptr))

#define NodeBit(ptr)	_GET((ptr),bit)	 /* NB this is the node's bit p->n.bit not the pointers bit PtrBit(ptr) */
#define Val(ptr)	_GET((ptr),val)

#define _Is(p, test)	(IsSet(p) && (test)(p))
#define xIsConst(p)	_Is(p, IsCostTag)
  
#define IsConst(p)	(IsSet(p) && IsConstTag(Tag(p)))
#define IsApply(p)	(IsSet(p) && IsApplyTag(Tag(p)))
#define IsCons(p)	(IsSet(p) && IsConsTag(Tag(p)))
#define IsStruct(p)	(IsSet(p) && IsStructTag(Tag(p)))
#define IsAtom(p)   (!IsStruct(p))
#define HasPointers(p)	(IsSet(p) && HasPointersTag(Tag(p)))
#define IsTernaryOp(p)	(IsSet(p) && IsTernaryOpTag(Tag(p)))
#define IsBinaryOp(p)	(IsSet(p) && IsBinaryOpTag(Tag(p)))
#define IsUnaryOp(p)	(IsSet(p) && IsUnaryOpTag(Tag(p)))
#define IsComb(p)	(IsSet(p) && IsCombTag(Tag(p)))

#define IsNilList(p)	IsNilListTag(Tag(p)))/*deprecated*/

#define IsNum(p)	(IsSet(p) && IsNumTag(Tag(p)))
#define IsDbl(p)	(IsSet(p) && IsDblTag(Tag(p)))
#define IsChar(p)	(IsSet(p) && IsCharTag(Tag(p)))
#define IsBool(p)	(IsSet(p) && IsBoolTag(Tag(p)))
#define IsName(p)	(IsSet(p) && IsNameTag(Tag(p)))
#define IsFail(p)	(IsSet(p) && IsFailTag(Tag(p)))

#define IsAbstract(p)	(IsSet(p) && IsAbstractTag(Tag(p)))
#define IsRecursiveAbstract(p)	(IsSet(p) && IsRecursiveAbstractTag(Tag(p)))
#define IsDef(p)	(IsSet(p) && IsDefTag(Tag(p)))


#define Tag(ptr)	_GET(ptr,t)

#define _GETV(ptr,item)	((Val(ptr)).item)


#ifdef typecheck_pointer_use
#define _CHECKTAG(ptr,item,check) (_GETV(ptr,t)==tag)
#define _GETVCHECK(ptr,item) ((check(Tag(ptr)) ? GETV(ptr,item) : ERR )

#define Hd(ptr)	        _GETVCHECK(ptr,pointers,IsStruct).hd	/* pointer */
#define Tl(ptr)		_GETVCHECK(ptr,pointers,IsStruct).tl	/* pointer */
#define Num(ptr)	_GETVCHECK(ptr,i,IsNum)
#define Dbl(ptr)	_GETVCHECK(ptr,d,IsDbl)
#define Bool(ptr)	_GETVCHECK(ptr,b,IsBool)
#define Char(ptr)	_GETVCHECK(ptr,c,IsChar)
#else
#define Pointers(ptr)	_GETV(ptr,pointers)	/* pointers- deprecated, not for general use! */
#define Hd(ptr)		_GETV(ptr,pointers).hd	/* pointer */
#define Tl(ptr)		_GETV(ptr,pointers).tl	/* pointer */
#define Num(ptr)	_GETV(ptr,i)
#define Dbl(ptr)	_GETV(ptr,d)
#define Bool(ptr)	_GETV(ptr,b)
#define Char(ptr)	_GETV(ptr,c)
#define Name(ptr)	_GETV(ptr,n)
#endif

#define H(x)  Hd(x)
#define T(x)  Tl(x)

#define HH(x) H(H(x))
#define HT(x) H(T(x))
#define TH(x) T(H(x))
#define TT(x) T(T(x))


#define EqName(n1, n2)	(!strcmp(Name(n1), Name(n2)))

/*
 * error handling - err...
 */
#include <setjmp.h>
extern jmp_buf jmpbuffer;

extern int err_parse(char *f, char *msg1, char *msg2);
extern int err_make(char *f, char *msg1, int i);
extern int err_out(char *f, char *msg1, char *msg2, int n);
extern int err_reduce(char *msg1);
extern int err_refc(char *msg1);
extern int err_store(char *msg);
extern int err_zone(char *msg1);

/*
 * debugging
 */
extern int debug;
/* 
 * defaults and environment variables
 */
extern int partial_compile;
extern int reduce_optimise;
extern int no_code;
/* 
 *   commmand line options 
 */
extern int partial_compile;
extern int reduce_optimise;


/*
 * assertions
 */
#define Assert(x) 

/*
 * outputting
 */

extern char *err_tag_name(tag t);

extern int out_tag(tag t);
extern pointer out(pointer n);
extern pointer out_debug(pointer n);
extern pointer out_debug_limit(pointer n, int limit);
extern pointer out_debug1(pointer n);
extern pointer out_debug_limit1(pointer n, int limit);

extern int count_name(pointer name, pointer p);
extern int is_duplicate_name(pointer name, pointer p);

/*
 * helper functions
 */
extern int list_length(pointer p);
