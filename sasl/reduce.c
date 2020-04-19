/* 
 * reducer - reduce...
 */
#include <stdio.h>
#include <string.h>
#include <math.h>


#include "common.h"

#include "store.h"
#include "abstract.h"

#include "reduce.h"


/*
 * reduce - reduction machine - continues to run until a constant is found - then returned for output
 *
 * uses stack to descend the graph to find reductions
 * recursive calls to reduce are used for 'strict' operations
 */



/*
 * reduce/type
 * reduce node, look for given type and return value of that type.  If type is wrong raise error.
 */

/*******************************************/
#ifdef notyet
static pointer **stack0;
static pointer **sp0;
static unsigned stack_size = 0;

#define Stacked (sp0-stack0)
#define Push(p) (Assert(Stacked < stack_size), *sp0++ = (p))
#define Pop     (Assert(Stacked > 0),         (*--sp0))

#define Depth   (sp0-base)

pointer reduce0(pointer *pp)
{
    pointer **base = sp0;
    
    while ( !IsNil(*pp)) {
        switch (Tag(*pp)) {
            case apply_t:
                Push(pp);
                continue;
            case cons_t:
                if (Depth > 0) {
                    
                }
                continue;
        }
    }
    
    return *pp;
    
}
#undef Depth
#endif

/*******************************************/

/* the stack - used by reduce */

#define STACK_SIZE (50000) /*was 10000*/
#define RM_SIZE (265)


static pointer stack[STACK_SIZE];
static pointer *sp = stack;
#define Depth	(sp-stack)
#define Stacked (sp-base)

/* reduce: local shortcuts to make reduce() 'clearer' to read */

/* Stack map at point of reduction:
 base
 ...
 sp[-2]                 Stacked == 3
 sp[-1]                 Stacked == 2

 sp[0] &Tl=arg1 <<--Top Stacked == 1
 sp[1] &Tl=arg2
 ...
 reduction re-writes Top, and possibly elides the pointer to Top (either sp+1 or 'n' dependeng on Depth
 */
#define Top	sp[0]

#define Pop(n)  (Assert(Stacked >= (n)), sp -= (n), sp[n]) /* assert((sp+n)>=base) value is previous Top of stack */
#define Push(n) (Assert(Stacked < STACK_SIZE),sp[1] = (n), sp++) /* sequencing to ensure Push(*sp) works correctly */

#if notyet
inline pointer pop(const int n, const pointer *base, pointer *sp)
{
    pointer *old_sp = sp;
    Assert(Stacked >= n);
    sp -= n;
    return *old_sp;
}
#endif
/*
 * log reductions to file as required
 */
static int reductions = 0;
static int tag_reductions[TagCount];

static void indent(FILE *where, long int n)
{
    static char b[] = "                                ";
    static long int max;
    
    max = strlen(b);
    
    if (n <= 0)
        return;
    
    if (n > max)
        n = max;
    
    fprintf(where, "%s", b+max-n);
}

#define Limit 16
#define ArgLimit 6
void reduce_log(pointer *base, pointer *sp)
{
    reductions++;
    tag_reductions[Tag(*sp)]++;
    
    if (debug) {
        int i = 0;
//
         char tag_nargs(pointer p); /* forward reference - fix */
//
        
        Debug("\n");
//        Debug4("reduction %d: %s (%ld/%ld Depth/Stacked)\n",  reductions, refc_pointer_info(Top), Depth, Stacked);
        Debug5("reduction %d: %s (%ld/%ld/%d Depth/Stacked/Nargs)\n",  reductions, refc_pointer_info(Top), Depth, Stacked, tag_nargs(Top));

        indent(stderr, Depth);
        out_debug_limit1(Top, Limit); /*arbitrary limit to output */
        Debug(" ");
        for (i=1; i < ArgLimit && i < Stacked; i++) {
            out_debug_limit1(Tl(sp[-i]), Limit); /*arbitrary limit to output */
            Debug(" ");
        }
        Debug("\n");
        
        //begin:pretty
        if (IsOp(Top) || IsConst(Top) || IsCons(Top))
        {
            Debug("<sasl-reduce>\n");
            pretty_print(stderr, base[1]);
            Debug("\n</sasl-reduce>\n");
        }
        //end:pretty
        
        (void) refc_check();
        
        Debug("\n");
    }
    
    return;
}

/* note possible optimisations */

static int poss_optimisations = 0;
static int poss_optimisations_max = 0;

void reduce_poss_optimise_log(pointer *base, pointer *sp, pointer *last)
{
    if (last && sp)
    {
        int i = (int)(sp - last);
        Debug2("reduce_optimise?: %s: got: %d\n", err_tag_name(Tag(*sp)), i);
        
        poss_optimisations++;
        if (i > poss_optimisations_max)
            poss_optimisations_max = i;
    }
    return;
}

static int optimisations;
static int tag_optimisations[TagCount];
void reduce_optimise_log(pointer p, int got)
{
    optimisations += got;
    tag_optimisations[Tag(p)] += got;
    Debug2("reduce_optimise:\t%s\t%d\n", err_tag_name(Tag(p)), got);
    return;
}

/*
 * reduce_log_report(where)
 *	(1) call refc_log_report() to report storag usage
 *	(2) print tab-separated listing of reductions performed, appending a TOTAL, checking that total tallys and printing and ERROR line if not
 */
void reduce_log_report(FILE *where)
{
    int i, check=0, opt_check = 0;
    
    if (! logging)
        return;

    refc_log_report(where);
    
    (void) fprintf(where,"operator\treductions\toptimisations\n");
    
    for (i=0; i<TagCount; i++)
        if (tag_reductions[i] > 0) {
            (void) fprintf(where,"%s\t%d\t%d\n", err_tag_name((tag) i), tag_reductions[i], tag_optimisations[i]);
            check += tag_reductions[i];
            opt_check += tag_optimisations[i];
        }
    
    (void) fprintf(where,"Total\t%d\t%d\n", reductions, optimisations);
    
#ifdef notdef
    (void) fprintf(where,"%s\t%d\n", "got_max", poss_optimisations_max);
#endif
    if (check != reductions || opt_check != optimisations)
        (void) fprintf(where, "%s\t%d\t%d\n", "Total Error!", check - reductions, opt_check - optimisations);
    
    return;
}

/*
 * reduce_final_report(where)
 * (1) call reduce_log_report(where)
 * (2) call refc_final_report(where)
 */
void reduce_final_report(FILE *where)
{
    reduce_log_report(where);
    refc_final_report(where);
}

int reduce_int(pointer *nn)
{
    reduce(nn);
    
    if (Tag(sp[1]) != int_t) {
        Debug1("reduce_int: got %s\n", err_tag_name(Tag(sp[1])));
        (void) err_reduce("expecting int");
    }
    
    return Num(*nn);
}

char reduce_bool(pointer *nn)
{
    reduce(nn);
    
    if (Tag(*nn) != bool_t) {
        Debug1("reduce_bool: got %s\n", err_tag_name(Tag(*nn)));
        (void) err_reduce("expecting bool");
    }
    
    return Bool(*nn);
}

char reduce_char(pointer *nn)
{
    reduce(nn);
    
    if (Tag(*nn) != char_t) {
        Debug1("reduce_char: got %s\n", err_tag_name(Tag(*nn)));
        (void) err_reduce("expecting char");
    }
    
    return Char(*nn);
}

/* implement int -> double widening */
double reduce_double(pointer n)
{
    /*todo*/
    return Dbl(n);
}

/* reduce to a list - NIL is a list too */
pointer reduce_list(pointer *nn)
{
    reduce(nn);
    
    if (!IsCons(*nn) && !IsNil(*nn)) {
        Debug1("reduce_list: got %s\n", err_tag_name(Tag(*nn)));
    (void) err_reduce("expecting cons");
    }
    
    return *nn;
}

/* equality is polymophic and expands through lists [SASL Manual 1983] 
 
 The equality operators = and ~=, however, are defined between arbitrary pairs of objects.
 Objects of different type are always unequal. E.g. - the following expressions all take the value true:
 2 + 2 = 4	1 ~= 2	 false ~= true
 1 ~= false	%A ~= %a
 (1,2,3) = (1,1+1,1+1+1)
 */
char reduce_is_equal(pointer *nn1, pointer *nn2)
{
    pointer n1, n2;
    
    reduce(nn1); /* update in place */
    n1 = *nn1;
    reduce(nn2); /* update in place */
    n2 = *nn2;

    if (IsCons(n1)) {
        return (IsCons(n2) &&
                reduce_is_equal(&Hd(n1), &Hd(n2)) &&
                reduce_is_equal(&Tl(n1), &Tl(n2)));
    }
    
    if (IsNil(n1) || IsConst(n1)) {
        
        if (IsNil(n1))
            return (IsNil(n2)); /* NIL == NIL,  NIL != non-nil */
        
        if (IsNil(n2))
            return 0; /* non-nil != NIL */
        
        Assert(IsSet(n1) && IsSet(n2));
        
        return ((IsConst(n1) && (Tag(n1) == Tag(n2)) &&
                 (
                  (IsNum(n1)  && (Num(n1)  == Num(n2))) ||
                  (IsChar(n1) && (Char(n1) == Char(n2))) ||
                  (IsBool(n1) && (Bool(n1) == Bool(n2))) ||
                  (IsDbl(n1)  && (Dbl(n1)  == Dbl(n2)))
                  /* IsFail() is never equal to anything */
                  )));
    }
    /* function never equal to function ... */
    return 0;
}

/*
 * like reduce_is_equal, but only the tags have to match
 * NB NIL never has same tag as *anything*
 */
int reduce_is_equal_tag(pointer *nn1, pointer *nn2)
{
    pointer n1, n2;

    reduce(nn1); /* update in place */
    n1 = *nn1;
    reduce(nn2); /* update in place */
    n2 = *nn2;

    return (IsSet(n1) &&
            IsSet(n2) && /* bugfix 2019-05-10 previously tested IsSet(n1) twice here */
            Tag(n1) == Tag(n2));
}

/*
 * taginfo - container for information about tags
 */

/*
 * kind - grouping of tags, broadly based on their suffices _comb _op etc.
 */
typedef enum kind{
    special,
    constant,
    fail,        /* 1 only */
    name,        /* 1 only */
    apply,    /* 1 only */
    cons,     /* 1 only */
    abstract,
    def,        /* 1 only */
    operator,
    builtin_op,        /* operator implemented by C function */
    combinator
} kind;


static struct taginfo {
    tag t;     /* the tag inquestion */
    kind k;/* tag grouping, constant, combinator etc */
    char *name;    /* printable name "+" "S" etc */
    char nargs;    /* (>=0) how many apply nodes need to be stacked for a reduction: 0 for constant, + nargs 2, S nargs 3 */
    char strict;    /* (>=0) how many arguments must be reduced (assumes first only, first+second only etc.) */
    char new;    /* (>=0) how many apply nodes after the reduction: 0 for constant  or Indirection I node, + needs 0, S needs 3 */
    // etc.
} taginfo[TagCount];

/*
 * add_tag - set up taginfo
 */
static void add_tag(tag t, kind k, char *name, char nargs, char strict, char new /* etc. */)
{
    taginfo[t] = (struct taginfo) {t, k, name, nargs, strict, new /* etc. */};
}

inline char *tag_name(pointer p)    { return taginfo[Tag(p)].name; }
inline kind tag_kind (pointer p)    { return taginfo[Tag(p)].k; }
/*inline*/char tag_nargs(pointer p)    { return taginfo[Tag(p)].nargs; }
inline char tag_strict(pointer p)   { return taginfo[Tag(p)].strict; }
inline char tag_needs(pointer p)    { return taginfo[Tag(p)].new; }

inline int is_tag(tag t, pointer p)       { return IsSet(p) && t ==         Tag(p); }
inline int is(kind t, pointer p)      { return IsSet(p) && t == taginfo[Tag(p)].k; }

inline int is_struct    (pointer p)    { return is(apply, p) || is(cons, p);  }

inline int is_op    (pointer p)    { return is(operator, p); }
inline int is_unary_op    (pointer p)    { return is_op(p) && tag_nargs(p) == 1; }
inline int is_binary_op    (pointer p)    { return is_op(p) && tag_nargs(p) == 2; }
inline int is_ternary_op(pointer p)    { return is_op(p) && tag_nargs(p) == 3; }

/*
 * tag_init call once to initialise taginfo - a bit like microcode load
 */
void tag_init()
{
    add_tag(zero_t,    special,    "zero_t ",    0, 0, 0);    /* Special - node not yet in use in graph nor freelist */
    add_tag(free_t,    special,    "free_t",    0, 0, 0);    /* Special - node that is on the free list */
    add_tag(deleting_t,    special,    "deleting_t",    0, 0, 0);    /* Special - node that is going to become free when refc_delete() has compltedrecursive deltions */
    
    
    
    /* Constants - node contains the value of the constant and neveer points to anything else */
    add_tag(int_t,    constant,    "int_t",    0, 0, 0);
    add_tag(floating_t,    constant,    "floating_t",    0, 0, 0);
    add_tag(char_t,    constant,    "char_t",    0, 0, 0);
    add_tag(bool_t,    constant,    "bool_t",    0, 0, 0);
    
    add_tag(fail_t,    fail,        "fail_t",    0, 0, 0);    /* Special constant - to indicate pattern match has failed */
    
    
    /* Apply/Cons - these have a hl and tl (aka car and cdr) which can point to any other node, or be NIL */
    add_tag(apply_t,    apply,    "apply_t",    1, 0, 0);
    add_tag(cons_t,    apply,    "cons_t",    0, 0, 0);    /* (list n) => nth-item - when applied - apply list to number ...*/
    
    /* Name/Abstract tags used for partially compiled code - todo decide whether these are prohibited post-compilation */
    add_tag(name_t,        name,         "name_t",        0, 0, 0);
    
    add_tag(abstract_condexp_t,    abstract,    "abstract_condexp_t",    1, 0, 0);
    add_tag(abstract_formals_t,    abstract,    "abstract_formals_t",    1, 0, 0);
    add_tag(abstract_where_t,    abstract,    "abstract_where_t",    1, 0, 0);
    add_tag(abstract_defs_t,    abstract,    "abstract_defs_t",    1, 0, 0);
    
    add_tag(def_t,        def,         "def_t",        0, 0, 0);//???
    
    /* TODO when there's time to conduct before/aafter testing
     # define HasPointers(t) ((t) >= 0  ) */
    
    /* Operators */
    add_tag(cond_op,        operator, "->",    3, 1, 0);
    
    /* binary operators */
    add_tag(colon_op,        operator,    ":",    2, 0, 1);    /* ((: a) b) => (a:b) */
    add_tag(plusplus_op,        operator,    "++",    2, 3 /*XXX or 0*/, 1);    /* ++ () x => x */
    /* ++ list1 list2 =>  (hd-list1 : ((++ tl-list1) list2))  */
    // add_tag(minusminus_op,    operator,    "--",    2, 0, 0);
    
    // add_tag(range_op,        operator,    "..",    2, 0, 0);
    
    add_tag(and_op,        operator,    "&",    2, 2, 0);
    
    add_tag(much_greater_op,    operator,    ">>",    2, 2, 0);
    add_tag(greater_op,        operator,    ">",    2, 2, 0);
    add_tag(greater_equal_op,    operator,    ">=",    2, 2, 0);
    add_tag(equal_op,        operator,    "=",    2, 2, 0);
    add_tag(not_equal_op,        operator,    "~=",    2, 2, 0);
    add_tag(less_equal_op,    operator,    "<=",    2, 2, 0);
    add_tag(less_op,        operator,    "<",    2, 2, 0);
    add_tag(much_less_op,        operator,    "<<",    2, 2, 0);
    
    add_tag(plus_op,        operator,    "+",    2, 2, 0);
    add_tag(minus_op,        operator,    "-",    2, 2, 0);
    
    add_tag(times_op,        operator,    "*",    2, 2, 0);
    add_tag(divide_op,        operator,    "/",    2, 2, 0);
    add_tag(int_divide_op,    operator,    "DIV",    2, 2, 0);
    add_tag(rem_op,        operator,    "REM",    2, 2, 0);
    
    add_tag(power_op,        operator,    "**",    2, 2, 0);
    
    /* unary operators */
    add_tag(unary_strict,        builtin_op,    "unary",1, 1, 0);    /* Uname(p) contains name built in one-argument test eg "function" */
    add_tag(unary_nonstrict,     builtin_op,    "unary",1, 0, 0);     /* Uname(p) contains name built-in one argument maths eg "sin" */
    
    add_tag(unary_not_op,        operator,    "~",    1, 1, 0);
    add_tag(or_op,        operator,    "|",    1, 1, 0);
    
    add_tag(unary_plus_op,    operator,        "+",    1, 1, 0);
    add_tag(unary_minus_op,    operator,        "-",    1, 1, 0);
    
    // add_tag(range_unbounded_op,    operator,    "...",    1, 1, 0);
    // add_tag(unary_count_op,    operator,    "#",    1, 1, 0);
    
    /* Combinators - tl contains first argument.  Optionally hd points to name_t name of variable being abstracted,    for debugging purposes */
    add_tag(I_comb,        combinator,    "I_comb",    1, 0, 0);
    add_tag(K_comb,    combinator,    "K_comb",    2, 0, 0);
    add_tag(K_nil_comb,    combinator,    "K_nil_comb",    2, 0, 0);    /* checks 2nd arg is NIL */
    add_tag(S_comb,    combinator,    "S_comb",    3, 0, 3);    /* S f g x => f x (g x)     == ((f x) (g x)) */
    add_tag(Sp_comb,    combinator,    "Sc_comb",    3, 0, 3);     /* cons instead of apply */
    add_tag(B_comb,    combinator,    "B_comb",    3, 0, 2);    /* B f g x => f (g x)        == (f (g x))
                                                                 add_tag(Bp_comb,    combinator,    "Bc_comb",    3, 0, 2);
                                                                 add_tag(C_comb,    combinator,    "C_comb",    3, 0, 2);    /* C f g x => f x g        == ((f x) g)
                                                                 add_tag(Cp_comb,    combinator,    "Cc_comb",    3, 0, 2);
                                                                 
                                                                 add_tag(Y_comb,    combinator,    "Y_comb",    1, 0, 1);    /* Y f => (f <self>) */
    add_tag(Yc_comb,    combinator,    "Yc_comb",    1, 0, 1);
    
    add_tag(Sc_comb,    combinator,     "Sp_comb",  4, 0, 4);    /* Sp f g h x => f (g x) (h x)    == ((f (g x)) (h x)) */
    add_tag(Spc_comb,   combinator,     "Spc_comb", 4, 0, 4);
    add_tag(Bc_comb,    combinator,     "Bp_comb",  4, 0, 3);    /* Bp f g h x => f g (h x)    == ((f g) (h x))*/
    add_tag(Bpc_comb,   combinator,     "Bpc_comb", 4, 0, 3);
    add_tag(Cc_comb,    combinator,     "Cp_comb",  4, 0, 4);    /* Cp f g h x => f (g x) (h x)    == ((f (g x)) (h x))*/
    add_tag(Cpc_comb,   combinator,     "Cpc_comb", 4, 0, 4);
    
    
    add_tag(U_comb,     combinator,     "U_comb",   2, 0, 4);    /* U f g => f (H g) (T g)    == ((f (H g)) (T g)) */    /* U f(x:y)=f x (f y) [Turner79] */
    add_tag(TRY_comb,   combinator,     "TRY_comb", 2, 0, 0);    /* TRY FAIL y => y; TRY x y => x */
    add_tag(TRYn_comb,  combinator,     "TRYn_comb",4, /*xxx FIX IT 4 or */5 , 0);    /* TRYn 1 f g x => TRY (f x) (g x) == ((TRY (f x)) (g x)) */
    add_tag(MATCH_comb, combinator,     "MATCH_comb",3, 0, 0);    /* MATCH const E x => const = x -> E; FAIL*/
    add_tag(MATCH_TAG_comb,combinator,  "MATCH_TAG_comb",3, 0, 0);    /* MATCH test E x => (test x)= FALSE -> FAIL; E */
    
    // add_tag(PAIR_comb,    combinator,    "PAIR_comb",    2, 0, 0);    /* PAIR x y => 'x:y" notused */
    add_tag(H_comb,     combinator,     "H_comb",   1, 1, 0);    /* (H x:y) => x */
    add_tag(T_comb,     combinator,     "T_comb",   1, 1, 0);    /* (T x:y) => y */
    
    // _LastTag      /* Never appears in a node, used to calculate size of the array-of tag values */
    
    return;
}
/*
 * sasl - primitive to sasl functions
 */

/* unary stricts
 * anything -> BOOL
 */
static pointer is_logical(pointer p)    { return new_bool(IsBool(p)); }
static pointer is_char(pointer p)       { return new_bool(IsChar(p)); }
static pointer is_list(pointer p)       { return new_bool(IsNil(p) || IsCons(p)); }
static pointer is_func(pointer p)       { return new_bool(IsApply(p) || IsFun(p)); }
static pointer is_number(pointer p)     { return new_bool(IsNum(p)); }

/* unary maths */
/* char -> num */
static pointer   code(pointer p)    { return IsChar(p) ? new_int( (int) Char(p)) : new_fail(); }

/* num -> char*/
static pointer decode(pointer p)    { return IsNum(p)  ? new_char((int)  Num(p)) : new_fail(); }

/* num -> num */
/* ToDo make these work with sasl doubles */
static pointer maths_abs(pointer p)     { return IsNum(p) ? new_int((int) fabs((double) Num(p))) :  new_fail();}
static pointer maths_arctan(pointer p)  { return IsNum(p) ? new_int((int) atan((double) Num(p))) :  new_fail();}
static pointer maths_cos(pointer p)     { return IsNum(p) ? new_int((int) cos((double)  Num(p))) :  new_fail();}
static pointer maths_entier(pointer p)  { return IsNum(p) ? new_int((int) floor((double) Num(p))):  new_fail();}
static pointer maths_exp(pointer p)     { return IsNum(p) ? new_int((int) exp((double)  Num(p))) :  new_fail();}
static pointer maths_log(pointer p)     { return IsNum(p) ? new_int((int) log((double)  Num(p))) :  new_fail();}
static pointer maths_sin(pointer p)     { return IsNum(p) ? new_int((int) sin((double)  Num(p))) :  new_fail();}
static pointer maths_sqrt(pointer p)    { return IsNum(p) ? new_int((int) sqrt((double) Num(p))) :  new_fail();}

/* binary maths */

/*
 * initialise reduction machine
 */
int reduce_init()
{
    /*
     * tags
     */
    tag_init();
    
    /*
     * functions primitive to sasl
     */
    refc_delete(&builtin); /* ensure reduce_init() can be called safely more than once */
    
    builtin = new_def(new_name("<primitive to sasl>"), NIL);
    
    /* type prediacates */
    builtin = add_to_def(builtin, new_name("char"),     new_unary_strict("char",     is_char));
    builtin = add_to_def(builtin, new_name("list"),     new_unary_strict("list",     is_list));
    builtin = add_to_def(builtin, new_name("logical"),  new_unary_strict("logical",  is_logical));
    builtin = add_to_def(builtin, new_name("function"), new_unary_nonstrict("function", is_func));
    builtin = add_to_def(builtin, new_name("number"),   new_unary_strict("number",   is_number));
    
    /* chars */
    builtin = add_to_def(builtin, new_name("code"),     new_unary_strict("code",     code));
    builtin = add_to_def(builtin, new_name("decode"),   new_unary_strict("decode",   decode));
    
    /* maths */
    builtin = add_to_def(builtin, new_name("abs"),      new_unary_strict("abs",      maths_abs));
    builtin = add_to_def(builtin, new_name("entier"),   new_unary_strict("entier",   maths_entier));
    builtin = add_to_def(builtin, new_name("arctan"),   new_unary_strict("arctan",   maths_arctan));
    builtin = add_to_def(builtin, new_name("cos"),      new_unary_strict("cos",      maths_cos));
    builtin = add_to_def(builtin, new_name("exp"),      new_unary_strict("exp",      maths_exp));
    builtin = add_to_def(builtin, new_name("log"),      new_unary_strict("log",      maths_log));
    builtin = add_to_def(builtin, new_name("sin"),      new_unary_strict("sin",      maths_sin));
    builtin = add_to_def(builtin, new_name("sqrt"),     new_unary_strict("sqrt",     maths_sqrt));
    
    return 0;
}

/*
 * reduce_print - reduce something, print it and delete the result
 */
pointer reduce_print(pointer *p)
{
    if (IsNil(*p))
        return NIL;
    
    /* reduce to find a constant, print it, free it */
    reduce(p);
    
    Debug("Reduce_print: ");    out_debug(*p);
    
    if (IsCons(*p)) {
        Hd(*p) = reduce_print(&Hd(*p));
        Tl(*p) = reduce_print(&Tl(*p));
    }
    else {
        pretty_print_const(stdout, *p);
        fflush(stdout); // ?? only when interactive
    }
    
    return *p;
}

#ifdef notdef
#include "red.c"	/* uses pointer-to-instruction model */
#endif


/*
 * reduce - the reduction machine
 *	travels down the chain of Hd() pointers in apply nodes
 * 	look at Tag and performs reduction
 *	uses stack[] to navigate back up the chain
 * 	iterates until no more reducible tags are found, leaving "n" rewritten as a constant, or cons node with constant in Hd
 *
 *	for "strict" operators (arithmetic, conditional), calls reduce() recursively to force the arguments as required
 *
 *	after reduce() stack pointer sp is unchanged
 *
 */

/*TEMP*/
#define Ref(x) refc_copyS(Top, x)

void reduce(pointer *n)
{
    pointer *base = sp;	/* remember Top on entry - used to calculate Stacked */
    tag tt = _LastTag; /* Tag(Top) before popping */
    
    if (IsNil(*n))
        return;
    
    /* stack size is arbitrary, so check for potential overflow */
    if (Stacked >= ((STACK_SIZE)*0.9)) {
        (void) err_reduce("stack depth within 90% of max");
        /*NOTREACHED*/
    }
    
    Push(*n);

    while (IsSet(Top) && Stacked > 0) {
        /* Within the loop, Top is set to NIL on error; also halt if nothing on stack - should never happen error */
        
        if (debug > 1) {
            indent(stderr, Depth); out_debug1(Top);
            Debug3("\t%s (%ld/%ld Depth/Stacked)", refc_pointer_info(Top), Depth, Stacked);
            Debug("\tTop\n");
        }
        
//      while (Tag(Top) == apply_t) { //2020-04-01 why not?
        if (Tag(Top) == apply_t) {
            /* travel down the 'spine' */
            Assert(Stacked <= ((STACK_SIZE)*0.9));
            Push(Hd(Top));
            
            continue;	/* loop */
            /*NOTREACHED*/
        }

        /* Top is not apply_t, evaluate / return */
        reduce_log(base, sp);
        {/*start:reduce loop*/
            if (Stacked >= 1) {
                static const int MAXARG = 4; /* max number of apply nodes in spine for a reduction */
                pointer *arg[MAXARG+1];
                int i;
                int nargs = tag_nargs(Top);
                
                tt = Tag(Top);
                
                /* locate args, numbered arg1, arg2, ... */
                for (i = 1; i <= nargs;  i++) arg[i] = &Tl(Pop(1));
//                for (i = 1; i <= nargs;  i++) arg[i] = &Tl(sp[i]);

                /* zero args: const => const | list => list */
                /* >0 args:   list number => nth item of list */
                switch (tt) {
                        
                    case free_t: {
                        err_reduce("reducing a free node");
                        continue;
                        /*NOTREACHED*/
                    }
                    case deleting_t: {
                        err_reduce("reducing a deleting node");
                        continue;
                        /*NOTREACHED*/
                    }
                    case cons_t: {

                        unsigned u;

                        if (Stacked == 1) {
                            Pop(1);
                            return;
                        }
                        
                        Pop(1);

                        reduce(&T(Top));
                        if (!IsNum(T(Top)))
                            err_reduce("applying a list to something that is not a number");
                        u = Num(T(Top));
                        if (u < 1)
                            err_reduce("applying a list to a number less then 1");

                        /* force "u" conses into existence */
                        {   /* 2018-11-30 https://tree.taiga.io/project/northgate91-project-one/issue/42 */
                            pointer *pp= &H(Top);
                            unsigned uu;
                            for (uu = u; uu >= 1; uu--) {
                                reduce(pp);
                                if (!pp || ! IsCons(*pp)) {
                                    err_reduce("applying list to a number: not enough elements in list");
                                    return; /*NOTREACHED*/
                                }
                                pp = &T(*pp);
                            }
                        }
//                        TODO simplify refc_copy_Nth() as the list has  been "forced" successfuly above
                        Top = refc_update_Itl(Top, refc_copyNth(Top, u));
                        Tag(Top) = apply_t;
                        continue;
                    }

                        /* constants -  no further reductions here */
                    case int_t:
                    case floating_t:
                    case char_t:
                    case bool_t:
                        if (Stacked != 1)
                            err_reduce("applying a constant as a function");
                        Pop(1);
                        return;
                    case name_t:
                        if (Stacked != 1)
                            err_reduce2("name undefined:", Name(Top));
                        Pop(1);
                        return;
                    case fail_t:  {/* FAIL anything ... => FAIL */
                        /*WIP here to fix 99.sasl; 3.1.20 q*/
                        //BUG xxx here not sure refc is adjusted correctly when Stacked > 0
                        pointer p = refc_copy(Top); /* fail */
                        Pop(Stacked);
                        refc_delete(n);
                        *n = p;
                        Push(*n); Pop(1);//XXtemp
                        return; /* return the FAIL */
                    }
                    case abstract_condexp_t:
                    case abstract_formals_t:
                    case abstract_where_t:
                    case abstract_defs_t:
                        /* !! these tags are unuie as their arguments reside in the tag node itself */
                        Tag(Top) = apply_t;
                        Top = refc_update_Itl(Top, (reduce_abstract(refc_copy(H(Top)), refc_copy(T(Top)),  tt))); // 2020-03-17 ... and why not refc_update_hdtl()?
                        continue;
                    case def_t:
                        (void) err_reduce2(err_tag_name(tt), "unexpected tag:");
                        /*NOTREACHED*/
                        continue;
#ifdef notdef
                        if (partial_compile) {
                            
                            ;	/* not allowing this tag at run time */
                            /* Here is just-in-time code generation ...*/
                        }
                        else {
                            
                        }
#endif
                        
                    default:
                        ; /*FALLTHRU*/
                        
                }
                if (Stacked > 1) {
                    /* unary: op arg1 => res */
                    pointer *arg1;
                    Pop(1);
                    arg1 = &Tl(Top); /* equivalent to Ref("T") but without the change of refcount */
                    switch (tt)
                    {
                            /* unary: op arg  => res*/
                        case unary_minus_op:
                            Top = refc_update_to_int (Top, - reduce_int(arg1)); continue;
                        case unary_plus_op:
                            Top = refc_update_to_int (Top,   reduce_int(arg1)); continue;
                        case unary_not_op:
                            Top = refc_update_to_bool(Top, ! reduce_bool(arg1)); continue;
                            //stack: push "H"; reduce; check-bool; theBool = -theBool; update
                            
                        case unary_strict:
                            /* ToDo - reduce_int() reduce_bool() etc to give better warning the "FAIL" */
                            reduce(arg1); /* strict */
                            /*FALLTHRU*/
                        case unary_nonstrict:
                            Assert(IsFun(H(Top)));
                            Top = refc_update_Itl(Top, Ufun(H(Top))(*arg1)); continue;
                            //stack: push "H"; theValue = uFun(theValie); update

                        case I_comb:
                            /* I x => x */
                            if (Stacked > 1) {
                                /* elide (I x) y ==> x y on stack
                                 * Afterwards Arg1 is become H(Top')
                                 */
                                Debug("**I_comb Stacked>2 case\n");
                                Assert(!SameNode(Top, *arg1)); /* avoid stack loops!?! */

                                Pop(1);
                                refc_update_hdS(&Top,"HT");
                                //stack: pop; push "HT"; push "T"; mk_apply; update

                            } else {
                                /* at the top of the stack  (I x) ==> x on stack
                                 * recurse to reduce x (short-circuit)
                                 * and update return value "*n" to x ie T(Top) */
                                Debug("**I_comb Stacked==2 case\n");/*XXX*/
                                Assert(n && ! IsNil(*n));   /*xxx*/
                                Assert(SameNode(*n, Top));  /* should always be the case for Depth==1 */

#if 1
                                Top = *n = refc_update_pointerS(n, "T");
#else
                                reduce(arg1); /* recurse - carry on reducing */
                                *n = refc_update_pointerS(n, "T");
                                Pop(1); Push(*n); // temp or not ??  Top = *n = refc_updat_pointerS ...
                                Pop(1);
#endif
                                //stack: push "T"; reduce; update
//                                Assert(SameNode(*n, Top));  /* should always be the case for Depth==1 */
//
//                                T(Top) = reduce(&T(Top)); /* carry on reducing OR *arg1 = reduce(arg1) */
//                                refc_update_pointerS(n, "T"); /* ie T(Top) */
//                                Pop(1);
//                                return;
                            }
                            continue;
                            
                        case Yc_comb:
                            Tag(Top) = cons_t;
                            /*FALLTHRU*/
                        case Y_comb: {
                            refc_delete(&Hd(Top)); /* loose the "Y" */ /*new update(sp, T, me)*/
                            Hd(Top) = Tl(Top);
                            Tl(Top) = refc_copy_make_cyclic(Top);
                            //stack: Y_comb: push "T"; push "."; mk_apply; update
                            continue;
                        }
                            
                            /* H (a:x) => a */
                            /* H other => FAIL */
                        case H_comb:
                            reduce(arg1);
                            if (IsCons(*arg1)) {
                                refc_updateIS(&Top, "TH");
                                Tag(Top) = apply_t;
                            }
                            else {
                                err_reduce("taking head of non-cons");
                            }
                            continue;
                            //stack: push "T"; reduce; not(isCons) ? (pop;new_comb(FAIL)) : (new_comb(I_Comb); mk_rev_apply)) ; update
                            //stack: push "T"; reduce; check(cons_t); pop; new_comb(I_comb); push("TT"); mk_apply)) ; update
                            
                            /* T (a:x) => x */
                            /* T other => FAIL */
                        case T_comb:
                            reduce(arg1);
                            if (IsCons(*arg1)) {
                                refc_updateIS(&Top, "TT");
                                Tag(Top) = apply_t;
                            } else {
                                err_reduce("taking tail of non-cons");
                            }
                            continue;
                            
                        default:
                            ;	/*FALLTHRU*/
                    }
                    if (Stacked > 1) {
                        /* binary: op arg1 arg2 => res */
                        pointer *arg2;
                        Pop(1);
                        arg2 = &Tl(Top);
                        switch (tt) {
                            case plus_op:	Top = refc_update_to_int(Top, reduce_int(arg1) + reduce_int(arg2));	continue;
                            case minus_op:	Top = refc_update_to_int(Top, reduce_int(arg1) - reduce_int(arg2));	continue;
                            case times_op:	Top = refc_update_to_int(Top, reduce_int(arg1) * reduce_int(arg2));	continue;
                            case int_divide_op:	Top = refc_update_to_int(Top, reduce_int(arg1) / reduce_int(arg2));	continue;
                                
                            case or_op:	Top = refc_update_to_bool(Top, reduce_bool(arg1) || reduce_bool(arg2));	continue;
                            case and_op:	Top = refc_update_to_bool(Top, reduce_bool(arg1) && reduce_bool(arg2));	continue;
                                
                            case colon_op:
                                /* (P x y) => (x:y)
                                 ((: a) b) => (a:b) */
                                refc_updateSS(&Top, "HT", "T");
                                Tag(Top) = cons_t;
                                continue;
                                //stack: colon_op: push "HT"; push "T"; mk_cons; update
#if 1 // old bug
                                /* ++ () x => x */
                                /* ++ list1 list2 =>  (hd-list1 : (++ tl-list1) list2)))  */
                            case plusplus_op:
                                /* check list2 is a list - no, leave it lazy */
                                reduce(arg1);
                                if ( ! (IsCons(*arg1) || IsNil(*arg1)))
                                    err_reduce("plusplus_op - non-list first arg");
//                                Top = refc_update_Itl(Top, make_append(Ref("HT"), Ref("T")));BUGGG
                                if (IsNil(*arg1)) {
                                    Top = refc_update_Itl(Top, Ref("T"));
                                } else {
                                    Top = refc_update_hdtl(Top, Ref("HTH"),
                                                           new_apply3(new_comb(plusplus_op), Ref("HTT"), Ref(("T"))));
                                    Tag(Top) = cons_t;
                                }
                                continue;
#else
                            case plus_plus_op: /* "should not evaluate it's arguments" */
                                /* ++ () x => x */
                                /* ++ list1 list2 =>  ((hd list1) : (++ (tl list1) list2))) */
                                /* 2020-04-02 fix: made properly lazy */
                                if (IsNil(*arg1)) {
                                    Top = refc_update_Itl(Top, Ref("T"));
                                } else {
                                    Top = refc_update_hdtl(Top,
                                                           new_apply(new_comb(H_comb), Ref("HT")),
                                                           new_apply3(new_comb(plusplus_op),
                                                                      new_apply(new_comb(T_comb), Ref("HT")),
                                                                      Ref("T")));
                                    Tag(Top) = cons_t;
                                }
#endif
                                //stack: plusplus_op: pushd "T"; reduce; (!IsCons || IsNil) ? <exception> : push "HT"; mk_rev_append; new_comb(I_comb); mk_rev_apply //WHY "I"?
                                //stack: plusplus_op: push "HT"; reduce; isNil ? (new(I_comb); rev_make_cons) : (check(cons_t); pop; push "HTH"; new(plusplus_op); push "HTT"; push "T"; make_apply; make_cons
                                
                            case minusminus_op:
                            case range_op:
                            case much_greater_op:
                                (void) err_reduce2( err_tag_name(tt), " unexpected");
                                /*NOTREACHED*/
                                continue;
                                
                            case greater_op:        Top = refc_update_to_bool(Top, reduce_int(arg1) >  reduce_int(arg2)); continue;
                            case greater_equal_op:  Top = refc_update_to_bool(Top, reduce_int(arg1) >= reduce_int(arg2)); continue;

                            case equal_op:          Top = refc_update_to_bool(Top, reduce_is_equal(arg1, arg2)); continue;
                            case not_equal_op:      Top = refc_update_to_bool(Top, !reduce_is_equal(arg1, arg2)); continue;
                            case less_equal_op:	    Top = refc_update_to_bool(Top, reduce_int(arg1) <= reduce_int(arg2));	continue;
                            case less_op:	        Top = refc_update_to_bool(Top, reduce_int(arg1) <  reduce_int(arg2));	continue;
                                
                                
                                
                                /* todo floating point */
                            case much_less_op:
                                err_reduce("much_less op not expected");
                            case divide_op:
                                err_reduce("divide op not expected");
                            case rem_op:
                                err_reduce("rem op not expected");
                            case power_op:
                                err_reduce("power op not expected");
                                continue;
                                
                                /* comb arg1 arg2 */ /*xxx shoulndt we only allow NIL here -> and be strict?*/
                            case K_nil_comb:
                                if ( ! (IsNil(*arg2) ||
                                        IsCons(*arg2) ||
                                        (IsApply(*arg2) &&
                                         ((Tag(Hd(*arg2)) == H_comb) ||
                                          (Tag(Hd(*arg2)) == T_comb))) /* allow "lazy" lists */
                                        ))
                                    err_reduce("K_nil_comb - non-list second arg");
                            case K_comb:
                                /* K x y => I x  [i node] */
                                Top = refc_update_Itl(Top, Ref("HT"));
                                continue;
                                //stack: K_comb: push "HT"; new_comb(I_comb); mk_rev_apply; update
                                
                            case U_comb:
                                /*  U f g => f (H g) (T g)    lazy version of U f (a:x) => f a x */
                                Top = refc_update_hdtl(Top,
                                                       new_apply(Ref("HT"), new_apply(new_comb(H_comb), Ref("T"))),
                                                       new_apply(new_comb(T_comb),  Ref("T")));
                                continue;
                                //stack: U_comb: push "HT"; new_comb(H_comb); push "T"; mk_apply; new_comb(T_comb); push "T"; mk_apply; mk_apply; mk_apply; update
                                
                            case TRY_comb: {
                                /* TRY FAIL y => y
                                 * TRY x    y => x */
                                reduce(arg1);
                                if (IsFail(*arg1)) {
                                    Top = refc_update_Itl(Top, Ref("T"));/*refc_update_IS(&Top, "T")*/
                                    //stack: new_comb(I_comb); push "T"; mk_apply; update
                                } else {
                                    Top = refc_update_Itl(Top, Ref("HT"));
                                }
                                continue;
                                //stack: TRY_comb: push "HT"; reduce; IsFail ? (pop; new_comb(I_comb); push "T"; mk_apply) : (new_comb(I_comb); rev_make_apply) update
                            }
                                
                            default:
                                ;	/*FALLTHRU*/
                        }
                        if (Stacked > 1) {
                            /* ternary: op arg1 arg2 arg3 => res */
                            pointer *arg3;
                            Pop(1);
                            arg3 = &Tl(Top);
                            switch (tt) {
                                    
                                case cond_op:	{
                                    /* cond b t f => t (or f) */
                                    if (reduce_bool(arg1))
                                        Top = refc_update_Itl(Top, Ref("HT"));
                                    else
                                        Top = refc_update_Itl(Top, Ref("T"));
                                    continue;
                                    //stack: cond_op: push "HHT"; reduce
                                }
                                case MATCH_comb: {
                                    /*  */
                                    /* ?should check arg1 is a constant? */
                                    if (reduce_is_equal(arg1, arg3)) {
                                        refc_updateIS(&Top, "HT");
                                    } else {
                                        Top = refc_update_to_fail(Top);
                                    }
                                    continue;
                                }
                                    
                                case MATCH_TAG_comb: {
                                    /* MATCH_TAG t E x => Tag(t) = Tag(x) -> E x; FAIL */
                                    
                                    if (reduce_is_equal_tag(arg1,  arg3)) {
                                        refc_updateSS(&Top, "HT", "T");
                                    } else {
                                        Top = refc_update_to_fail(Top);
                                    }
                                    continue;
                                }
                                    
#ifdef match_with_test
                                case MATCH_TEST_comb: {
                                    /* MATCH test E x => (test x)= FALSE -> FAIL; E */
                                    pointer res = new_apply(Ref("HHT"), Ref("T"));
                                    char c = reduce_bool(res);
                                    refc_delete(&res);
                                    
                                    if (c) {
                                        Top = refc_update_Itl(Top, Ref("HT"));
                                    } else {
                                        Top = refc_update_to_fail(Top);
                                    }
                                    continue;
                                }
#endif
                                    /*[x] f g			=> S f g x	=> (f x) (g x)*/
                                    /*[x] f g0	=> S f (K g)	=> C f g x	=> f x g */
                                    /*[x] f0 g	=> S (K f) g	=> B f g x	=> f (g x) */
                                    /*[x] f0 g0	=> S (K f)(K g)			=> f g */
                                case Sc_comb:	 /* Sc f g x => (f x):(g x) */
                                    Tag(Top) = cons_t;
                                    /*FALLTHRU*/
                                case S_comb:	 /* S f g x => (f x)(g x) */
                                {
#ifdef notyet
                                    long int got = 0;
                                    if (got >= 2) {
                                        /* optimise - update in place, no reference counts changed, apart from new pointer to Arg3 */
                                        reduce_optimise_log(*sp, 2);
                                        refc_delete(&Hd(Top)); /* loose the 'S' */
                                        Hd(Top) = Tl(Top); Tl(Top) = Tl(Top);
                                        Hd(Top) = Tl(Top); Tl(Top) = refc_copy(Tl(Top));
                                        Hd(Top) = Top;     Tl(Top) = Top;
                                    }
                                    else
#endif
                                    {
                                        /*new update top-of-stack ap(ap(THH,T), ap(TH,T))*/
                                        pointer n1 = new_apply(Ref("HHT"), Ref("T"));
                                        pointer n2 = new_apply(Ref("HT"), Ref("T"));
                                        Top = refc_update_hdtl(Top, n1, n2);
                                    }
                                    continue;
                                }
                                case Bc_comb:	/* Bc f g x => f:(g x) */
                                    Tag(Top) = cons_t;
                                case B_comb:	/* B  f g x => f (g x) */
                                    Top = refc_update_hdtl(Top,
                                                              Ref("HHT"),
                                                              new_apply(Ref("HT"), Ref("T")));
                                    continue;
                                    
                                case Cc_comb:	/* Cc f g x => f x:g */
                                    Tag(Top) = cons_t;
                                case C_comb:	/* C  f g x => f x g */
                                    Top = refc_update_hdtl(Top,
                                                              new_apply(Ref("HHT"), Ref("T")),
                                                              Ref("HT"));
                                    continue;
                                    
                                    
                                    /* WIP WIP WIP */
#ifdef V2
                                    
                                {
                                    pointer
                                        f = Ref("HHT"),
                                        g = Ref("HT"),
                                        x1 = Ref("T"),
                                        x2 = Ref("T"); // xxx OR ELSE use just "x" and then "refc_copy(x)"
                                    
                                    pointer
                                        ap1 = make_apply(f,x1),
                                        ap2 = make_apply(g,x2);// xxx secretly recycle apply nodes on spine when not needed

                                    refc_update_hdtl(Top, ap1, ap2);

//                                    int got = (reduce_optimise ? (sp - last) : 0);
//                                    reduce_optimise_log(*sp, got);
//                                    if (got == 0) {     /* no optimisations */
//                                        Pointers(Top) = (struct pointers) {new_apply(f, x1), new_apply(g, x2)};
//                                    }
//                                    else if (got == 1) { /* one optimisation - Top is apply_t with ALLrefc==1 so re-usable */
//                                        Pointers(Top) = (struct pointers) {f, x1};
//                                        Pointers(Top) = (struct pointers) {Top, new_apply(g, x2)};
//                                    }
//                                    else {    /* two optimisations assert got >= 2 */
//                                        Pointers(Top) = (struct pointers) {f, x1};
//                                        Pointers(Top) = (struct pointers) {g, x2};
//                                        Pointers(Top) = (struct pointers) {Top, Top};//BUG HERE !!!!
//                                    }
                                    /*
                                     sp -= 2;
                                     *sp = n1;
                                     */
                                    continue;
                                }
                                    
#endif
#ifdef V3
                                {
                                    pointer
                                    f = Arg1,
                                    g = Arg2,
                                    x = Arg3;
                                    
                                    int got = (reduce_optimise ? (sp - last) : 0);
                                    reduce_optimise_log(*sp, got);
                                    if (got == 0) {	 /* no optimisations */
                                        Hd(Top) = new_apply(f, x); Tl(Top) = new_apply(f, refc_copy(x));
                                    }
#ifdef notdef
                                    else if (got == 1) { /* one optimisation - Top is apply_t with ALLrefc==1 so re-usable */
                                        Hd(Top) = f; Tl(Top) = x;
                                        Hd(Top) = Top; Tl(Top) = new_apply(f, refc_copy(x));
                                    }
#endif
                                    else if (got >= 2) /*WIP*/{	/* two optimisations assert got >= 2 */
                                        Hd(Top) = f; Tl(Top) = x;
                                        Hd(Top) = g; Tl(Top) = refc_copy(x);
                                        Hd(Top) = Top; Tl(Top) = Top;
                                    }
                                    /*
                                     sp -= 2;
                                     *sp = n1;
                                     */
                                    continue;
                                }
#endif
                                default:
                                    ;	/*FALLTHRU*/
                            }
                            
                            if (Stacked > 1) {
                                /* quaternary: op arg1 arg2 arg3 arg4 => res */
                                pointer *arg4;
                                Pop(1);
                                arg4 = &Tl(Top);

                                switch (tt) {
                                        
                                    case TRYn_comb: {
                                        /* TRYn 0 f g x => FAIL       || should never happen! */
                                        /* TRYn 1 f g x => TRY        (f x) (g x) */
                                        /* TRYn n f g x => TRYn (n-1) (f x) (g x)*/
                                        if (!IsNum(*arg1) || (Num(*arg1) < 1)) {
                                            Top = refc_update_to_fail(Top);  /* err_reduce("problem with matching") */
                                        } else if (Num(*arg1) == 1) {
                                            Top = refc_update_hdtl(Top,
                                                                   new_apply(new_comb(TRY_comb),
                                                                             new_apply(refc_copyS(Top, "HHT"), refc_copyS(Top, "T"))),
                                                                   new_apply(refc_copyS(Top, "HT"), refc_copyS(Top, "T")));
                                        } else {
                                            Top = refc_update_hdtl(Top,
                                                                   new_apply3(new_comb(TRYn_comb),
                                                                              new_int(Num(*arg1) - 1),
                                                                              new_apply(refc_copyS(Top, "HHT"), refc_copyS(Top, "T"))),
                                                                   new_apply(refc_copyS(Top, "HT"), refc_copyS(Top, "T")));
                                        }
                                        continue;
                                    }

                                    case Sp_comb:	{
                                        /* Sp f g h x => f (g x) (h x) */
                                        /*todo make this less tedious to specify!*/
#ifdef lesstedious
                                        pointer f = Ref("HHHT"),
                                                g = Ref("HHT"),
                                                h = Ref("HT"),
                                                x1= Ref("T"),
                                                x2= Ref("T");
                                        pointer p1 = new_apply(f, new_apply(g, x1));
                                        pointer p2 = new_apply(h, x2);
                                        Top = refc_update_hdtl(Top, p1, p2)
//                                        refc_update(ap(f, ap(g,x1)), ap(h,x2));
#endif
                                        Top = refc_update_hdtl(Top,
                                                                  new_apply(Ref("HHHT"), new_apply(Ref("HHT"), Ref("T"))),
                                                                  new_apply(Ref("HT"), Ref("T")));
                                        continue;
                                    }
                                        
                                    default:
                                        ; /*FALLTHRU*/
                                }
                            }}}}}}/*end:reduce loop*/
        /* nothing found */
        if (debug) {
            int i=0;
            Debug2("unimplemented tag: Stacked %ld:%s\n", Stacked, err_tag_name(tt));
            
            do {Debug1("Stack[%d]: ",i--); out_debug(sp[i]); } while (-i < Stacked);
        }
        err_reduce("unimplemented tag");
        /*NOTREACHED*/
    }
    {
        /* THN - this never happens? */
        if (debug) {
            int i=0;
            Debug1("reduce done Stacked=%ld\n",  Stacked);/*XXX*/

            out_debug(Top);

            do {Debug1("Stack[%d]: ",i--); out_debug(sp[i]); } while (-i < Stacked);
        }
//      xxx  Assert(Stacked == 1 || IsCons(Top));
        Pop(Stacked);
        return;
    }
}

#undef Top
#undef Arg1
#undef Arg2
#undef Arg3
#undef Arg4
#undef Depth
#undef Pop
