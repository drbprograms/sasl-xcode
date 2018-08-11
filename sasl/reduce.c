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
 * sasl - primitive to sasl functions
 */

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

#define Limit 24
#define ArgLimit 6
void reduce_log(pointer *base, pointer *sp)
{
    reductions++;
    tag_reductions[Tag(*sp)]++;
    
    if (debug) {
        int i = 0;
        
        fprintf(stderr, "\n");
        fprintf(stderr, "reduction %d: %s (%ld/%ld Depth/Stacked)\n",  reductions, refc_pointer_info(Top), Depth, Stacked);
        
        indent(stderr, Depth);
        out_debug_limit1(Top, Limit); /*arbitrary limit to output */
        fprintf(stderr, " ");
        for (i=1; i < ArgLimit && i < Stacked; i++) {
            out_debug_limit1(Tl(sp[-i]), Limit); /*arbitrary limit to output */
            fprintf(stderr, " ");
        }
        fprintf(stderr, "\n");
        
        (void) refc_check();
        
        fprintf(stderr, "\n");
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
        if (debug)
            (void) fprintf(stderr, "reduce_optimise?: %s: got: %d\n", err_tag_name(Tag(*sp)), i);
        
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
    if (debug)
        fprintf(stderr, "reduce_optimise:\t"); out_tag(Tag(p)); fprintf(stderr, "\t%d\n", got);
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
        (void) fprintf(where, "%s\t%d\t§%d\n", "Total Error!", check - reductions, opt_check - optimisations);
    
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
    *nn = reduce(nn);
    
    if (Tag(*nn) != int_t) {
        if (debug)
            fprintf(stderr, "reduce_int: got %s\n", err_tag_name(Tag(*nn)));
        (void) err_reduce("expecting int");
    }
    
    return Num(*nn);
}

char reduce_bool(pointer *nn)
{
    *nn = reduce(nn);
    
    if (Tag(*nn) != bool_t) {
        if (debug)
            fprintf(stderr, "reduce_bool: got %s\n", err_tag_name(Tag(*nn)));
        (void) err_reduce("expecting bool");
    }
    
    return Bool(*nn);
}

char reduce_char(pointer *nn)
{
    *nn = reduce(nn);
    
    if (Tag(*nn) != char_t) {
        if (debug)
            fprintf(stderr, "reduce_char: got %s\n", err_tag_name(Tag(*nn)));
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

pointer reduce_cons(pointer *nn)
{
    *nn = reduce(nn);
    
    if (!IsCons(*nn)) {
        (void) err_reduce("expecting cons");
    }
    
    return *nn;
}

/* equality is polymophic and expands through lists [SASL Manual 1983] 
 
 The equality operators = and ∼=, however, are defined between arbitrary pairs of objects.
 Objects of different type are always unequal. E.g. — the following expressions all take the value true:
 2 + 2 = 4	1 ∼= 2	 false ∼= true
 1 ∼= false	%A ∼= %a
 (1,2,3) = (1,1+1,1+1+1)
 */
char reduce_is_equal(pointer *nn1, pointer *nn2)
{
    pointer n1, n2;
    
    n1 = *nn1 = reduce(nn1); /* update in place */
    n2 = *nn2 = reduce(nn2); /* update in place */
    
    if (IsCons(n1))
        return (IsCons(n2) &&
                reduce_is_equal(&Hd(n1), &Hd(n2)) &&
                reduce_is_equal(&Tl(n1), &Tl(n2)));
    
    
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
 * like reduce_is_equal, but on the tags have to match
 * NB NIL never has same tag as *anything*
 */
int reduce_is_equal_tag(pointer *nn1, pointer *nn2)
{
    pointer n1, n2;
    
    n1 = *nn1 = reduce(nn1); /* update in place */
    n2 = *nn2 = reduce(nn2); /* update in place */
    
    return (IsSet(n1) && IsSet(n1) && Tag(n1) == Tag(n2));
}

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

/*
 * initialise reduction machine
 */
int reduce_init()
{
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
 * reduce_show - print a constant - unstructured results of reduction
 */
void reduce_show(pointer p)
{
    if (IsNum(p))
        printf("%d", Num(p));
    else if (IsDbl(p))
        printf("%g", Dbl(p));
    else if (IsChar(p))
        printf("%c", Char(p));
    else if (IsBool(p))
        printf("%s", Bool(p) ? "TRUE" : "FALSE");
}

/*
 * reduce_print - reduce something, print it and delete the result
 */
pointer reduce_print(pointer *p)
{
    if (IsNil(*p))
        return NIL;
    
    /* reduce to find a constant, print it, free it */
    *p = reduce(p);
    
    if (debug) {
        fprintf(stderr, "Reduce_print: ");
        out_debug(*p);
    }
    
    if (IsCons(*p)) {
        Hd(*p) = reduce_print(&Hd(*p));
        Tl(*p) = reduce_print(&Tl(*p));
    }
    else {
        reduce_show(*p);
        /*    fflush(stdout); * temporary */
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

pointer reduce(pointer *n)
{
    pointer *base = sp;	/* remember Top on entry - used to calculate Stacked */
    
    if (IsNil(*n))
        return NIL;
    
    /* stack size is arbitrary, so check for potential overflow */
    if (Stacked >= ((STACK_SIZE)*0.9)) {
        (void) err_reduce("stack depth within 90% of max");
        /*NOTREACHED*/
    }
    
    Push(*n);
#if !old
#define R (*n = Top, Pop(Stacked), *n)
#else
#define R (*n = Pop(Stacked))
#endif
    while (IsSet(Top) && Stacked > 0) {
        /* Within the loop, Top is set to NIL on error; also halt if nothing on stack - should never happen error */
        
        if (debug > 1) {
            indent(stderr, Depth); out_debug1(Top);
            fprintf(stderr, "\t%s (%ld/%ld Depth/Stacked)", refc_pointer_info(Top), Depth, Stacked);
            fprintf(stderr, "\tTop\n");
        }
        
//      while (Tag(Top) == apply_t) {
        if (Tag(Top) == apply_t) {
            /* travel down the 'spine' */
            Assert(Stacked <= ((STACK_SIZE)*0.9));
            Push(Hd(Top));
            
            continue;	/* loop */
            /*NOTREACHED*/
        }

        /* Top is not apply_t, evaluate / return */
        reduce_log(base, sp);
        {
            if (Stacked >= 1) {
                tag tt = Tag(Top);
                /* zero args: const => const | list => list */
                /* >0 args:   list number => nth item of list */
                switch (tt) {
                        
                    case free_t: {
                        err_reduce("reducing a free node");
                        /*NOTREACHED*/
                    }
                    case cons_t: {

                        unsigned u;

                        if (Stacked == 1)
                            return R;
                        
                        Pop(1);

                        T(Top) = reduce(&T(Top));
                        if (!IsNum(T(Top)))
                            err_reduce("applying a list to something that is not a number");
                        u = Num(T(Top));
                        if (u < 1)
                            err_reduce("applying a list to a number less then 1");

                        Top = refc_update_Itl(Top, refc_copyNth(Top, u));
                        continue;
                    }

                        /* constants -  no further reductions here */
                    case int_t:
                    case floating_t:
                    case char_t:
                    case bool_t:
                        if (Stacked == 1)
                            return R;
                        err_reduce("applying a constant as a function");
                        /*NOTREACHED*/;
                    case name_t:
                        if (Stacked == 1)
                            return R;
                        err_reduce2("name undefined:", Name(Top));
                        /*NOTREACHED*/;
                    case fail_t:  {/* FAIL anything => FAIL */
                        if (Stacked == 1)
                            return R;
                        *n = refc_copy(Top);
                        Pop(Stacked);
                   /*XXX*/     refc_delete(sp + 1);  /* delete "FAIL anything ..." */
                        return (*n); /* return the FAIL */
                    }
                    case abstract_condexp_t: {
                        (void) err_reduce("abstract_condexp_t unexpected");
                        /*NOTREACHED*/
                    }
                    case abstract_formals_t: {
                        (void) err_reduce("abstract_formals_t unexpected");
                        /*NOTREACHED*/
                    }
                    case abstract_defs_t: {
                        (void) err_reduce("abstract_defs_t unexpected");
                        /*NOTREACHED*/
                    }
                    case def_t:
                        (void) err_reduce("def_t unexpected");
                        /*NOTREACHED*/
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
                    arg1 = &Tl(Top);
                    switch (tt)
                    {
                            /* unary: op arg  => res*/
                        case unary_minus_op:
                            Top = refc_update_to_int (Top, - reduce_int(arg1)); continue;
                        case unary_plus_op:
                            Top = refc_update_to_int (Top,   reduce_int(arg1)); continue;
                        case unary_not_op:
                            Top = refc_update_to_bool(Top, ! reduce_bool(arg1)); continue;
                            
                        case unary_strict:
                            /* ToDo - reduce_int() reduce_bool() etc to give better warning the "FAIL" */
                            *arg1 = reduce(arg1); /* strict */
                            /*FALLTHRU*/
                        case unary_nonstrict:
                            Assert(IsFun(H(Top)));
                            Top = refc_update_Itl(Top, Ufun(H(Top))(*arg1)); continue;

                        case I_comb:
                            /* I x => x */
                            if (Stacked > 1) {
                                /* elide (I x) y ==> x y on stack
                                 * Afterwards Arg1 has become H(Top')
                                 */
                                if (debug)
                                    fprintf(stderr,"**I_comb Stacked>1 case\n");
                                Pop(1);
                                Assert(!SameNode(Top, *arg1)); /* avoid stack loops!?! */
                                refc_update_hdS(&Top,"HT");
                            } else {
                                /* reduce: special case - leave Top node; replace it with arg1 as top of stack */
                                if (debug)
                                    fprintf(stderr,"**I_comb Stacked==1 case\n");/*XXX*/
                                Assert(SameNode(*n, Top));  /* should always be the case for Depth==1 */
                                Assert(n && ! IsNil(*n));   /*xxx*/
                                Pop(1);
                                refc_update_pointerS(n, "T");
                                Push(*n);
                                Assert(Stacked == 1);
                            }
                            continue;
                            
                        case Yc_comb:
                            Tag(Top) = cons_t;
                            /*FALLTHRU*/
                        case Y_comb: {
                            refc_delete(&Hd(Top)); /* loose the "Y" *//*new update(sp, T, me)*/
                            Hd(Top) = Tl(Top);
                            Tl(Top) = refc_copy_make_cyclic(Top);
                            continue;
                        }
                            
                            /* H (a:x) => a */
                            /* H other => FAIL */
                        case H_comb:
                            *arg1 = reduce(arg1);
                            /*xxx*/ Assert(SameNode(T(Top), *arg1));
                            if (IsCons(*arg1))
                                refc_updateIS(&Top, "TH");
                            else
                                err_reduce("taking head of non-cons");
                            Tag(Top) = apply_t;
                            continue;
                            
                            /* T (a:x) => x */
                            /* T other => FAIL */
                        case T_comb:
                            *arg1 = reduce(arg1);
                            if (IsCons(*arg1))
                                refc_updateIS(&Top, "TT");
                            else
                                err_reduce("taking tail of non-cons");
                            Tag(Top) = apply_t;
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
                                Top = refc_update_hdtl(Top, Ref("HT"), Ref("T"));
                                Tag(Top) = cons_t;
                                continue;
                                
                            case plusplus_op:
                                /* ++ list1 list2 => append list2 to end of list1 NB () ++ x => x */
                                /* xxx check list2 is a list?? */
                                *arg1 = reduce(arg1);
                                if ( ! (IsCons(*arg1) || IsNil(*arg1)))
                                    err_reduce("plusplus_op - non-list first arg");
                                Top = refc_update_Itl(Top, make_append(Ref("HT"), Ref("T")));
                                continue;
                                
                            case minusminus_op:
                                err_reduce("minusminus op not expected");
                                /*FALLTHRU*/
                            case range_op:
                                err_reduce("much_greater op not expected");
                                /*FALLTHRU*/
                            case much_greater_op:
                                err_reduce("much_greater op not expected");
                                continue;
                                
                            case greater_op:        Top = refc_update_to_bool(Top, reduce_int(arg1) >  reduce_int(arg2)); continue;
                            case greater_equal_op:  Top = refc_update_to_bool(Top, reduce_int(arg1) >= reduce_int(arg2)); continue;

                            case equal_op:          Top = refc_update_to_bool(Top, reduce_is_equal(arg1, arg2)); continue;
                            case not_equal_op:      Top = refc_update_to_bool(Top, !reduce_is_equal(arg1, arg2)); continue;
#ifdef old
//       ?????                     case not_equal_op:        Top = refc_update_to_bool(Top, reduce_int(arg1) != reduce_int(arg2)); continue;
#endif
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
                                
                                /* comb arg1 arg2 *//*xxx shoulndt we only allow NIL here -> and be strict?*/
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
                                
                            case U_comb:
                                /*  U f g => f (H g) (T g)    lazy version of U f (a:x) => f a x */
                                Top = refc_update_hdtl(Top,
                                                       new_apply(Ref("HT"), new_apply(new_comb(H_comb), Ref("T"))),
                                                       new_apply(new_comb(T_comb),  Ref("T")));
                                continue;
                                
                            case TRY_comb: {
                                /* TRY FAIL y => y
                                 * TRY x    y => x */
                                *arg1 = reduce(arg1);
                                if (IsFail(*arg1)) {
                                    Top = refc_update_Itl(Top, Ref("T"));
                                } else {
                                    Top = refc_update_Itl(Top, Ref("HT"));
                                }
                                continue;
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
                                }
                                case MATCH_comb: {
                                    /* MATCH const E x => const = x -> E; FAIL*/
                                    if (reduce_is_equal(arg1, arg3)) {
                                        refc_updateIS(&Top, "HT");
                                    } else {
                                        Top = refc_update_to_fail(Top);
                                    }
                                    continue;
                                }
                                    
                                case MATCH_TAG_comb: {
                                    /* MATCH tag E x => tag = Tag(x) -> E x; FAIL */
                                    
                                    if (reduce_is_equal_tag(arg1,  arg3)) {
                                        refc_updateSS(&Top, "HT", "T");
                                    } else {
                                        Top = refc_update_to_fail(Top);
                                    }
                                    continue;
                                }
                                    
#ifdef match_with_test
                                case MATCH_comb: {
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
                                    long int got = 0;
                                    if (got >= 2) {
                                        /* optimise - update in place, no reference counts changed, apart from new pointer to Arg3 */
                                        reduce_optimise_log(*sp, 2);
                                        refc_delete(&Hd(Top)); /* loose the 'S' */
                                        Hd(Top) = Tl(Top); Tl(Top) = Tl(Top);
                                        Hd(Top) = Tl(Top); Tl(Top) = refc_copy(Tl(Top));
                                        Hd(Top) = Top;     Tl(Top) = Top;
                                    }
                                    else {
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
                                    f = Arg1,
                                    g = Arg2,
                                    x = Arg3;
                                    
                                    int got = (reduce_optimise ? (sp - last) : 0);
                                    reduce_optimise_log(*sp, got);
                                    if (got == 0) {	 /* no optimisations */
                                        Pointers(Top) = (struct pointers) {new_apply(f, x), new_apply(g, refc_copy(x))};
                                    }
                                    else if (got == 1) { /* one optimisation - Top is apply_t with ALLrefc==1 so re-usable */
                                        Pointers(Top) = (struct pointers) {f, x};
                                        Pointers(Top) = (struct pointers) {Top, new_apply(g, refc_copy(x))};
                                    }
                                    else {	/* two optimisations assert got >= 2 */
                                        Pointers(Top) = (struct pointers) {f, x};
                                        Pointers(Top) = (struct pointers) {g, x};
                                        Pointers(Top) = (struct pointers) {Top, Top};
                                    }
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
                                        Top = refc_update_hdtl(Top,
                                                                  new_apply(Ref("HHHT"), new_apply(Ref("HHT"), Ref("T"))),
                                                                  new_apply(Ref("HT"), Ref("T")));
                                        continue;
                                    }
                                        
                                    default:
                                        ; /*FALLTHRU*/
                                }
                            }}}}}}
        /* nothing found */
        if (debug) {
            int i=0;
            fprintf(stderr, "unimplemented tag: Stacked %ld:%s\n", Stacked, refc_pointer_info(Top));
            
            do {fprintf(stderr, "Stack[%d]: ",i--); out_debug(sp[i]); } while (-i < Stacked);
        }
        err_reduce("unimplemented tag");
        /*NOTREACHED*/
    }
    {
        /* THN - this never happens? */
        if (debug)
            fprintf(stderr, "reduce done Stacked=%ld\n",  Stacked);/*XXX*/
        Assert(Stacked == 1 || IsCons(Top));
        return R;
    }
}

#undef Top
#undef Arg1
#undef Arg2
#undef Arg3
#undef Arg4
#undef Depth
#undef Pop
