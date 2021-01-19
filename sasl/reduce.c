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

 sp[0] &Tl=arg[1] <<--Top Stacked == 1
 sp[1] &Tl=arg[2]
 ...
 reduction re-writes Top, and possibly elides the pointer to Top (either sp+1 or 'n' dependeng on Depth
 */
#define Top	sp[0]

#define Pop(n)  (Assert(Stacked >= (n)), sp -= (n), sp[n]) /* /Users/dad/Dropbox/Dev/xcode/sasl/sasl/common.c://#define Pop(n)  (sp -= (n), sp[n])assert((sp+n)>=base) value is previous Top of stack */
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
        for (i=1; i < ArgLimit && i < Stacked; i++) {
            Debug("\t");
            out_debug_limit1(Tl(sp[-i]), Limit); /*arbitrary limit to output */
        }
        Debug("\n");
        
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
        if (fflush(stdout)) /* xxx should be only when interactive */
            err_reduce2("problem when writing the output: ", strerror(errno));
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
    
    if (IsNil(*n))
        return;
    
    /* stack size is arbitrary, so check for potential overflow */
    if (Stacked >= ((STACK_SIZE)*0.9)) {
        (void) err_reduce("stack depth within 90% of max");
        /*NOTREACHED*/
    }
    
    Push(*n); /* Assert(Stacked == 1); */
    
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

        {/*start:reduce inner loop*/
            int r = reductions;
            if (Stacked > 0) {
                const int MAXARG = 4; /* max number of apply nodes in spine for a reduction */
                pointer *arg[MAXARG+1];
                int i;
                int nargs = tag_nargs(Top); /* leaf plus this number of apply nodes need to be found on the stack */
                
                tag tt = Tag(Top);
                
                /* locate args, numbered arg[1], arg[2], ... */
                if (nargs >= Stacked)
                    err_reduce2("problem reducing ", err_tag_name(tt));
                
                for (i = 1; i <= nargs;  i++) {
                    Pop(1);
                    arg[i] = &Tl(Top);
                }

                if (debug && nargs > 0){
                    Debug1("<sasl-reduce %d> ", r);
                    indent(stderr, Depth); pretty_print(stderr, Top);
                    Debug(" </sasl-reduce>\n");
                }
                
                switch (tt) {
                        
                    case free_t: {
                        err_reduce("reducing a free node");
                        break;
                        /*NOTREACHED*/
                    }
                    case deleting_t: {
                        err_reduce("reducing a deleting node");
                        break;
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
                                    /*NOTREACHED*/
                                    return;
                                }
                                pp = &T(*pp);
                            }
                        }
                        //                        TODO simplify refc_copy_Nth() as the list has  been "forced" successfuly above
                        Top = refc_update_Itl(Top, refc_copyNth(Top, u));
                        Tag(Top) = apply_t;
                        break;
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
                        err_reduce2("name undefined:", Name(Top));
                        /*NOTREACHED*/
                        return;
                    case fail_t: /* FAIL anything ... => FAIL; FAIL = FAIL */
                        if (Stacked > 1) {
                            /* Note this is a short-circuit reduction: FAIL x y z => FAIL y z => FAIL z => FAIL */
                            Pop(Stacked -1);
                            refc_update_to_fail(Top);
                        }
                        Pop(1);
                        return;
                    case abstract_formals_t:
                    case abstract_where_t:
                    case abstract_defs_t:
                        /* !! these tags are irregular as their arguments both reside in the tag node itself */
                        Tag(Top) = apply_t;
                        Top = refc_update_Itl(Top, (reduce_abstract(refc_copy(H(Top)), refc_copy(T(Top)),  tt))); // 2020-03-17 ... and why not refc_update_hdtl()?
                        break;
                    case def_t:
                        (void) err_reduce2(err_tag_name(tt), "unexpected tag:");
                        /*NOTREACHED*/
                        break;
#ifdef notdef
                        if (partial_compile) {
                            
                            ;	/* not allowing this tag at run time */
                            /* Here is just-in-time code generation ...*/
                        }
                        else {
                            
                        }
#endif
                        
                        /* unary: op arg  => res*/
                    case unary_minus_op:
                        Top = refc_update_to_int (Top, - reduce_int(arg[1])); break;
                    case unary_plus_op:
                        Top = refc_update_to_int (Top,   reduce_int(arg[1])); break;
                    case unary_not_op:
                        Top = refc_update_to_bool(Top, ! reduce_bool(arg[1])); break;
                        //stack: push "H"; reduce; check-bool; theBool = -theBool; update
                        
                    case unary_strict:
                        /* ToDo - reduce_int() reduce_bool() etc to give better warning the "FAIL" */
                        reduce(arg[1]); /* strict */
                        /*FALLTHRU*/
                    case unary_nonstrict:
                        Assert(IsFun(H(Top)));
                        Top = refc_update_Itl(Top, Ufun(H(Top))(*arg[1])); break;
                        //stack: push "H"; theValue = uFun(theValie); update
                        
                    case I_comb:
                        /* I x => x */
                        if (Stacked > 1) {
                            /* elide (I x) y ==> x y on stack
                             * Afterwards arg[1] becomes H(Top')
                             */
                            Debug("**I_comb Stacked>2 case\n");
                            Assert(!SameNode(Top, *arg[1])); /* avoid stack loops!?! */
                            
                            Pop(1);
                            refc_update_hdS(&Top,"HT");
                            //stack: pop; push "HT"; push "T"; mk_apply; update
                            
                        } else {
                            /* at the top of the stack  (I x) ==> x on stack
                             * recurse to reduce x (short-circuit)
                             * and update return value "*n" to x ie T(Top) */
                            Debug("**I_comb Stacked==2 case\n");/*XXX*/
                            Assert(n && ! IsNil(*n));   /*xxx*/
                            
#if 1
                            Top = *n = refc_update_pointerS(n, "T");
#else
                            reduce(arg[1]); /* recurse - carry on reducing */
                            *n = refc_update_pointerS(n, "T");
                            Pop(1); Push(*n); // temp or not ??  Top = *n = refc_updat_pointerS ...
                            Pop(1);
#endif
                            //stack: push "T"; reduce; update
                            //                                Assert(SameNode(*n, Top));  /* should always be the case for Depth==1 */
                            //
                            //                                T(Top) = reduce(&T(Top)); /* carry on reducing OR *arg[1] = reduce(arg[1]) */
                            //                                refc_update_pointerS(n, "T"); /* ie T(Top) */
                            //                                Pop(1);
                            //                                return;
                        }
                        break;
                        
                    case Yc_comb:
                        Tag(Top) = cons_t;
                        /*FALLTHRU*/
                    case Y_comb: {
                        refc_delete(&Hd(Top)); /* loose the "Y" */ /*new update(sp, T, me)*/
                        Hd(Top) = Tl(Top);
                        Tl(Top) = refc_copy_make_cyclic(Top);
                        //stack: Y_comb: push "T"; push "."; mk_apply; update
                        break;
                    }
//NotYet                    case Y_comb:
//NotYet                        if (Y_loop) {
//NotYet                            /* "knot-tying" using self-reference: Y f => f <self> */
//NotYet                           refc_delete(&Hd(Top)); /* loose the "Y" */ /*new update(sp, T, me)*/
//NotYet                            Hd(Top) = Tl(Top);
//NotYet                            Tl(Top) = refc_copy_make_cyclic(Top);
//NotYet                            //stack: Y_comb: push "T"; push "."; mk_apply; update
//NotYet                       } else {
//NotYet                            /* without loop: Y f => f (Y f) */
//NotYet                            Top = refc_update_hdtl(Top, Ref("T"), new_apply(Ref("H"), Ref("T")));
 //NotYet                       }
 //NotYet                       break;
                        
                        /* H (a:x) => a */
                    case H_comb:
                        reduce(arg[1]);
                        if (IsCons(*arg[1])) {
                            refc_updateIS(&Top, "TH");
                            Tag(Top) = apply_t;
                        }
                        else {
                            err_reduce("taking head of non-cons");
                        }
                        break;
                        //stack: push "T"; reduce; not(isCons) ? (pop;new_comb(FAIL)) : (new_comb(I_Comb); mk_rev_apply)) ; update
                        //stack: push "T"; reduce; check(cons_t); pop; new_comb(I_comb); push("TT"); mk_apply)) ; update
                        
                        /* T (a:x) => x */
                    case T_comb:
                        reduce(arg[1]);
                        if (IsCons(*arg[1])) {
                            refc_updateIS(&Top, "TT");
                            Tag(Top) = apply_t;
                        } else {
                            err_reduce("taking tail of non-cons");
                        }
                        break;
                        
                    case plus_op:	Top = refc_update_to_int(Top, reduce_int(arg[1]) + reduce_int(arg[2]));	break;
                    case minus_op:	Top = refc_update_to_int(Top, reduce_int(arg[1]) - reduce_int(arg[2]));	break;
                    case times_op:	Top = refc_update_to_int(Top, reduce_int(arg[1]) * reduce_int(arg[2]));	break;
                    case int_divide_op:	Top = refc_update_to_int(Top, reduce_int(arg[1]) / reduce_int(arg[2]));	break;
                        
                    case or_op:	Top = refc_update_to_bool(Top, reduce_bool(arg[1]) || reduce_bool(arg[2]));	break;
                    case and_op:	Top = refc_update_to_bool(Top, reduce_bool(arg[1]) && reduce_bool(arg[2]));	break;
                        
                    case colon_op:
                        /* (P x y) => (x:y)
                         ((: a) b) => (a:b) */
                        refc_updateSS(&Top, "HT", "T");
                        Tag(Top) = cons_t;
                        break;
                        //stack: colon_op: push "HT"; push "T"; mk_cons; update
#if 1 // old bug
                        /* ++ () x => x */
                        /* ++ list1 list2 =>  (hd-list1 : (++ tl-list1) list2)))  */
                    case plusplus_op:
                        /* check list2 is a list - no, leave it lazy */
                        reduce(arg[1]);//XXXX wrong?
                        if ( ! (IsCons(*arg[1]) || IsNil(*arg[1])))
                            err_reduce("plusplus_op - non-list first arg");
                        //                                Top = refc_update_Itl(Top, make_append(Ref("HT"), Ref("T")));BUGGG
                        if (IsNil(*arg[1])) {
                            Top = refc_update_Itl(Top, Ref("T"));
                        } else {
                            Top = refc_update_hdtl(Top, Ref("HTH"),
                                                   new_apply3(new_comb(plusplus_op), Ref("HTT"), Ref(("T"))));
                            Tag(Top) = cons_t;
                        }
                        break;
#else
                    case plus_plus_op: /* "should not evaluate it's arguments" */
                        /* ++ () x => x */
                        /* ++ list1 list2 =>  ((hd list1) : (++ (tl list1) list2))) */
                        /* 2020-04-02 fix: made properly lazy */
                        if (IsNil(*arg[1])) {
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
                        break;
                        
                    case greater_op:        Top = refc_update_to_bool(Top, reduce_int(arg[1]) >  reduce_int(arg[2])); break;
                    case greater_equal_op:  Top = refc_update_to_bool(Top, reduce_int(arg[1]) >= reduce_int(arg[2])); break;
                        
                    case equal_op:          Top = refc_update_to_bool(Top, reduce_is_equal(arg[1], arg[2])); break;
                    case not_equal_op:      Top = refc_update_to_bool(Top, !reduce_is_equal(arg[1], arg[2])); break;
                    case less_equal_op:	    Top = refc_update_to_bool(Top, reduce_int(arg[1]) <= reduce_int(arg[2]));	break;
                    case less_op:	        Top = refc_update_to_bool(Top, reduce_int(arg[1]) <  reduce_int(arg[2]));	break;
                        
                        
                        
                        /* todo floating point */
                    case much_less_op:
                        err_reduce("much_less op not expected");
                    case divide_op:
                        err_reduce("divide op not expected");
                    case rem_op:
                        err_reduce("rem op not expected");
                    case power_op:
                        err_reduce("power op not expected");
                        break;
                        
                        /* comb arg[1] arg[2] */ /*xxx shoulndt we only allow NIL here -> and be strict?*/
                    case K_nil_comb:
                        if ( ! (IsNil(*arg[2]) ||
                                IsCons(*arg[2]) ||  //xxx bug? "a:x" will never evalute to "()"
                                (IsApply(*arg[2]) && //xxx bug?
                                 ((Tag(Hd(*arg[2])) == H_comb) || //xxx bug?
                                  (Tag(Hd(*arg[2])) == T_comb))//xxx bug?
                                 ) /* allow "lazy" lists */
                                ))
                            err_reduce("K_nil_comb - non-list second arg");
                    case K_comb:
                        /* K x y => I x  [i node] */
                        Top = refc_update_Itl(Top, Ref("HT"));
                        break;
                        //stack: K_comb: push "HT"; new_comb(I_comb); mk_rev_apply; update
                        
                    case U_comb:
                        /*  U f g => f (H g) (T g)    lazy version of U f (a:x) => f a x */
                        Top = refc_update_hdtl(Top,
                                               new_apply(Ref("HT"), new_apply(new_comb(H_comb), Ref("T"))),
                                               new_apply(new_comb(T_comb),  Ref("T")));
                        break;
                        //stack: U_comb: push "HT"; new_comb(H_comb); push "T"; mk_apply; new_comb(T_comb); push "T"; mk_apply; mk_apply; mk_apply; update
                        
                    case TRY_comb: {
                        /* TRY FAIL y => y
                         * TRY x    y => x */
                        reduce(arg[1]);
                        if (IsFail(*arg[1])) {
                            Top = refc_update_Itl(Top, Ref("T"));/*refc_update_IS(&Top, "T")*/
                            //stack: new_comb(I_comb); push "T"; mk_apply; update
                        } else {
                            Top = refc_update_Itl(Top, Ref("HT"));
                        }
                        break;
                        //stack: TRY_comb: push "HT"; reduce; IsFail ? (pop; new_comb(I_comb); push "T"; mk_apply) : (new_comb(I_comb); rev_make_apply) update
                    }
                        
                    case cond_op:	{
                        /* cond b t f => t (or f) */
                        if (reduce_bool(arg[1]))
                            Top = refc_update_Itl(Top, Ref("HT"));
                        else
                            Top = refc_update_Itl(Top, Ref("T"));
                        break;
                        //stack: cond_op: push "HHT"; reduce
                    }
                    case MATCH_comb: {
                        /*  MATCH x E x => E; MATCH x E y => FAIL*/
                        /* ?should check arg[1] is a constant? */
                        if (reduce_is_equal(arg[1], arg[3])) {
                            refc_updateIS(&Top, "HT");
                        } else {
                            Top = refc_update_to_fail(Top);
                        }
                        break;
                    }
                        
                    case MATCH_TAG_comb: {
                        /* MATCH_TAG t E x => Tag(t) = Tag(x) -> E x; FAIL */
                        
                        if (reduce_is_equal_tag(arg[1],  arg[3])) {
                            refc_updateSS(&Top, "HT", "T");
                        } else {
                            Top = refc_update_to_fail(Top);
                        }
                        break;
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
                        break;
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
                            /* optimise - update in place, no reference counts changed, apart from new pointer to arg[3] */
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
                        break;
                    }
                    case Bc_comb:	/* Bc f g x => f:(g x) */
                        Tag(Top) = cons_t;
                    case B_comb:	/* B  f g x => f (g x) */
                        Top = refc_update_hdtl(Top,
                                               Ref("HHT"),
                                               new_apply(Ref("HT"), Ref("T")));
                        break;
                        
                    case Cc_comb:	/* Cc f g x => f x:g */
                        Tag(Top) = cons_t;
                    case C_comb:	/* C  f g x => f x g */
                        Top = refc_update_hdtl(Top,
                                               new_apply(Ref("HHT"), Ref("T")),
                                               Ref("HT"));
                        break;
                        
                        
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
                        break;
                    }
                        
#endif
#ifdef V3
                    {
                        pointer
                        f = arg[1],
                        g = arg[2],
                        x = arg[3];
                        
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
                        break;
                    }
#endif
                        
                    case TRYn_comb: {
                        /* TRYn 0 f g x => FAIL       || should never happen! */
                        /* TRYn 1 f g x => TRY        (f x) (g x) */
                        /* TRYn n f g x => TRYn (n-1) (f x) (g x)*/
                        if (!IsNum(*arg[1]) || (Num(*arg[1]) < 1)) {
                            Top = refc_update_to_fail(Top);  /* err_reduce("problem with matching") */
                        } else if (Num(*arg[1]) == 1) {
#if 1
                            // fix for 35.2  "about to delete something which is NOT free" which then recovers ...
                            // is the problem that "T" is used twice - if so happens all over the place ...
                            pointer
                            hht = refc_copyS(Top, "HHT"),
                            t1 = refc_copyS(Top, "T"),
                            t2 = refc_copyS(Top, "T"),
                            ht = refc_copyS(Top, "HT") ;
                            Top = refc_update_hdtl(Top,
                                                   new_apply(new_comb(TRY_comb),
                                                             new_apply(hht, t1)),
                                                   new_apply(ht, t2));

#else
                            Top = refc_update_hdtl(Top,
                                                   new_apply(new_comb(TRY_comb),
                                                             new_apply(refc_copyS(Top, "HHT"), refc_copyS(Top, "T"))),
                                                   new_apply(refc_copyS(Top, "HT"), refc_copyS(Top, "T")));
#endif
                        } else {
                            Top = refc_update_hdtl(Top,
                                                   new_apply3(new_comb(TRYn_comb),
                                                              new_int(Num(*arg[1]) - 1),
                                                              new_apply(refc_copyS(Top, "HHT"), refc_copyS(Top, "T"))),
                                                   new_apply(refc_copyS(Top, "HT"), refc_copyS(Top, "T")));
                        }
                        break;
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
                        break;
                    }
                        
                    default:
                        /* nothing found */
                        if (debug) {
                            int i=0;
                            Debug2("unimplemented tag: Stacked %ld:%s\n", Stacked, err_tag_name(tt));
                            
                            do {Debug1("Stack[%d]: ",i--); out_debug(sp[i]); } while (-i < Stacked);
                        }
                        err_reduce("unimplemented tag");
                        /*NOTREACHED*/
                }
            }
            //
            if (debug) {
                Debug1("<sasl-reduce %d> ", r);
                indent(stderr, Depth);Debug("=> "); pretty_print(stderr, Top);
                Debug(" </sasl-reduce>\n");
            }
            //
        }/*end:reduce inner loop*/
        
    }
    
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

#undef Top
#undef Depth
#undef Pop
