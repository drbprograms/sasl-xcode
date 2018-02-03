/* 
 * reducer - reduce...
 */
#include <stdio.h>
#include <string.h>

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

/* the stack - used by reduce */

#define STACK_SIZE (10000)
#define RM_SIZE (265)


static pointer stack[STACK_SIZE];
static pointer *sp = stack;
#define Depth	(sp-stack)

/* reduce: local shortcuts to make reduce() 'clearer' to read */
#define Top	sp[0]

#define Stack1	sp[-1]
#define Stack2	sp[-2]
#define Stack3	sp[-3]
#define Stack4	sp[-4]

#define Arg1	Tl(Stack1)
#define Arg2	Tl(Stack2)
#define Arg3	Tl(Stack3)
#define Arg4	Tl(Stack4)

#define	Stacked	(sp-base)
#define Pop(n)	(sp -= (n)) /* assert(sp>=base) */

/*
 * log reductions to file as required
 */
static int reductions = 0;
static int tag_reductions[TagCount];

static void indent(FILE *where, long int n)
{
    static char b[] = "                ";
    static long int max;
    
    max = strlen(b);
    
    if (n <= 0)
        return;
    
    if (n > max)
        n = max;
    
    fprintf(where, "%s", b+max-n);
}

#define Limit 24
void reduce_log(pointer *base, pointer *sp)
{
    reductions++;
    tag_reductions[Tag(*sp)]++;
    
    if (debug) {
        int i = 0;
        long int height = sp - stack;
        long int stacked = sp - base;
        long int frame = base - stack;
        
        fprintf(stderr, "reduction %d: %s (Stacked=%ld Stack height %ld)\n",  reductions, refc_pointer_info(*sp), stacked, height);
        
        indent(stderr, frame);
        out_debug_limit1(sp[0], Limit); /*arbitrary limit to output */
        fprintf(stderr, " ");
        for (i=1; i<5 && i < stacked; i++) {
            out_debug_limit1(Tl(sp[-i]), Limit); /*arbitrary limit to output */
            fprintf(stderr, " ");
        }
        fprintf(stderr, "\n\n");
        
        (void) refc_check();
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

int reduce_int(pointer n)
{
    n = reduce(n);
    
    if (Tag(n) != int_t)
        (void) err_reduce("reduce: expecting int");
    
    return Num(n);
}

char reduce_bool(pointer n)
{
    n = reduce(n);
    
    if (Tag(n) != bool_t)
        (void) err_reduce("reduce: expecting bool");
    
    return Bool(n);
}

char reduce_char(pointer n)
{
    n = reduce(n);
    
    if (Tag(n) != char_t)
        (void) err_reduce("reduce: expecting char");
    
    return Char(n);
}

/* implement int -> double widening */
double reduce_double(pointer n)
{
    /*todo*/
    return Dbl(n);
}

pointer reduce_cons(pointer n)
{
    n = reduce(n);
    
    if (!IsCons(n))
        (void) err_reduce("reduce: expecting cons");
    
    return n;
}

/* equality is polymophic and expands through lists [SASL Manual 1983] 
 
 The equality operators = and ∼=, however, are defined between arbitrary pairs of objects.
 Objects of different type are always unequal. E.g. — the following expressions all take the value true:
 2 + 2 = 4	1 ∼= 2	 false ∼= true
 1 ∼= false	%A ∼= %a
 (1,2,3) = (1,1+1,1+1+1)
 */
char reduce_is_equal(pointer n1, pointer n2)
{
    if (IsCons(n1))
        return (IsCons(n2) &&
                reduce_is_equal(Hd(n1), Hd(n2)) &&
                reduce_is_equal(Tl(n1), Tl(n2)));
    
    n1 = reduce(n1);
    
    if (IsNil(n1) || IsConst(n1)) {
        
        n2 = reduce(n2);
        
        if (IsNil(n1))
            return (IsNil(n2)); /* NIL == NIL and NIL != non-nil */
        
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
void reduce_print(pointer p)
{
    if (IsNil(p))
        return;
    
    /* reduce to find a constant, print it, free it */
    p = reduce(p);
    
    if (debug) {
        REDUCE_DEBUG("Reduce_print ouput #",0);
        out_debug(p);
    }
    
    if (IsCons(p)) {
        reduce_print(Hd(p));
        reduce_print(Tl(p));
    } else {
        reduce_show(p);
        fflush(stdout); /* temporary */
    }
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
pointer reduce(pointer n)
{
    pointer *base = sp;	/* remember Top on entry - ?? is this variable really necessary */
    
    if (IsNil(n))
        return n;
    
    /* stack size is arbitrary, so check for potential overflow */
    if ((sp-stack) >= ((STACK_SIZE)*0.9)) {
        (void) err_reduce("reduce: stack depth within 90% of max");
        return NIL; /*NOTREACHED*/
    }
    
    sp[1] = n; /* Push(n) */
    sp++;
    
    while (!IsNil(*sp) && (sp > base)) { /* ?? (IsSet(Top) && Stacked > 0) */
        /* Within the loop, Top is set to NIL on error; also halt if nothing on stack - should never happen error */
        pointer *last = 0; /* optimiser: 'last' points to highest *successive* node on stack with ALLrefc==1, if any, otherwise (pointer *)0 */
        
        if (Tag(*sp) == apply_t) {
            /* travel down the 'spine' */
            
            if (reduce_optimise) { /* optimiser - reset after reduction has moved sp */
                if (last && last >= sp)
                    last = 0;
                if (Srefc(*sp) == 1) {	/* assert this node will be free if pointer sp[-1] is deleted */
                    if (!last)	/* start eligible */
                        last = sp;
                }
                else
                    last = 0;	/* not eligible */
            }
            
            sp[1] = Hd(*sp);  /* Push(Hd(Top)) */
            sp++;
            
            continue;	/* loop */
            /*NOTREACHED*/
        }
        
        {
            /* evaluate / return */
            reduce_log(base, sp);
            
            if (Stacked >= 1)
            /* constants -  no further reductions here */
                switch (Tag(*sp)) {
                        /* nothing to do */
                    case cons_t:
                    case int_t:
                    case floating_t:
                    case char_t:
                    case bool_t:
                    case name_t:
                    case fail_t:  /* FAIL anything => FAIL */
                    {
                        pointer here = Top;
                        Pop(Stacked);
                        return here;
                    }
#ifdef old
                        sp = base;
                        return sp[1]; /* assert sp[1] == n */
#endif
                        break;	/* ??? */
                        
                    case abstract_t:
                        sp = base;
                        (void) err_reduce("abstract_t unexpected");
                        return NIL; /*NOTREACHED*/
                    case recursive_abstract_t:
                        sp = base;
                        (void) err_reduce("recursive_abstract_t unexpected");
                        return NIL; /*NOTREACHED*/
                    case def_t:
                        sp = base;
                        (void) err_reduce("def_t unexpected");
                        return NIL; /*NOTREACHED*/
#ifdef notdef
                        if (partial_compile) {
                            
                            ;	/* not allowing this tag at run time */
                        }
                        else {
                            
                        }
#endif
                        
                    default:
                        ; /*FALLTHRU*/
                        
                }
            if (Stacked >= 2)
            /* unary: op arg => res */
                switch (Tag(Top))
            {
                    /* unary: op arg  => res*/
                case unary_minus_op:
                    Stack1 = refc_update_to_int(Stack1, - reduce_int(Arg1)); Pop(1); continue;
                case unary_plus_op:
                    Stack1 = refc_update_to_int(Stack1,   reduce_int(Arg1)); Pop(1); continue;
                case unary_not_op:
                    Stack1 = refc_update_to_bool(Stack1, ! reduce_bool(Arg1)); Pop(1); continue;
                    
                case unary_count_op:
                case range_unbounded_op:
                    break;
#ifdef broken
                    /* xxx ??? *** zark! */
                case I_comb: {
                    
                    if (Stacked > 2) {
                        /* I x y => x y */
                        /* Assert(Stacked >= 3) */
                        if (debug)
                            fprintf(stderr, "reduce: I_comb: refc_update_hd()\n");
                        
                        Stack2 = refc_update_hd(Stack2, refc_copy(Arg1));
                        Pop(2);
                    } else {
                        tag t = Tag(Arg1);
                        if (debug)
                            fprintf(stderr, "reduce: I_comb: Push(x)\n");
                        
                        /* this code is refc_update() */
                        if (HasPointers(Arg1)) {/*WIPWIP */
                            Stack1 = refc_update_hdtl(Stack1, refc_copy(Hd(Arg1)), refc_copy(Tl(Arg1)));
                        } else { /*todo wrap this all in simpler update-in-place refc_update() */
                            union val v = Val(Arg1);
                            Stack1 = refc_update_hdtl(Stack1, NIL, NIL);
                            Val(Stack1) = v;
                            Tag(Stack1) = t;
                        }
                        Tag(Stack1) = t;
                        Pop(1);
#ifdef was
                        Pop(2);
                        sp[1] = x;	/* Push(x) #define Push(x) (sp++, *sp = (x))*/
                        sp++;
#endif
                    }
                    continue;
                }
#endif 
                case I_comb:
                    /* I x => x*/
                    Stack1 = refc_update(Stack1, Arg1);
                    Pop(1);
                    continue;
                    
                case Yc_comb:
                    Tag(Stack1) = cons_t;
                    /*FALLTHRU*/
                case Y_comb: {
                    refc_delete(&Hd(Stack1)); /* loose the "Y" */
                    Hd(Stack1) = Tl(Stack1);
                    Tl(Stack1) = refc_copy_make_cyclic(Stack1);
                    Pop(1);
                    continue;
                }
                    
                    /* H (a:x) => a */
                    /* H other => FAIL */
                case H_comb:
                    Arg1 = reduce(Arg1);
                    if (IsCons(Arg1))
                        Stack1 = refc_update_hdtl(Stack1, new_comb(I_comb), refc_copy(Hd(Arg1)));
                    else
                        Stack1 = refc_update_to_fail(Stack1);
                    Pop(1);
                    continue;
                    
                    /* T (a:x) => x */
                    /* T other => FAIL */
                case T_comb:
                    Arg1 = reduce(Arg1);
                    if (IsCons(Arg1))
                        Stack1 = refc_update_hdtl(Stack1, new_comb(I_comb), refc_copy(Tl(Arg1)));
                    else
                        Stack1 = refc_update_to_fail(Stack1);
                    Pop(1);
                    continue;
                    
                default:
                    ;	/*FALLTHRU*/
            }
            if (Stacked >= 3)
            /* binary: op arg1 arg2 => res */
                switch (Tag(Top)) {
                    case plus_op:	Stack2 = refc_update_to_int(Stack2, reduce_int(Arg1) + reduce_int(Arg2));	Pop(2);	continue;
                    case minus_op:	Stack2 = refc_update_to_int(Stack2, reduce_int(Arg1) - reduce_int(Arg2));	Pop(2);	continue;
                    case times_op:	Stack2 = refc_update_to_int(Stack2, reduce_int(Arg1) * reduce_int(Arg2));	Pop(2);	continue;
                    case int_divide_op:	Stack2 = refc_update_to_int(Stack2, reduce_int(Arg1) / reduce_int(Arg2));	Pop(2);	continue;
                        
                    case or_op:	Stack2 = refc_update_to_bool(Stack2, reduce_bool(Arg1) || reduce_bool(Arg2));	Pop(2);	continue;
                    case and_op:	Stack2 = refc_update_to_bool(Stack2, reduce_bool(Arg1) && reduce_bool(Arg2));	Pop(2);	continue;
                        
                    case colon_op:
                        Stack2 = refc_update_hdtl(Stack2, refc_copy(Arg1), refc_copy(Arg2));
                        Tag(Stack2) = cons_t;
                        Pop(2); continue; /* P x y => x:y */
                    case plusplus_op:
                    case minusminus_op:
                    case range_op:
                        
                    case much_greater_op:
                        return Top; /* todo */
                        
                        
                    case greater_op:	Stack2 = refc_update_to_bool(Stack2, reduce_int(Arg1) >  reduce_int(Arg2));	Pop(2);	continue;
                    case greater_equal_op:
                        Stack2 = refc_update_to_bool(Stack2, reduce_int(Arg1) >= reduce_int(Arg2));	Pop(2);	continue;
                    case equal_op:
                        Stack2 = refc_update_to_bool(Stack2, reduce_is_equal(Arg1, Arg2));
                        Pop(2);	continue;
                        
                    case not_equal_op:	Stack2 = refc_update_to_bool(Stack2, reduce_int(Arg1) != reduce_int(Arg2));	Pop(2);	continue;
                        
                    case less_equal_op:	Stack2 = refc_update_to_bool(Stack2, reduce_int(Arg1) <= reduce_int(Arg2));	Pop(2);	continue;
                    case less_op:	Stack2 = refc_update_to_bool(Stack2, reduce_int(Arg1) <  reduce_int(Arg2));	Pop(2);	continue;
                        
                        
                        
                        /* floating point */
                    case much_less_op:
                        
                    case divide_op:
                    case rem_op:
                    case power_op:
                        return Top; /* todo */
                        
                        /* comb arg1 arg2 */
                    case K_nil_comb:
                        if (IsCons(Arg2))
                            err_reduce("K_nil_comb - non-list second arg");
                    case K_comb:
                        /* K x y => I x  [i node] */
                        Stack2 = refc_update_hdtl(Stack2, new_comb(I_comb), refc_copy(Arg1));
                        
                        Pop(2);
                        continue;

                    case U_comb:
                        /*  U f g => f (H g) (T g) */
                        Stack2 = refc_update_hdtl(Stack2,
                                                  new_apply(refc_copy(Arg1), new_apply(new_comb(H_comb), refc_copy(Arg2))),
                                                  new_apply(new_comb(T_comb),  refc_copy(Arg2)));
                        Pop(2);
                        continue;

                    case TRY_comb: {
                        /* TRY FAIL y => y
                           TRY x    y => x */
                        Arg1 = reduce(Arg1);
                        if (IsFail(Arg1)) {
                            Stack2 = refc_update(Stack2, refc_copy(Arg2));
                        } else {
                            Stack2 = refc_update(Stack2, refc_copy(Arg1));
                        }
                        Pop(2);
                        continue;
                    }
                        
#ifdef incorrect
                    case U_comb:
                        /*  U f (x:y) => (f x) y */
                        /*  U f other => FAIL */
                        Arg2 = reduce(Arg2);
                        if (IsCons(Arg2)) {
                            Stack2 = refc_update_hdtl(Stack2, new_apply(refc_copy(Arg1), refc_copy(Hd(Arg2))), refc_copy(Tl(Arg2)));
                        }
                        else {
                            Stack2 = refc_update_to_fail(Stack2);
                        }
                        Pop(2);
                        continue;
#endif

                    default:
                        ;	/*FALLTHRU*/
                }
            if (Stacked >= 4)
            /* ternary: op arg1 arg2 arg3 => res */
                switch (Tag(Top)) {
                        
                    case cond_op:	{
                        /* cond b t f => t (or f) */
                        if (reduce_bool(Arg1))
                            Stack3 = refc_update_hdtl(Stack3, new_comb(I_comb), refc_copy(Arg2));
                        else
                            Stack3 = refc_update_hdtl(Stack3, new_comb(I_comb), refc_copy(Arg3));
                        
                        Pop(3);
                        continue;
                    }
                    case MATCH_comb: {
                        /* MATCH const E x => const = x -> E; FAIL*/
                        if (reduce_is_equal(Arg1, Arg3)) {
                            Stack3 = refc_update_hdtl(Stack3, new_comb(I_comb), refc_copy(Arg2));
                        } else {
                            Stack3 = refc_update_to_fail(Stack3);
                        }
                        Pop(3);
                        continue;
                    }
#ifdef match_with_test
                    case MATCH_comb: {
                        /* MATCH test E x => (test x)= FALSE -> FAIL; E */
                        pointer res = new_apply(refc_copy(Arg1), refc_copy(Arg3));
                        char c = reduce_bool(res);
                        refc_delete(&res);
                        
                        if (c) {
                            Stack3 = refc_update_hdtl(Stack3, new_comb(I_comb), refc_copy(Arg2));
                        } else {
                            Stack3 = refc_update_to_fail(Stack3);
                        }
                        Pop(3);
                        continue;
                    }
#endif
                        /*[x] f g			=> S f g x	=> (f x) (g x)*/
                        /*[x] f g0	=> S f (K g)	=> C f g x	=> f x g */
                        /*[x] f0 g	=> S (K f) g	=> B f g x	=> f (g x) */
                        /*[x] f0 g0	=> S (K f)(K g)			=> f g */
                    case Sc_comb:	 /* Sc f g x => (f x):(g x) */
                        Tag(Stack3) = cons_t;
                        /*FALLTHRU*/
                    case S_comb:	 /* S f g x => (f x)(g x) */
                    {
                        long int got = (reduce_optimise ? (sp - last) : 0);
                        if (got >= 2) {
                            /* optimise - update in place, no reference counts changed, apart from new pointer to Arg3 */
                            reduce_optimise_log(*sp, 2);
                            refc_delete(&Hd(Stack1)); /* loose the 'S' */
                            Hd(Stack1) = Tl(Stack1); Tl(Stack1) = Tl(Stack3);
                            Hd(Stack2) = Tl(Stack2); Tl(Stack2) = refc_copy(Tl(Stack3));
                            Hd(Stack3) = Stack1;     Tl(Stack3) = Stack2;
                        }
                        else {
                            /*new update top-of-stack ap(ap(THH,T), ap(TH,T))*/
                            pointer n1 = new_apply(refc_copy(Arg1), refc_copy(Arg3));
                            pointer n2 = new_apply(refc_copy(Arg2), refc_copy(Arg3));
                            Stack3 = refc_update_hdtl(Stack3, n1, n2);
                        }
                        Pop(3);
                        continue;
                    }
                    case Bc_comb:	/* Bc f g x => f:(g x) */
                        Tag(Stack3) = cons_t;
                    case B_comb:	/* B  f g x => f (g x) */
                        Stack3 = refc_update_hdtl(Stack3,
                                                  refc_copy(Arg1),
                                                  new_apply(refc_copy(Arg2), refc_copy(Arg3)));
                        Pop(3);
                        continue;
                        
                    case Cc_comb:	/* Cc f g x => f x:g */
                        Tag(Stack3) = cons_t;
                    case C_comb:	/* C  f g x => f x g */
                        Stack3 = refc_update_hdtl(Stack3,
                                                  new_apply(refc_copy(Arg1), refc_copy(Arg3)),
                                                  refc_copy(Arg2));
                        Pop(3);
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
                            Pointers(Stack3) = (struct pointers) {new_apply(f, x), new_apply(g, refc_copy(x))};
                        }
                        else if (got == 1) { /* one optimisation - Stack1 is apply_t with ALLrefc==1 so re-usable */
                            Pointers(Stack1) = (struct pointers) {f, x};
                            Pointers(Stack3) = (struct pointers) {Stack1, new_apply(g, refc_copy(x))};
                        }
                        else {	/* two optimisations assert got >= 2 */
                            Pointers(Stack1) = (struct pointers) {f, x};
                            Pointers(Stack2) = (struct pointers) {g, x};
                            Pointers(Stack3) = (struct pointers) {Stack1, Stack2};
                        }
                        /*
                         sp -= 2;
                         *sp = n1;
                         */
                        Pop(3);
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
                            Hd(Stack3) = new_apply(f, x); Tl(Stack3) = new_apply(f, refc_copy(x));
                        }
#ifdef notdef
                        else if (got == 1) { /* one optimisation - Stack1 is apply_t with ALLrefc==1 so re-usable */
                            Hd(Stack1) = f; Tl(Stack1) = x;
                            Hd(Stack3) = Stack1; Tl(Stack3) = new_apply(f, refc_copy(x));
                        }
#endif
                        else if (got >= 2) /*WIP*/{	/* two optimisations assert got >= 2 */
                            Hd(Stack1) = f; Tl(Stack1) = x;
                            Hd(Stack2) = g; Tl(Stack2) = refc_copy(x);
                            Hd(Stack3) = Stack1; Tl(Stack3) = Stack2;
                        }
                        /*
                         sp -= 2;
                         *sp = n1;
                         */
                        Pop(3);
                        continue;
                    }
#endif
                    case TRYn_comb: {
                        /* TRYn 0 f g x => FAIL       || should never happen! */
                        /* TRYn 1 f g x => TRY        (f x) (g x) */
                        /* TRYn n f g x => TRYn (n-1) (f x) (g x)*/
                        if (!IsNum(Arg1) || (Num(Arg1) < 1)) {
                            Stack3 = refc_update_to_fail(Stack3);
                        } else if (Num(Arg1) == 1) {
                            Stack3 = refc_update_hdtl(Stack3,
                                                      new_apply(new_comb(TRY_comb),
                                                                new_apply(refc_copy(Arg2), refc_copy(Arg4))),
                                                      new_apply(refc_copy(Arg3), refc_copy(Arg4)));
                        } else {
                            Stack3 = refc_update_hdtl(Stack3,
                                                      new_apply(new_apply(new_comb(TRYn_comb), new_int(Num(Arg1) - 1)),
                                                                new_apply(refc_copy(Arg2), refc_copy(Arg4))),
                                                      new_apply(refc_copy(Arg3), refc_copy(Arg4)));
                            
                        }
                        Pop(3);
                        continue;
                    }
                        
                    default:
                        ;	/*FALLTHRU*/
                }
            
            if (Stacked >= 5)
            /* quaternary: op arg1 arg2 arg3 arg4 => res */
                switch (Tag(Top)) {
                        
                    case Sp_comb:	{
                        /* Sp f g h x => f (g x) (h x) */
                        /*todo make this less tedious to specify!*/
                        Stack4 = refc_update_hdtl(Stack4,
                                                  new_apply(refc_copy(Arg1), new_apply(refc_copy(Arg2), refc_copy(Arg4))),
                                                  new_apply(refc_copy(Arg3), refc_copy(Arg4)));
                        Pop(4);
                        continue;
                    }
                        
                    default:
                        ; /*FALLTHRU*/
                }
        }
        /* nothing found */
        if (debug) {
            int i=0;
            fprintf(stderr, "unimplemented tag: Stacked %ld:%s\n", Stacked, refc_pointer_info(Top));
            
            do {fprintf(stderr, "Stack[%d]: ",i--); out_debug(sp[i]); } while (-i < Stacked);
        }
        return ((void)(err_reduce("unimplemented tag")), /*NOTREACHED*/ (pointer) NIL);
    }
    sp = base;
    return *base;	/* assert *base==n */
}

#undef Top
#undef Arg1
#undef Arg2
#undef Arg3
#undef Arg4
#undef Depth
#undef Pop
