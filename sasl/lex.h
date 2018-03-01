
/* lex.h */

/* tok - tokens provided by lexer */
typedef enum tok 
  {
    tok_where,

    tok_then,

    tok_question_mark,

    tok_left_paren,
    tok_right_paren,
    tok_semicolon,
    tok_comma,
    tok_equal,

    tok_generator, /* ZF */
    tok_left_brace, /* ZF */
    tok_right_brace, /* ZF */

    tok_constant, /* see tok_const below for sub-types */

    tok_indentation,

    tok_colon,	/*todo nb also an operator ;-) */
    tok_operator,  /* see tok_op below for sub-types */
   
    tok_def,

    tok_command,
    tok_shell_command,

    tok_name,

    tok_newline,

    tok_offside,

    tok_unrecognised_char,
      
    tok_eof
    
  } tok;

/* Constants = subtypes for tok_constant */
typedef enum tok_const {
  tok_const_nil,

  /* numbers */
  tok_const_integer,
  tok_const_floating,
  /* end numbers */

  tok_const_true,
  tok_const_false,

  tok_const_char,
  tok_const_special_char_NL,
  tok_const_special_char_NP,
  tok_const_special_char_SP,
  tok_const_special_char_TAB,

  tok_const_string_start,
  tok_const_string_start_nested,
  tok_const_string_end,
  tok_const_string_end_nested,
  tok_const_string_char
} tok_const;

/* Operators = subtypes for tok_operator */
typedef enum tok_op {
  op_colon, 	/* 0 */
  op_plusplus, 	/* 1 */
  op_minusminus, 	/* 2 */
  op_range, 	/* 3 */
  op_range_unbounded, 	/* 4 */
  op_or, 	/* 5 */
  op_and, 	/* 6 */
  op_unary_not, 	/* 7 */
  op_much_greater, 	/* 8 */
  op_greater, 	/* 9 */
  op_greater_equal, 	/* 10 */
  op_equal, 	/* 11 */
  op_not_equal, 	/* 12 */
  op_less_equal, 	/* 13 */
  op_less, 	/* 14 */
  op_much_less, 	/* 15 */
  op_plus, 	/* 16 */
  op_minus, 	/* 17 */
  op_times, 	/* 18 */
  op_divide, 	/* 19 */
  op_int_divide, 	/* 20 */
  op_rem, 	/* 21 */
  op_power, 	/* 22 */
  op_period,	/* 23 */
  op_unary_count 	/* 24 */

} tok_op;


extern tok lex_get_token(void);
extern int lex_looking_at(tok t);
extern int lex_peeking_at_simple(void);
extern int lex_looking_at_or_onside_newline(tok t);
extern int lex_looking_at_no_newline(tok t, char no_newline);

extern int lex_looking_at_operator(tok_op op);
extern int lex_looking_at_operator_fix(char f);
extern int lex_looking_at_operator_fix_prio(char f, char prio);

/* flex(1) */

extern int yylex(void);	/* get next token */
extern char *yytext;	/* current token */
extern unsigned long yyleng;	/* length of current token */
/* there are others less useful ... */

void lex_onside(void);
void lex_offside(void);
  
extern tok_const lex_tc;

extern int lex_indent;	/* leading whitespace */
extern tok_const lex_tc;	/* subtype when tok_constant is found */
extern int lex_whitespace;	/* current token is preceeded only by whitespace */
extern tok_op lex_oper;	/* subtype set when tok_operator found */
extern char lex_oper_prio;
extern char lex_oper_assoc;	/* indicator set when tok_operator found - 'r' right associative, 'l' left acssociative, 'n'=non-associative, '? ???*/
extern char *lex_oper_fix;	/* indicator set when tok_operator found - "i"=infix, "p"=prefix, "o"=postfix also "ip" if can be prefix *or* infix */

extern void lex_do_get(char *f);

extern int debug;

#define LEX_ERROR1(string)      fprintf(stderr, string)
#define LEX_ERROR2(string, value)      fprintf(stderr, string, value)

#define LEX_DEBUG0(string)      ((debug) ? fprintf(stderr, string) : 0)
#define LEX_DEBUG(string,value)			((debug) ? fprintf(stderr, string, value) : 0)
#define LEX_DEBUG2(string,value1, value2)	((debug) ? fprintf(stderr, string, value1, value2) : 0)

/* end lex.h */
