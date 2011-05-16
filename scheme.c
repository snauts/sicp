#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#define HEAP_SIZE		0x10000 // must be multiple of two
#define NAME_HEAP		0x1000
#define MAX_SYMBOLS		0x1000
#define MAX_FUNCTIONS		0x1000
#define TOKEN_SIZE		0x100

#define NUMBER_TAG		0x0
#define POINTER_TAG		0x1
#define SYMBOL_TAG		0x2
#define PRIMITIVE_TAG		0x3

#define USED_BIT		0x8
#define CELL_COUNT		0x2

#define TAG_MASK		0x7
#define TAG_BITS		4

#define MAKE_SYMBOL(x)		(((x) << TAG_BITS) | SYMBOL_TAG)
#define MAKE_PRIMITIVE(x)	(((x) << TAG_BITS) | PRIMITIVE_TAG)
#define IS_ODD(x)		((x) & 0x10)

// special/reserved symbols
#define NIL			MAKE_SYMBOL(0)
#define BROKEN_HEART_SYMBOL	MAKE_SYMBOL(1)
#define CLOSING_BRACKET		MAKE_SYMBOL(2)
#define DOT			MAKE_SYMBOL(3)
#define LAMBDA			MAKE_SYMBOL(4)
#define COMPILED		MAKE_SYMBOL(5)
#define EMPTY			MAKE_SYMBOL(6)

// first 256 symbols are special/reserverd
static unsigned free_symbol = 256;

static unsigned heap[HEAP_SIZE];

static int mp = 0; // memory pointer
static int memory_usage = 0;
static unsigned global_env;
static int loop = 1;

static unsigned *bp;

static unsigned error(const char *msg) {
    fprintf(stderr, "\nERROR -- %s\n", msg);
    return EMPTY;
}

static void display(unsigned args);
static void newline();

// --- type operations --------------------------------------------------------

static int is_pointer(unsigned x) {
    return ((x & TAG_MASK) == POINTER_TAG) && !IS_ODD(x);
}

static int is_symbol(unsigned x) {
    return (x & TAG_MASK) == SYMBOL_TAG;
}

static int is_primitive(unsigned x) {
    return (x & TAG_MASK) == PRIMITIVE_TAG;
}

static int is_number(unsigned x) {
    return (x & TAG_MASK) == NUMBER_TAG;
}

static unsigned lisp_pointer(int offset) {
    return (offset << TAG_BITS) | POINTER_TAG;
}

static unsigned lisp_number(int num) {
    return (unsigned) (num << TAG_BITS) | NUMBER_TAG;
}

static unsigned remove_tag(unsigned x) {
    return x >> TAG_BITS;
}

// --- storage allocation -----------------------------------------------------

static int in_heap(unsigned x) {
    return is_pointer(x) && remove_tag(x) < HEAP_SIZE;
}

static int ref_error(unsigned x) {    
    if (!in_heap(x)) {
	error("object passed to car/cdr is not of the correct type");
	return 1;
    }
    else {
	return 0;
    }
}

static unsigned car(unsigned x) {   
    if (!ref_error(x)) {
	return heap[remove_tag(x) + 0] & ~USED_BIT;
    }
    else {
	return x;
    }
}

static void set_car(unsigned x, unsigned val) {
    if (!ref_error(x)) {
	heap[remove_tag(x) + 0] = val | USED_BIT;
    }
}

static unsigned cdr(unsigned x) {
    if (!ref_error(x)) {
	return heap[remove_tag(x) + 1];
    }
    else {
	return x;
    }
}

static void set_cdr(unsigned x, unsigned val) {
    if (!ref_error(x)) {
	heap[remove_tag(x) + 1] = val;
    }
}

static unsigned first(unsigned x) {
    return car(x);
}

static unsigned second(unsigned x) {
    return car(cdr(x));
}

static unsigned third(unsigned x) {
    return car(cdr(cdr(x)));
}

static int is_marked(unsigned x) {
    return (x & USED_BIT);
}

static void mark(unsigned x) {
    if (in_heap(x)) {
	unsigned i = remove_tag(x);
	if (!is_marked(heap[i])) {
	    memory_usage += CELL_COUNT;
	    heap[i] |= USED_BIT;
	    mark(car(x));
	    mark(cdr(x));
	}	
    }
}

static void walk_native_stack(unsigned stack_ref) {
    unsigned *index = (unsigned *) stack_ref;
    while (index <= bp) {
	mark(*index);
	index++;
    }
}

static void flush_x86(void) {
    unsigned regs[3];
    unsigned esp;
    __asm("movl %%ebx, %0" : "=m" (regs[0]));
    __asm("movl %%esi, %0" : "=m" (regs[1]));
    __asm("movl %%edi, %0" : "=m" (regs[2]));

    __asm("movl %%esp, %0" : "=m" (esp));
    walk_native_stack(esp);
}

static void unmark_all(void) {
    int i;
    memory_usage = 0;
    for (i = 0; i < HEAP_SIZE; i += 2) {
	heap[i] &= ~USED_BIT;	
    }
}

static void do_garbage_collection(void) {
    unmark_all();
    flush_x86();
    mark(global_env);
}

static unsigned get_new_cell(void) {
    if (memory_usage >= HEAP_SIZE) {	
	do_garbage_collection();
	if (memory_usage >= HEAP_SIZE) {
	    error("out of memory");
	    exit(0);
	}
    }
    while (heap[mp] & USED_BIT) {
	mp = (mp + CELL_COUNT) % HEAP_SIZE;
    }
    unsigned ptr = lisp_pointer(mp);
    mp = (mp + CELL_COUNT) % HEAP_SIZE;    
    memory_usage += CELL_COUNT;
    return ptr;
}

static unsigned cons(unsigned a, unsigned b) {
    unsigned new_cell = get_new_cell();
    set_car(new_cell, a);
    set_cdr(new_cell, b);
    return new_cell;
}

// --- symbol manipulation ----------------------------------------------------

static int symbol_index = 0;
static struct symbol_dsc {
    unsigned symbol;
    const char *print_name;
} obarray[MAX_SYMBOLS];

static unsigned free_name_heap = 0;
static char name_heap[NAME_HEAP];

static unsigned find_symbol(const char *print_name) {
    int i;
    for (i = 0; i < symbol_index; i++) {
	if (strcmp(print_name, obarray[i].print_name) == 0) {
	    return obarray[i].symbol;
	}
    }
    return NIL;
}

static void copy_symbol_name(const char *print_name) {
    if (NAME_HEAP - free_name_heap > strlen(print_name) + 1) {
	strcpy(name_heap + free_name_heap, print_name);
	obarray[symbol_index].print_name = name_heap + free_name_heap;
	free_name_heap += strlen(print_name) + 1;
    }
    else {
	error("name heap exhausted");
	exit(0);
    }
}

static unsigned get_symbol(const char *print_name) {
    unsigned symbol = find_symbol(print_name);
    if (symbol == NIL) {
	if (symbol_index < MAX_SYMBOLS) {
	    obarray[symbol_index].symbol = MAKE_SYMBOL(free_symbol);
	    symbol = obarray[symbol_index].symbol;
	    copy_symbol_name(print_name);
	    symbol_index++;
	    free_symbol++;
	}
	else {
	    error("reached max symbol amount");
	    exit(0);
	}	
    }
    return symbol;
}

static const char *symbol_name(unsigned x) {    
    int i;
    if (is_symbol(x)) {
	for (i = 0; i < symbol_index; i++) {
	    if (obarray[i].symbol == x) {
		return obarray[i].print_name;
	    }
	}
    }
    error("could not find symbol name");
    return "<symbol-error>";
}

static unsigned true;
static unsigned false;
static unsigned quote;
static unsigned begin;
static unsigned lambda;
static unsigned if_sym;
static unsigned else_sym;
static unsigned cond;
static unsigned let;
static unsigned def;
static unsigned set;
static unsigned ok;

static void init_symbols(void) {
    ok = get_symbol("ok");
    let = get_symbol("let");
    cond = get_symbol("cond");
    set = get_symbol("set!");
    true = get_symbol("true");
    false = get_symbol("false");
    lambda = get_symbol("lambda");
    begin = get_symbol("begin");
    quote = get_symbol("quote");
    if_sym = get_symbol("if");
    def = get_symbol("define");
    else_sym = get_symbol("else");
}

// --- reader -----------------------------------------------------------------

static unsigned input(int in_list);

static int is_newline(char c) {
    return c == '\n';
}

static int is_whitespace(char c) {
    return c == ' ' || c == '\t' || c == '\r' || is_newline(c);
}

static int is_special_symbol(char c) {
    return c == '(' || c == ')' || c == '.' || c == '\'' || c == ';';
}

static char get_char(void) {
    int c = getchar();
    if (c == EOF) {
	printf("\n");
	exit(0);
    }
    return c;
}

static char token_buf[TOKEN_SIZE];
static char get_token() {
    static char c = 0;
    if (!c) c = get_char();
    while (is_whitespace(c)) {
	c = get_char();
    }
    if (is_special_symbol(c)) {
	char ret = c;
	c = 0;
	return ret;
    }
    else {
	int i = 0;
	int is_number = 1;
	do {
	    if (i >= TOKEN_SIZE) {
		error("too large token");
		exit(0);
	    }
	    token_buf[i] = c;
	    if (!isdigit(c)) is_number = 0;
	    c = get_char();	    
	    i++;
	} while (!is_whitespace(c) && !is_special_symbol(c));
	token_buf[i] = 0;
	return is_number ? 'n' : 's';
    }    
}

static unsigned handle_opening_bracket(void) {
    unsigned first = input(0);
    if (first == CLOSING_BRACKET) {
	return NIL;
    }
    else {
	return cons(first, input(1));
    }
}

static char *downcase(char *buf) {
    int i = 0;
    while (buf[i]) {
	buf[i] = tolower(buf[i]);
	i++;
    }
    return buf;
}

static unsigned input_token(int in_list) {    
    switch (get_token()) {
    case '\'': 
	return cons(quote, cons(input(0), NIL));
    case '(':
	return handle_opening_bracket();
    case ')':
	return in_list ? NIL : CLOSING_BRACKET;
    case '.':	
	return DOT;
    case ';':
	while (!is_newline(get_char())) { }
	return input(in_list);
    case 'n':
	return lisp_number(atoi(token_buf));	
    case 's':	
	return get_symbol(downcase(token_buf));
    default:
	return error("input error");
    }
}

static unsigned input(int in_list) {
    unsigned val = input_token(in_list);
    if (val == DOT) {
	if (in_list) {
	    val = input(0);
	    if (input(0) == CLOSING_BRACKET) {
		return val;
	    }
	}
	return error("ill formed dotted list");
    }
    return (in_list && val != NIL) ? cons(val, input(in_list)) : val;
}

static unsigned read(void) {
    unsigned val = input(0);
    if (val == CLOSING_BRACKET) {
	return error("unbalanced parentheses");
    }
    else {
	return val;
    }
}

static unsigned read_primitive(unsigned args) {
    args = NIL;
    return read();
}

// --- printer ----------------------------------------------------------------

static void output(unsigned x, int in_list) {
    if (is_number(x)) {
	printf("%i", remove_tag(x));
    }
    else if (is_primitive(x)) {
	printf("<primitive %u>", remove_tag(x));
    }
    else if (x == EMPTY) {
	// do nothing
    }
    else if (is_symbol(x) && x != NIL) {
	printf("%s", symbol_name(x));
    }
    else if (x == NIL) {
	printf("()");
    }
    else if (car(x) == LAMBDA) {
	printf("<lambda>");
    }
    else {
	unsigned rest = cdr(x);
	if (!in_list) printf("(");
	output(car(x), 0);	
	if (rest != NIL) {
	    if (is_pointer(rest)) {
		printf(" ");
		output(rest, 1);
	    }
	    else {
		printf(" . ");
		output(rest, 0);
		printf(")");
	    }
	}
	else {
	    printf(")");
	}
    }
}

static void display(unsigned args) {
    output(args, 0);
}

static unsigned display_primitive(unsigned args) {
    display(first(args));
    return EMPTY;
}

static void newline() {
    printf("\n");
}

static unsigned newline_primitive(unsigned args) {
    newline();
    return args;
}

// --- primitives -------------------------------------------------------------

static unsigned add(unsigned args) {
    return lisp_number(remove_tag(first(args)) + remove_tag(second(args)));
}

static unsigned sub(unsigned args) {
    return lisp_number(remove_tag(first(args)) - remove_tag(second(args)));
}

static unsigned mul(unsigned args) {
    return lisp_number(remove_tag(first(args)) * remove_tag(second(args)));
}

static unsigned divide(unsigned args) {
    unsigned a = first(args);
    unsigned b = second(args);
    if (remove_tag(b) == 0) {
	return error("divide by zero");
    }
    else {
	return lisp_number(remove_tag(a) / remove_tag(b));
    }
}

static unsigned bool(int p) {
    if (p) {
	return true;
    }
    else {
	return false;
    }    
}

static unsigned is_null(unsigned args) {
    return bool(first(args) == NIL);
}

static unsigned is_pair(unsigned args) {
    return bool(is_pointer(first(args)));
}

static unsigned eq(unsigned args) {
    return bool(first(args) == second(args));
}

static unsigned less(unsigned args) {
    return bool(remove_tag(first(args)) < remove_tag(second(args)));
}

static unsigned more(unsigned args) {
    return bool(remove_tag(first(args)) > remove_tag(second(args)));
}

static unsigned is_even(unsigned args) {
    return bool(remove_tag(first(args)) % 2 == 0);
}

static unsigned is_true(unsigned args) {
    return bool(first(args) != false);
}

static unsigned primitive_not(unsigned args) {
    return bool(first(args) == false);
}

static unsigned is_number_primitive(unsigned args) {
    return bool(is_number(first(args)));
}

static unsigned is_symbol_primitive(unsigned args) {
    return bool(is_symbol(first(args)));
}

static unsigned make_pair(unsigned args) {
    set_cdr(args, second(args));
    return args;
}

static unsigned pair_car(unsigned args) {
    return car(first(args));
}

static unsigned pair_cdr(unsigned args) {
    return cdr(first(args));
}

static unsigned pair_set_car(unsigned args) {
    set_car(first(args), second(args));
    return ok;
}

static unsigned pair_set_cdr(unsigned args) {
    set_cdr(first(args), second(args));
    return ok;
}

static unsigned apply_fn(unsigned fn, unsigned args);
static unsigned primitive_apply(unsigned args) {
    return apply_fn(first(args), second(args));
}

static unsigned quit(unsigned args) {
    loop = 0;
    return args;
}

unsigned primitive_index = 0;
unsigned(*primitive_table[MAX_FUNCTIONS])(unsigned);

static unsigned assoc(unsigned key, unsigned a_list) {
    if (a_list == NIL) {
	return false;
    }
    else {
	if (car(car(a_list)) == key) {
	    return car(a_list);
	}
	else {
	    return assoc(key, cdr(a_list));
	}
    }
}

static unsigned map(unsigned(*fn)(unsigned), unsigned list) {
    if (list == NIL) {
	return NIL;
    }
    else {
	return cons(fn(car(list)), map(fn, cdr(list)));
    }
}

static void define_variable(unsigned var, unsigned val, unsigned env) {
    unsigned lookup = assoc(var, car(env));
    if (lookup == false) {
	set_car(env, cons(cons(var, val), car(env)));
    }
    else {
	set_cdr(lookup, val);
    }
}

static unsigned zipper(unsigned a, unsigned b) {
    if (a == NIL || b == NIL) {
	return NIL;
    }
    else if (!is_pointer(a)) {
	return cons(cons(a, b), NIL);
    }
    else {
	return cons(cons(car(a), car(b)), zipper(cdr(a), cdr(b)));
    }
}

static unsigned add_new_frame(unsigned parms, unsigned args, unsigned env) {
    return cons(zipper(parms, args), env);
}

static void set_variable(unsigned var, unsigned val, unsigned env) {
    if (env == NIL) {
	error("unbound variable");
    }
    else {
	unsigned lookup = assoc(var, car(env));
	if (lookup == false) {
	    set_variable(var, val, cdr(env));
	}
	else {
	    set_cdr(lookup, val);
	}
    }
}

static unsigned lookup_variable(unsigned var, unsigned env) {
    if (env == NIL) {
	return error("variable lookup failed");
    }
    else {
	unsigned lookup = assoc(var, car(env));
	if (lookup == false) {
	    return lookup_variable(var, cdr(env));
	}
	else {
	    return cdr(lookup);
	}
    }
}

static unsigned install_primitive(unsigned sym, unsigned(*fn)(unsigned)) {
    if (primitive_index >= MAX_FUNCTIONS) {
	return error("maximum primitive count reached");
    }

    unsigned obj = MAKE_PRIMITIVE(primitive_index);
    primitive_table[primitive_index] = fn;
    define_variable(sym, obj, global_env);
    primitive_index++;
    return obj;
}

static unsigned list4(unsigned a, unsigned b, unsigned c, unsigned d) {
    return cons(a, cons(b, cons(c, cons(d, NIL))));
}

static unsigned install_compiled(unsigned sym, 
				 unsigned env,
				 unsigned parameters,
				 unsigned proc) {
    unsigned obj = list4(COMPILED, proc, parameters, env);
    define_variable(sym, obj, env);
    return obj;
}

static void init_global_env(void) {
    global_env = cons(NIL, NIL);
    install_primitive(get_symbol("+"), &add);
    install_primitive(get_symbol("-"), &sub);
    install_primitive(get_symbol("*"), &mul);
    install_primitive(get_symbol("/"), &divide);
    install_primitive(get_symbol("="), &eq);
    install_primitive(get_symbol("car"), &pair_car);
    install_primitive(get_symbol("cdr"), &pair_cdr);
    install_primitive(get_symbol("cons"), &make_pair);
    install_primitive(get_symbol("null?"), &is_null);
    install_primitive(get_symbol("pair?"), &is_pair);
    install_primitive(get_symbol("eq?"), &eq);
    install_primitive(get_symbol("<"), &less);
    install_primitive(get_symbol(">"), &more);
    install_primitive(get_symbol("even?"), &is_even);
    install_primitive(get_symbol("quit"), &quit);
    install_primitive(get_symbol("read"), &read_primitive);
    install_primitive(get_symbol("newline"), &newline_primitive);
    install_primitive(get_symbol("display"), &display_primitive);
    install_primitive(get_symbol("set-car!"), &pair_set_car);
    install_primitive(get_symbol("set-cdr!"), &pair_set_cdr);
    install_primitive(get_symbol("number?"), &is_number_primitive);
    install_primitive(get_symbol("symbol?"), &is_symbol_primitive);
    install_primitive(get_symbol("apply"), &primitive_apply);
    install_primitive(get_symbol("true?"), &is_true);
    install_primitive(get_symbol("not"), &primitive_not);
    define_variable(true, true, global_env);
    define_variable(false, false, global_env);
}

// --- evaluator --------------------------------------------------------------

static unsigned eval(unsigned exp, unsigned env);

static int tagged(unsigned exp, unsigned symbol) {
    return car(exp) == symbol; 
}

static int is_self_evaluating(unsigned exp) {
    return is_number(exp) || exp == NIL || exp == true || exp == false;
}

static unsigned eval_if(unsigned exp, unsigned env) {
    unsigned test = car(exp);
    unsigned consequent = second(exp);
    unsigned alternative = third(exp);
    return eval(eval(test, env) == false ? alternative : consequent, env);
}

static unsigned apply_primitive(unsigned fn, unsigned args) {
    return primitive_table[remove_tag(fn)](args);
}

static unsigned eval_args(unsigned args, unsigned env) {
    if (args == NIL) {
	return args;
    }
    else {
	return cons(eval(car(args), env), eval_args(cdr(args), env));
    }
}

static unsigned eval_assign(unsigned exp, unsigned env) {
    if (is_symbol(first(exp))) {
	set_variable(first(exp), eval(second(exp), env), env);
    }
    else {
	return error("malformed set!");
    }
    return ok;
}

static unsigned eval_lambda(unsigned exp, unsigned env) {
    return cons(LAMBDA, cons(exp, env));
}

static unsigned eval_define(unsigned exp, unsigned env) {
    if (is_symbol(first(exp))) {
	define_variable(first(exp), eval(second(exp), env), env);
    }
    else if (is_pointer(first(exp))) {
	unsigned lambda = eval_lambda(cons(cdr(car(exp)), cdr(exp)), env);
	define_variable(car(car(exp)), lambda, env);
    }
    else {
	return error("malformed define");
    }
    return ok;
}

static unsigned eval_sequence(unsigned list, unsigned env) {
    if (list == NIL) {
	return error("ill formed special form");
    }
    else {
	if (cdr(list) == NIL) {
	    return eval(car(list), env);
	}
	else {
	    eval(car(list), env);
	    return eval_sequence(cdr(list), env);
	}
    }
}

static unsigned eval_let(unsigned exp, unsigned env) {
    unsigned proc = cons(lambda, cons(map(&car, car(exp)), cdr(exp)));
    unsigned form = cons(proc, map(&second, car(exp)));
    return eval(form, env);
}

static unsigned list3(unsigned a, unsigned b, unsigned c) {
    return cons(a, cons(b, cons(c, NIL)));
}

static unsigned expand_clauses(unsigned exp);

static unsigned expand_predicate_clause(unsigned first, unsigned rest) {
    unsigned body = list4(if_sym, ok, ok, expand_clauses(rest));
    return cons(list3(lambda, cons(ok, NIL), body), first);
}

static unsigned expand_one_clause(unsigned first, unsigned rest) {
    unsigned consequent = cons(begin, cdr(first));
    return list4(if_sym, car(first), consequent, expand_clauses(rest));
}

static unsigned expand_clauses(unsigned exp) {
    if (exp == NIL) {
	return false;
    }
    else {
	unsigned first = car(exp);
	unsigned rest = cdr(exp);
	if (car(first) == else_sym) {
	    if (rest == NIL) {
		return cons(begin, cdr(first));
	    }
	    else {
		return error("else clause isn't last");
	    }
	}
	else {
	    if(cdr(first) == NIL) {
		return expand_predicate_clause(first, rest);
	    }
	    else {
		return expand_one_clause(first, rest);
	    }
	}
    }
}

static unsigned eval_cond(unsigned exp, unsigned env) {
    return eval(expand_clauses(exp), env);
}

static unsigned apply_lambda(unsigned fn, unsigned args) {
    unsigned new_env = add_new_frame(car(car(fn)), args, cdr(fn));
    return eval_sequence(cdr(car(fn)), new_env);
}

static unsigned apply_compiled(unsigned fn, unsigned args) {
    unsigned new_env = add_new_frame(second(fn), args, third(fn));
    return primitive_table[remove_tag(first(fn))](new_env);
}

static unsigned apply_fn(unsigned fn, unsigned args) {
    if (is_primitive(fn)) {
	return apply_primitive(fn, args);
    }
    if (tagged(fn, COMPILED)) {
	return apply_compiled(cdr(fn), args);
    }
    if (tagged(fn, LAMBDA)) {
	return apply_lambda(cdr(fn), args);
    }
    return error("unknown procedure");
}

static unsigned apply(unsigned exp, unsigned env) {
    return apply_fn(eval(car(exp), env), eval_args(cdr(exp), env));
}

static unsigned eval(unsigned exp, unsigned env) {
    if (is_self_evaluating(exp)) {
	return exp;
    } else if(is_symbol(exp)) {
	return lookup_variable(exp, env);
    } else if (tagged(exp, quote)) {
	return second(exp);
    } else if (tagged(exp, def)) {
	return eval_define(cdr(exp), env);
    } else if (tagged(exp, set)) {
	return eval_assign(cdr(exp), env);
    } else if (tagged(exp, begin)) {
	return eval_sequence(cdr(exp), env);
    } else if (tagged(exp, let)) {
	return eval_let(cdr(exp), env);
    } else if (tagged(exp, cond)) {
	return eval_cond(cdr(exp), env);
    } else if (tagged(exp, lambda)) {
	return eval_lambda(cdr(exp), env);
    } else if (tagged(exp, if_sym)) {
	return eval_if(cdr(exp), env);
    } else if (is_pointer(exp)) {
	return apply(exp, env);
    } else {
	return error("unknown expression type");
    }
}

static void read_eval_loop(void) {
    while (loop) {
	printf("> ");
	display(eval(read(), global_env));
	newline();
    }
}

static unsigned done(void) {
    return ok;
}

#include "scheme.h"

int main() {
    unsigned stack_ref;
    bp = &stack_ref;
    unmark_all();
    init_symbols();
    init_global_env();
    init_lisp_symbols();
    printf("compiled: ");
    display(lisp_main(global_env));
    newline();
    read_eval_loop();
    newline();
    return 0;
}
