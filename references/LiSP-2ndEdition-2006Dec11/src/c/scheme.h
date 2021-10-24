/* $Id: scheme.h,v 4.0 1995/07/10 06:52:30 queinnec Exp $ */

/* (((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
 * This file is part of the files that accompany the book:
 *     LISP Implantation Semantique Programmation (InterEditions, France)
 * By Christian Queinnec <Christian.Queinnec@INRIA.fr>
 * Newest version may be retrieved from:
 *   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
 * Check the README file before using this file.
 *(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
 */

/* The scheme.h header associated to the chap10[ek].scm compilers.
 */

/* Prevent double inclusion 
 */

#ifndef SCHEME_H
#define SCHEME_H

/* Include all other needed header file so the compiler can only cite
 * scheme.h and nothing else.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <setjmp.h>

/* Machine dependences
 * This macro knows how the stack grows. Generally on Unix, stack grows
 * downward. So A is in the dynamic extent of B if address(A) <= address(B).
 * This is used in scheme.c to decide if a throw to a catch is legal.
 * If your stack grows the other way, just redefine the macro to be >=.
 * You can test that very easily with a little C program.
 */

#define SCM_STACK_HIGHER <=
 
/* No other (known) machine dependences.
 */


/* Error codes.
 * All errors are uniformly signalled. The line number appears as well to
 * ease finding additional informations on the error. 
 */

#define SCM_error(code) SCM_signal_error(code,__LINE__,__FILE__)

#define SCM_ERR_PLUS		70
#define SCM_ERR_MINUS		71
#define SCM_ERR_TIMES		72
#define SCM_ERR_QUOTIENT	73
#define SCM_ERR_REMAINDER	74
#define SCM_ERR_GTP		75
#define SCM_ERR_LTP		76
#define SCM_ERR_EQNP		77
#define SCM_ERR_GEP		78
#define SCM_ERR_LEP		79

#define SCM_ERR_CANNOT_APPLY	50
#define SCM_ERR_WRONG_ARITY	51
#define SCM_ERR_INTERNAL	52
#define SCM_ERR_MISSING_ARGS	53
#define SCM_ERR_OUT_OF_EXTENT	59

#define SCM_ERR_CAR		60
#define SCM_ERR_CDR		61
#define SCM_ERR_SET_CAR		62
#define SCM_ERR_SET_CDR		63
#define SCM_ERR_APPLY_ARG	40
#define SCM_ERR_APPLY_SIZE	41

#define SCM_ERR_UNINITIALIZED	11
#define SCM_ERR_CANT_ALLOC	100

/* General notations:
 * All defined entities start with the SCM prefix.
 * Macros have Capitalized names and no underscore between multiple words.
 * Functions have lowercase names (and possibly underscores).
 */

/* Macros used to make generated C look less uglier.
 *
 * SCM_CheckedGlobal(variable) extracts the value of a global variable
 *       while checking that it is initialized.
 * SCM_Free(variable) extracts the value of a free variable from a closure.
 * SCM_Content(box) extracts the content of a box.
 * 
 * Functions are declared with SCM_DeclareFunction(variable) while
 * variables are mentioned with SCM_DeclareLocalVariable or
 * SCM_DeclareLocalDottedVariable along with their rank. If a function does
 * not enclose anything then it is declared with SCM_DeclareCombinator.
 * Many of the previous macros use the names self_, size_ and arguments_;
 * since these names cannot be generated they cannot clash with other Scheme
 * names translated to C.
 * 
 * A combination may use SCM_invokeN to improve reading. They are actually
 * trivially defined but could be associated to specialized invokers.
 */

#define SCM_CheckedGlobal(Cname) \
  ((Cname != SCM_undefined)? Cname : SCM_error(SCM_ERR_UNINITIALIZED))

#define SCM_DeclareFunction(Cname) \
  SCM Cname (struct Cname *self_, unsigned long size_, va_list arguments_)

/* Only used by chap10i.scm (a variant of chap10e.scm). */
#define SCM_DeclareCombinator(Cname) \
  SCM Cname (struct SCM_closure *self_, unsigned long size_, va_list arguments_)

#define SCM_Free(Cname) ((*self_).Cname)

#define SCM_Content(e) ((e)->box.content)

#define SCM_DeclareLocalVariable(Cname,rank) \
  SCM Cname = va_arg(arguments_,SCM)

#define SCM_DeclareLocalDottedVariable(Cname,rank) \
  SCM Cname = SCM_list(size_-rank,arguments_)

#define SCM_invoke0(f)       SCM_invoke(f,0)
#define SCM_invoke1(f,x)     SCM_invoke(f,1,x)
#define SCM_invoke2(f,x,y)   SCM_invoke(f,2,x,y)
#define SCM_invoke3(f,x,y,z) SCM_invoke(f,3,x,y,z)

/* This macro is used to handle Cfunction. It is necessary to cast everything
   to this type before storing them into data structures then one must cast
   back Cfunction to the appropriate type. [Thanks to Christian.Jullien]
 */

#define SCM_CfunctionAddress(Cfunction) \
    ((SCM (*)(void)) Cfunction)

/* Fixnums are encoded with a low bit set to one.  A value can be
 * checked whether it is a fixed number: attention, the result is a C
 * boolean not a Scheme one. If it is a fixnum then it can be
 * converted into a C integer back and forth but no overflow detection
 * is done! 
 */

#define SCM_FixnumP(x)    ((unsigned long)(x) & (unsigned long)1)
#define SCM_Fixnum2int(x) ((long)(x)>>1)
#define SCM_Int2fixnum(i) ((SCM)(((i)<<1) | 1))

/* Scheme values are received through a SCM pointer. If this pointer has
 * a lower bit set to one then it is a fixnum that can be interpreted with
 * the previous macros. If the lower bit is a zero (as for a real C pointer)
 * then it is a pointer to a wrapped object ie a pointer that designates the 
 * first interesting field of an object right after its type. The type or
 * tag can be obtained with SCM_2tag, two coercers allow to transform a
 * reference to a wrapped object into a reference to an unwrapped object
 * and back. We use pointer arithmetic for that; note that it is a static
 * translation. 
 *
 * SCM refers to wrapped objects or fixnums while SCMref refers to 
 * unwrapped objects ie to a memory zone starting with a tag. Users should
 * probably only use SCM types.
 */

typedef union SCM_unwrapped_object *SCMref;
typedef union SCM_object           *SCM;

#define SCM_Wrap(x)       ((SCM) (((union SCM_header *) x) + 1))
#define SCM_Unwrap(x)     ((SCMref) (((union SCM_header *) x) - 1))
#define SCM_2tag(x)       ((SCM_Unwrap((SCM) x))->object.header.tag)

/* Macros used to define (and allocate global resources such as
 * quotations, global variables etc.)
 *
 * Define an initialized global variable. Ignores for now the string that
 * defines what was its name under Scheme. It might be used to add some
 * reflectivity or debug capability.
 */

#define SCM_DefineInitializedGlobalVariable(Cname,string,value) \
  SCM Cname = SCM_Wrap(value)

/* Define a uninitialized global variable. Ignores for now the string that
 * defines what was its name under Scheme. This information may be used
 * if incorporating debug informations.
 */

#define SCM_DefineGlobalVariable(Cname,string) \
  SCM_DefineInitializedGlobalVariable(Cname,string,&SCM_undefined_object)

/* Define a named cons cell. 
 */

#define SCM_DefinePair(pair,car,cdr) \
  static struct SCM_unwrapped_pair pair = {{SCM_PAIR_TAG}, cdr, car }

/* Define a symbol. Due to the way quotations are generated there are no
 * duplicates. Care must be taken if we were doing separate compilation.
 */

#define SCM_DefineSymbol(symbol,pname) \
  static struct SCM_unwrapped_symbol symbol = {{SCM_SYMBOL_TAG}, pname }

/* Define a string. They are implemented as C strings to ease coercions 
 * from and to C.
 */

#define SCM_DefineString(Cname,string) \
  struct Cname##_struct {           \
    union SCM_header header;        \
    char Cstring[1+sizeof(string)];};  \
  static struct Cname##_struct Cname = \
    {{SCM_STRING_TAG}, string }

/* Define the structure of a closure. It is a regular object that can be
 * cast to SCM_object. It has a behavior (a reference to a C function),
 * an arity (filled when created) and additional fields holding closed 
 * values.
 */

#define SCM_DefineClosure(struct_name,fields) \
  struct struct_name {		\
    SCM (*behavior)(void);	\
    long arity;			\
    fields }

/* Define a primitive. Its definition is written in C directly.  arity
 * is a long integer. If arity>0, it represents a fixed arity (eg:
 * arity of cons is 2); if negative it represents the opposite of the
 * number of expected arguments minus one (eg: list, which takes at
 * least 0 arguments, has -1 as arity while display, which has one
 * mandatory argument, has arity -2. Use SCM_MinimalArity to decode
 * this number without bothering how it is encoded. */

#define SCM_MinimalArity(i) (-(i)-1)

#define SCM_DefinePredefinedFunctionVariable(subr,string,arity,Cfunction) \
  static struct SCM_unwrapped_subr subr##_object = \
    {{SCM_SUBR_TAG}, SCM_CfunctionAddress(Cfunction), arity}; \
  SCM_DefineInitializedGlobalVariable(subr,string,&(subr##_object))

/* C booleans can be converted into Scheme booleans.
 */

#define SCM_2bool(i) ((i) ? SCM_true : SCM_false )

/* Macros to handle usual Scheme values.
 * Predicates end with P and return a C boolean.
 */

#define SCM_Car(x)     (SCM_Unwrap(x)->pair.car)
#define SCM_Cdr(x)     (SCM_Unwrap(x)->pair.cdr)
#define SCM_NullP(x)   ((x)==SCM_nil)
#define SCM_PairP(x)   ( (! SCM_FixnumP(x) ) && (SCM_2tag(x)==SCM_PAIR_TAG) )
#define SCM_SymbolP(x) ( (! SCM_FixnumP(x) ) && (SCM_2tag(x)==SCM_SYMBOL_TAG) )
#define SCM_StringP(x) ( (! SCM_FixnumP(x) ) && (SCM_2tag(x)==SCM_STRING_TAG) )
#define SCM_EqP(x,y)   ((x)==(y))

/* Macros to perform usual arithmetic. 
 * They can be largely improved due to the specific tagging scheme which is
 * adopted here. These macros are independent of the representations.
 */

#define SCM_Plus(x,y)						\
 ( ( SCM_FixnumP(x) && SCM_FixnumP(y) )				\
   ? SCM_Int2fixnum( SCM_Fixnum2int(x) + SCM_Fixnum2int(y) ) 	\
   : SCM_error(SCM_ERR_PLUS) )
#define SCM_Minus(x,y)						\
 ( ( SCM_FixnumP(x) && SCM_FixnumP(y) )				\
   ? SCM_Int2fixnum( SCM_Fixnum2int(x) - SCM_Fixnum2int(y) ) 	\
   : SCM_error(SCM_ERR_MINUS) )
#define SCM_Times(x,y)						\
 ( ( SCM_FixnumP(x) && SCM_FixnumP(y) )				\
   ? SCM_Int2fixnum( SCM_Fixnum2int(x) * SCM_Fixnum2int(y) ) 	\
   : SCM_error(SCM_ERR_TIMES) )
#define SCM_Quotient(x,y)					\
 ( ( SCM_FixnumP(x) && SCM_FixnumP(y) )				\
   ? SCM_Int2fixnum( SCM_Fixnum2int(x) / SCM_Fixnum2int(y) ) 	\
   : SCM_error(SCM_ERR_QUOTIENT) )
#define SCM_Remainder(x,y)					\
 ( ( SCM_FixnumP(x) && SCM_FixnumP(y) )				\
   ? SCM_Int2fixnum( SCM_Fixnum2int(x) % SCM_Fixnum2int(y) ) 	\
   : SCM_error(SCM_ERR_REMAINDER) )

#define SCM_GtP(x,y)						\
 ( ( SCM_FixnumP(x) && SCM_FixnumP(y) )				\
   ? SCM_2bool( SCM_Fixnum2int(x) > SCM_Fixnum2int(y) ) 	\
   : SCM_error(SCM_ERR_GTP) )
#define SCM_LtP(x,y)						\
 ( ( SCM_FixnumP(x) && SCM_FixnumP(y) )				\
   ? SCM_2bool( SCM_Fixnum2int(x) < SCM_Fixnum2int(y) ) 	\
   : SCM_error(SCM_ERR_LTP) )
#define SCM_GeP(x,y)						\
 ( ( SCM_FixnumP(x) && SCM_FixnumP(y) )				\
   ? SCM_2bool( SCM_Fixnum2int(x) >= SCM_Fixnum2int(y) ) 	\
   : SCM_error(SCM_ERR_GEP) )
#define SCM_LeP(x,y)						\
 ( ( SCM_FixnumP(x) && SCM_FixnumP(y) )				\
   ? SCM_2bool( SCM_Fixnum2int(x) <= SCM_Fixnum2int(y) ) 	\
   : SCM_error(SCM_ERR_LEP) )
#define SCM_EqnP(x,y)						\
 ( ( SCM_FixnumP(x) && SCM_FixnumP(y) )				\
   ? SCM_2bool( SCM_Fixnum2int(x) == SCM_Fixnum2int(y) ) 	\
   : SCM_error(SCM_ERR_EQNP) )

/* Escape structures are made of two parts. This one only appears in
 * the stack. The other part is a regular first-class value of Scheme
 * called escape and pointed by back_pointer. This escape structure is not
 * a first-class value so it does not need tags.
 */

struct SCM_jmp_buf {
  SCM back_pointer;
  jmp_buf jb;
};

/* Boxed Scheme objects.
 * They are represented by a struct whose first item is a tag specifying
 * their type. The value of the tag is specified to ease debug. To force
 * the tag to have the same length a pointer has, wrap the tag into an 
 * union.
 */

enum SCM_tag { 
  SCM_NULL_TAG		=0xaaa0,
  SCM_PAIR_TAG		=0xaaa1,
  SCM_BOOLEAN_TAG	=0xaaa2,
  SCM_UNDEFINED_TAG	=0xaaa3,
  SCM_SYMBOL_TAG	=0xaaa4,
  SCM_STRING_TAG	=0xaaa5,
  SCM_SUBR_TAG		=0xaaa6,
  SCM_CLOSURE_TAG	=0xaaa7,
  SCM_ESCAPE_TAG	=0xaaa8
};

union SCM_header { 
  enum SCM_tag tag;
  SCM ignored;
};

/* This is the definition of unwrapped object (with tag). The same definition
 * without tags also appears below and can be used directly from C users.
 *
 * NOTE 1: Closures do not require to record their size to be
 * invoked. GC will need it but this size can be stored in a shared
 * position. We don't need this here since there is no GC.
 *
 * This is what SCMref refers to:
 */

union SCM_unwrapped_object {
  struct SCM_unwrapped_immediate_object {
    union SCM_header header;
  } object;
  struct SCM_unwrapped_pair {
    union SCM_header header;
    SCM cdr;
    SCM car;
  } pair;
  struct SCM_unwrapped_string {
    union SCM_header header;
    char Cstring[8];
  } string;
  struct SCM_unwrapped_symbol {
    union SCM_header header;
    SCM pname;
  } symbol;
  struct SCM_unwrapped_subr {
    union SCM_header header;
    SCM (*behavior)(void);
    long arity;
  } subr;
  struct SCM_unwrapped_closure {
    union SCM_header header;
    SCM (*behavior)(void);
    long arity;
    SCM environment[1];
  } closure;
  struct SCM_unwrapped_escape {
    union SCM_header header;
    struct SCM_jmp_buf *stack_address;
  } escape;
};  

/* This is what SCM refers to:
 */

union SCM_object {
  struct SCM_pair {
    SCM cdr;
    SCM car;
  } pair;
  struct SCM_string {
    char Cstring[8];
  } string;
  struct SCM_symbol {
    SCM pname;
  } symbol;
  struct SCM_box {
    SCM content;
  } box;
  struct SCM_subr {
    SCM (*behavior)(void);
    long arity;
  } subr;
  struct SCM_closure {
    SCM (*behavior)(void);
    long arity;
    SCM environment[1];
  } closure;
  struct SCM_escape {
    struct SCM_jmp_buf *stack_address;
  } escape;
};  

/* The global (pseudo or not) values that are needed for the compilation.
 * These are: true, false, (), undefined, etc. Don't confuse the object
 * itself and its address. Since objects are seldom used, their name is 
 * more complex.
 */

#define SCM_DefineImmediateObject(name,tag) \
  struct SCM_unwrapped_immediate_object name = {{tag}}

extern struct SCM_unwrapped_immediate_object SCM_true_object;      /*    #t  */
extern struct SCM_unwrapped_immediate_object SCM_false_object;     /*    #f  */
extern struct SCM_unwrapped_immediate_object SCM_nil_object;       /*    ()  */
extern struct SCM_unwrapped_immediate_object SCM_undefined_object; /* #<UFO> */
#define SCM_true        SCM_Wrap(&SCM_true_object)
#define SCM_false       SCM_Wrap(&SCM_false_object)
#define SCM_nil         SCM_Wrap(&SCM_nil_object)
#define SCM_undefined   SCM_Wrap(&SCM_undefined_object)

/* General runtime functions.
 */

extern SCM SCM_invoke(SCM fun, unsigned long number, ...);
extern SCM SCM_close(SCM (*Cfunc)(void), long arity, unsigned long size, ...);
extern SCM SCM_signal_error (unsigned long code, unsigned long line, char *file);
extern SCM SCM_allocate_box (SCM v);
extern SCM SCM_allocate_continuation (struct SCM_jmp_buf *address);
extern SCM SCM_list (unsigned long count, va_list arguments);
extern SCM SCM_prin (SCM x);
extern SCM SCM_apply (unsigned long number, va_list arguments);

/* Prototypes of Scheme library functions. They are obtained with an
 * utility of chap10h.scm (generate-declarations g.init (current-output-port))
 */

#define SCM_DeclareConstant(var) extern SCM var
#define SCM_DeclareSubr0(var,Cname) \
  SCM_DeclareConstant(var); extern SCM Cname (void)
#define SCM_DeclareSubr1(var,Cname) \
  SCM_DeclareConstant(var); extern SCM Cname (SCM x) 
#define SCM_DeclareSubr2(var,Cname) \
  SCM_DeclareConstant(var); extern SCM Cname (SCM x, SCM y) 
#define SCM_DeclareSubr3(var,Cname) \
  SCM_DeclareConstant(var); extern SCM Cname (SCM x, SCM y, SCM z)

SCM_DeclareSubr1(CALLEP,SCM_callep);
SCM_DeclareSubr2(GREATERP,SCM_gtp);
SCM_DeclareSubr2(LESSP,SCM_ltp);
SCM_DeclareSubr2(EQN,SCM_eqnp);
SCM_DeclareSubr2(NOT_LESSP,SCM_gep);
SCM_DeclareSubr2(NOT_GREATERP,SCM_lep);
SCM_DeclareSubr2(REMAINDER,SCM_remainder);
SCM_DeclareSubr2(QUOTIENT,SCM_quotient);
SCM_DeclareSubr2(TIMES,SCM_times);
SCM_DeclareSubr2(DIFFERENCE,SCM_minus);
SCM_DeclareSubr2(PLUS,SCM_plus);
SCM_DeclareSubr1(INTEGERp,SCM_fixnump);
SCM_DeclareSubr2(EQ,SCM_eqp);
SCM_DeclareSubr1(STRINGP,SCM_stringp);
SCM_DeclareSubr1(SYMBOLP,SCM_symbolp);
SCM_DeclareSubr1(NULLp,SCM_nullp);
SCM_DeclareSubr1(CONSP,SCM_pairp);
SCM_DeclareSubr2(RPLACD,SCM_set_cdr);
SCM_DeclareSubr2(RPLACA,SCM_set_car);
SCM_DeclareSubr1(CDR,SCM_cdr);
SCM_DeclareSubr1(CAR,SCM_car);
SCM_DeclareSubr2(CONS,SCM_cons);

/* Some additional functions or constants needed for tests. Apply,
 * list and call/cc depends on the type of library (CPS-style or not).
 */

SCM_DeclareConstant(T);
SCM_DeclareConstant(F);
SCM_DeclareConstant(NIL);

SCM_DeclareSubr1(PRINT,SCM_print);
SCM_DeclareConstant(APPLY);
SCM_DeclareConstant(LIST);
SCM_DeclareConstant(CALLCC);

#endif		/* SCHEME_H */

/* end of scheme.h */

