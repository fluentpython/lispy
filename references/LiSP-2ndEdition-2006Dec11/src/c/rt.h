/* $Id: rt.h,v 4.0 1995/07/10 06:52:27 queinnec Exp $ */

/* (((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
 * This file is part of the files that accompany the book:
 *     LISP Implantation Semantique Programmation (InterEditions, France)
 * By Christian Queinnec <Christian.Queinnec@INRIA.fr>
 * Newest version may be retrieved from:
 *   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
 * Check the README file before using this file.
 *(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
 */

/* The C header describing the run-time of chap6f 
 * Scheme objects representation. 
 * All objects are boxed for simplicity.			        */

/* Prevent double inclusion */
#ifndef RTLIB_SCHEME_H
#define RTLIB_SCHEME_H

/* TEMP pour l'instant                        TEMP */
#define NO_GC

/* Compilation products only need to include this file which in turn
 * include what is needed. This simplifies code generation.		*/
#include <stdio.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <setjmp.h>
#include <stdlib.h>

/***********************************************************************
 * General machine dependencies:
 * SonyNews ignores size_t so we redefine some primitives that return
 * size_t values. We thus define and use SIZE_T for all natural numbers
 * that may be used to compute on addresses.				*/
#ifdef sony_news
typedef unsigned int SIZE_T ;
#else
typedef size_t SIZE_T ;
#endif

#define MALLOC(x) ((void*) malloc((SIZE_T) x))
#define SIZEOF(x) ((SIZE_T) sizeof(x))
#define STRLEN(x) ((SIZE_T) strlen(x))

/* this macro tries to concatenate two tokens into a single one.
 * The first definition only works in ANSI-C (and gcc)
 * The second works on most of other cc.				*/
#ifdef __STDC__
#define SCM_CONC(name1,name2) name1##name2
#else
#define SCM_CONC(name1,name2) name1/**/name2 
#endif

/* This macro computes the address which separates two ordered addresses.
 * Better that the result is word-aligned since it will be used as the
 * starting address of a heap.						*/
#define SCM_mid_address(start,end) \
  ((char*) start + (((((char*) end) - ((char*) start))/2) & ~3))

/***********************************************************************
 * The library is compiled with respect to the following constants.
 * If you want to change them you have to recompile the library for them
 * to take effect.							*/

/* This constant controls whether some dynamic checks are performed or
 * not. For example whether the last argument of apply is a real list.	
 *                  1 means true, 0 means false.	
 * If you set it to false, the associated tests will become dead code.
 * Usual compilers are able to remove it automatically.			*/
#define SCM_CAREFUL 1 

/* This constant defines the characters that are predefined and prestored
 * in an array. 256 corresponds to ISO_Latin1 code size and 128 is also
 * possible for old Ascii coding. Smaller values are not advised.
 * NOTE: This constant must be a power of 2 (see SCM_charcodep()).	*/
#define SCM_CHAR_SIZE 256

/* This constant defines the greatest small fixnum which is preallocated.
 * When a fixnum must be allocated, the preallocated corresponding one
 * is returned if it exists. The gain is considerable even for (fib 20).
 * You can set this number to zero if you do not want preallocated
 * fixnums. 								*/
#define SCM_SMALL_FIXNUM_LIMIT 1024

/* This constant defines the maximal number of arguments a function 
 * called by apply can take. Actually the interface for functions with
 * large number of variables allows any number of arguments. Due to the
 * actual implementation of apply, there is a limit which might be 
 * removed in future releases. 
 * 256 is a good choice but you can enlarge it if you consider having
 * functions with more than 256 actual arguments.			*/
#define SCM_MAX_APPLY_ARGUMENTS 256

/* This constant defines the unit (in bytes) of the size of the zones
 * that will be allocated. 						*/
#define SCM_ZONE_SIZE 1024

/* This constant is the ratio of the size of a space to the occupied 
 * size. Appel says that 8 is correct ie a stop-and-copy works well
 * when there is 8 more space than the total space used.		*/
#define SCM_GC_EXTENSION_THRESHOLD 8

/***********************************************************************
 * All external names defined by this header file are prefixed by SCM.
 * The C functions that will become the behavior of Scheme functions
 * ie have the correct signature, have a second capitalized name. For
 * example SCM_Cons. Predicates have a P suffix like SCM_EqP.
 * There is a lot of macros or small functions like SCM_cons which 
 * implements the effect associated to their name. They exist for internal
 * use and are generally unsafe. Their name (apart the SCM_ prefix) only
 * contains small letters. Scheme global variables are in uppercase
 * and prefixed with SCM_V_. Legal characters of Scheme that are illegal 
 * for C are mapped on legal C characters. For instance, pair? becomes
 * SCM_V_PAIRP while symbol->string becomes SCM_V_SYMBOL2STRING.

/***********************************************************************
 * SCM_object (see below) is the type of all Scheme objects
 * SCM_reference is the type of references onto these objects.
 * Usually regular functions just use references everywhere so
 * SCM_reference is aliased to SCM as a shorthand which allows one
 * to believe that only objects are handled.				*/
   
typedef union SCM_object *SCM_reference ;
typedef SCM_reference    SCM	        ;

/***********************************************************************
 * All allocated objects have a header containing a tag from which 
 * their type can be derived. This tag is an unsigned int rather than
 * an enum type since it might be extended dynamically.			*/

typedef unsigned int SCM_TAG ;

#define SCM_OBJECT_TAG		9       /* Never zero ! */
#define SCM_CLASS_TAG		1
#define SCM_GENERIC_TAG		2
#define SCM_FIELD_TAG		3
#define SCM_MONO_FIELD_TAG	4
#define SCM_POLY_FIELD_TAG	5
#define SCM_ANOMALY_TAG		6
#define SCM_MODULE_TAG		7
#define SCM_ROOT_TAG		8
#define SCM_PAIR_TAG		10	/* for pairs			*/
#define SCM_SYMBOL_TAG		11	/* for symbols			*/
#define SCM_EMPTY_TAG		12	/* ()				*/
#define SCM_CLOSURE_TAG		13	/* for closures			*/
#define SCM_PRIMITIVE_TAG	14	/* for primitives 		*/
#define SCM_FIXNUM_TAG		15	/* for small integers 		*/
#define SCM_UNDETERMINED_TAG	16	/* The UFO			*/
#define SCM_TRUE_TAG		17	/* #t				*/
#define SCM_FALSE_TAG		18	/* #f				*/
#define SCM_EOF_TAG		19	/* The end of file		*/
#define SCM_BOX_TAG		20	/* for boxes (mutable variables)*/
#define SCM_STRING_TAG		21	/* for strings			*/
#define SCM_CHAR_TAG		22	/* for characters		*/
#define SCM_PORT_TAG		23	/* for ports (streams)		*/
#define SCM_FLOATNUM_TAG	24	/* for float (timings !:)	*/
#define SCM_STACK_SLICE_TAG	25	/* for stack_slice		*/
#define SCM_CONTINUATION_TAG	26	/* for continuations		*/
#define SCM_PROCESS_TAG		27	/* for processes		*/

#define SCM_FRAME_TAG           30      /* for environment frames       */
#define SCM_FUNCTION_TAG        31      /* for functions                */
#define SCM_NULLENV_TAG         32      /* for end of environments      */
#define SCM_DYNENV_TAG          33      /* for dynamic environment      */

/***********************************************************************
  Scheme objects							*/

typedef struct { 
  SCM_TAG tag ;
  SCM link ; /* for a stop and copy GC */
  } SCM_object_header ;

union SCM_object {
  struct SCM_immediate_object {
    SCM_object_header header ;
  } object ;
  struct SCM_fixnum {
    SCM_object_header header ;
    int value ;
  } fixnum ;
  struct SCM_floatnum {
    SCM_object_header header ;
    float value ;
  } floatnum ;
  struct SCM_pair {
    SCM_object_header header ;
    SCM cdr ;
    SCM car ;
  } pair ;
  struct SCM_box {
    SCM_object_header header ;
    SCM content ;
  } box ;
  struct SCM_primitive {
    SCM_object_header header ;
    /* A pointer to a function that returns a Scheme value */
    SCM (*behavior)() ; 
    int arity ;
    SCM name ;           /* for debug purposes, primitives have names */
  } primitive ;
  struct SCM_closure {
    SCM_object_header header ;
    SCM (*behavior)() ;
    int arity ;
    SIZE_T size ;
    SCM environment[1] ; /* A poly-field */
  } closure ;
  struct SCM_process {
    SCM_object_header header ;
    SCM next ;          /* doubly linked list */
    SCM previous ;
    SIZE_T size ;
    SCM term[2] ;      /* A poly-field */
  } process ;
  struct SCM_module {
    SCM_object_header header ;
    SIZE_T size ;
    SCM global[1] ;      /* A poly-field */
  } module ;
  struct SCM_root {
    SCM_object_header header ;
    SIZE_T size ;
    SCM module[1] ;      /* A poly-field */
  } root ;
  struct SCM_char {
    SCM_object_header header ;
    char C_char ;
  } character ;
  struct SCM_symbol {
    SCM_object_header header ;
    SCM string ;
  } symbol ;
  struct SCM_port {
    SCM_object_header header ;
    int flag ;            /* O_RDONLY, O_WRONLY ... */
    SCM poken_character ; /* (SCM) NULL or SCM_eof or a Scheme character */
    FILE *file ;
  } port ;
  struct SCM_string {
    SCM_object_header header ;
    SIZE_T size ;
    char *C_string ;
    /* There might be characters here if dynamically created. */
  } string ;
  struct SCM_continuation {
    SCM_object_header header ;
    SCM transformer ;
    SCM below ;
  } continuation ;
  struct SCM_stack_slice {
    SCM_object_header header ;
    SIZE_T stack_slice_length ;
    jmp_buf stack_jmp_buf ;
    char stack_char[1] ; /* A poly-field */
  } stack_slice ;
  struct SCM_frame {
    SCM_object_header header ;
    SCM next ;
    SIZE_T size ;
    SCM value[1] ;       /* A poly-field */
  } frame ;
  struct SCM_function {
    SCM_object_header header ;
    SCM (*behavior)() ;
    SCM environment ;
  } function ;
  struct SCM_dynamic_link {
    SCM_object_header header ;
    SCM next ;
    SCM key ;
    SCM value ;
  } dynamic_link ;
} ;

/***********************************************************************
 * Arity is coded as a positive integer if fixed (ie the arity of cons is 2) 
 * and as a negative integer if a minimal arity exists. The arity
 * of list is -1 (ie at least zero argument).				*/

/***********************************************************************
 * Prototypes of library functions or macros that might appear in 
 * generated C.								*/

extern SCM SCM_allocate(SCM_TAG tag, SIZE_T size) ;
#define SCM_tag_of(x)        (((x)->object).header).tag
/* this compares pointers to data objects */
#define SCM_eqp(x,y)         ((void*)(x)==(void*)(y))
/* while this compares pointers to C functions */
#define SCM_Cfunctions_eqp(x,y) ((void(*)())(x)==(void(*)())(y))
/* Compare addresses rather than tags. 
 * Note: This imposes a unique undef object.				*/
#define SCM_undeterminedp(x) (SCM_eqp(x,SCM_undef))
/* Convert a C boolean value to a Scheme boolean value			*/
#define SCM_2bool(x)         ((x)?SCM_true:SCM_false)

extern SCM SCM_make_fixnum (int value) ;
extern struct SCM_fixnum SCM_small_fixnums[] ;
/* SCM_fixnump (as all other predicates with a trailing p, returns
 * a C boolean rather than a Scheme boolean.				*/
#define SCM_fixnump(x)       (SCM_FIXNUM_TAG==SCM_tag_of(x))
#define SCM_fixnum2int(x)    ((int)(x->fixnum).value)

extern SCM SCM_make_floatnum (float value) ;
#define SCM_floatnump(x)       (SCM_FLOATNUM_TAG==SCM_tag_of(x))
#define SCM_floatnum2float(x)  ((float)(x->floatnum).value)

extern SCM SCM_make_pair (SCM car, SCM cdr) ;
#define SCM_cons(x,y)        SCM_make_pair((x),(y))
#define SCM_car(x)           ((x)->pair).car
#define SCM_cdr(x)           ((x)->pair).cdr
#define SCM_set_car(x,y)     (SCM_car(x) = (y))
#define SCM_set_cdr(x,y)     (SCM_cdr(x) = (y))
#define SCM_pairp(x)         (SCM_PAIR_TAG==SCM_tag_of(x))
/* Compare addresses rather than tags
 * #define SCM_nullp(x)         (SCM_EMPTY_TAG==SCM_tag_of(x))		*/
#define SCM_nullp(x)         (SCM_eqp(x,SCM_empty))
#define SCM_pairp2bool(x)    (SCM_2bool(SCM_pairp(x)))

extern SCM SCM_make_continuation (SCM t, SCM q) ;
#define SCM_continuation2transformer(x) ((x)->continuation).transformer
#define SCM_continuation2below(x)       ((x)->continuation).below

extern SCM SCM_make_box (SCM content) ;
#define SCM_boxp(x)              (SCM_BOX_TAG==SCM_tag_of(x))
#define SCM_box_content(x)       ((x)->box).content
#define SCM_set_box_content(x,y) (SCM_box_content(x) = (y))

extern SCM SCM_make_symbol (SCM string) ;
#define SCM_symbolp(x)       (SCM_SYMBOL_TAG==SCM_tag_of(x))
#define SCM_symbol2string(x) ((x)->symbol).string

extern SCM SCM_make_string (char* C_string) ;
extern SCM SCM_allocate_string (SIZE_T size) ;
#define SCM_stringp(x)        (SCM_STRING_TAG==SCM_tag_of(x))
#define SCM_string2Cstring(x) ((x)->string).C_string
#define SCM_string2size(x)    ((x)->string).size
#define SCM_mutable_stringp(x) \
   ( ((char*)SCM_string2Cstring(x)) == ((char*)SCM_string2Cstring(x))+1 )

extern struct SCM_char SCM_characters[] ;
#define SCM_charp(x)          (SCM_CHAR_TAG==SCM_tag_of(x))
/* returns a C int instead of a SCM int.			
 * #define SCM_char2int(x)       ((int)((x)->character).C_char)	*/
#define SCM_int2char(x)       ((SCM) &(SCM_characters[x]))
#define SCM_char2int(x)       (((struct SCM_char*)(x))-SCM_characters)

#define SCM_streamp(x)            (SCM_PORT_TAG==SCM_tag_of(x))
#define SCM_port2stream(x)        (((x)->port).file)
#define SCM_port2poken_char(x)    (((x)->port).poken_character)

extern SCM SCM_make_primitive(SCM (*behavior)(), int arity, SCM name) ;
#define SCM_primitivep(x)         (SCM_PRIMITIVE_TAG==SCM_tag_of(x))
/* Returns a SCM object (generally a string).				*/
#define SCM_primitive2name(x)     (((x)->primitive).name)

extern SCM SCM_make_closure(SCM (*behavior)(), int arity, SIZE_T size) ;
#define SCM_closurep(x)         (SCM_CLOSURE_TAG==SCM_tag_of(x))
#define SCM_closed_value(c,i)   (((c)->closure).environment[i])
#define SCM_set_closed_value(c,i,x) (SCM_closed_value(c,i)=(x))
#define SCM_minimal_arity(arity)  (-(arity+1))
#define SCM_CreatePrimitive(SCM_var,C_function,arity,C_string) \
{ static /*const*/ char *name = C_string ; 			        \
  SCM_var = SCM_make_primitive(C_function,arity,SCM_make_string(name)) ; }

extern SCM SCM_make_root (SIZE_T size) ;
extern SCM SCM_make_module (SIZE_T size) ;

/* This is used in rtd_scheme.c to copy slices of the C stack. */
extern SCM SCM_make_stack_slice (SIZE_T size) ;

/* This is used by the GC to determine if an object is static. These
 * are immutable objects and writers verify that.			*/
#ifdef GC_STOP_AND_COPY
#define SCM_staticp(x)   ((char*)(x) < ((char*) zone_static_end) )
#else
#define SCM_staticp(x)   0
#endif /* GC_STOP_AND_COPY */

/***********************************************************************
 * Some constant values. These constants are roots for the GC and must
 * known, so define them with DefineRoot.				*/

extern SCM SCM_true, 		/* #t */
  SCM_false, 			/* #f */
  SCM_empty, 			/* () */
  SCM_undef, 			/* undefined */
  SCM_eof ;			/* end-of-file */
extern struct SCM_immediate_object SCM_true_object,
  SCM_false_object,
  SCM_empty_object,
  SCM_undef_object ;
extern SCM SCM_stdin, 		/* stdin */
  SCM_stdout, 			/* stdout */
  SCM_stderr ;			/* stderr */
extern SCM SCM_options ;	/* the list of options for the command */
extern void *SCM_Cstack_bottom ;	/* the address of the bottom 
                                           of the stack (useful to copy
                                           stack slices). */

typedef enum SCM_stack_growing_type { UP=1, DOWN } SCM_stack_growing_type ;
extern SCM_stack_growing_type SCM_stack_grows ; /* the way the stack grows */

typedef enum SCM_gc_state { GC_NEEDED=1, GC_NOT_NEEDED } SCM_gc_state ;
extern SCM_gc_state SCM_gc_neededp(SIZE_T room) ;
extern void SCM_gc (unsigned int number, SCM roots[]) ;
/* This structure will reference all the module objects of the application.
 * These in turn will reference all global variables.			*/
extern SCM SCM_roots ;

#define SCM_truep(x)    (SCM_TRUE_TAG==SCM_tag_of(x))
#define SCM_falsep(x)   (SCM_FALSE_TAG==SCM_tag_of(x))
#define SCM_emptyp(x)   (SCM_EMPTY_TAG==SCM_tag_of(x))
#define SCM_undefp(x)   (SCM_UNDETERMINED_TAG==SCM_tag_of(x))
#define SCM_eofp(x)     (SCM_EOF_TAG==(SCM_tag_of(x)))

/* Some global functions or variables					*/

extern SCM SCM_PairP (SCM x) ;
extern SCM SCM_NullP (SCM x) ;
extern SCM SCM_FixnumP (SCM x) ;
extern SCM SCM_SymbolP (SCM x) ;
extern SCM SCM_StringP (SCM x) ;
extern SCM SCM_CharP (SCM x) ;
extern SCM SCM_EofP (SCM x) ;
extern SCM SCM_ProcedureP (SCM x) ;
extern SCM SCM_EqP (SCM x, SCM y) ;
extern SCM SCM_Cons (SCM x, SCM y) ;
extern SCM SCM_Car (SCM x) ;
extern SCM SCM_Cdr (SCM x) ;
extern SCM SCM_Set_Car (SCM x, SCM y) ;
extern SCM SCM_Set_Cdr (SCM x, SCM y) ;
extern SCM SCM_Plus (SCM x, SCM y) ;
extern SCM SCM_Minus (SCM x, SCM y) ;
extern SCM SCM_Times (SCM x, SCM y) ;
extern SCM SCM_Quotient (SCM x, SCM y) ;
extern SCM SCM_Remainder (SCM x, SCM y) ;
extern SCM SCM_GtP (SCM x, SCM y) ;
extern SCM SCM_LtP (SCM x, SCM y) ;
extern SCM SCM_GeP (SCM x, SCM y) ;
extern SCM SCM_LeP (SCM x, SCM y) ;
extern SCM SCM_EqnP (SCM x, SCM y) ;
extern SCM SCM_Basic_Write (SCM x) ;
extern SCM SCM_Integer2Char (SCM x) ;
extern SCM SCM_Char2Integer (SCM x) ;
extern SCM SCM_Symbol2String (SCM x) ;
extern SCM SCM_Make_Symbol (SCM x) ;
extern SCM SCM_Exit (SCM x) ;
extern SCM SCM_Get_Universal_Time () ;
extern SCM SCM_Float_Plus (SCM x, SCM y) ;
extern SCM SCM_Float_Minus (SCM x, SCM y) ;
extern SCM SCM_Float_Multiply (SCM x, SCM y) ;
extern SCM SCM_Float_Divide (SCM x, SCM y) ;
extern SCM SCM_Float_GreaterP (SCM x, SCM y) ;
extern SCM SCM_Float_LessP (SCM x, SCM y) ;
extern SCM SCM_Float_EqnP (SCM x, SCM y) ;
extern SCM SCM_Read_Char (SCM stream) ;
extern SCM SCM_Peek_Char (SCM stream) ;
extern SCM SCM_String_Length (SCM string) ;
extern SCM SCM_String_Ref (SCM string, SCM index) ;
extern SCM SCM_String_Set (SCM string, SCM index, SCM character) ;
extern SCM SCM_Not (SCM x) ;
extern SCM SCM_Call_CC (SCM f) ;
extern SCM SCM_Bind_DE (SCM key, SCM value, SCM thunk) ;
extern SCM SCM_Assoc_DE (SCM key, SCM success) ;

extern SCM SCM_T ;
extern SCM SCM_F ;
extern SCM SCM_NIL ;
extern SCM SCM_STDIN ;
extern SCM SCM_STDOUT ;
extern SCM SCM_STDERR ;
extern SCM SCM_PAIRP ;
extern SCM SCM_NULLP ;
extern SCM SCM_INTEGERP ;
extern SCM SCM_SYMBOLP ;
extern SCM SCM_STRINGP ;
extern SCM SCM_CHARP ;
extern SCM SCM_ENDOFFILEP ;
extern SCM SCM_PROCEDUREP ;
extern SCM SCM_EQP ;
extern SCM SCM_CONS ;
extern SCM SCM_CAR ;
extern SCM SCM_CDR ;
extern SCM SCM_SETCAR ;
extern SCM SCM_SETCDR ;
extern SCM SCM_LIST ;
extern SCM SCM_PLUS ;
extern SCM SCM_MINUS ;
extern SCM SCM_TIMES ;
extern SCM SCM_QUOTIENT ;
extern SCM SCM_REMAINDER ;
extern SCM SCM_GTP ;
extern SCM SCM_GEP ;
extern SCM SCM_LTP ;
extern SCM SCM_LEP ;
extern SCM SCM_EQNP ;
extern SCM SCM_BASICWRITE ;
extern SCM SCM_INT2CHAR ;
extern SCM SCM_CHAR2INT ;
extern SCM SCM_SYMBOL2STRING ;
extern SCM SCM_MAKE_SYMBOL ;
extern SCM SCM_EXIT ;
extern SCM SCM_GETUNIVERSALTIME ;
extern SCM SCM_FLOAT_PLUS ;
extern SCM SCM_FLOAT_MINUS ;
extern SCM SCM_FLOAT_TIMES ;
extern SCM SCM_FLOAT_DIVIDE ;
extern SCM SCM_FLOAT_GTP ;
extern SCM SCM_FLOAT_LTP ;
extern SCM SCM_FLOAT_GEP ;
extern SCM SCM_FLOAT_LEP ;
extern SCM SCM_FLOAT_EQNP ;
extern SCM SCM_READ_CHAR ;
extern SCM SCM_PEEK_CHAR ;
extern SCM SCM_STRING_LENGTH ;
extern SCM SCM_STRING_REF ;
extern SCM SCM_STRING_SET ;
extern SCM SCM_NOT ;
extern SCM SCM_CALL_CC ;
extern SCM SCM_APPLY ;
extern SCM SCM_BIND_DE;
extern SCM SCM_ASSOC_DE ;

/* Other resources */
extern SCM SCM_error (unsigned int code) ;
extern SCM SCM_report_error(unsigned int code, char *file, unsigned int line);

/* Run-time utilities.
 * This initializes the heap as well as all the predefined objects. 
 * The first function is independent of VTS style while the second
 * deals with continuation-specific resources. 				*/

extern void SCM_initialize (int argc, char *argv[]) ;
extern void SCM_report_usage () ;

/* The generic function callers.
 * Fixed arity functions may not have more than 4 variables:
 *    SCM foo (SCM self,x,y,z,t)
 * Otherwise they are represented by SCM foo (int num, SCM args[]).
 * Nary functions have no more than 4 mandatory variables:
 *    SCM bar (SCM x,y, int number, SCM args[]) 
 * Otherwise they are represented by SCM bar (int num, SCM args[]).
 *
 * Same for primitives which are closures without environment ie 
 * without SELF variable. 
 * SCM_first_invokation is the first invoker to begin with. It sets up
 * some global variables.						*/

extern void SCM_first_invokation (SCM thunk) ;
extern SCM SCM_invoke0 (SCM fun) ;
extern SCM SCM_invoke1 (SCM fun, SCM) ;
extern SCM SCM_invoke2 (SCM fun, SCM, SCM) ;
extern SCM SCM_invoke3 (SCM fun, SCM, SCM, SCM) ;
extern SCM SCM_invoke4 (SCM fun, SCM, SCM, SCM, SCM) ;
extern SCM SCM_invokeN (SCM fun, int arity, SCM args[]) ;


/*************************************
Things added for chap6f *************/

extern SCM SCM_The_Environment ;
extern SCM SCM_The_Null_Environment ;
extern SCM SCM_The_Dynamic_Environment ;
extern SCM SCM_The_Null_Frame ;

extern SCM SCM_make_function (SCM (*Cfunction)(), SCM environment) ;
extern SCM SCM_make_frame (SIZE_T size) ;
extern SCM SCM_call (SCM fun, SCM frame) ;

extern SCM SCM_Bind_DE (SCM key, SCM value, SCM thunk) ;
extern SCM SCM_Assoc_DE (SCM key, SCM success) ;

extern SCM SCM_List (SCM frame, SCM nullenv) ;

extern SCM SCM_Apply (SCM frame, SCM nullenv) ;

extern SCM SCM_listify (SCM frame, SIZE_T arity) ;

#endif /* RTLIB_SCHEME_H */
