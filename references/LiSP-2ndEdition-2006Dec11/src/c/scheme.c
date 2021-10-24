/* $Id: scheme.c,v 4.0 1995/07/10 06:52:28 queinnec Exp $ */

/* (((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
 * This file is part of the files that accompany the book:
 *     LISP Implantation Semantique Programmation (InterEditions, France)
 * By Christian Queinnec <Christian.Queinnec@INRIA.fr>
 * Newest version may be retrieved from:
 *   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
 * Check the README file before using this file.
 *(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
 */

/* The scheme.c runtime associated to the chap10[ek].scm compilers.
 */

#include "scheme.h"

/* Identify this library.
 */

static char *rcsid = 
  "@(#)$Id: scheme.c,v 4.0 1995/07/10 06:52:28 queinnec Exp $";

/* Static allocation of the basic constants #t, #f, nil and the pseudo
 * value `undefined' that appears as value of uninitialized variables.
 */

SCM_DefineImmediateObject(SCM_true_object,SCM_BOOLEAN_TAG);
SCM_DefineImmediateObject(SCM_false_object,SCM_BOOLEAN_TAG);
SCM_DefineImmediateObject(SCM_nil_object,SCM_NULL_TAG);
SCM_DefineImmediateObject(SCM_undefined_object,SCM_UNDEFINED_TAG);

/* Allocations.  
 * Allocate boxes for mutable variables. Boxes are not
 * first class Scheme values so their type need not be encoded. This
 * allocation is ridiculous since to use malloc to allocate a single
 * word has a very high overhead, but you can use Boehm's GC quite
 * easily. Boxes are not wrapped. 
 */

SCM SCM_allocate_box (SCM v) {
  SCM cell = (SCM) malloc(sizeof(struct SCM_box));
  if (cell == (SCM) NULL) SCM_error(SCM_ERR_CANT_ALLOC);
  cell->box.content = v;
  return (cell);
}

/* Allocate pairs.
 */

SCM SCM_cons (SCM x, SCM y) {
  SCMref cell = (SCMref) malloc(sizeof(struct SCM_unwrapped_pair));
  if (cell == (SCMref) NULL) SCM_error(SCM_ERR_CANT_ALLOC);
  cell->pair.header.tag = SCM_PAIR_TAG;
  cell->pair.car = x;
  cell->pair.cdr = y;
  return SCM_Wrap(cell);
}

/* Allocate continuations.  
 * They are restricted to their dynamic extent and are implemented
 * with a jmp buffer in the stack and the continuation object in the
 * heap.  A backpointer from the stack to the heap exists. If it is
 * corrupted then the continuation is surely out of its dynamic
 * extent. If it is not then to be sure, one would have to compare the
 * current stack pointer to see if it is above this jmp_buf but this
 * requires to known the way the stack grows since this knowledge would
 * only be useful to raise an error, let the `longjmp botch' of the
 * C library do the work.
 */

SCM SCM_allocate_continuation (struct SCM_jmp_buf *address) {
  SCMref continuation = (SCMref) malloc(sizeof(struct SCM_unwrapped_escape));
  if (continuation == (SCMref) NULL) SCM_error(SCM_ERR_CANT_ALLOC);
  continuation->escape.header.tag = SCM_ESCAPE_TAG;
  continuation->escape.stack_address = address;
  return SCM_Wrap(continuation);
}

/* Build a closure.  
 * Since a closure can close over many free variables, this function
 * is implemented as a vararg C function. Since C structures cannot 
 * have a varying length, we cheat a little and allocate a SCM_closure
 * with the appropriate length and hope that C will not detect that the
 * additional free variables are beyond the normal end of the structure.
 */

SCM SCM_close (SCM (*Cfunction)(void), long arity, unsigned long size, ...) {
  SCMref result = (SCMref) malloc(sizeof(struct SCM_unwrapped_closure) 
                                  + (size-1)*sizeof(SCM) );
  unsigned long i;
  va_list args;
  if (result == (SCMref) NULL) SCM_error(SCM_ERR_CANT_ALLOC);
  result->closure.header.tag = SCM_CLOSURE_TAG;
  result->closure.behavior = Cfunction;
  result->closure.arity = arity;
  va_start(args,size);
  for ( i=0 ; i<size ; i++ ) {
    result->closure.environment[i] = va_arg(args,SCM);
  }
  va_end(args);
  return SCM_Wrap(result);
}

/* Invocation is the heart of the run-time.  To ease reading C
 * generated programs, combinations use the SCM_invoke function with a
 * variable number of arguments. It would be interesting to developp
 * specialized invokers for a fixed number of arguments. Some
 * temporary variables a0, a1 and a2 are defined out of SCM_invoke to
 * reduce the stack consumption of SCM_invoke; one may also qualify
 * them with `register' and hope they don't consume stack which would
 * reduce recursion depth.  These variables are necessary since the
 * order of evaluation is not guaranteed in C, it is thus necessary to
 * take the values out of args one after one.  The jumpvalue variable
 * is needed when invoking continuations since we cannot ensure that
 * the transmitted value is different from zero so it is passed
 * through the store rather than by longjmp. 
 */

static SCM jumpvalue;

SCM SCM_invoke(SCM function, unsigned long number, ...) {
  if ( SCM_FixnumP(function) ) {
    return SCM_error(SCM_ERR_CANNOT_APPLY); /* Cannot apply a number! */
  } else {
    switch SCM_2tag(function) {
    case SCM_SUBR_TAG: {
      SCM (*behavior)(void) = (SCM_Unwrap(function)->subr).behavior;
      long arity = (SCM_Unwrap(function)->subr).arity;
      SCM result;
      if ( arity >= 0 ) {         /* Fixed arity subr */
        if ( arity != number ) {
          return SCM_error(SCM_ERR_WRONG_ARITY); /* Wrong arity! */
        } else {
          if ( arity == 0) {
            result = behavior();
          } else {
            va_list args;
            va_start(args,number);
            { SCM a0 ;
              a0 = va_arg(args,SCM);
              if ( arity == 1 ) {
                result = ((SCM (*)(SCM)) *behavior)(a0);
              } else {
                SCM a1 ;
                a1 = va_arg(args,SCM);
                if ( arity == 2 ) {
                  result = ((SCM (*)(SCM,SCM)) *behavior)(a0,a1);
                } else {
                  SCM a2 ;
                  a2 = va_arg(args,SCM);
                  if ( arity == 3 ) {
                    result = ((SCM (*)(SCM,SCM,SCM)) *behavior)(a0,a1,a2);
                  } else {
                    /* No fixed arity subr with more than 3 variables */
                    return SCM_error(SCM_ERR_INTERNAL); 
                  }
                }
              }
            }
            va_end(args);
          }
          return result;
        }
      } else {                  /* Nary subr */
        long min_arity = SCM_MinimalArity(arity) ;
        if ( number < min_arity ) {
          return SCM_error(SCM_ERR_MISSING_ARGS); /* Too less arguments! */
        } else {
          va_list args;
          SCM result;
          va_start(args,number);
          result = ((SCM (*)(unsigned long,va_list)) *behavior)(number,args);
          va_end(args);
          return result;
        }
      }
    }
    case SCM_CLOSURE_TAG: {
      SCM (*behavior)(void) = (SCM_Unwrap(function)->closure).behavior ;
      long arity = (SCM_Unwrap(function)->closure).arity ;
      SCM result;
      va_list args;
      va_start(args,number);
      if ( arity >= 0 ) {
        if ( arity != number ) { /* Wrong arity! */
          return SCM_error(SCM_ERR_WRONG_ARITY);
        } else {
          result = ((SCM (*)(SCM,unsigned long,va_list)) *behavior)(function,number,args);
        }
      } else {
        long min_arity = SCM_MinimalArity(arity) ;
        if ( number < min_arity ) { 
          return SCM_error(SCM_ERR_MISSING_ARGS);     /* Too less arguments! */
        } else {
          result = ((SCM (*)(SCM,unsigned long,va_list)) *behavior)(function,number,args);
        }
      }
      va_end(args);
      return result;
    }
    case SCM_ESCAPE_TAG: {
      if ( number == 1) {
        va_list args;
        va_start(args,number);
        jumpvalue = va_arg(args,SCM);
        va_end(args);
        { struct SCM_jmp_buf *address = 
            SCM_Unwrap(function)->escape.stack_address;
          if ( SCM_EqP(address->back_pointer,function) 
              && ( (void *) &address SCM_STACK_HIGHER (void *) address ) ) {
            longjmp(address->jb,1);
          } else {    /* surely out of dynamic extent! */
            return SCM_error(SCM_ERR_OUT_OF_EXTENT); 
          }
        }
      } else {
        return SCM_error(SCM_ERR_MISSING_ARGS);     /* Too less arguments! */
      }
    }
    default: {
      return SCM_error(SCM_ERR_CANNOT_APPLY);       /* Cannot apply! */
    } 
    }
  }
}

/* List functions
 * SCM_list is also used to fill dotted variables.
 */

SCM SCM_list (unsigned long count, va_list arguments) {
  if ( count == 0 ) { 
    return SCM_nil;
  } else {
    SCM arg = va_arg(arguments,SCM);
    return SCM_cons(arg,SCM_list(count-1,arguments));
  }
}

/* These are safe accessors. It is of course more efficient to perform 
 * type recovery or control flow analysis to generate safe calls to the
 * unsafe accessors.
 */

SCM SCM_car (SCM x) {
  if ( SCM_PairP(x) ) { 
    return SCM_Car(x);
  } else return SCM_error(SCM_ERR_CAR);
}

SCM SCM_cdr (SCM x) { 
  if ( SCM_PairP(x) ) { 
    return SCM_Cdr(x);
  } else return SCM_error(SCM_ERR_CDR);
}

SCM SCM_set_car (SCM x, SCM y) {
  if ( SCM_PairP(x) ) {
    SCM_Unwrap(x)->pair.car = y;
    return x;
  } else return SCM_error(SCM_ERR_SET_CAR);
}

SCM SCM_set_cdr (SCM x, SCM y) {
  if ( SCM_PairP(x) ) {
    SCM_Unwrap(x)->pair.cdr = y;
    return x;
  } else return SCM_error(SCM_ERR_SET_CDR);
}

/* Various type predicates.
 */

#define DefMonadicPred(name,operator)	\
SCM name (SCM x) {			\
  return (SCM_2bool(operator(x)));	\
}

DefMonadicPred(SCM_pairp,SCM_PairP)
DefMonadicPred(SCM_nullp,SCM_NullP)
DefMonadicPred(SCM_fixnump,SCM_FixnumP)
DefMonadicPred(SCM_symbolp,SCM_SymbolP)
DefMonadicPred(SCM_stringp,SCM_StringP)

/* This is the procedure? predicates that answers true to many types.
 */

SCM SCM_procedurep (SCM x) { 
  if (SCM_FixnumP(x) ) {
    return SCM_false;
  } else {
    switch (SCM_2tag(x)) {
    case SCM_SUBR_TAG:
    case SCM_CLOSURE_TAG:
    case SCM_ESCAPE_TAG:
      return SCM_true;
    default:
      return SCM_false;
    }
  }
}

#define DefDyadicPred(name,operator)	\
SCM name (SCM x, SCM y)	{               \
  return (SCM_2bool(operator(x,y))); 	\
}

DefDyadicPred(SCM_eqp,SCM_EqP)

/* Arithmetic operations.
 * It is possible to do better and reduce the cost of wrapping/unwrapping.
 * Since here [n] is represented by 2n+1, then, for instance, [n+p] is 
 * directly [n]+[p]-1.
 */

#define DefDyadicFunction(name,macro)	\
SCM name (SCM x, SCM y)	{		\
  return macro(x,y);			\
}

DefDyadicFunction(SCM_plus,SCM_Plus)
DefDyadicFunction(SCM_minus,SCM_Minus)
DefDyadicFunction(SCM_times,SCM_Times)
DefDyadicFunction(SCM_quotient,SCM_Quotient)
DefDyadicFunction(SCM_remainder,SCM_Remainder)

DefDyadicFunction(SCM_gtp,SCM_GtP) 
DefDyadicFunction(SCM_ltp,SCM_LtP)
DefDyadicFunction(SCM_eqnp,SCM_EqnP) 
DefDyadicFunction(SCM_gep,SCM_GeP)
DefDyadicFunction(SCM_lep,SCM_LeP)

/* Escape functions.
 * Similar to call/cc but for the extent of the continuation.
 * The value got by the continuation when invoked is stored temporarily
 * in jumpvalue then returned by SCM_callep to avoid passing zero through
 * longjmp [cf. Harbison & Steele]. 
 */

SCM SCM_callep (SCM f) {
  struct SCM_jmp_buf scmjb;
  SCM continuation = SCM_allocate_continuation(&scmjb);
  scmjb.back_pointer = continuation;
  if ( setjmp(scmjb.jb) != 0 ) {
    return jumpvalue;
  } else {
    return SCM_invoke1(f,continuation);
  }
}

/* Signal errors and stop execution.  This is the crudest way: abort
 * immediately but be friendly and deliver a status from which the
 * error can be found. Flush the stdout first so that error messages
 * come after normal output.
 */

SCM SCM_signal_error (unsigned long code, unsigned long line, char *file) {
  fflush(stdout);
  fprintf(stderr,"Error %u, Line %u, File %s.\n",code,line,file);
  exit(code);
}

/* Basic printing machinery.  

 * This could have been defined more simply in Scheme with just s
 * print-string facility. But it is useful to have this sort of thing
 * here to debug the library. SCM_prin prints anything while
 * SCM_prin_list assumes its argument to be a pair.  All calls to
 * printf et al. should check the return status in case of problems.
 */
SCM SCM_prin (SCM x);

void SCM_prin_list (SCM x) {
  if ( SCM_FixnumP(x) ) {
    fprintf(stdout," . %d",SCM_Fixnum2int(x));
  } else {
    switch SCM_2tag(x) {
    case SCM_NULL_TAG: {
      break;
    }
    case SCM_PAIR_TAG: {
      fprintf(stdout," ");
      SCM_prin(SCM_car(x));
      SCM_prin_list(SCM_cdr(x));
      break;
    }
    default: {
      fprintf(stdout," . ");
      SCM_prin(x);
      break;
    }
    }
  }
}

SCM SCM_prin (SCM x) {
  if ( SCM_FixnumP(x) ) {
    fprintf(stdout,"%d",SCM_Fixnum2int(x));
  } else {
    switch SCM_2tag(x) {
    case SCM_NULL_TAG: {
      fprintf(stdout,"()");
      break;
    }
    case SCM_PAIR_TAG: {
      fprintf(stdout,"(");
      SCM_prin(SCM_Car(x));
      SCM_prin_list(SCM_cdr(x));
      fprintf(stdout,")");
      break;
    }
    case SCM_BOOLEAN_TAG: {
      fprintf(stdout,"#%c",(SCM_EqP(x,SCM_true)?'T':'F'));
      break;
    }
    case SCM_UNDEFINED_TAG: {
      fprintf(stdout,"#<UFO>");
      break;
    }
    case SCM_SYMBOL_TAG: {
      SCM str = SCM_Unwrap(x)->symbol.pname;
      char *Cstring = SCM_Unwrap(str)->string.Cstring;
      fprintf(stdout,"%s",Cstring);
      break;
    }
    case SCM_STRING_TAG: {
      char *Cstring = SCM_Unwrap(x)->string.Cstring;
      fprintf(stdout,"\"%s\"",Cstring);
      break;
    }
    case SCM_SUBR_TAG: { 
      fprintf(stdout,"#<Subr@%p>",(void *)(x));
      break;
    }
    case SCM_CLOSURE_TAG: {
      fprintf(stdout,"#<Closure@%p>",(void *)(x));
      break;
    }
    case SCM_ESCAPE_TAG: {
      fprintf(stdout,"#<Continuation@%p>",(void *)(x));
      break;
    }
    default:
      fprintf(stdout,"#<Something@%p>",(void *)(x));
      break;
    }
  }
  return (x);
}

SCM SCM_print (SCM x) {
  SCM_prin(x);
  printf("\n");
  return (x);
}

/* end of scheme.c */
