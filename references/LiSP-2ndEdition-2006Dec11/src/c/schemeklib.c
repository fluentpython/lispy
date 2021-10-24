/* $Id: schemeklib.c,v 4.0 1995/07/10 06:52:31 queinnec Exp $ */

/* (((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
 * This file is part of the files that accompany the book:
 *     LISP Implantation Semantique Programmation (InterEditions, France)
 * By Christian Queinnec <Christian.Queinnec@INRIA.fr>
 * Newest version may be retrieved from:
 *   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
 * Check the README file before using this file.
 *(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
 */

/* The schemeklib.c runtime associated to the chap10k.scm compiler.
 * It defines the global variables that hold primitives.
 */

#include "scheme.h"

/* Identify this library.
 */

static char *rcsid = 
  "@(#)$Id: schemeklib.c,v 4.0 1995/07/10 06:52:31 queinnec Exp $";

/* Global constants
 */

SCM_DefineInitializedGlobalVariable(NIL,"NIL",&SCM_nil_object);
SCM_DefineInitializedGlobalVariable(F,"F",&SCM_false_object);
SCM_DefineInitializedGlobalVariable(T,"T",&SCM_true_object);

#define SCM_DefineCPSsubr1(newname,oldname)	\
  SCM newname (SCM k, SCM x) { 			\
    return SCM_invoke1(k,oldname(x));		\
  }
#define SCM_DefineCPSsubr2(newname,oldname)	\
  SCM newname (SCM k, SCM x, SCM y) { 		\
    return SCM_invoke1(k,oldname(x,y));		\
  }
#define SCM_DefineCPSsubrN(newname,oldname)		\
  SCM newname (unsigned long number, va_list arguments) {	\
    SCM k = va_arg(arguments,SCM);			\
    return SCM_invoke1(k,oldname(number-1,arguments));	\
}

SCM_DefineCPSsubr2(SCMq_gtp,SCM_gtp)
SCM_DefinePredefinedFunctionVariable(GREATERP,">",3,SCMq_gtp);
SCM_DefineCPSsubr2(SCMq_ltp,SCM_ltp)
SCM_DefinePredefinedFunctionVariable(LESSP,"<",3,SCMq_ltp);
SCM_DefineCPSsubr2(SCMq_eqnp,SCM_eqnp)
SCM_DefinePredefinedFunctionVariable(EQN,"=",3,SCMq_eqnp);
SCM_DefineCPSsubr2(SCMq_gep,SCM_gep)
SCM_DefinePredefinedFunctionVariable(NOT_LESSP,">=",3,SCMq_gep);
SCM_DefineCPSsubr2(SCMq_lep,SCM_lep)
SCM_DefinePredefinedFunctionVariable(NOT_GREATERP,"<=",3,SCMq_lep);
SCM_DefineCPSsubr2(SCMq_remainder,SCM_remainder)
SCM_DefinePredefinedFunctionVariable(REMAINDER,"REMAINDER",3,SCMq_remainder);
SCM_DefineCPSsubr2(SCMq_quotient,SCM_quotient)
SCM_DefinePredefinedFunctionVariable(QUOTIENT,"/",3,SCMq_quotient);
SCM_DefineCPSsubr2(SCMq_times,SCM_times)
SCM_DefinePredefinedFunctionVariable(TIMES,"*",3,SCMq_times);
SCM_DefineCPSsubr2(SCMq_minus,SCM_minus)
SCM_DefinePredefinedFunctionVariable(DIFFERENCE,"-",3,SCMq_minus);
SCM_DefineCPSsubr2(SCMq_plus,SCM_plus)
SCM_DefinePredefinedFunctionVariable(PLUS,"+",3,SCMq_plus);
SCM_DefineCPSsubr1(SCMq_fixnump,SCM_fixnump)
SCM_DefinePredefinedFunctionVariable(INTEGERp,"INTEGER?",2,SCMq_fixnump);
SCM_DefineCPSsubr2(SCMq_eqp,SCM_eqp)
SCM_DefinePredefinedFunctionVariable(EQ,"EQ?",3,SCMq_eqp);
SCM_DefineCPSsubr1(SCMq_stringp,SCM_stringp)
SCM_DefinePredefinedFunctionVariable(STRINGP,"STRINGP",2,SCMq_stringp);
SCM_DefineCPSsubr1(SCMq_symbolp,SCM_symbolp)
SCM_DefinePredefinedFunctionVariable(SYMBOLP,"SYMBOL?",2,SCMq_symbolp);
SCM_DefineCPSsubr1(SCMq_nullp,SCM_nullp)
SCM_DefinePredefinedFunctionVariable(NULLp,"NULL?",2,SCMq_nullp);
SCM_DefineCPSsubr1(SCMq_pairp,SCM_pairp)
SCM_DefinePredefinedFunctionVariable(CONSP,"PAIR?",2,SCMq_pairp);
SCM_DefineCPSsubr2(SCMq_set_cdr,SCM_set_cdr)
SCM_DefinePredefinedFunctionVariable(RPLACD,"SET-CDR!",3,SCMq_set_cdr);
SCM_DefineCPSsubr2(SCMq_set_car,SCM_set_car)
SCM_DefinePredefinedFunctionVariable(RPLACA,"SET-CAR!",3,SCMq_set_car);
SCM_DefineCPSsubr1(SCMq_cdr,SCM_cdr)
SCM_DefinePredefinedFunctionVariable(CDR,"CDR",2,SCMq_cdr);
SCM_DefineCPSsubr1(SCMq_car,SCM_car)
SCM_DefinePredefinedFunctionVariable(CAR,"CAR",2,SCMq_car);
SCM_DefineCPSsubr2(SCMq_cons,SCM_cons)
SCM_DefinePredefinedFunctionVariable(CONS,"CONS",3,SCMq_cons);

/* Additional functions
 */

SCM_DefineCPSsubr1(SCMq_print,SCM_print)
SCM_DefinePredefinedFunctionVariable(PRINT,"PRINT",2,SCMq_print);

/* Apply and List now takes at least one more argument: the continuation
 * so their signature is changed.
 */

SCM SCMq_apply (unsigned long number, va_list arguments) {
  SCM args[32];
  SCM last_arg;
  SCM k   = va_arg(arguments,SCM);
  SCM fun = va_arg(arguments,SCM);
  unsigned long i;
  for ( i=0 ; i<number-2 ; i++  ) {
    args[i] = va_arg(arguments,SCM);
  }
  last_arg = args[--i];
  while ( SCM_PairP(last_arg) ) {
    args[i++] = SCM_Car(last_arg);
    last_arg = SCM_Cdr(last_arg);
  }
  if ( ! SCM_NullP(last_arg) ) {
    SCM_error(SCM_ERR_APPLY_ARG);
  }
  switch ( i ) {
  case 0: return SCM_invoke(fun,1,k);
  case 1: return SCM_invoke(fun,2,k,args[0]);
  case 2: return SCM_invoke(fun,3,k,args[0],args[1]);
  case 3: return SCM_invoke(fun,4,k,args[0],args[1],args[2]);
  case 4: return SCM_invoke(fun,5,k,args[0],args[1],args[2],args[3]);
  case 5: return SCM_invoke(fun,6,k,args[0],args[1],args[2],args[3],
                            args[4]);
  case 6: return SCM_invoke(fun,7,k,args[0],args[1],args[2],args[3],
                            args[4],args[5]);
  case 7: return SCM_invoke(fun,8,k,args[0],args[1],args[2],args[3],
                            args[4],args[5],args[6]);
  case 8: return SCM_invoke(fun,9,k,args[0],args[1],args[2],args[3],
                            args[4],args[5],args[6],args[7]);
  case 9: return SCM_invoke(fun,10,k,args[0],args[1],args[2],args[3],
                             args[4],args[5],args[6],args[7],args[8]);
  case 10: return SCM_invoke(fun,11,k,args[0],args[1],args[2],args[3],
                             args[4],args[5],args[6],args[7],args[8],args[9]);
  case 11: return SCM_invoke(fun,12,k,args[0],args[1],args[2],args[3],
                             args[4],args[5],args[6],args[7],args[8],args[9],
                             args[10]);
  case 12: return SCM_invoke(fun,13,k,args[0],args[1],args[2],args[3],
                             args[4],args[5],args[6],args[7],args[8],args[9],
                             args[10],args[11]);
  case 13: return SCM_invoke(fun,14,k,args[0],args[1],args[2],args[3],
                             args[4],args[5],args[6],args[7],args[8],args[9],
                             args[10],args[11],args[12]);
  case 14: return SCM_invoke(fun,15,k,args[0],args[1],args[2],args[3],
                             args[4],args[5],args[6],args[7],args[8],args[9],
                             args[10],args[11],args[12],args[13]);
  /* If this were less cumbersome, I will add cases up to 31. */
  default: return SCM_error(SCM_ERR_APPLY_SIZE);
  }
}

SCM_DefinePredefinedFunctionVariable(APPLY,"APPLY",-4,SCMq_apply);
SCM_DefineCPSsubrN(SCMq_list,SCM_list)
SCM_DefinePredefinedFunctionVariable(LIST,"LIST",-2,SCMq_list);

/* In the next function, current_k is unused since it is the abandoned
   continuation. It may raise a warning.
 */

SCM 
SCM_invoke_continuation (SCM self, unsigned long number, va_list arguments) {
  SCM current_k = va_arg(arguments,SCM);
  SCM value     = va_arg(arguments,SCM);
  return SCM_invoke1(SCM_Unwrap(self)->closure.environment[0],value);
}
  
SCM SCM_callcc (SCM k, SCM f) {
  SCM reified_k = SCM_close(SCM_CfunctionAddress(SCM_invoke_continuation),2,1,k);
  return SCM_invoke2(f,k,reified_k);
}

SCM_DefinePredefinedFunctionVariable(CALLCC,"CALL/CC",2,SCM_callcc);

/* end of schemeklib.c */
