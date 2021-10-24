/* $Id: schemelib.c,v 4.0 1995/07/10 06:52:32 queinnec Exp $ */

/* (((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
 * This file is part of the files that accompany the book:
 *     LISP Implantation Semantique Programmation (InterEditions, France)
 * By Christian Queinnec <Christian.Queinnec@INRIA.fr>
 * Newest version may be retrieved from:
 *   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
 * Check the README file before using this file.
 *(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
 */

/* The schemelib.c runtime associated to the chap10e.scm compiler.
 * It defines the global variables that hold primitives.
 */

#include "scheme.h"

/* Identify this library.
 */

static char *rcsid = 
  "@(#)$Id: schemelib.c,v 4.0 1995/07/10 06:52:32 queinnec Exp $";

/* Global constants
 */

SCM_DefineInitializedGlobalVariable(NIL,"NIL",&SCM_nil_object);
SCM_DefineInitializedGlobalVariable(F,"F",&SCM_false_object);
SCM_DefineInitializedGlobalVariable(T,"T",&SCM_true_object);

SCM_DefinePredefinedFunctionVariable(CALLEP,"CALL/EP",1,SCM_callep);
SCM_DefinePredefinedFunctionVariable(GREATERP,">",2,SCM_gtp);
SCM_DefinePredefinedFunctionVariable(LESSP,"<",2,SCM_ltp);
SCM_DefinePredefinedFunctionVariable(EQN,"=",2,SCM_eqnp);
SCM_DefinePredefinedFunctionVariable(NOT_LESSP,">=",2,SCM_gep);
SCM_DefinePredefinedFunctionVariable(NOT_GREATERP,"<=",2,SCM_lep);
SCM_DefinePredefinedFunctionVariable(REMAINDER,"REMAINDER",2,SCM_remainder);
SCM_DefinePredefinedFunctionVariable(QUOTIENT,"/",2,SCM_quotient);
SCM_DefinePredefinedFunctionVariable(TIMES,"*",2,SCM_times);
SCM_DefinePredefinedFunctionVariable(DIFFERENCE,"-",2,SCM_minus);
SCM_DefinePredefinedFunctionVariable(PLUS,"+",2,SCM_plus);
SCM_DefinePredefinedFunctionVariable(INTEGERp,"INTEGER?",1,SCM_fixnump);
SCM_DefinePredefinedFunctionVariable(EQ,"EQ?",2,SCM_eqp);
SCM_DefinePredefinedFunctionVariable(STRINGP,"STRINGP",1,SCM_stringp);
SCM_DefinePredefinedFunctionVariable(SYMBOLP,"SYMBOL?",1,SCM_symbolp);
SCM_DefinePredefinedFunctionVariable(NULLp,"NULL?",1,SCM_nullp);
SCM_DefinePredefinedFunctionVariable(CONSP,"PAIR?",1,SCM_pairp);
SCM_DefinePredefinedFunctionVariable(RPLACD,"SET-CDR!",2,SCM_set_cdr);
SCM_DefinePredefinedFunctionVariable(RPLACA,"SET-CAR!",2,SCM_set_car);
SCM_DefinePredefinedFunctionVariable(CDR,"CDR",1,SCM_cdr);
SCM_DefinePredefinedFunctionVariable(CAR,"CAR",1,SCM_car);
SCM_DefinePredefinedFunctionVariable(CONS,"CONS",2,SCM_cons);

/* Additional functions
 */

SCM_DefinePredefinedFunctionVariable(PRINT,"PRINT",1,SCM_print);

/* Apply is special, it takes at least two arguments. It is invoked as 
 * an nary subr. List has a similar structure.
 */

/* Apply is restricted to 31 arguments, [cf. ISO-C 9899:1990 section
 * 2.2.4.1].  SCM_apply is defined as a primitive that expects at least
 * two arguments.  In fact, it is very usual to have restrictions on
 * apply and they are most of the time unseen since one usually does not
 * write such programs. Another not portable solution would be to forge
 * a va_list value and jump right into SCM_invoke.
 */

SCM SCM_apply (unsigned long number, va_list arguments) {
  SCM args[31];
  SCM last_arg;
  SCM fun = va_arg(arguments,SCM);
  unsigned long i;
  for ( i=0 ; i<number-1 ; i++  ) {
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
  case 0: return SCM_invoke(fun,0);
  case 1: return SCM_invoke(fun,1,args[0]);
  case 2: return SCM_invoke(fun,2,args[0],args[1]);
  case 3: return SCM_invoke(fun,3,args[0],args[1],args[2]);
  case 4: return SCM_invoke(fun,4,args[0],args[1],args[2],args[3]);
  case 5: return SCM_invoke(fun,5,args[0],args[1],args[2],args[3],
                            args[4]);
  case 6: return SCM_invoke(fun,6,args[0],args[1],args[2],args[3],
                            args[4],args[5]);
  case 7: return SCM_invoke(fun,7,args[0],args[1],args[2],args[3],
                            args[4],args[5],args[6]);
  case 8: return SCM_invoke(fun,8,args[0],args[1],args[2],args[3],
                            args[4],args[5],args[6],args[7]);
  case 9: return SCM_invoke(fun,9,args[0],args[1],args[2],args[3],
                             args[4],args[5],args[6],args[7],args[8]);
  case 10: return SCM_invoke(fun,10,args[0],args[1],args[2],args[3],
                             args[4],args[5],args[6],args[7],args[8],args[9]);
  case 11: return SCM_invoke(fun,11,args[0],args[1],args[2],args[3],
                             args[4],args[5],args[6],args[7],args[8],args[9],
                             args[10]);
  case 12: return SCM_invoke(fun,12,args[0],args[1],args[2],args[3],
                             args[4],args[5],args[6],args[7],args[8],args[9],
                             args[10],args[11]);
  case 13: return SCM_invoke(fun,13,args[0],args[1],args[2],args[3],
                             args[4],args[5],args[6],args[7],args[8],args[9],
                             args[10],args[11],args[12]);
  case 14: return SCM_invoke(fun,14,args[0],args[1],args[2],args[3],
                             args[4],args[5],args[6],args[7],args[8],args[9],
                             args[10],args[11],args[12],args[13]);
  /* If this were less cumbersome, I will add cases up to 31. */
  default: return SCM_error(SCM_ERR_APPLY_SIZE);
  }
}

SCM_DefinePredefinedFunctionVariable(APPLY,"APPLY",-3,SCM_apply);
SCM_DefinePredefinedFunctionVariable(LIST,"LIST",-1,SCM_list);

/* end of schemelib.c */
