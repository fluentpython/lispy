/* Compiler to C $Revision: 1.23 $ 
(BEGIN
  (SET! INDEX 1)
  ((LAMBDA
     (CNTER . TMP)
     (SET! TMP (CNTER (LAMBDA (I) (LAMBDA X (CONS I X)))))
     (IF CNTER (CNTER TMP) INDEX))
    (LAMBDA (F) (SET! INDEX (+ 1 INDEX)) (F INDEX))
    'FOO))  */

#include "scheme.h"

/* Global environment: */
SCM_DefineGlobalVariable(INDEX,"INDEX");

/* Quotations: */
#define thing3 SCM_nil	/* () */
SCM_DefineString(thing4_object,"FOO");
#define thing4 SCM_Wrap(&thing4_object)
SCM_DefineSymbol(thing2_object,thing4);       /* FOO */
#define thing2 SCM_Wrap(&thing2_object)
#define thing1 SCM_Int2fixnum(1)
#define thing0 thing1	/* 1 */

/* Functions: */
SCM_DefineClosure(function_0, SCM I; );

SCM_DeclareFunction(function_0) {
  SCM_DeclareLocalVariable(v_25,0);
  SCM_DeclareLocalDottedVariable(X,1);
  SCM v_27; SCM v_26; 
  return (v_26=SCM_Free(I),
          (v_27=X,
           SCM_invoke1(v_25,
                       SCM_cons(v_26,
                                v_27))));
}

SCM_DefineClosure(function_1, );

SCM_DeclareFunction(function_1) {
  SCM_DeclareLocalVariable(v_24,0);
  SCM_DeclareLocalVariable(I,1);
  return SCM_invoke1(v_24,
                     SCM_close(function_0,-2,1,I));
}

SCM_DefineClosure(function_2, SCM v_15; SCM CNTER; SCM TMP; );

SCM_DeclareFunction(function_2) {
  SCM_DeclareLocalVariable(v_21,0);
  SCM v_20; SCM v_19; SCM v_18; SCM v_17; 
  return (v_17=(SCM_Content(SCM_Free(TMP))=v_21),
          (v_18=SCM_Free(CNTER),
           ((v_18 != SCM_false)
            ? (v_19=SCM_Free(CNTER),
               (v_20=SCM_Content(SCM_Free(TMP)),
                SCM_invoke2(v_19,
                            SCM_Free(v_15),
                            v_20)))
            : SCM_invoke1(SCM_Free(v_15),
                          SCM_CheckedGlobal(INDEX)))));
}

SCM_DefineClosure(function_3, );

SCM_DeclareFunction(function_3) {
  SCM_DeclareLocalVariable(v_15,0);
  SCM_DeclareLocalVariable(CNTER,1);
  SCM_DeclareLocalVariable(TMP,2);
  SCM v_23; SCM v_22; SCM v_16; 
  return (v_16=TMP= SCM_allocate_box(TMP),
          (v_22=CNTER,
           (v_23=SCM_close(function_1,2,0),
            SCM_invoke2(v_22,
                        SCM_close(function_2,1,3,v_15,CNTER,TMP),
                        v_23))));
}

SCM_DefineClosure(function_4, );

SCM_DeclareFunction(function_4) {
  SCM_DeclareLocalVariable(v_8,0);
  SCM_DeclareLocalVariable(F,1);
  SCM v_11; SCM v_10; SCM v_9; SCM v_12; SCM v_14; SCM v_13; 
  return (v_13=thing1,
          (v_14=SCM_CheckedGlobal(INDEX),
           (v_12=SCM_Plus(v_13,
                          v_14),
            (v_9=(INDEX=v_12),
             (v_10=F,
              (v_11=SCM_CheckedGlobal(INDEX),
               SCM_invoke2(v_10,
                           v_8,
                           v_11)))))));
}

SCM_DefineClosure(function_5, );

SCM_DeclareFunction(function_5) {
  SCM_DeclareLocalVariable(v_1,0);
  return v_1;
}

SCM_DefineClosure(function_6, );

SCM_DeclareFunction(function_6) {
  SCM v_5; SCM v_7; SCM v_6; SCM v_4; SCM v_3; SCM v_2; SCM v_28; 
  return (v_28=thing0,
          (v_2=(INDEX=v_28),
           (v_3=SCM_close(function_3,3,0),
            (v_4=SCM_close(function_4,2,0),
             (v_6=thing2,
              (v_7=thing3,
               (v_5=SCM_cons(v_6,
                             v_7),
                SCM_invoke3(v_3,
                            SCM_close(function_5,1,0),
                            v_4,
                            v_5))))))));
}


/* Expression: */
void main(argc,argv) {
  SCM r;
  int i;
  for ( i=0; i<=10000; i++) r = (SCM_invoke0(SCM_close(function_6,0,0)));
  SCM_print(r);
  exit(0);
}

/* End of generated code. */
