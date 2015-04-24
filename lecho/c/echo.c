/***
 *
 * Module:      lecho/c/echo.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of lecho/lisp/echo.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "echo.h"


static const Str_9 str_const
  = { 7, 5, 5, "LECHO" };

static const Str_9 str_const_1
  = { 7, 6, 6, "--help" };

static const Str_69 str_const_2
  = { 7, 66, 66, "Usage: lecho [arg] ...~%  all arguments will be echoed to stdout~%" };

static const Str_5 str_const_3
  = { 7, 2, 2, "-n" };

/* Translated from MAIN(T) = FIXNUM */

sint32 main_1 (Obj args)
{
  Obj g;
  unsigned char *g_1;
  unsigned char *g_2;
  int temp;
  Obj terpriP;
  unsigned char *g_3;
  unsigned char *g_4;
  Obj firstP, arg, tl_loop_list_;

  g = ((args!=NULL) ? CAR(args) : (Obj)NULL);
  args = ((args!=NULL) ? CDR(args) : (Obj)NULL);
  (void)g;
  if (args!=NULL) {
    g_1 = coerce_to_string((args!=NULL) ? CAR(args) : (Obj)NULL);
    g_2 = (((Str *)(&str_const_1))->body);      /* "--help" */
    temp = (strcmp((char *)g_1,(char *)g_2)==0);
  }
  else 
    temp = 0;
  if (temp) {
    format_function((Obj)(&T),((Str *)(&str_const_2))->body,    /* "Usage: lecho [arg] ...~%  all arguments will be ec..." */
        (Obj)NULL);
    return -1;
  }
  else {
    g_3 = coerce_to_string((args!=NULL) ? CAR(args) : (Obj)NULL);
    g_4 = (((Str *)(&str_const_3))->body);      /* "-n" */
    if (strcmp((char *)g_3,(char *)g_4)==0) {
      g = ((args!=NULL) ? CAR(args) : (Obj)NULL);
      args = ((args!=NULL) ? CDR(args) : (Obj)NULL);
      (void)g;
      terpriP = (Obj)NULL;
    }
    else 
      terpriP = (Obj)(&T);
    firstP = (Obj)(&T);
    arg = (Obj)NULL;
    tl_loop_list_ = args;
    for (;tl_loop_list_!=NULL;firstP = (Obj)NULL) {
      arg = CAR(tl_loop_list_);
      tl_loop_list_ = CDR(tl_loop_list_);
      if (firstP==NULL) 
        write_char(' ',(Obj)NULL);
      write_string_function(((Str *)arg)->body,(Obj)NULL,0,(Obj)NULL);
    }
    if (terpriP!=NULL) 
      terpri((Obj)NULL);
    return 0;
  }
}

/* Translated from SYMS-LECHO-ECHO() = VOID */

void syms_lecho_echo (void)
{
  return;
}


/* Translated from INIT-LECHO-ECHO() = VOID */

void init_lecho_echo (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "LECHO" */
  return;
}

