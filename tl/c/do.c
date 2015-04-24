/***
 *
 * Module:      tl/c/do.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/do.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "do.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

/* Translated from SYMS-TL-DO() = VOID */

void syms_tl_do (void)
{
  return;
}


/* Translated from INIT-TL-DO() = VOID */

void init_tl_do (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  return;
}

