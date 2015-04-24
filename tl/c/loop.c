/***
 *
 * Module:      tl/c/loop.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/loop.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "loop.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

/* Translated from SYMS-TL-LOOP() = VOID */

void syms_tl_loop (void)
{
  return;
}


/* Translated from INIT-TL-LOOP() = VOID */

void init_tl_loop (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  return;
}

