/***
 *
 * Module:      tl/c/tl-time.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/tl-time.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "tl-time.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

/* Translated from GET-INTERNAL-REAL-TIME() = FIXNUM */

sint32 get_internal_real_time (void)
{
  return (sint32)cronometer();
}

/* Translated from GET-INTERNAL-RUN-TIME() = FIXNUM */

sint32 get_internal_run_time (void)
{
  return (sint32)cpu_run_time();
}

Obj internal_time_units_per_second = (Obj)(&Unbound);

/* Translated from SLEEP(DOUBLE-FLOAT) = T */

Obj sleep_1 (double seconds)
{
  sleep_ticks((long)(sint32)floor(seconds*(double)UNBOXFIX(GET_GLOBAL(internal_time_units_per_second))));
  return (Obj)NULL;
}

/* Translated from SYMS-TL-TL-TIME() = VOID */

void syms_tl_tl_time (void)
{
  return;
}


/* Translated from INIT-TL-TL-TIME() = VOID */

void init_tl_tl_time (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  init_cronometer();
  internal_time_units_per_second = BOXFIX(ticks_per_second());
  return;
}

