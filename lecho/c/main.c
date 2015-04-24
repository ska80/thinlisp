/***
 *
 * Module:      lecho/c/main.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of lecho/lisp/main.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "main.h"


static const Str_9 str_const
  = { 7, 7, 7, "KEYWORD" };

static const Str_5 str_const_1
  = { 7, 1, 1, "T" };

static const Str_5 str_const_2
  = { 7, 2, 2, "TL" };

static const Str_9 str_const_3
  = { 7, 7, 7, "TL-USER" };

static const Str_13 str_const_4
  = { 7, 11, 11, "COMMON-LISP" };

int main (int argc, char **argv)
{
  Obj lisp_argv, string_temp;
  sint32 index;

  malloc_block_into_region(0,65536,1);
  malloc_block_into_region(1,65536,1);
  malloc_block_into_region(2,65536,1);
  all_packages = (Obj)NULL;
  make_package_1(((Str *)(&str_const))->body,(Obj)NULL);    /* "KEYWORD" */
  init_symbol_into_package((Obj)(&T),(Obj)(&str_const_1),84,    /* "T" */
      make_package_1(((Str *)(&str_const_2))->body,(Obj)NULL));     /* "TL" */
  T.symbol_value = (&T);
  T.external = 1;
  make_package_1(((Str *)(&str_const_3))->body,alloc_cons(  /* "TL-USER" */
      (Obj)(&str_const_2),(Obj)NULL,0));        /* "TL" */
  make_package_1(((Str *)(&str_const_4))->body,(Obj)NULL);  /* "COMMON-LISP" */
  syms_tl_boot();
  syms_tl_stubs();
  syms_tl_tl_types();
  syms_tl_inline();
  syms_tl_tl_prim();
  syms_tl_do();
  syms_tl_format();
  syms_tl_input();
  syms_tl_tl_basics();
  syms_tl_loop();
  syms_tl_apply();
  syms_tl_generic_math();
  syms_tl_generic_prim();
  syms_tl_packages();
  syms_tl_tl_util();
  syms_tl_versions();
  syms_tl_forward();
  syms_tl_tl_extension();
  syms_tl_tl_time();
  syms_lecho_boot();
  syms_lecho_echo();
  init_tl_boot();
  init_tl_stubs();
  init_tl_tl_types();
  init_tl_inline();
  init_tl_tl_prim();
  init_tl_do();
  init_tl_format();
  init_tl_input();
  init_tl_tl_basics();
  init_tl_loop();
  init_tl_apply();
  init_tl_generic_math();
  init_tl_generic_prim();
  init_tl_packages();
  init_tl_tl_util();
  init_tl_versions();
  init_tl_forward();
  init_tl_tl_extension();
  init_tl_tl_time();
  init_lecho_boot();
  init_lecho_echo();
  lisp_argv = (Obj)NULL;
  for (index = (argc-1);index>=0;index--) {
    string_temp = alloc_string(strlen(argv[index]),0,7);
    strcpy((char *)(((Str *)string_temp)->body),argv[index]);
    lisp_argv = alloc_cons(string_temp,lisp_argv,0);
  }
  main_1(lisp_argv);
  return 0;
}

/* Translated from SYMS-LECHO-MAIN() = VOID */

void syms_lecho_main (void)
{
  return;
}


/* Translated from INIT-LECHO-MAIN() = VOID */

void init_lecho_main (void)
{
  return;
}

