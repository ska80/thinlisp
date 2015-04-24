/***
 *
 * Module:      tl/c/boot.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/boot.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "boot.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

static Obj cons_const[38]
  = 
#if defined(NO_ADDRESS_CONSTANTS)
  {(Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, 
      (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, 
      (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, 
      (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, 
      (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, 
      (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, 
      (Obj)NULL}
#else
  {(Obj)(tl_boot_symbols+4), (Obj)(((uint32)(&(cons_const[2])))     /* BOOT */
      +2), (Obj)(tl_boot_symbols+5), (Obj)(((uint32)(&(cons_const[4])))     /* STUBS */
      +2), (Obj)(tl_boot_symbols+6), (Obj)(((uint32)(&(     /* TL-TYPES */
      cons_const[6])))+2), (Obj)(tl_boot_symbols+7), (Obj)((    /* INLINE */
      (uint32)(&(cons_const[8])))+2), (Obj)(tl_boot_symbols+8),     /* TL-PRIM */
      (Obj)(((uint32)(&(cons_const[10])))+2), (Obj)(tl_boot_symbols+9),     /* DO */
      (Obj)(((uint32)(&(cons_const[12])))+2), (Obj)(tl_boot_symbols+10),    /* FORMAT */
      (Obj)(((uint32)(&(cons_const[14])))+2), (Obj)(tl_boot_symbols+11),    /* INPUT */
      (Obj)(((uint32)(&(cons_const[16])))+2), (Obj)(tl_boot_symbols+12),    /* TL-BASICS */
      (Obj)(((uint32)(&(cons_const[18])))+2), (Obj)(tl_boot_symbols+13),    /* LOOP */
      (Obj)(((uint32)(&(cons_const[20])))+2), (Obj)(tl_boot_symbols+14),    /* APPLY */
      (Obj)(((uint32)(&(cons_const[22])))+2), (Obj)(tl_boot_symbols+15),    /* GENERIC-MATH */
      (Obj)(((uint32)(&(cons_const[24])))+2), (Obj)(tl_boot_symbols+16),    /* GENERIC-PRIM */
      (Obj)(((uint32)(&(cons_const[26])))+2), (Obj)(tl_boot_symbols+17),    /* PACKAGES */
      (Obj)(((uint32)(&(cons_const[28])))+2), (Obj)(tl_boot_symbols+18),    /* TL-UTIL */
      (Obj)(((uint32)(&(cons_const[30])))+2), (Obj)(tl_boot_symbols+19),    /* VERSIONS */
      (Obj)(((uint32)(&(cons_const[32])))+2), (Obj)(tl_boot_symbols+20),    /* FORWARD */
      (Obj)(((uint32)(&(cons_const[34])))+2), (Obj)(tl_boot_symbols+21),    /* TL-EXTENSION */
      (Obj)(((uint32)(&(cons_const[36])))+2), (Obj)(tl_boot_symbols+22),    /* TL-TIME */
      (Obj)NULL}
#endif
  ;

static const Str_9 str_const_1
  = { 7, 7, 7, "TL-USER" };

static const Str_9 str_const_2
  = { 7, 7, 7, "KEYWORD" };

static const Str_17 str_const_3
  = { 7, 16, 16, "SYSTEM-NICKNAMES" };

static const Str_21 str_const_4
  = { 7, 19, 19, "SYSTEM-USED-SYSTEMS" };

static const Str_17 str_const_5
  = { 7, 14, 14, "SYSTEM-MODULES" };

static const Str_5 str_const_6
  = { 7, 4, 4, "BOOT" };

static const Str_9 str_const_7
  = { 7, 5, 5, "STUBS" };

static const Str_9 str_const_8
  = { 7, 8, 8, "TL-TYPES" };

static const Str_9 str_const_9
  = { 7, 6, 6, "INLINE" };

static const Str_9 str_const_10
  = { 7, 7, 7, "TL-PRIM" };

static const Str_5 str_const_11
  = { 7, 2, 2, "DO" };

static const Str_9 str_const_12
  = { 7, 6, 6, "FORMAT" };

static const Str_9 str_const_13
  = { 7, 5, 5, "INPUT" };

static const Str_13 str_const_14
  = { 7, 9, 9, "TL-BASICS" };

static const Str_5 str_const_15
  = { 7, 4, 4, "LOOP" };

static const Str_9 str_const_16
  = { 7, 5, 5, "APPLY" };

static const Str_13 str_const_17
  = { 7, 12, 12, "GENERIC-MATH" };

static const Str_13 str_const_18
  = { 7, 12, 12, "GENERIC-PRIM" };

static const Str_9 str_const_19
  = { 7, 8, 8, "PACKAGES" };

static const Str_9 str_const_20
  = { 7, 7, 7, "TL-UTIL" };

static const Str_9 str_const_21
  = { 7, 8, 8, "VERSIONS" };

static const Str_9 str_const_22
  = { 7, 7, 7, "FORWARD" };

static const Str_13 str_const_23
  = { 7, 12, 12, "TL-EXTENSION" };

static const Str_9 str_const_24
  = { 7, 7, 7, "TL-TIME" };

Sym tl_boot_symbols[23];

/* Translated from SYMS-TL-BOOT() = VOID */

void syms_tl_boot (void)
{
  Obj cached_tl_user_package, cached_keyword_package, cached_tl_package;

  cached_tl_user_package = find_package_1((Obj)(&str_const_1));     /* "TL-USER" */
  init_symbol_into_package((Obj)(&(tl_boot_symbols[0])),(Obj)(&str_const),  /* "TL" */
      228,cached_tl_user_package);
  cached_keyword_package = find_package_1((Obj)(&str_const_2));     /* "KEYWORD" */
  init_symbol_into_package((Obj)(&(tl_boot_symbols[1])),(Obj)(&str_const_3),    /* "SYSTEM-NICKNAMES" */
      53782,cached_keyword_package);
  (tl_boot_symbols[1]).external = 1;
  (tl_boot_symbols[1]).symbol_value = (Obj)(&(tl_boot_symbols[1]));
  init_symbol_into_package((Obj)(&(tl_boot_symbols[2])),(Obj)(&str_const_4),    /* "SYSTEM-USED-SYSTEMS" */
      5299,cached_keyword_package);
  (tl_boot_symbols[2]).external = 1;
  (tl_boot_symbols[2]).symbol_value = (Obj)(&(tl_boot_symbols[2]));
  init_symbol_into_package((Obj)(&(tl_boot_symbols[3])),(Obj)(&str_const_5),    /* "SYSTEM-MODULES" */
      62509,cached_keyword_package);
  (tl_boot_symbols[3]).external = 1;
  (tl_boot_symbols[3]).symbol_value = (Obj)(&(tl_boot_symbols[3]));
  init_symbol_into_package((Obj)(&(tl_boot_symbols[4])),(Obj)(&str_const_6),    /* "BOOT" */
      998,cached_tl_user_package);
  init_symbol_into_package((Obj)(&(tl_boot_symbols[5])),(Obj)(&str_const_7),    /* "STUBS" */
      1555,cached_tl_user_package);
  init_symbol_into_package((Obj)(&(tl_boot_symbols[6])),(Obj)(&str_const_8),    /* "TL-TYPES" */
      15025,cached_tl_user_package);
  cached_tl_package = find_package_1((Obj)(&str_const));    /* "TL" */
  init_symbol_into_package((Obj)(&(tl_boot_symbols[7])),(Obj)(&str_const_9),    /* "INLINE" */
      3677,cached_tl_package);
  (tl_boot_symbols[7]).external = 1;
  init_symbol_into_package((Obj)(&(tl_boot_symbols[8])),(Obj)(&str_const_10),   /* "TL-PRIM" */
      7495,cached_tl_user_package);
  init_symbol_into_package((Obj)(&(tl_boot_symbols[9])),(Obj)(&str_const_11),   /* "DO" */
      199,cached_tl_package);
  (tl_boot_symbols[9]).external = 1;
  init_symbol_into_package((Obj)(&(tl_boot_symbols[10])),(Obj)(&str_const_12),  /* "FORMAT" */
      3906,cached_tl_package);
  (tl_boot_symbols[10]).external = 1;
  init_symbol_into_package((Obj)(&(tl_boot_symbols[11])),(Obj)(&str_const_13),  /* "INPUT" */
      1886,cached_tl_user_package);
  init_symbol_into_package((Obj)(&(tl_boot_symbols[12])),(Obj)(&str_const_14),  /* "TL-BASICS" */
      30329,cached_tl_user_package);
  init_symbol_into_package((Obj)(&(tl_boot_symbols[13])),(Obj)(&str_const_15),  /* "LOOP" */
      914,cached_tl_package);
  (tl_boot_symbols[13]).external = 1;
  init_symbol_into_package((Obj)(&(tl_boot_symbols[14])),(Obj)(&str_const_16),  /* "APPLY" */
      1809,cached_tl_package);
  (tl_boot_symbols[14]).external = 1;
  init_symbol_into_package((Obj)(&(tl_boot_symbols[15])),(Obj)(&str_const_17),  /* "GENERIC-MATH" */
      51071,cached_tl_user_package);
  init_symbol_into_package((Obj)(&(tl_boot_symbols[16])),(Obj)(&str_const_18),  /* "GENERIC-PRIM" */
      51172,cached_tl_user_package);
  init_symbol_into_package((Obj)(&(tl_boot_symbols[17])),(Obj)(&str_const_19),  /* "PACKAGES" */
      14173,cached_tl_user_package);
  init_symbol_into_package((Obj)(&(tl_boot_symbols[18])),(Obj)(&str_const_20),  /* "TL-UTIL" */
      7542,cached_tl_user_package);
  init_symbol_into_package((Obj)(&(tl_boot_symbols[19])),(Obj)(&str_const_21),  /* "VERSIONS" */
      13963,cached_tl_user_package);
  init_symbol_into_package((Obj)(&(tl_boot_symbols[20])),(Obj)(&str_const_22),  /* "FORWARD" */
      7708,cached_tl_user_package);
  init_symbol_into_package((Obj)(&(tl_boot_symbols[21])),(Obj)(&str_const_23),  /* "TL-EXTENSION" */
      47407,cached_tl_user_package);
  init_symbol_into_package((Obj)(&(tl_boot_symbols[22])),(Obj)(&str_const_24),  /* "TL-TIME" */
      7435,cached_tl_user_package);
  return;
}


/* Translated from INIT-TL-BOOT() = VOID */

void init_tl_boot (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  SET_GLOBAL(current_system_being_loaded,(Obj)(tl_boot_symbols+0));     /* TL */
  SET_GLOBAL(all_systems,alloc_cons((Obj)(tl_boot_symbols+0),   /* TL */
      GET_GLOBAL(all_systems),0));
  set_get((Obj)(tl_boot_symbols+0),(Obj)(tl_boot_symbols+1),    /* TL, SYSTEM-NICKNAMES */
      (Obj)NULL);
  set_get((Obj)(tl_boot_symbols+0),(Obj)(tl_boot_symbols+2),    /* TL, SYSTEM-USED-SYSTEMS */
      (Obj)NULL);
#if defined(NO_ADDRESS_CONSTANTS)
  {
    hook_up_cdrs(cons_const,19,(Obj)NULL);
    (cons_const[0]) = (Obj)(tl_boot_symbols+4);     /* BOOT */
    (cons_const[2]) = (Obj)(tl_boot_symbols+5);     /* STUBS */
    (cons_const[4]) = (Obj)(tl_boot_symbols+6);     /* TL-TYPES */
    (cons_const[6]) = (Obj)(tl_boot_symbols+7);     /* INLINE */
    (cons_const[8]) = (Obj)(tl_boot_symbols+8);     /* TL-PRIM */
    (cons_const[10]) = (Obj)(tl_boot_symbols+9);    /* DO */
    (cons_const[12]) = (Obj)(tl_boot_symbols+10);   /* FORMAT */
    (cons_const[14]) = (Obj)(tl_boot_symbols+11);   /* INPUT */
    (cons_const[16]) = (Obj)(tl_boot_symbols+12);   /* TL-BASICS */
    (cons_const[18]) = (Obj)(tl_boot_symbols+13);   /* LOOP */
    (cons_const[20]) = (Obj)(tl_boot_symbols+14);   /* APPLY */
    (cons_const[22]) = (Obj)(tl_boot_symbols+15);   /* GENERIC-MATH */
    (cons_const[24]) = (Obj)(tl_boot_symbols+16);   /* GENERIC-PRIM */
    (cons_const[26]) = (Obj)(tl_boot_symbols+17);   /* PACKAGES */
    (cons_const[28]) = (Obj)(tl_boot_symbols+18);   /* TL-UTIL */
    (cons_const[30]) = (Obj)(tl_boot_symbols+19);   /* VERSIONS */
    (cons_const[32]) = (Obj)(tl_boot_symbols+20);   /* FORWARD */
    (cons_const[34]) = (Obj)(tl_boot_symbols+21);   /* TL-EXTENSION */
    (cons_const[36]) = (Obj)(tl_boot_symbols+22);   /* TL-TIME */
  }
#endif
  set_get((Obj)(tl_boot_symbols+0),(Obj)(tl_boot_symbols+3),    /* TL, SYSTEM-MODULES */
      (Obj)(((uint32)(&(cons_const[0])))+2));
  return;
}

