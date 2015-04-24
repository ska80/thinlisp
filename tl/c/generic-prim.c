/***
 *
 * Module:      tl/c/generic-prim.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/generic-prim.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "generic-prim.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

static const Str_41 str_const_1
  = { 7, 39, 39, "Unrecognized array-type of ~s for AREF." };

/* Translated from GENERIC-AREF(T FIXNUM) = T */

Obj generic_aref (Obj array, sint32 index)
{
  sint32 temp;

  switch (TYPE_TAG(array,temp)) {
   case 6:
    return ((Sv *)array)->body[index];
   case 7:
    return BOXCHAR(((Str *)array)->body[index]);
   case 8:
    return BOXFIX(((Sa_uint8 *)array)->body[index]);
   case 9:
    return BOXFIX(((Sa_uint16 *)array)->body[index]);
   case 18:
    return BOXFIX(((Sa_sint16 *)array)->body[index]);
   case 10:
    return alloc_ldouble(((Sa_double *)array)->body[index],-1,5);
   default:
    return error_one_arg((Obj)(&str_const_1),   /* "Unrecognized array-type of ~s for AREF." */
        array);
  }
}

static const Str_45 str_const_2
  = { 7, 43, 43, "Unrecognized array-type of ~s for SET-AREF." };

/* Translated from GENERIC-SET-AREF(T FIXNUM T) = T */

Obj generic_set_aref (Obj array, sint32 index, Obj value)
{
  sint32 temp;

  switch (TYPE_TAG(array,temp)) {
   case 6:
    (((Sv *)array)->body[index]) = value;
    break;
   case 7:
    (((Str *)array)->body[index]) = UNBOXCHAR(value);
    break;
   case 8:
    (((Sa_uint8 *)array)->body[index]) = (uint8)UNBOXFIX(value);
    break;
   case 9:
    (((Sa_uint16 *)array)->body[index]) = (uint16)UNBOXFIX(value);
    break;
   case 18:
    (((Sa_sint16 *)array)->body[index]) = (sint16)UNBOXFIX(value);
    break;
   case 10:
    (((Sa_double *)array)->body[index]) = (((Ldouble *)value)->body);
    break;
   default:
    error_one_arg((Obj)(&str_const_2),          /* "Unrecognized array-type of ~s for SET-AREF." */
        array);
    break;
  }
  return value;
}

static const Str_45 str_const_3
  = { 7, 41, 41, "Unrecognized sequence type of ~s for ELT." };

/* Translated from GENERIC-ELT(T FIXNUM) = T */

Obj generic_elt (Obj sequence, sint32 index)
{
  sint32 temp;

  switch (TYPE_TAG(sequence,temp)) {
   case 0:
   case 2:
    return nth(index,sequence);
   case 6:
    return ((Sv *)sequence)->body[index];
   case 7:
    return BOXCHAR(((Str *)sequence)->body[index]);
   case 8:
    return BOXFIX(((Sa_uint8 *)sequence)->body[index]);
   case 9:
    return BOXFIX(((Sa_uint16 *)sequence)->body[index]);
   case 18:
    return BOXFIX(((Sa_sint16 *)sequence)->body[index]);
   case 10:
    return alloc_ldouble(((Sa_double *)sequence)->body[index],-1,5);
   default:
    return error_one_arg((Obj)(&str_const_3),   /* "Unrecognized sequence type of ~s for ELT." */
        sequence);
  }
}

static const Str_49 str_const_4
  = { 7, 45, 45, "Unrecognized sequence type of ~s for SET-ELT." };

/* Translated from GENERIC-SET-ELT(T FIXNUM T) = T */

Obj generic_set_elt (Obj sequence, sint32 index, Obj value)
{
  sint32 temp;

  switch (TYPE_TAG(sequence,temp)) {
   case 0:
   case 2:
    CAR(nthcdr(index,sequence)) = value;
    break;
   case 6:
    (((Sv *)sequence)->body[index]) = value;
    break;
   case 7:
    (((Str *)sequence)->body[index]) = UNBOXCHAR(value);
    break;
   case 8:
    (((Sa_uint8 *)sequence)->body[index]) = (uint8)UNBOXFIX(value);
    break;
   case 9:
    (((Sa_uint16 *)sequence)->body[index]) = (uint16)UNBOXFIX(value);
    break;
   case 18:
    (((Sa_sint16 *)sequence)->body[index]) = (sint16)UNBOXFIX(value);
    break;
   case 10:
    (((Sa_double *)sequence)->body[index]) = (((Ldouble *)value)->body);
    break;
   default:
    error_one_arg((Obj)(&str_const_4),          /* "Unrecognized sequence type of ~s for SET-ELT." */
        sequence);
    break;
  }
  return value;
}

/* Translated from SYMS-TL-GENERIC-PRIM() = VOID */

void syms_tl_generic_prim (void)
{
  return;
}


/* Translated from INIT-TL-GENERIC-PRIM() = VOID */

void init_tl_generic_prim (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  return;
}

