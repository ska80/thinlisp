/***
 *
 * Module:      tl/c/generic-math.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/generic-math.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "generic-math.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

static const Str_49 str_const_1
  = { 7, 47, 47, "Non-number argument to ~a: arg1 = ~a, arg2 = ~a" };

/* Translated from MATH-TYPE-ERROR(T T T) = VOID */

void math_type_error (Obj op_string, Obj number1, Obj number2)
{
  error_three_args((Obj)(&str_const_1),         /* "Non-number argument to ~a: arg1 = ~a, arg2 = ~a" */
      op_string,number1,number2);
  return;
}

static const Str_33 str_const_2
  = { 7, 29, 29, "Non-number argument to ~a: ~a" };

/* Translated from MATH-ONE-ARG-TYPE-ERROR(T T) = VOID */

void math_one_arg_type_error (Obj op_string, Obj number)
{
  error_two_args((Obj)(&str_const_2),           /* "Non-number argument to ~a: ~a" */
      op_string,number);
  return;
}

static const Str_61 str_const_3
  = { 7, 59, 59, "Overflowed the range of integers in the following: ~a ~a ~a" };

/* Translated from FIXNUM-OVERFLOW-ERROR(T T T) = VOID */

void fixnum_overflow_error (Obj op_string, Obj number1, Obj number2)
{
  error_three_args((Obj)(&str_const_3),         /* "Overflowed the range of integers in the following:..." */
      number1,op_string,number2);
  return;
}

/* Translated from GENERIC-LESS-THAN(NUMBER NUMBER) = T */

Obj generic_less_than (Obj arg1, Obj arg2)
{
  if (IMMED_TAG(arg1)==1) {                     /* Fixnump */
    if (IMMED_TAG(arg2)==1)                     /* Fixnump */
      return (((sint32)arg1)<(sint32)arg2) ? ((Obj)(&T)) : (Obj)NULL;
    else 
      return (((double)UNBOXFIX(arg1))<(((Ldouble *)arg2)->body)) ? ((Obj)(
          &T)) : (Obj)NULL;
  }
  else if (IMMED_TAG(arg2)==1)                  /* Fixnump */
    return (((Ldouble *)arg1)->body<(double)UNBOXFIX(arg2)) ? ((Obj)(&T)) 
        : (Obj)NULL;
  else 
    return (((Ldouble *)arg1)->body<(((Ldouble *)arg2)->body)) ? ((Obj)(
        &T)) : (Obj)NULL;
}

/* Translated from GENERIC-GREATER-THAN(NUMBER NUMBER) = T */

Obj generic_greater_than (Obj arg1, Obj arg2)
{
  if (IMMED_TAG(arg1)==1) {                     /* Fixnump */
    if (IMMED_TAG(arg2)==1)                     /* Fixnump */
      return (((sint32)arg1)>(sint32)arg2) ? ((Obj)(&T)) : (Obj)NULL;
    else 
      return (((double)UNBOXFIX(arg1))>(((Ldouble *)arg2)->body)) ? ((Obj)(
          &T)) : (Obj)NULL;
  }
  else if (IMMED_TAG(arg2)==1)                  /* Fixnump */
    return (((Ldouble *)arg1)->body>(double)UNBOXFIX(arg2)) ? ((Obj)(&T)) 
        : (Obj)NULL;
  else 
    return (((Ldouble *)arg1)->body>(((Ldouble *)arg2)->body)) ? ((Obj)(
        &T)) : (Obj)NULL;
}

/* Translated from GENERIC-LESS-THAN-OR-EQUAL(NUMBER NUMBER) = T */

Obj generic_less_than_or_equal (Obj arg1, Obj arg2)
{
  if (IMMED_TAG(arg1)==1) {                     /* Fixnump */
    if (IMMED_TAG(arg2)==1)                     /* Fixnump */
      return (((sint32)arg1)<=(sint32)arg2) ? ((Obj)(&T)) : (Obj)NULL;
    else 
      return (((double)UNBOXFIX(arg1))<=(((Ldouble *)arg2)->body)) ? ((Obj)(
          &T)) : (Obj)NULL;
  }
  else if (IMMED_TAG(arg2)==1)                  /* Fixnump */
    return (((Ldouble *)arg1)->body<=(double)UNBOXFIX(arg2)) ? ((Obj)(&T)) 
        : (Obj)NULL;
  else 
    return (((Ldouble *)arg1)->body<=(((Ldouble *)arg2)->body)) ? ((Obj)(
        &T)) : (Obj)NULL;
}

/* Translated from GENERIC-GREATER-THAN-OR-EQUAL(NUMBER NUMBER) = T */

Obj generic_greater_than_or_equal (Obj arg1, Obj arg2)
{
  if (IMMED_TAG(arg1)==1) {                     /* Fixnump */
    if (IMMED_TAG(arg2)==1)                     /* Fixnump */
      return (((sint32)arg1)>=(sint32)arg2) ? ((Obj)(&T)) : (Obj)NULL;
    else 
      return (((double)UNBOXFIX(arg1))>=(((Ldouble *)arg2)->body)) ? ((Obj)(
          &T)) : (Obj)NULL;
  }
  else if (IMMED_TAG(arg2)==1)                  /* Fixnump */
    return (((Ldouble *)arg1)->body>=(double)UNBOXFIX(arg2)) ? ((Obj)(&T)) 
        : (Obj)NULL;
  else 
    return (((Ldouble *)arg1)->body>=(((Ldouble *)arg2)->body)) ? ((Obj)(
        &T)) : (Obj)NULL;
}

/* Translated from GENERIC-NUMERIC-EQUAL(NUMBER NUMBER) = T */

Obj generic_numeric_equal (Obj arg1, Obj arg2)
{
  if (IMMED_TAG(arg1)==1) {                     /* Fixnump */
    if (IMMED_TAG(arg2)==1)                     /* Fixnump */
      return (((sint32)arg1)==(sint32)arg2) ? ((Obj)(&T)) : (Obj)NULL;
    else 
      return (((double)UNBOXFIX(arg1))==(((Ldouble *)arg2)->body)) ? ((Obj)(
          &T)) : (Obj)NULL;
  }
  else if (IMMED_TAG(arg2)==1)                  /* Fixnump */
    return (((Ldouble *)arg1)->body==(double)UNBOXFIX(arg2)) ? ((Obj)(&T)) 
        : (Obj)NULL;
  else 
    return (((Ldouble *)arg1)->body==(((Ldouble *)arg2)->body)) ? ((Obj)(
        &T)) : (Obj)NULL;
}

/* Translated from GENERIC-NUMERIC-NOT-EQUAL(NUMBER NUMBER) = T */

Obj generic_numeric_not_equal (Obj arg1, Obj arg2)
{
  if (IMMED_TAG(arg1)==1) {                     /* Fixnump */
    if (IMMED_TAG(arg2)==1)                     /* Fixnump */
      return (((sint32)arg1)!=(sint32)arg2) ? ((Obj)(&T)) : (Obj)NULL;
    else 
      return (((double)UNBOXFIX(arg1))!=(((Ldouble *)arg2)->body)) ? ((Obj)(
          &T)) : (Obj)NULL;
  }
  else if (IMMED_TAG(arg2)==1)                  /* Fixnump */
    return (((Ldouble *)arg1)->body!=(double)UNBOXFIX(arg2)) ? ((Obj)(&T)) 
        : (Obj)NULL;
  else 
    return (((Ldouble *)arg1)->body!=(((Ldouble *)arg2)->body)) ? ((Obj)(
        &T)) : (Obj)NULL;
}

static const Str_5 str_const_4
  = { 7, 1, 1, "+" };

/* Translated from GENERIC-PLUS(NUMBER NUMBER) = NUMBER */

Obj generic_plus (Obj number1, Obj number2)
{
  sint32 temp, temp_1;
  Obj temp_2;
  sint32 temp_3;
  Obj temp_4;

  switch (TYPE_TAG(number1,temp)) {
   case 1:
    switch (TYPE_TAG(number2,temp_1)) {
     case 1:
      temp_2 = (Obj)((((sint32)number1)+(sint32)number2)-1);    /* Fixnum add */
      break;
     case 5:
      temp_2 = alloc_ldouble(((double)UNBOXFIX(number1))+(((Ldouble *)number2)->body),
          -1,5);
      break;
     default:
      math_type_error((Obj)(&str_const_4),number1,number2);     /* "+" */
      temp_2 = BOXFIX(0);
      break;
    }
    return temp_2;
   case 5:
    switch (TYPE_TAG(number2,temp_3)) {
     case 1:
      temp_4 = alloc_ldouble(((Ldouble *)number1)->body+(double)UNBOXFIX(number2),
          -1,5);
      break;
     case 5:
      temp_4 = alloc_ldouble(((Ldouble *)number1)->body+(((Ldouble *)number2)->body),
          -1,5);
      break;
     default:
      math_type_error((Obj)(&str_const_4),number1,number2);     /* "+" */
      temp_4 = BOXFIX(0);
      break;
    }
    return temp_4;
   default:
    math_type_error((Obj)(&str_const_4),number1,number2);   /* "+" */
    return BOXFIX(0);
  }
}

static const Str_5 str_const_5
  = { 7, 1, 1, "-" };

/* Translated from GENERIC-MINUS(NUMBER NUMBER) = NUMBER */

Obj generic_minus (Obj number1, Obj number2)
{
  sint32 temp, temp_1;
  Obj temp_2;
  sint32 temp_3;
  Obj temp_4;

  switch (TYPE_TAG(number1,temp)) {
   case 1:
    switch (TYPE_TAG(number2,temp_1)) {
     case 1:
      temp_2 = (Obj)((((sint32)number1)-(sint32)number2)+1);    /* Fixnum subtract */
      break;
     case 5:
      temp_2 = alloc_ldouble(((double)UNBOXFIX(number1))-(((Ldouble *)number2)->body),
          -1,5);
      break;
     default:
      math_type_error((Obj)(&str_const_5),number1,number2);     /* "-" */
      temp_2 = BOXFIX(0);
      break;
    }
    return temp_2;
   case 5:
    switch (TYPE_TAG(number2,temp_3)) {
     case 1:
      temp_4 = alloc_ldouble(((Ldouble *)number1)->body-(double)UNBOXFIX(number2),
          -1,5);
      break;
     case 5:
      temp_4 = alloc_ldouble(((Ldouble *)number1)->body-(((Ldouble *)number2)->body),
          -1,5);
      break;
     default:
      math_type_error((Obj)(&str_const_5),number1,number2);     /* "-" */
      temp_4 = BOXFIX(0);
      break;
    }
    return temp_4;
   default:
    math_type_error((Obj)(&str_const_5),number1,number2);   /* "-" */
    return BOXFIX(0);
  }
}

static const Str_5 str_const_6
  = { 7, 1, 1, "*" };

/* Translated from GENERIC-MULTIPLY(NUMBER NUMBER) = NUMBER */

Obj generic_multiply (Obj number1, Obj number2)
{
  sint32 temp, temp_1;
  Obj temp_2;
  sint32 temp_3;
  Obj temp_4;

  switch (TYPE_TAG(number1,temp)) {
   case 1:
    switch (TYPE_TAG(number2,temp_1)) {
     case 1:
      temp_2 = BOXFIX(UNBOXFIX(number1)*UNBOXFIX(number2));
      break;
     case 5:
      temp_2 = alloc_ldouble(((double)UNBOXFIX(number1))*(((Ldouble *)number2)->body),
          -1,5);
      break;
     default:
      math_type_error((Obj)(&str_const_6),number1,number2);     /* "*" */
      temp_2 = BOXFIX(0);
      break;
    }
    return temp_2;
   case 5:
    switch (TYPE_TAG(number2,temp_3)) {
     case 1:
      temp_4 = alloc_ldouble(((Ldouble *)number1)->body*(double)UNBOXFIX(number2),
          -1,5);
      break;
     case 5:
      temp_4 = alloc_ldouble(((Ldouble *)number1)->body*(((Ldouble *)number2)->body),
          -1,5);
      break;
     default:
      math_type_error((Obj)(&str_const_6),number1,number2);     /* "*" */
      temp_4 = BOXFIX(0);
      break;
    }
    return temp_4;
   default:
    math_type_error((Obj)(&str_const_6),number1,number2);   /* "*" */
    return BOXFIX(0);
  }
}

static const Str_5 str_const_7
  = { 7, 1, 1, "/" };

/* Translated from GENERIC-DIVIDE(NUMBER NUMBER) = NUMBER */

Obj generic_divide (Obj number1, Obj number2)
{
  sint32 temp, temp_1;
  Obj temp_2;
  sint32 temp_3;
  Obj temp_4;

  switch (TYPE_TAG(number1,temp)) {
   case 1:
    switch (TYPE_TAG(number2,temp_1)) {
     case 1:
      integer_divide_error(number1,number2);
      temp_2 = BOXFIX(0);
      break;
     case 5:
      temp_2 = alloc_ldouble(((double)UNBOXFIX(number1))/(((Ldouble *)number2)->body),
          -1,5);
      break;
     default:
      math_type_error((Obj)(&str_const_7),number1,number2);     /* "/" */
      temp_2 = BOXFIX(0);
      break;
    }
    return temp_2;
   case 5:
    switch (TYPE_TAG(number2,temp_3)) {
     case 1:
      temp_4 = alloc_ldouble(((Ldouble *)number1)->body/(double)UNBOXFIX(number2),
          -1,5);
      break;
     case 5:
      temp_4 = alloc_ldouble(((Ldouble *)number1)->body/(((Ldouble *)number2)->body),
          -1,5);
      break;
     default:
      math_type_error((Obj)(&str_const_7),number1,number2);     /* "/" */
      temp_4 = BOXFIX(0);
      break;
    }
    return temp_4;
   default:
    math_type_error((Obj)(&str_const_7),number1,number2);   /* "/" */
    return BOXFIX(0);
  }
}

static const Str_77 str_const_8
  = { 7, 73, 73, "Integer /, use floor or another truncating operator: arg1 = ~a, arg2 = ~a" };

/* Translated from INTEGER-DIVIDE-ERROR(T T) = VOID */

void integer_divide_error (Obj number1, Obj number2)
{
  error_two_args((Obj)(&str_const_8),           /* "Integer /, use floor or another truncating operato..." */
      number1,number2);
  return;
}

/* Translated from GENERIC-NEGATE(T) = NUMBER */

Obj generic_negate (Obj number)
{
  sint32 temp;
  Obj temp_1;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    temp_1 = (Obj)(2-(sint32)number);
    break;
   case 5:
    temp_1 = alloc_ldouble(-(((Ldouble *)number)->body),-1,5);
    break;
   default:
    temp_1 = ((math_one_arg_type_error((Obj)(&str_const_5),number),     /* "-" */
        (Obj)NULL));
    break;
  }
  return temp_1;
}

/* Translated from FIXNUM-FLOOR(FIXNUM FIXNUM) = (VALUES FIXNUM FIXNUM) */

Obj fixnum_floor (sint32 fixnum, sint32 divisor_fixnum)
{
  sint32 floor_result, remainder_result, positive_fixnum, positive_divisor, 
        positive_fixnum_1;
  Obj temp;

  floor_result = 0;
  remainder_result = 0;
  if (divisor_fixnum>0) {
    if (fixnum>=0) {
      floor_result = (fixnum/divisor_fixnum);
      remainder_result = (fixnum%divisor_fixnum);
    }
    else {
      positive_fixnum = (-fixnum);
      floor_result = (-((positive_fixnum+(divisor_fixnum-1))/divisor_fixnum));
      remainder_result = (positive_fixnum%divisor_fixnum);
      if (remainder_result!=0) 
        remainder_result = (divisor_fixnum-remainder_result);
    }
  }
  else {
    positive_divisor = (-divisor_fixnum);
    if (fixnum>=0) {
      floor_result = (-((fixnum+(positive_divisor-1))/positive_divisor));
      remainder_result = (fixnum%positive_divisor);
      if (remainder_result!=0) 
        remainder_result = (divisor_fixnum+remainder_result);
    }
    else {
      positive_fixnum_1 = (-fixnum);
      floor_result = (positive_fixnum_1/positive_divisor);
      remainder_result = (-(positive_fixnum_1%positive_divisor));
    }
  }
  temp = BOXFIX(floor_result);
  (Values_buffer[0]) = BOXFIX(remainder_result);
  Values_count = 2;
  return temp;
}

/* Translated from FIXNUM-FLOOR-FIRST(FIXNUM FIXNUM) = FIXNUM */

sint32 fixnum_floor_first (sint32 fixnum, sint32 divisor_fixnum)
{
  sint32 positive_fixnum, positive_divisor;

  if (divisor_fixnum>0) {
    if (fixnum>=0) 
      return fixnum/divisor_fixnum;
    else {
      positive_fixnum = (-fixnum);
      return -((positive_fixnum+(divisor_fixnum-1))/divisor_fixnum);
    }
  }
  else if (fixnum>=0) {
    positive_divisor = (-divisor_fixnum);
    return -((fixnum+(positive_divisor-1))/positive_divisor);
  }
  else 
    return fixnum/divisor_fixnum;
}

static const Str_9 str_const_9
  = { 7, 5, 5, "floor" };

/* Translated from GENERIC-FLOOR(T T) = (VALUES FIXNUM NUMBER) */

Obj generic_floor (Obj number, Obj divisor)
{
  sint32 temp, temp_1;
  double float_number;
  Obj temp_2, temp_3;
  sint32 temp_4;
  double float_divisor;
  Obj temp_5, temp_6, temp_7, temp_8;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    switch (TYPE_TAG(divisor,temp_1)) {
     case 1:
      return fixnum_floor(UNBOXFIX(number),UNBOXFIX(divisor));
     case 5:
      float_number = (double)UNBOXFIX(number);
      temp_2 = BOXFIX((sint32)floor(float_number/(((Ldouble *)divisor)->body)));
      (Values_buffer[0]) = alloc_ldouble(mod_float(float_number,((Ldouble *)divisor)->body),
          -1,5);
      Values_count = 2;
      return temp_2;
     default:
      math_type_error((Obj)(&str_const_9),number,divisor);  /* "floor" */
      temp_3 = BOXFIX(0);
      (Values_buffer[0]) = BOXFIX(0);
      Values_count = 2;
      return temp_3;
    }
   case 5:
    switch (TYPE_TAG(divisor,temp_4)) {
     case 1:
      float_divisor = (double)UNBOXFIX(divisor);
      temp_5 = BOXFIX((sint32)floor(((Ldouble *)number)->body/float_divisor));
      (Values_buffer[0]) = alloc_ldouble(mod_float(((Ldouble *)number)->body,
          float_divisor),-1,5);
      Values_count = 2;
      return temp_5;
     case 5:
      temp_6 = BOXFIX((sint32)floor(((Ldouble *)number)->body/(((Ldouble *)divisor)->body)));
      (Values_buffer[0]) = alloc_ldouble(mod_float(((Ldouble *)number)->body,
          ((Ldouble *)divisor)->body),-1,5);
      Values_count = 2;
      return temp_6;
     default:
      math_type_error((Obj)(&str_const_9),number,divisor);  /* "floor" */
      temp_7 = BOXFIX(0);
      (Values_buffer[0]) = BOXFIX(0);
      Values_count = 2;
      return temp_7;
    }
   default:
    math_type_error((Obj)(&str_const_9),number,divisor);    /* "floor" */
    temp_8 = BOXFIX(0);
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_8;
  }
}

/* Translated from GENERIC-FLOOR-ONE(T) = (VALUES FIXNUM NUMBER) */

Obj generic_floor_one (Obj number)
{
  sint32 temp;
  Obj temp_1;
  sint32 floored_value;
  Obj temp_2, temp_3;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    temp_1 = number;
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_1;
   case 5:
    floored_value = (sint32)floor(((Ldouble *)number)->body);
    temp_2 = BOXFIX(floored_value);
    (Values_buffer[0]) = alloc_ldouble(((Ldouble *)number)->body-(double)floored_value,
        -1,5);
    Values_count = 2;
    return temp_2;
   default:
    math_one_arg_type_error((Obj)(&str_const_9),number);    /* "floor" */
    temp_3 = BOXFIX(0);
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_3;
  }
}

static const Str_9 str_const_10
  = { 7, 6, 6, "ffloor" };

/* Translated from GENERIC-FFLOOR(T T) = (VALUES DOUBLE-FLOAT NUMBER) */

Obj generic_ffloor (Obj number, Obj divisor)
{
  sint32 temp, temp_1;
  Obj temp_2;
  double float_number;
  Obj temp_3, temp_4;
  sint32 temp_5;
  double float_divisor;
  Obj temp_6, temp_7, temp_8, temp_9;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    switch (TYPE_TAG(divisor,temp_1)) {
     case 1:
      temp_2 = alloc_ldouble((double)fixnum_floor_first(UNBOXFIX(number),
          UNBOXFIX(divisor)),-1,5);
      (Values_buffer[0]) = BOXFIX(mod_fixnums(UNBOXFIX(number),UNBOXFIX(divisor)));
      Values_count = 2;
      return temp_2;
     case 5:
      float_number = (double)UNBOXFIX(number);
      temp_3 = alloc_ldouble(floor(float_number/(((Ldouble *)divisor)->body)),
          -1,5);
      (Values_buffer[0]) = alloc_ldouble(mod_float(float_number,((Ldouble *)divisor)->body),
          -1,5);
      Values_count = 2;
      return temp_3;
     default:
      math_type_error((Obj)(&str_const_10),number,divisor);     /* "ffloor" */
      temp_4 = alloc_ldouble(0.0,-1,5);
      (Values_buffer[0]) = BOXFIX(0);
      Values_count = 2;
      return temp_4;
    }
   case 5:
    switch (TYPE_TAG(divisor,temp_5)) {
     case 1:
      float_divisor = (double)UNBOXFIX(divisor);
      temp_6 = alloc_ldouble(floor(((Ldouble *)number)->body/float_divisor),
          -1,5);
      (Values_buffer[0]) = alloc_ldouble(mod_float(((Ldouble *)number)->body,
          float_divisor),-1,5);
      Values_count = 2;
      return temp_6;
     case 5:
      temp_7 = alloc_ldouble(floor(((Ldouble *)number)->body/(((Ldouble *)divisor)->body)),
          -1,5);
      (Values_buffer[0]) = alloc_ldouble(mod_float(((Ldouble *)number)->body,
          ((Ldouble *)divisor)->body),-1,5);
      Values_count = 2;
      return temp_7;
     default:
      math_type_error((Obj)(&str_const_10),number,divisor);     /* "ffloor" */
      temp_8 = alloc_ldouble(0.0,-1,5);
      (Values_buffer[0]) = BOXFIX(0);
      Values_count = 2;
      return temp_8;
    }
   default:
    math_type_error((Obj)(&str_const_10),number,divisor);   /* "ffloor" */
    temp_9 = alloc_ldouble(0.0,-1,5);
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_9;
  }
}

/* Translated from GENERIC-FFLOOR-ONE(T) = (VALUES DOUBLE-FLOAT NUMBER) */

Obj generic_ffloor_one (Obj number)
{
  sint32 temp;
  Obj temp_1;
  double floored_value;
  Obj temp_2, temp_3;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    temp_1 = alloc_ldouble((double)UNBOXFIX(number),-1,5);
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_1;
   case 5:
    floored_value = floor(((Ldouble *)number)->body);
    temp_2 = alloc_ldouble(floored_value,-1,5);
    (Values_buffer[0]) = alloc_ldouble(((Ldouble *)number)->body-floored_value,
        -1,5);
    Values_count = 2;
    return temp_2;
   default:
    math_one_arg_type_error((Obj)(&str_const_10),number);   /* "ffloor" */
    temp_3 = alloc_ldouble(0.0,-1,5);
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_3;
  }
}

static const Str_9 str_const_11
  = { 7, 7, 7, "ceiling" };

/* Translated from GENERIC-CEILING(T T) = (VALUES FIXNUM NUMBER) */

Obj generic_ceiling (Obj number, Obj divisor)
{
  sint32 ceiling_value, temp, temp_1;
  Obj temp_2;
  sint32 arg_temp;
  double float_number;
  Obj temp_3;
  double arg_temp_1;
  Obj temp_4;
  sint32 temp_5;
  double float_divisor;
  Obj temp_6;
  double arg_temp_2;
  Obj temp_7;
  double arg_temp_3;
  Obj temp_8, temp_9;

  ceiling_value = 0;
  switch (TYPE_TAG(number,temp)) {
   case 1:
    switch (TYPE_TAG(divisor,temp_1)) {
     case 1:
      ceiling_value = (sint32)ceil(((double)UNBOXFIX(number))/(double)UNBOXFIX(divisor));
      temp_2 = BOXFIX(ceiling_value);
      arg_temp = UNBOXFIX(number);
      (Values_buffer[0]) = BOXFIX(arg_temp-(UNBOXFIX(divisor)*ceiling_value));
      Values_count = 2;
      return temp_2;
     case 5:
      float_number = (double)UNBOXFIX(number);
      ceiling_value = (sint32)ceil(float_number/(((Ldouble *)divisor)->body));
      temp_3 = BOXFIX(ceiling_value);
      arg_temp_1 = float_number;
      (Values_buffer[0]) = alloc_ldouble(arg_temp_1-(((Ldouble *)divisor)->body
          *(double)ceiling_value),-1,5);
      Values_count = 2;
      return temp_3;
     default:
      math_type_error((Obj)(&str_const_11),number,divisor);     /* "ceiling" */
      temp_4 = BOXFIX(0);
      (Values_buffer[0]) = BOXFIX(0);
      Values_count = 2;
      return temp_4;
    }
   case 5:
    switch (TYPE_TAG(divisor,temp_5)) {
     case 1:
      float_divisor = (double)UNBOXFIX(divisor);
      ceiling_value = (sint32)ceil(((Ldouble *)number)->body/float_divisor);
      temp_6 = BOXFIX(ceiling_value);
      arg_temp_2 = (((Ldouble *)number)->body);
      (Values_buffer[0]) = alloc_ldouble(arg_temp_2-(float_divisor*(double)ceiling_value),
          -1,5);
      Values_count = 2;
      return temp_6;
     case 5:
      ceiling_value = (sint32)ceil(((Ldouble *)number)->body/(((Ldouble *)divisor)->body));
      temp_7 = BOXFIX(ceiling_value);
      arg_temp_3 = (((Ldouble *)number)->body);
      (Values_buffer[0]) = alloc_ldouble(arg_temp_3-(((Ldouble *)divisor)->body
          *(double)ceiling_value),-1,5);
      Values_count = 2;
      return temp_7;
     default:
      math_type_error((Obj)(&str_const_11),number,divisor);     /* "ceiling" */
      temp_8 = BOXFIX(0);
      (Values_buffer[0]) = BOXFIX(0);
      Values_count = 2;
      return temp_8;
    }
   default:
    math_type_error((Obj)(&str_const_11),number,divisor);   /* "ceiling" */
    temp_9 = BOXFIX(0);
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_9;
  }
}

/* Translated from GENERIC-CEILING-ONE(T) = (VALUES FIXNUM NUMBER) */

Obj generic_ceiling_one (Obj number)
{
  sint32 temp;
  Obj temp_1;
  sint32 ceiled_value;
  Obj temp_2, temp_3;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    temp_1 = number;
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_1;
   case 5:
    ceiled_value = (sint32)ceil(((Ldouble *)number)->body);
    temp_2 = BOXFIX(ceiled_value);
    (Values_buffer[0]) = alloc_ldouble(((Ldouble *)number)->body-(double)ceiled_value,
        -1,5);
    Values_count = 2;
    return temp_2;
   default:
    math_one_arg_type_error((Obj)(&str_const_11),number);   /* "ceiling" */
    temp_3 = BOXFIX(0);
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_3;
  }
}

static const Str_9 str_const_12
  = { 7, 8, 8, "fceiling" };

/* Translated from GENERIC-FCEILING(T T) = (VALUES DOUBLE-FLOAT NUMBER) */

Obj generic_fceiling (Obj number, Obj divisor)
{
  double ceiling_value;
  sint32 temp, temp_1, fixnum_ceiling_value;
  Obj temp_2;
  sint32 arg_temp;
  double float_number;
  Obj temp_3;
  double arg_temp_1;
  Obj temp_4;
  sint32 temp_5;
  double float_divisor;
  Obj temp_6;
  double arg_temp_2;
  Obj temp_7;
  double arg_temp_3;
  Obj temp_8, temp_9;

  ceiling_value = 0.0;
  switch (TYPE_TAG(number,temp)) {
   case 1:
    switch (TYPE_TAG(divisor,temp_1)) {
     case 1:
      fixnum_ceiling_value = (sint32)ceil(((double)UNBOXFIX(number))/(double)
          UNBOXFIX(divisor));
      ceiling_value = (double)fixnum_ceiling_value;
      temp_2 = alloc_ldouble(ceiling_value,-1,5);
      arg_temp = UNBOXFIX(number);
      (Values_buffer[0]) = BOXFIX(arg_temp-(fixnum_ceiling_value*UNBOXFIX(divisor)));
      Values_count = 2;
      return temp_2;
     case 5:
      float_number = (double)UNBOXFIX(number);
      ceiling_value = ceil(float_number/(((Ldouble *)divisor)->body));
      temp_3 = alloc_ldouble(ceiling_value,-1,5);
      arg_temp_1 = float_number;
      (Values_buffer[0]) = alloc_ldouble(arg_temp_1-(ceiling_value*(((Ldouble *)divisor)->body)),
          -1,5);
      Values_count = 2;
      return temp_3;
     default:
      math_type_error((Obj)(&str_const_12),number,divisor);     /* "fceiling" */
      temp_4 = alloc_ldouble(0.0,-1,5);
      (Values_buffer[0]) = BOXFIX(0);
      Values_count = 2;
      return temp_4;
    }
   case 5:
    switch (TYPE_TAG(divisor,temp_5)) {
     case 1:
      float_divisor = (double)UNBOXFIX(divisor);
      ceiling_value = ceil(((Ldouble *)number)->body/float_divisor);
      temp_6 = alloc_ldouble(ceiling_value,-1,5);
      arg_temp_2 = (((Ldouble *)number)->body);
      (Values_buffer[0]) = alloc_ldouble(arg_temp_2-(ceiling_value*float_divisor),
          -1,5);
      Values_count = 2;
      return temp_6;
     case 5:
      ceiling_value = ceil(((Ldouble *)number)->body/(((Ldouble *)divisor)->body));
      temp_7 = alloc_ldouble(ceiling_value,-1,5);
      arg_temp_3 = (((Ldouble *)number)->body);
      (Values_buffer[0]) = alloc_ldouble(arg_temp_3-(ceiling_value*(((Ldouble *)divisor)->body)),
          -1,5);
      Values_count = 2;
      return temp_7;
     default:
      math_type_error((Obj)(&str_const_12),number,divisor);     /* "fceiling" */
      temp_8 = alloc_ldouble(0.0,-1,5);
      (Values_buffer[0]) = BOXFIX(0);
      Values_count = 2;
      return temp_8;
    }
   default:
    math_type_error((Obj)(&str_const_12),number,divisor);   /* "fceiling" */
    temp_9 = alloc_ldouble(0.0,-1,5);
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_9;
  }
}

/* Translated from GENERIC-FCEILING-ONE(T) = (VALUES DOUBLE-FLOAT NUMBER) */

Obj generic_fceiling_one (Obj number)
{
  sint32 temp;
  Obj temp_1;
  double ceiled_value;
  Obj temp_2, temp_3;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    temp_1 = alloc_ldouble((double)UNBOXFIX(number),-1,5);
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_1;
   case 5:
    ceiled_value = ceil(((Ldouble *)number)->body);
    temp_2 = alloc_ldouble(ceiled_value,-1,5);
    (Values_buffer[0]) = alloc_ldouble(((Ldouble *)number)->body-ceiled_value,
        -1,5);
    Values_count = 2;
    return temp_2;
   default:
    math_one_arg_type_error((Obj)(&str_const_12),number);   /* "fceiling" */
    temp_3 = alloc_ldouble(0.0,-1,5);
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_3;
  }
}

static const Str_9 str_const_13
  = { 7, 8, 8, "truncate" };

/* Translated from GENERIC-TRUNCATE-ONE-FIRST(T) = FIXNUM */

sint32 generic_truncate_one_first (Obj number)
{
  sint32 temp;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    return UNBOXFIX(number);
   case 5:
    return (sint32)(((Ldouble *)number)->body);
   default:
    math_one_arg_type_error((Obj)(&str_const_13),number);   /* "truncate" */
    return 0;
  }
}

/* Translated from GENERIC-TRUNCATE-ONE(T) = (VALUES FIXNUM NUMBER) */

Obj generic_truncate_one (Obj number)
{
  sint32 temp;
  Obj temp_1;
  sint32 truncate_value;
  Obj temp_2, temp_3;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    temp_1 = number;
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_1;
   case 5:
    truncate_value = (sint32)(((Ldouble *)number)->body);
    temp_2 = BOXFIX(truncate_value);
    (Values_buffer[0]) = alloc_ldouble(((Ldouble *)number)->body-(double)truncate_value,
        -1,5);
    Values_count = 2;
    return temp_2;
   default:
    math_one_arg_type_error((Obj)(&str_const_13),number);   /* "truncate" */
    temp_3 = BOXFIX(0);
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_3;
  }
}

/* Translated from GENERIC-TRUNCATE-TWO-FIRST(T T) = FIXNUM */

sint32 generic_truncate_two_first (Obj number, Obj divisor)
{
  sint32 temp, temp_1, temp_2;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    switch (TYPE_TAG(divisor,temp_1)) {
     case 1:
      return UNBOXFIX(number)/UNBOXFIX(divisor);
     case 5:
      return (sint32)(((double)UNBOXFIX(number))/(((Ldouble *)divisor)->body));
     default:
      math_type_error((Obj)(&str_const_13),number,divisor);     /* "truncate" */
      return 0;
    }
   case 5:
    switch (TYPE_TAG(divisor,temp_2)) {
     case 1:
      return (sint32)(((Ldouble *)number)->body/(double)UNBOXFIX(divisor));
     case 5:
      return (sint32)(((Ldouble *)number)->body/(((Ldouble *)divisor)->body));
     default:
      math_type_error((Obj)(&str_const_13),number,divisor);     /* "truncate" */
      return 0;
    }
   default:
    math_type_error((Obj)(&str_const_13),number,divisor);   /* "truncate" */
    return 0;
  }
}

/* Translated from GENERIC-TRUNCATE-TWO(T T) = (VALUES FIXNUM NUMBER) */

Obj generic_truncate_two (Obj number, Obj divisor)
{
  sint32 temp, temp_1;
  Obj temp_2, temp_3, temp_4;
  sint32 temp_5;
  Obj temp_6, temp_7, temp_8, temp_9;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    switch (TYPE_TAG(divisor,temp_1)) {
     case 1:
      temp_2 = BOXFIX(UNBOXFIX(number)/UNBOXFIX(divisor));
      (Values_buffer[0]) = BOXFIX(UNBOXFIX(number)%UNBOXFIX(divisor));
      Values_count = 2;
      return temp_2;
     case 5:
      temp_3 = BOXFIX((sint32)(((double)UNBOXFIX(number))/(((Ldouble *)divisor)->body)));
      (Values_buffer[0]) = alloc_ldouble(fmod((double)UNBOXFIX(number),
          ((Ldouble *)divisor)->body),-1,5);
      Values_count = 2;
      return temp_3;
     default:
      math_type_error((Obj)(&str_const_13),number,divisor);     /* "truncate" */
      temp_4 = BOXFIX(0);
      (Values_buffer[0]) = BOXFIX(0);
      Values_count = 2;
      return temp_4;
    }
   case 5:
    switch (TYPE_TAG(divisor,temp_5)) {
     case 1:
      temp_6 = BOXFIX((sint32)(((Ldouble *)number)->body/(double)UNBOXFIX(divisor)));
      (Values_buffer[0]) = alloc_ldouble(fmod(((Ldouble *)number)->body,
          (double)UNBOXFIX(divisor)),-1,5);
      Values_count = 2;
      return temp_6;
     case 5:
      temp_7 = BOXFIX((sint32)(((Ldouble *)number)->body/(((Ldouble *)divisor)->body)));
      (Values_buffer[0]) = alloc_ldouble(fmod(((Ldouble *)number)->body,
          ((Ldouble *)divisor)->body),-1,5);
      Values_count = 2;
      return temp_7;
     default:
      math_type_error((Obj)(&str_const_13),number,divisor);     /* "truncate" */
      temp_8 = BOXFIX(0);
      (Values_buffer[0]) = BOXFIX(0);
      Values_count = 2;
      return temp_8;
    }
   default:
    math_type_error((Obj)(&str_const_13),number,divisor);   /* "truncate" */
    temp_9 = BOXFIX(0);
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_9;
  }
}

/* Translated from FTRUNCATE-TWO-ARG-MULT-VALUE(DOUBLE-FLOAT DOUBLE-FLOAT) = (VALUES DOUBLE-FLOAT DOUBLE-FLOAT) */

Obj ftruncate_two_arg_mult_value (double number, double divisor)
{
  double quotient, integer_quotient;
  Obj temp;
  double arg_temp;

  quotient = (number/divisor);
  if (quotient>=0.0) 
    integer_quotient = floor(quotient);
  else 
    integer_quotient = ceil(quotient);
  temp = alloc_ldouble(integer_quotient,-1,5);
  arg_temp = number;
  (Values_buffer[0]) = alloc_ldouble(arg_temp-(divisor*integer_quotient),
      -1,5);
  Values_count = 2;
  return temp;
}

/* Translated from FTRUNCATE-ONE-ARG-MULT-VALUE(DOUBLE-FLOAT) = (VALUES DOUBLE-FLOAT DOUBLE-FLOAT) */

Obj ftruncate_one_arg_mult_value (double number)
{
  double quotient;
  Obj temp;

  if (number>=0.0) 
    quotient = floor(number);
  else 
    quotient = ceil(number);
  temp = alloc_ldouble(quotient,-1,5);
  (Values_buffer[0]) = alloc_ldouble(number-quotient,-1,5);
  Values_count = 2;
  return temp;
}

static const Str_9 str_const_14
  = { 7, 5, 5, "round" };

/* Translated from GENERIC-ROUND-ONE-FIRST(T) = FIXNUM */

sint32 generic_round_one_first (Obj number)
{
  sint32 temp;
  double g;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    return UNBOXFIX(number);
   case 5:
    g = (((Ldouble *)number)->body);
    return (g<0.0) ? ((sint32)(g-0.5)) : (sint32)(g+0.5);
   default:
    math_one_arg_type_error((Obj)(&str_const_14),number);   /* "round" */
    return 0;
  }
}

/* Translated from GENERIC-ROUND-ONE(T) = (VALUES FIXNUM NUMBER) */

Obj generic_round_one (Obj number)
{
  sint32 temp;
  Obj temp_1;
  sint32 round_value;
  double g;
  Obj temp_2, temp_3;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    temp_1 = number;
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_1;
   case 5:
    g = (((Ldouble *)number)->body);
    round_value = ((g<0.0) ? ((sint32)(g-0.5)) : (sint32)(g+0.5));
    temp_2 = BOXFIX(round_value);
    (Values_buffer[0]) = alloc_ldouble(((Ldouble *)number)->body-(double)round_value,
        -1,5);
    Values_count = 2;
    return temp_2;
   default:
    math_one_arg_type_error((Obj)(&str_const_14),number);   /* "round" */
    temp_3 = BOXFIX(0);
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_3;
  }
}

/* Translated from GENERIC-ROUND-TWO-FIRST(T T) = FIXNUM */

sint32 generic_round_two_first (Obj number, Obj divisor)
{
  sint32 temp;
  double temp_1;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    break;
   case 5:
    break;
   default:
    math_type_error((Obj)(&str_const_14),number,divisor);   /* "round" */
    break;
  }
  switch (TYPE_TAG(divisor,temp)) {
   case 1:
    break;
   case 5:
    break;
   default:
    math_type_error((Obj)(&str_const_14),number,divisor);   /* "round" */
    break;
  }
  temp_1 = (((IMMED_TAG(number)==1) ? ((double)(((sint32)number)    /* Fixnump */
      >>2)) : (((Ldouble *)number)->body))/((IMMED_TAG(divisor)==1)     /* Fixnump */
      ? ((double)(((sint32)divisor)>>2)) : (((Ldouble *)divisor)->body)));
  return (temp_1<0.0) ? ((sint32)(temp_1-0.5)) : (sint32)(temp_1+0.5);
}

/* Translated from GENERIC-ROUND-TWO(T T) = (VALUES FIXNUM NUMBER) */

Obj generic_round_two (Obj number, Obj divisor)
{
  sint32 round_value, temp, temp_1;
  double temp_2;
  Obj temp_3;
  sint32 arg_temp;
  double temp_4;
  Obj temp_5;
  double arg_temp_1;
  Obj temp_6;
  sint32 temp_7;
  double temp_8;
  Obj temp_9;
  double arg_temp_2, temp_10;
  Obj temp_11;
  double arg_temp_3;
  Obj temp_12, temp_13;

  round_value = 0;
  switch (TYPE_TAG(number,temp)) {
   case 1:
    switch (TYPE_TAG(divisor,temp_1)) {
     case 1:
      temp_2 = (((double)UNBOXFIX(number))/(double)UNBOXFIX(divisor));
      round_value = ((temp_2<0.0) ? ((sint32)(temp_2-0.5)) : (sint32)(temp_2
          +0.5));
      temp_3 = BOXFIX(round_value);
      arg_temp = UNBOXFIX(number);
      (Values_buffer[0]) = BOXFIX(arg_temp-(round_value*UNBOXFIX(divisor)));
      Values_count = 2;
      return temp_3;
     case 5:
      temp_4 = (((double)UNBOXFIX(number))/(((Ldouble *)divisor)->body));
      round_value = ((temp_4<0.0) ? ((sint32)(temp_4-0.5)) : (sint32)(temp_4
          +0.5));
      temp_5 = BOXFIX(round_value);
      arg_temp_1 = (double)UNBOXFIX(number);
      (Values_buffer[0]) = alloc_ldouble(arg_temp_1-(((double)round_value)
          *(((Ldouble *)divisor)->body)),-1,5);
      Values_count = 2;
      return temp_5;
     default:
      math_type_error((Obj)(&str_const_14),number,divisor);     /* "round" */
      temp_6 = BOXFIX(0);
      (Values_buffer[0]) = BOXFIX(0);
      Values_count = 2;
      return temp_6;
    }
   case 5:
    switch (TYPE_TAG(divisor,temp_7)) {
     case 1:
      temp_8 = (((Ldouble *)number)->body/(double)UNBOXFIX(divisor));
      round_value = ((temp_8<0.0) ? ((sint32)(temp_8-0.5)) : (sint32)(temp_8
          +0.5));
      temp_9 = BOXFIX(round_value);
      arg_temp_2 = (((Ldouble *)number)->body);
      (Values_buffer[0]) = alloc_ldouble(arg_temp_2-(double)(round_value
          *UNBOXFIX(divisor)),-1,5);
      Values_count = 2;
      return temp_9;
     case 5:
      temp_10 = (((Ldouble *)number)->body/(((Ldouble *)divisor)->body));
      round_value = ((temp_10<0.0) ? ((sint32)(temp_10-0.5)) : (sint32)(temp_10
          +0.5));
      temp_11 = BOXFIX(round_value);
      arg_temp_3 = (((Ldouble *)number)->body);
      (Values_buffer[0]) = alloc_ldouble(arg_temp_3-(((double)round_value)
          *(((Ldouble *)divisor)->body)),-1,5);
      Values_count = 2;
      return temp_11;
     default:
      math_type_error((Obj)(&str_const_14),number,divisor);     /* "round" */
      temp_12 = BOXFIX(0);
      (Values_buffer[0]) = BOXFIX(0);
      Values_count = 2;
      return temp_12;
    }
   default:
    math_type_error((Obj)(&str_const_14),number,divisor);   /* "round" */
    temp_13 = BOXFIX(0);
    (Values_buffer[0]) = BOXFIX(0);
    Values_count = 2;
    return temp_13;
  }
}

/* Translated from MOD-FIXNUMS(FIXNUM FIXNUM) = FIXNUM */

sint32 mod_fixnums (sint32 fixnum, sint32 divisor_fixnum)
{
  sint32 remainder_result, positive_fixnum, positive_divisor;

  remainder_result = 0;
  if (divisor_fixnum>0) {
    if (fixnum>=0) 
      remainder_result = (fixnum%divisor_fixnum);
    else {
      positive_fixnum = (-fixnum);
      remainder_result = (positive_fixnum%divisor_fixnum);
      if (remainder_result!=0) 
        remainder_result = (divisor_fixnum-remainder_result);
    }
  }
  else if (fixnum>=0) {
    positive_divisor = (-divisor_fixnum);
    remainder_result = (fixnum%positive_divisor);
    if (remainder_result!=0) 
      remainder_result = (divisor_fixnum+remainder_result);
  }
  else 
    remainder_result = (fixnum%divisor_fixnum);
  return remainder_result;
}

/* Translated from MOD-FLOAT(DOUBLE-FLOAT DOUBLE-FLOAT) = DOUBLE-FLOAT */

double mod_float (double float_1, double divisor_float)
{
  double remainder_result;

  remainder_result = 0.0;
  if (divisor_float>0.0) {
    if (float_1>=0.0) 
      remainder_result = fmod(float_1,divisor_float);
    else {
      remainder_result = fmod(-float_1,divisor_float);
      if (remainder_result!=0.0) 
        remainder_result = (divisor_float-remainder_result);
    }
  }
  else if (float_1>=0.0) {
    remainder_result = fmod(float_1,-divisor_float);
    if (remainder_result!=0.0) 
      remainder_result = (divisor_float+remainder_result);
  }
  else 
    remainder_result = fmod(float_1,divisor_float);
  return remainder_result;
}

static const Str_5 str_const_15
  = { 7, 3, 3, "mod" };

/* Translated from GENERIC-MOD(T T) = NUMBER */

Obj generic_mod (Obj number, Obj divisor)
{
  sint32 temp, temp_1;
  Obj temp_2;
  sint32 temp_3;
  Obj temp_4;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    switch (TYPE_TAG(divisor,temp_1)) {
     case 1:
      temp_2 = BOXFIX(mod_fixnums(UNBOXFIX(number),UNBOXFIX(divisor)));
      break;
     case 5:
      temp_2 = alloc_ldouble(mod_float((double)UNBOXFIX(number),((Ldouble *)divisor)->body),
          -1,5);
      break;
     default:
      math_type_error((Obj)(&str_const_15),number,divisor);     /* "mod" */
      temp_2 = BOXFIX(0);
      break;
    }
    return temp_2;
   case 5:
    switch (TYPE_TAG(divisor,temp_3)) {
     case 1:
      temp_4 = alloc_ldouble(mod_float(((Ldouble *)number)->body,(double)
          UNBOXFIX(divisor)),-1,5);
      break;
     case 5:
      temp_4 = alloc_ldouble(mod_float(((Ldouble *)number)->body,((Ldouble *)divisor)->body),
          -1,5);
      break;
     default:
      math_type_error((Obj)(&str_const_15),number,divisor);     /* "mod" */
      temp_4 = BOXFIX(0);
      break;
    }
    return temp_4;
   default:
    math_type_error((Obj)(&str_const_15),number,divisor);   /* "mod" */
    return BOXFIX(0);
  }
}

static const Str_5 str_const_16
  = { 7, 3, 3, "rem" };

/* Translated from GENERIC-REM(T T) = NUMBER */

Obj generic_rem (Obj number, Obj divisor)
{
  sint32 temp, temp_1;
  Obj temp_2;
  sint32 temp_3;
  Obj temp_4;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    switch (TYPE_TAG(divisor,temp_1)) {
     case 1:
      temp_2 = BOXFIX(UNBOXFIX(number)%UNBOXFIX(divisor));
      break;
     case 5:
      temp_2 = alloc_ldouble(fmod((double)UNBOXFIX(number),((Ldouble *)divisor)->body),
          -1,5);
      break;
     default:
      math_type_error((Obj)(&str_const_16),number,divisor);     /* "rem" */
      temp_2 = BOXFIX(0);
      break;
    }
    return temp_2;
   case 5:
    switch (TYPE_TAG(divisor,temp_3)) {
     case 1:
      temp_4 = alloc_ldouble(fmod(((Ldouble *)number)->body,(double)UNBOXFIX(divisor)),
          -1,5);
      break;
     case 5:
      temp_4 = alloc_ldouble(fmod(((Ldouble *)number)->body,((Ldouble *)divisor)->body),
          -1,5);
      break;
     default:
      math_type_error((Obj)(&str_const_16),number,divisor);     /* "rem" */
      temp_4 = BOXFIX(0);
      break;
    }
    return temp_4;
   default:
    math_type_error((Obj)(&str_const_16),number,divisor);   /* "rem" */
    return BOXFIX(0);
  }
}

static const Str_5 str_const_17
  = { 7, 3, 3, "abs" };

/* Translated from GENERIC-ABS(T) = NUMBER */

Obj generic_abs (Obj number)
{
  sint32 temp;
  Obj temp_1;

  switch (TYPE_TAG(number,temp)) {
   case 1:
    temp_1 = BOXFIX((sint32)abs((int)UNBOXFIX(number)));
    break;
   case 5:
    temp_1 = alloc_ldouble(fabs(((Ldouble *)number)->body),-1,5);
    break;
   default:
    math_one_arg_type_error((Obj)(&str_const_17),number);   /* "abs" */
    temp_1 = number;
    break;
  }
  return temp_1;
}

/* Translated from INTEGER-LENGTH(FIXNUM) = FIXNUM */

sint32 integer_length (sint32 fixnum)
{
  sint32 count;

  if (fixnum<0) 
    fixnum = (~fixnum);
  count = 0;
  if (fixnum>=16777216) {
    count = (count+24);
    fixnum = (fixnum>>24);
  }
  else if (fixnum>=65536) {
    count = (count+16);
    fixnum = (fixnum>>16);
  }
  else if (fixnum>=256) {
    count = (count+8);
    fixnum = (fixnum>>8);
  }
  while (fixnum>0) {
    fixnum = (fixnum>>1);
    count = (count+1);
  }

  return count;
}

/* Translated from LOGCOUNT(FIXNUM) = FIXNUM */

sint32 logcount (sint32 fixnum)
{
  sint32 count;

  if (fixnum<0) 
    fixnum = (~fixnum);
  count = 0;
  for (;fixnum>0;count = (count+1)) 
    fixnum = (fixnum-(fixnum&(-fixnum)));
  return count;
}

/* Translated from ISQRT(FIXNUM) = FIXNUM */

sint32 isqrt (sint32 n)
{
  sint32 n_len_quarter, n_half, n_half_isqrt, init_value, iterated_value;

  if (n<=24) {
    if (n>15) 
      return 4;
    else if (n>8) 
      return 3;
    else if (n>3) 
      return 2;
    else if (n>0) 
      return 1;
    else 
      return 0;
  }
  else {
    n_len_quarter = (integer_length(n)>>2);
    n_half = (n>>(n_len_quarter<<1));
    n_half_isqrt = isqrt(n_half);
    init_value = ((n_half_isqrt+1)<<n_len_quarter);
    while (1) {
      iterated_value = ((init_value+(n/init_value))>>1);
      if (!(iterated_value<init_value)) 
        return init_value;
      init_value = iterated_value;
    }

    return (sint32)(Obj)NULL;
  }
}

/* Translated from EXPT-FIXNUM(FIXNUM FIXNUM) = FIXNUM */

sint32 expt_fixnum (sint32 base, sint32 power)
{
  sint32 accumulator, tl_loop_repeat_;

  accumulator = 1;
  tl_loop_repeat_ = power;
  while (tl_loop_repeat_>0) {
    tl_loop_repeat_ = (tl_loop_repeat_-1);
    accumulator = (accumulator*base);
  }

  return accumulator;
}

/* Translated from GENERIC-EXPT(NUMBER NUMBER) = NUMBER */

Obj generic_expt (Obj base, Obj power)
{
  Obj if_result_temp;

  if (IMMED_TAG(base)==1) {                     /* Fixnump */
    if (IMMED_TAG(power)==1)                    /* Fixnump */
      if_result_temp = BOXFIX(expt_fixnum(UNBOXFIX(base),UNBOXFIX(power)));
    else 
      if_result_temp = alloc_ldouble(pow((double)UNBOXFIX(base),((Ldouble *)power)->body),
          -1,5);
    return if_result_temp;
  }
  else 
    return alloc_ldouble(pow(((Ldouble *)base)->body,(IMMED_TAG(power)==1)      /* Fixnump */
        ? ((IMMED_TAG(power)==1) ? ((double)(((sint32)power)    /* Fixnump */
        >>2)) : (((Ldouble *)power)->body)) : (((Ldouble *)power)->body)),
        -1,5);
}

/* Translated from SYMS-TL-GENERIC-MATH() = VOID */

void syms_tl_generic_math (void)
{
  return;
}


/* Translated from INIT-TL-GENERIC-MATH() = VOID */

void init_tl_generic_math (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  return;
}

