/***
 *
 * Module:      tl/c/tl-extension.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/tl-extension.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "tl-extension.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

Obj debug_1 = (Obj)(&Unbound);

Obj debug_2 = (Obj)(&Unbound);

Obj debug_3 = (Obj)(&Unbound);

Obj debug_4 = (Obj)(&Unbound);

Obj debug_5 = (Obj)(&Unbound);

Obj debug_6 = (Obj)(&Unbound);

Obj debug_7 = (Obj)(&Unbound);

Obj debug_8 = (Obj)(&Unbound);

Obj debug_9 = (Obj)(&Unbound);

Obj debug_10 = (Obj)(&Unbound);

Obj debug_11 = (Obj)(&Unbound);

Obj debug_12 = (Obj)(&Unbound);

Obj lisp_package_1 = (Obj)(&Unbound);

static const Str_5 str_const_1
  = { 7, 4, 4, "LISP" };

Obj keyword_package_1 = (Obj)(&Unbound);

static const Str_9 str_const_2
  = { 7, 7, 7, "KEYWORD" };

Obj within_managed_object_scope = (Obj)(&Unbound);

/* Translated from TRUNCATEF-FIRST(FIXNUM FIXNUM) = FIXNUM */

sint32 truncatef_first (sint32 fixnum, sint32 divisor_fixnum)
{
  if (fixnum>0) {
    if (divisor_fixnum>0) 
      return fixnum/divisor_fixnum;
    else 
      return -(fixnum/(-divisor_fixnum));
  }
  else if (divisor_fixnum>0) 
    return -((-fixnum)/divisor_fixnum);
  else 
    return (-fixnum)/(-divisor_fixnum);
}

/* Translated from REM-FIXNUMS(FIXNUM FIXNUM) = FIXNUM */

sint32 rem_fixnums (sint32 fixnum, sint32 fixnum_divisor)
{
  if (fixnum<0) {
    if (fixnum_divisor<0) 
      return -((-fixnum)%(-fixnum_divisor));
    else 
      return -((-fixnum)%fixnum_divisor);
  }
  else if (fixnum_divisor<0) 
    return fixnum%(-fixnum_divisor);
  else 
    return fixnum%fixnum_divisor;
}

/* Translated from TWO-ARG-LCMF(FIXNUM FIXNUM) = FIXNUM */

sint32 two_arg_lcmf (sint32 n, sint32 m)
{
  sint32 arg_temp, arg_temp_1, g, g_1, g_2, g_3;

  g = n;
  g_1 = m;
  arg_temp_1 = ((g>g_1) ? g : g_1);
  arg_temp = truncatef_first(arg_temp_1,two_arg_gcdf(n,m));
  g_2 = n;
  g_3 = m;
  return arg_temp*((g_2<g_3) ? g_2 : g_3);
}

/* Translated from TWO-ARG-GCDF(FIXNUM FIXNUM) = FIXNUM */

sint32 two_arg_gcdf (sint32 u, sint32 v)
{
  sint32 k, u_1, v_1, temp;

  if (u==0) 
    return v;
  else if (v==0) 
    return u;
  else {
    k = 0;
    u_1 = (sint32)abs((int)u);
    v_1 = (sint32)abs((int)v);
    while (1) {
      if (((u_1|v_1)&1)!=0) 
        goto end_loop;
      k = (k+1);
      u_1 = (u_1>>1);
      v_1 = (v_1>>1);
    }

   end_loop:
    if ((u_1&1)!=0) 
      temp = (-v_1);
    else 
      temp = (u_1>>1);
    while (1) {
      if ((temp&1)!=0) {
        if (temp>0) 
          u_1 = temp;
        else 
          v_1 = (-temp);
        temp = (u_1-v_1);
        if (0==temp) 
          return u_1<<k;
      }
      temp = (temp>>1);
    }

    return (sint32)(Obj)NULL;
  }
}

Obj negative_fifty_million = (Obj)(&Unbound);

Obj compile_time_most_positive_fixnum = (Obj)(&Unbound);

Obj float_test_value = (Obj)(&Unbound);

Obj fixnum_test_value = (Obj)(&Unbound);

static const Str_125 str_const_3
  = { 7, 124, 124, { 'T','h','e',' ','f','o','l','l','o','w','i','n','g',
      ' ','p','r','o','b','l','e','m',' ','h','a','s',' ','b','e','e','n',
      ' ','d','e','t','e','c','t','e','d',':',' ',' ','C','o','m','p','i',
      'l','e',' ','t','i','m','e',' ','m','o','s','t','-','p','o','s','i',
      't','i','v','e','-','f','i','x','n','u','m',' ','i','s',' ','d','i',
      'f','f','e','r','e','n','t',' ','f','r','o','m',' ','r','u','n','-',
      't','i','m','e',' ','m','o','s','t','-','p','o','s','i','t','i','v',
      'e','-','f','i','x','n','u','m','.', '\000' } };

static const Str_77 str_const_4
  = { 7, 74, 74, "The following problem has been detected:  Right shift doesn\'t sign extend." };

static const Str_113 str_const_5
  = { 7, 110, 110, "The following problem has been detected:  Casting floats to integers cannot be relied on to perform truncation" };

static const Str_121 str_const_6
  = { 7, 117, 117, "The following problem has been detected:  Division of fixnums does not reliably perform truncation with negative args" };

/* Translated from VALIDATE-FIXNUM-ASSUMPTIONS() = NULL */

Obj validate_fixnum_assumptions (void)
{
  if (((sint32)BOXFIX(536870911))!=(sint32)GET_GLOBAL(compile_time_most_positive_fixnum)) 
    error((char *)(((Str *)(&str_const_3))->body));     /* "The following problem has been detected:  Compile ..." */
  if ((UNBOXFIX(GET_GLOBAL(negative_fifty_million))>>8)!=(-195313)) 
    error((char *)(((Str *)(&str_const_4))->body));     /* "The following problem has been detected:  Right sh..." */
  if (!((((((sint32)(((Ldouble *)GET_GLOBAL(float_test_value))->body/1.0))
      ==13) && (((sint32)((-(((Ldouble *)GET_GLOBAL(float_test_value))->body))
      /1.0))==(-13))) && (((sint32)(((Ldouble *)GET_GLOBAL(float_test_value))->body
      /-1.0))==(-13))) && (((sint32)((-(((Ldouble *)GET_GLOBAL(float_test_value))->body))
      /-1.0))==13))) 
    error((char *)(((Str *)(&str_const_5))->body));     /* "The following problem has been detected:  Casting ..." */
  if (!((((UNBOXFIX((Obj)(2-(sint32)GET_GLOBAL(fixnum_test_value)))/2)==(-5))
       && ((UNBOXFIX(GET_GLOBAL(fixnum_test_value))/(-2))==(-5))) && ((
      UNBOXFIX((Obj)(2-(sint32)GET_GLOBAL(fixnum_test_value)))/(-2))==5))) 
    error((char *)(((Str *)(&str_const_6))->body));     /* "The following problem has been detected:  Division..." */
  return (Obj)NULL;
}

/* Translated from FTRUNCATEE-UP(NUMBER) = DOUBLE-FLOAT */

double ftruncatee_up (Obj float_1)
{
  if (((Ldouble *)float_1)->body>0.0) 
    return ((Ldouble *)generic_fceiling_one(float_1))->body;
  else 
    return ((Ldouble *)generic_ffloor_one(float_1))->body;
}

/* Translated from COERCE-TO-DOUBLE-FLOAT-FUNCTION(NUMBER) = DOUBLE-FLOAT */

double coerce_to_double_float_function (Obj x)
{
  return (IMMED_TAG(x)==1) ? ((double)(((sint32)x)>>2))     /* Fixnump */
      : (((Ldouble *)x)->body);
}

Obj destination_for_symbol_with_preserved_cells = (Obj)(&Unbound);

/* Translated from EQ-OR-MEMQ(T T) = T */

Obj eq_or_memq (Obj x, Obj y)
{
  if (!(IMMED_TAG(y)==2))                       /* Consp */
    return (x==y) ? ((Obj)(&T)) : (Obj)NULL;
  else 
    return memq(x,y);
}

/* Translated from GETFQ-FUNCTION-NO-DEFAULT(T T) = T */

Obj getfq_function_no_default (Obj plist, Obj indicator)
{
  Obj g;

  g = plist;
  while (g!=NULL) {
    if (CAR(g)==indicator) {
      g = CAR(CDR(g));
      return g;
    }
    else 
      g = CDR(CDR(g));
  }

  return (Obj)NULL;
}

/* Translated from GETFQ-FUNCTION(T T T) = T */

Obj getfq_function (Obj plist, Obj indicator, Obj default_1)
{
  Obj g;

  g = plist;
  while (g!=NULL) {
    if (CAR(g)==indicator) {
      g = CAR(CDR(g));
      return g;
    }
    else 
      g = CDR(CDR(g));
  }

  return default_1;
}

Obj special_variable_for_use_value_macro = (Obj)(&Unbound);

static const Str_5 str_const_7
  = { 7, 2, 2, "#<" };

/* Translated from PRINT-RANDOM-OBJECT-PREFIX(T T) = STRING */

unsigned char *print_random_object_prefix (Obj object, Obj stream)
{
  (void)object;                                 /* OBJECT was declared ignore */
  return write_string_function(((Str *)(&str_const_7))->body,   /* "#<" */
      stream,0,(Obj)NULL);
}

/* Translated from PRINT-RANDOM-OBJECT-SUFFIX(T T) = CHARACTER */

unsigned char print_random_object_suffix (Obj object, Obj stream)
{
  sint32 arg_temp;

  write_char(' ',stream);
  arg_temp = (sint32)object;
  write_fixnum(arg_temp,16,0,stream);
  return write_char('>',stream);
}

/* Translated from SYMS-TL-TL-EXTENSION() = VOID */

void syms_tl_tl_extension (void)
{
  return;
}


/* Translated from INIT-TL-TL-EXTENSION() = VOID */

void init_tl_tl_extension (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  if (debug_1==(Obj)(&Unbound)) 
    debug_1 = (Obj)NULL;
  if (debug_2==(Obj)(&Unbound)) 
    debug_2 = (Obj)NULL;
  if (debug_3==(Obj)(&Unbound)) 
    debug_3 = (Obj)NULL;
  if (debug_4==(Obj)(&Unbound)) 
    debug_4 = (Obj)NULL;
  if (debug_5==(Obj)(&Unbound)) 
    debug_5 = (Obj)NULL;
  if (debug_6==(Obj)(&Unbound)) 
    debug_6 = (Obj)NULL;
  if (debug_7==(Obj)(&Unbound)) 
    debug_7 = (Obj)NULL;
  if (debug_8==(Obj)(&Unbound)) 
    debug_8 = (Obj)NULL;
  if (debug_9==(Obj)(&Unbound)) 
    debug_9 = (Obj)NULL;
  if (debug_10==(Obj)(&Unbound)) 
    debug_10 = (Obj)NULL;
  if (debug_11==(Obj)(&Unbound)) 
    debug_11 = (Obj)NULL;
  if (debug_12==(Obj)(&Unbound)) 
    debug_12 = (Obj)NULL;
  if (lisp_package_1==(Obj)(&Unbound)) 
    lisp_package_1 = find_package_1((Obj)(&str_const_1));   /* "LISP" */
  if (keyword_package_1==(Obj)(&Unbound)) 
    keyword_package_1 = find_package_1((Obj)(&str_const_2));    /* "KEYWORD" */
  if (within_managed_object_scope==(Obj)(&Unbound)) 
    within_managed_object_scope = (Obj)NULL;
  negative_fifty_million = BOXFIX(-50000000);
  compile_time_most_positive_fixnum = BOXFIX(536870911);
  float_test_value = alloc_ldouble(13.4,0,5);
  fixnum_test_value = BOXFIX(11);
  validate_fixnum_assumptions();
  if (destination_for_symbol_with_preserved_cells==(Obj)(&Unbound)) 
    destination_for_symbol_with_preserved_cells = (Obj)NULL;
  if (special_variable_for_use_value_macro==(Obj)(&Unbound)) 
    special_variable_for_use_value_macro = (Obj)NULL;
  return;
}

