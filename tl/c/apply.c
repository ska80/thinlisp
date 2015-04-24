/***
 *
 * Module:      tl/c/apply.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/apply.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "apply.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

static const Str_45 str_const_1
  = { 7, 43, 43, "NIL given as the function argument to apply" };

static const Str_49 str_const_2
  = { 7, 45, 45, "Cannot apply ~s, it does not name a function." };

static const Str_45 str_const_3
  = { 7, 43, 43, "~s given as the function argument to apply." };

static const Str_45 str_const_4
  = { 7, 41, 41, "Argument count mismatch in APPLY ~s on ~s" };

static const Str_57 str_const_5
  = { 7, 56, 56, "Calling APPLY on ~a with ~a args, it can only handle ~a." };

/* Translated from APPLY-1(T T) = * */

Obj apply_1 (Obj function, Obj args)
{
  sint32 rest_length;
  Obj function_args, next_to_last_cons;
  sint32 given_arg_count;
  Obj compiled_function;
  sint32 temp, actual_arg_count;
  Obj conses;
  Obj temp_list[40];
  Obj cons_1, arg_cons, next_arg_consP, g, g_1, g_2, g_3, g_4, g_5, g_6, 
        g_7, g_8, g_9, g_10, g_11, g_12, g_13, g_14, g_15, g_16, g_17, 
        g_18, g_19, g_20, temp_1, temp_2, temp_3, temp_4, temp_5, temp_6, 
        temp_7, temp_8, temp_9, temp_10, temp_11, temp_12, temp_13, temp_14, 
        temp_15, temp_16, temp_17, temp_18, temp_19, temp_20, temp_21, 
        temp_22;

  rest_length = length(args);
  if (rest_length>1) {
    next_to_last_cons = nthcdr(rest_length-2,args);
    CDR(next_to_last_cons) = CAR(CDR(next_to_last_cons));
    function_args = args;
  }
  else 
    function_args = CAR(args);
  given_arg_count = length(function_args);
  switch (TYPE_TAG(function,temp)) {
   case 0:
    error((char *)(((Str *)(&str_const_1))->body));     /* "NIL given as the function argument to apply" */
    compiled_function = (Obj)NULL;
    break;
   case 11:
    if (((Sym *)function)->symbol_function!=(Obj)(&Unbound)) 
      compiled_function = (((Sym *)function)->symbol_function);
    else 
      compiled_function = error_one_arg((Obj)(&str_const_2),    /* "Cannot apply ~s, it does not name a function." */
          function);
    break;
   case 12:
    compiled_function = function;
    break;
   default:
    compiled_function = error_one_arg((Obj)(&str_const_3),  /* "~s given as the function argument to apply." */
        function);
    break;
  }
  actual_arg_count = (sint32)(((Func *)compiled_function)->arg_count);
  if (given_arg_count!=actual_arg_count) {
    if ((given_arg_count<actual_arg_count) && ((given_arg_count+(sint32)(
        ((Func *)compiled_function)->optional_arguments))>=actual_arg_count)) {
      if (function_args!=NULL) {
        (temp_list[0]) = (Obj)NULL;
        (temp_list[2]) = (Obj)NULL;
        (temp_list[4]) = (Obj)NULL;
        (temp_list[6]) = (Obj)NULL;
        (temp_list[8]) = (Obj)NULL;
        (temp_list[10]) = (Obj)NULL;
        (temp_list[12]) = (Obj)NULL;
        (temp_list[14]) = (Obj)NULL;
        (temp_list[16]) = (Obj)NULL;
        (temp_list[18]) = (Obj)NULL;
        (temp_list[20]) = (Obj)NULL;
        (temp_list[22]) = (Obj)NULL;
        (temp_list[24]) = (Obj)NULL;
        (temp_list[26]) = (Obj)NULL;
        (temp_list[28]) = (Obj)NULL;
        (temp_list[30]) = (Obj)NULL;
        (temp_list[32]) = (Obj)NULL;
        (temp_list[34]) = (Obj)NULL;
        (temp_list[36]) = (Obj)NULL;
        (temp_list[38]) = (Obj)NULL;
        conses = hook_up_cdrs(temp_list,20,NULL);
        cons_1 = conses;
        arg_cons = function_args;
        next_arg_consP = (Obj)NULL;
        for (;1;arg_cons = next_arg_consP) {
          next_arg_consP = CDR(arg_cons);
          if (next_arg_consP==NULL) 
            goto end_loop;
          CAR(cons_1) = CAR(arg_cons);
          cons_1 = CDR(cons_1);
        }
       end_loop:
        CAR(cons_1) = CAR(arg_cons);
        CDR(cons_1) = nthcdr(((sint32)(((Func *)compiled_function)->optional_arguments))
            -(actual_arg_count-given_arg_count),((Func *)compiled_function)->default_arguments);
        function_args = conses;
      }
      else 
        function_args = (((Func *)compiled_function)->default_arguments);
    }
    else 
      error_two_args((Obj)(&str_const_4),       /* "Argument count mismatch in APPLY ~s on ~s" */
          compiled_function,function_args);
  }
  Closure_env = (((Func *)compiled_function)->closure_environment);
  g = function_args;
  g_1 = (Obj)NULL;
  g_2 = (Obj)NULL;
  g_3 = (Obj)NULL;
  g_4 = (Obj)NULL;
  g_5 = (Obj)NULL;
  g_6 = (Obj)NULL;
  g_7 = (Obj)NULL;
  g_8 = (Obj)NULL;
  g_9 = (Obj)NULL;
  g_10 = (Obj)NULL;
  g_11 = (Obj)NULL;
  g_12 = (Obj)NULL;
  g_13 = (Obj)NULL;
  g_14 = (Obj)NULL;
  g_15 = (Obj)NULL;
  g_16 = (Obj)NULL;
  g_17 = (Obj)NULL;
  g_18 = (Obj)NULL;
  g_19 = (Obj)NULL;
  g_20 = (Obj)NULL;
  if (g==NULL) 
    goto exit_set_vars;
  g_1 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_2 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_3 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_4 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_5 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_6 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_7 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_8 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_9 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_10 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_11 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_12 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_13 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_14 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_15 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_16 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_17 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_18 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_19 = CAR(g);
  g = CDR(g);
  if (g==NULL) 
    goto exit_set_vars;
  g_20 = CAR(g);
  g = CDR(g);
 exit_set_vars:
  switch (actual_arg_count) {
   case 0:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_0 *)compiled_function)->c_function();
    else {
      temp_1 = ((Func_0 *)compiled_function)->c_function();
      Values_count = 1;
      return temp_1;
    }
   case 1:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_1 *)compiled_function)->c_function(g_1);
    else {
      temp_2 = ((Func_1 *)compiled_function)->c_function(g_1);
      Values_count = 1;
      return temp_2;
    }
   case 2:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_2 *)compiled_function)->c_function(g_1,g_2);
    else {
      temp_3 = ((Func_2 *)compiled_function)->c_function(g_1,g_2);
      Values_count = 1;
      return temp_3;
    }
   case 3:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_3 *)compiled_function)->c_function(g_1,g_2,g_3);
    else {
      temp_4 = ((Func_3 *)compiled_function)->c_function(g_1,g_2,g_3);
      Values_count = 1;
      return temp_4;
    }
   case 4:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_4 *)compiled_function)->c_function(g_1,g_2,g_3,g_4);
    else {
      temp_5 = ((Func_4 *)compiled_function)->c_function(g_1,g_2,g_3,g_4);
      Values_count = 1;
      return temp_5;
    }
   case 5:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_5 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5);
    else {
      temp_6 = ((Func_5 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5);
      Values_count = 1;
      return temp_6;
    }
   case 6:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_6 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6);
    else {
      temp_7 = ((Func_6 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6);
      Values_count = 1;
      return temp_7;
    }
   case 7:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_7 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7);
    else {
      temp_8 = ((Func_7 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7);
      Values_count = 1;
      return temp_8;
    }
   case 8:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_8 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7,g_8);
    else {
      temp_9 = ((Func_8 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7,g_8);
      Values_count = 1;
      return temp_9;
    }
   case 9:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_9 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7,g_8,g_9);
    else {
      temp_10 = ((Func_9 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7,g_8,g_9);
      Values_count = 1;
      return temp_10;
    }
   case 10:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_10 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7,g_8,g_9,g_10);
    else {
      temp_11 = ((Func_10 *)compiled_function)->c_function(g_1,g_2,g_3,
          g_4,g_5,g_6,g_7,g_8,g_9,g_10);
      Values_count = 1;
      return temp_11;
    }
   case 11:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_11 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7,g_8,g_9,g_10,g_11);
    else {
      temp_12 = ((Func_11 *)compiled_function)->c_function(g_1,g_2,g_3,
          g_4,g_5,g_6,g_7,g_8,g_9,g_10,g_11);
      Values_count = 1;
      return temp_12;
    }
   case 12:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_12 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12);
    else {
      temp_13 = ((Func_12 *)compiled_function)->c_function(g_1,g_2,g_3,
          g_4,g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12);
      Values_count = 1;
      return temp_13;
    }
   case 13:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_13 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12,g_13);
    else {
      temp_14 = ((Func_13 *)compiled_function)->c_function(g_1,g_2,g_3,
          g_4,g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12,g_13);
      Values_count = 1;
      return temp_14;
    }
   case 14:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_14 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12,g_13,g_14);
    else {
      temp_15 = ((Func_14 *)compiled_function)->c_function(g_1,g_2,g_3,
          g_4,g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12,g_13,g_14);
      Values_count = 1;
      return temp_15;
    }
   case 15:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_15 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12,g_13,g_14,g_15);
    else {
      temp_16 = ((Func_15 *)compiled_function)->c_function(g_1,g_2,g_3,
          g_4,g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12,g_13,g_14,g_15);
      Values_count = 1;
      return temp_16;
    }
   case 16:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_16 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12,g_13,g_14,g_15,g_16);
    else {
      temp_17 = ((Func_16 *)compiled_function)->c_function(g_1,g_2,g_3,
          g_4,g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12,g_13,g_14,g_15,g_16);
      Values_count = 1;
      return temp_17;
    }
   case 17:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_17 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12,g_13,g_14,g_15,g_16,g_17);
    else {
      temp_18 = ((Func_17 *)compiled_function)->c_function(g_1,g_2,g_3,
          g_4,g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12,g_13,g_14,g_15,g_16,g_17);
      Values_count = 1;
      return temp_18;
    }
   case 18:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_18 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12,g_13,g_14,g_15,g_16,g_17,g_18);
    else {
      temp_19 = ((Func_18 *)compiled_function)->c_function(g_1,g_2,g_3,
          g_4,g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12,g_13,g_14,g_15,g_16,g_17,
          g_18);
      Values_count = 1;
      return temp_19;
    }
   case 19:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_19 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12,g_13,g_14,g_15,g_16,g_17,g_18,
          g_19);
    else {
      temp_20 = ((Func_19 *)compiled_function)->c_function(g_1,g_2,g_3,
          g_4,g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12,g_13,g_14,g_15,g_16,g_17,
          g_18,g_19);
      Values_count = 1;
      return temp_20;
    }
   case 20:
    if (((sint32)(((Func *)compiled_function)->sets_values_count))!=0) 
      return ((Func_20 *)compiled_function)->c_function(g_1,g_2,g_3,g_4,
          g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12,g_13,g_14,g_15,g_16,g_17,g_18,
          g_19,g_20);
    else {
      temp_21 = ((Func_20 *)compiled_function)->c_function(g_1,g_2,g_3,
          g_4,g_5,g_6,g_7,g_8,g_9,g_10,g_11,g_12,g_13,g_14,g_15,g_16,g_17,
          g_18,g_19,g_20);
      Values_count = 1;
      return temp_21;
    }
   default:
    temp_22 = error_three_args((Obj)(&str_const_5),     /* "Calling APPLY on ~a with ~a args, it can only hand..." */
        compiled_function,BOXFIX(actual_arg_count),BOXFIX(20));
    Values_count = 1;
    return temp_22;
  }
}

/* Translated from SYMS-TL-APPLY() = VOID */

void syms_tl_apply (void)
{
  return;
}


/* Translated from INIT-TL-APPLY() = VOID */

void init_tl_apply (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  return;
}

