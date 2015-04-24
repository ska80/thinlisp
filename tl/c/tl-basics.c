/***
 *
 * Module:      tl/c/tl-basics.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/tl-basics.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "tl-basics.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

static const Str_41 str_const_1
  = { 7, 39, 39, "Non-sequence object ~a given to length." };

/* Translated from LENGTH(T) = FIXNUM */

sint32 length (Obj sequence)
{
  sint32 temp, count;
  Obj consP;

  switch (TYPE_TAG(sequence,temp)) {
   case 0:
    return 0;
   case 2:
    count = 1;
    consP = CDR(sequence);
    for (;consP!=NULL;consP = CDR(consP)) 
      count = (count+1);
    return count;
   case 6:
    return (sint32)(((Sv *)sequence)->length);
   case 7:
    return (sint32)(((Str *)sequence)->fill_length);
   case 8:
    return (sint32)(((Sa_uint8 *)sequence)->fill_length);
   case 9:
    return (sint32)(((Sa_uint16 *)sequence)->fill_length);
   case 10:
    return (sint32)(((Sa_double *)sequence)->length);
   default:
    error_one_arg((Obj)(&str_const_1),          /* "Non-sequence object ~a given to length." */
        sequence);
    return 0;
  }
}

/* Translated from EQUAL(T T) = T */

Obj equal (Obj a, Obj b)
{
  sint32 temp;
  int temp_1;
  Obj a_cons, b_cons;
  int temp_2;

  temp_2 = (a==b);
  if (!temp_2) {
    switch (TYPE_TAG(a,temp)) {
     case 5:
      temp_1 = (((b!=NULL) && ((IMMED_TAG(b)==0) && (STD_TAG(b)==5)))   /* DOUBLE-FLOAT-P */
           && (((Ldouble *)a)->body==(((Ldouble *)b)->body)));
      break;
     case 7:
      temp_1 = (((b!=NULL) && ((IMMED_TAG(b)==0) && (STD_TAG(b)==7)))   /* STRING-P */
           && (strcmp((char *)(((Str *)a)->body),(char *)(((Str *)b)->body))
          ==0));
      break;
     case 2:
      a_cons = a;
      b_cons = b;
      for (;(IMMED_TAG(a_cons)==2) && (IMMED_TAG(b_cons)==2);   /* Consp, Consp */
          b_cons = CDR(b_cons)) {
        if (equal(CAR(a_cons),CAR(b_cons))==NULL) {
          temp_1 = 0;
          goto exit_nil;
        }
        a_cons = CDR(a_cons);
      }
      temp_1 = ((a_cons==b_cons) || (((!(IMMED_TAG(a_cons)==2))     /* Consp */
           && (!(IMMED_TAG(b_cons)==2))) && (equal(a_cons,  /* Consp */
          b_cons)!=NULL)));
      goto exit_nil;
     exit_nil:
      break;
     default:
      temp_1 = 0;
      break;
    }
    temp_2 = temp_1;
  }
  if (temp_2) 
    return (Obj)(&T);
  else 
    return (Obj)NULL;
}

/* Translated from COPY-LIST(LIST) = LIST */

Obj copy_list (Obj list)
{
  sint32 length_1;
  Obj new_list, old_cons, old_cdr, new_cons;

  if (list==NULL) 
    return (Obj)NULL;
  else {
    length_1 = length(list);
    new_list = alloc_list(length_1,1,(Obj)NULL,-1);
    old_cons = list;
    old_cdr = CDR(old_cons);
    new_cons = new_list;
    while (IMMED_TAG(old_cdr)==2) {             /* Consp */
      CAR(new_cons) = CAR(old_cons);
      old_cons = old_cdr;
      old_cdr = CDR(old_cons);
      new_cons = CDR(new_cons);
    }

    CAR(new_cons) = CAR(old_cons);
    CDR(new_cons) = old_cdr;
    goto exit_nil;
   exit_nil:
    return new_list;
  }
}

/* Translated from APPEND-FUNCTION(T) = T */

Obj append_function (Obj lists)
{
  Obj new_list, cons_in_new_listP, list_holder, next_list_holder, old_list, 
        new_copy;

  if (lists==NULL) 
    return (Obj)NULL;
  else {
    new_list = (Obj)NULL;
    cons_in_new_listP = (Obj)NULL;
    list_holder = lists;
    next_list_holder = CDR(lists);
    old_list = CAR(list_holder);
    while (IMMED_TAG(next_list_holder)==2) {    /* Consp */
      if (old_list!=NULL) {
        new_copy = copy_list(old_list);
        if (cons_in_new_listP!=NULL) {
          CDR(last(cons_in_new_listP)) = new_copy;
          cons_in_new_listP = new_copy;
        }
        else {
          new_list = new_copy;
          cons_in_new_listP = new_copy;
        }
      }
      list_holder = next_list_holder;
      next_list_holder = CDR(list_holder);
      old_list = CAR(list_holder);
    }

    if (cons_in_new_listP!=NULL) 
      CDR(last(cons_in_new_listP)) = old_list;
    else 
      new_list = old_list;
    goto exit_nil;
   exit_nil:
    return new_list;
  }
}

/* Translated from IDENTITY(T) = (VALUES T) */

Obj identity (Obj x)
{
  Values_count = 1;
  return x;
}

/* Translated from ASSQ(T LIST) = T */

Obj assq (Obj item, Obj alist)
{
  Obj g, g_1;

  g = alist;
  for (;g!=NULL;g = CDR(g)) {
    g_1 = CAR(g);
    if (CAR(g_1)==item) 
      return g_1;
  }
  return (Obj)NULL;
}

/* Translated from ASSOC-EQL(T LIST) = T */

Obj assoc_eql (Obj item, Obj alist)
{
  Obj g, g_1;

  g = alist;
  for (;g!=NULL;g = CDR(g)) {
    g_1 = CAR(g);
    if (eql(CAR(g_1),item)!=NULL) 
      return g_1;
  }
  return (Obj)NULL;
}

/* Translated from ASSOC-EQUAL(T LIST) = T */

Obj assoc_equal (Obj item, Obj alist)
{
  Obj g, g_1;

  g = alist;
  for (;g!=NULL;g = CDR(g)) {
    g_1 = CAR(g);
    if (equal(CAR(g_1),item)!=NULL) 
      return g_1;
  }
  return (Obj)NULL;
}

/* Translated from MEMQ(T LIST) = T */

Obj memq (Obj item, Obj list)
{
  Obj g;

  g = list;
  for (;g!=NULL;g = CDR(g)) {
    if (item==CAR(g)) 
      return g;
  }
  return (Obj)NULL;
}

/* Translated from MEMBER-EQL(T LIST) = T */

Obj member_eql (Obj item, Obj list)
{
  Obj g;

  g = list;
  for (;g!=NULL;g = CDR(g)) {
    if (eql(item,CAR(g))!=NULL) 
      return g;
  }
  return (Obj)NULL;
}

/* Translated from MEMBER-EQUAL(T LIST) = T */

Obj member_equal (Obj item, Obj list)
{
  Obj g;

  g = list;
  for (;g!=NULL;g = CDR(g)) {
    if (equal(item,CAR(g))!=NULL) 
      return g;
  }
  return (Obj)NULL;
}

/* Translated from DELQ(T LIST &OPTIONAL T) = T */

Obj delq (Obj item, Obj list, Obj count)
{
  Obj g;
  sint32 g_1;
  Obj g_2, g_3, g_4;

  g = list;
  g_1 = 0;
  g_2 = (Obj)NULL;
  g_3 = (Obj)NULL;
  g_4 = g;
  while (g_4!=NULL) {
    g_3 = CDR(g_4);
    if (CAR(g_4)==item) {
      if (g_2!=NULL) 
        CDR(g_2) = g_3;
      else 
        g = g_3;
      if (count!=NULL) {
        g_1 = (g_1+1);
        if (g_1>=UNBOXFIX(count)) 
          return g;
      }
    }
    else 
      g_2 = g_4;
    g_4 = g_3;
  }

  return g;
}

/* Translated from DELETE-EQL(T LIST &OPTIONAL T) = T */

Obj delete_eql (Obj item, Obj list, Obj count)
{
  Obj g;
  sint32 g_1;
  Obj g_2, g_3, g_4;

  g = list;
  g_1 = 0;
  g_2 = (Obj)NULL;
  g_3 = (Obj)NULL;
  g_4 = g;
  while (g_4!=NULL) {
    g_3 = CDR(g_4);
    if (eql(CAR(g_4),item)!=NULL) {
      if (g_2!=NULL) 
        CDR(g_2) = g_3;
      else 
        g = g_3;
      if (count!=NULL) {
        g_1 = (g_1+1);
        if (g_1>=UNBOXFIX(count)) 
          return g;
      }
    }
    else 
      g_2 = g_4;
    g_4 = g_3;
  }

  return g;
}

/* Translated from DELETE-EQUAL(T LIST &OPTIONAL T) = T */

Obj delete_equal (Obj item, Obj list, Obj count)
{
  Obj g;
  sint32 g_1;
  Obj g_2, g_3, g_4;

  g = list;
  g_1 = 0;
  g_2 = (Obj)NULL;
  g_3 = (Obj)NULL;
  g_4 = g;
  while (g_4!=NULL) {
    g_3 = CDR(g_4);
    if (equal(CAR(g_4),item)!=NULL) {
      if (g_2!=NULL) 
        CDR(g_2) = g_3;
      else 
        g = g_3;
      if (count!=NULL) {
        g_1 = (g_1+1);
        if (g_1>=UNBOXFIX(count)) 
          return g;
      }
    }
    else 
      g_2 = g_4;
    g_4 = g_3;
  }

  return g;
}

/* Translated from NTHCDR(FIXNUM LIST) = T */

Obj nthcdr (sint32 n, Obj list)
{
  sint32 count;

  count = 0;
  for (;!((count>=n) || (list==NULL));count = (count+1)) 
    list = CDR(list);
  return list;
}

/* Translated from NTH(FIXNUM LIST) = T */

Obj nth (sint32 n, Obj list)
{
  sint32 count;

  count = 0;
  for (;list!=NULL;count = (count+1)) {
    if (count>=n) 
      return CAR(list);
    else 
      list = CDR(list);
  }
  return (Obj)NULL;
}

static const Str_45 str_const_2
  = { 7, 42, 42, "ENDP requires a list argument, received ~s" };

/* Translated from ENDP-ERROR-FUNCTION(T) = VOID */

void endp_error_function (Obj arg)
{
  error_one_arg((Obj)(&str_const_2),            /* "ENDP requires a list argument, received ~s" */
      arg);
  return;
}

static const Str_49 str_const_3
  = { 7, 45, 45, "Non-array object ~a given to array-dimension." };

/* Translated from GENERIC-ARRAY-DIMENSION(T) = FIXNUM */

sint32 generic_array_dimension (Obj array)
{
  sint32 temp;

  switch (TYPE_TAG(array,temp)) {
   case 6:
    return (sint32)(((Sv *)array)->length);
   case 7:
    return (sint32)(((Str *)array)->length);
   case 8:
    return (sint32)(((Sa_uint8 *)array)->length);
   case 9:
    return (sint32)(((Sa_uint16 *)array)->length);
   case 10:
    return (sint32)(((Sa_double *)array)->length);
   default:
    error_one_arg((Obj)(&str_const_3),          /* "Non-array object ~a given to array-dimension." */
        array);
    return 0;
  }
}

static const Str_33 str_const_4
  = { 7, 32, 32, "~a does not have a fill-pointer." };

/* Translated from GENERIC-FILL-POINTER(T) = FIXNUM */

sint32 generic_fill_pointer (Obj vector)
{
  sint32 temp;

  switch (TYPE_TAG(vector,temp)) {
   case 7:
    return (sint32)(((Str *)vector)->fill_length);
   case 8:
    return (sint32)(((Sa_uint8 *)vector)->fill_length);
   case 9:
    return (sint32)(((Sa_uint16 *)vector)->fill_length);
   default:
    error_one_arg((Obj)(&str_const_4),          /* "~a does not have a fill-pointer." */
        vector);
    return 0;
  }
}

static const Str_49 str_const_5
  = { 7, 47, 47, "~a does not have a fill-pointer, cannot set it." };

/* Translated from GENERIC-SET-FILL-POINTER(T FIXNUM) = FIXNUM */

sint32 generic_set_fill_pointer (Obj vector, sint32 new_fill_pointer)
{
  sint32 temp;
  Str *string;

  switch (TYPE_TAG(vector,temp)) {
   case 7:
    string = (Str *)vector;
    (string->body[new_fill_pointer]) = '\000';
    return string->fill_length = new_fill_pointer;
   case 8:
    return ((Sa_uint8 *)vector)->fill_length = new_fill_pointer;
   case 9:
    return ((Sa_uint16 *)vector)->fill_length = new_fill_pointer;
   default:
    error_one_arg((Obj)(&str_const_5),          /* "~a does not have a fill-pointer, cannot set it." */
        vector);
    return 0;
  }
}

/* Translated from ARRAY-HAS-FILL-POINTER-P(T) = T */

Obj array_has_fill_pointer_p (Obj vector)
{
  sint32 temp;

  switch (TYPE_TAG(vector,temp)) {
   case 7:
    return (Obj)(&T);
   case 8:
    return (Obj)(&T);
   case 9:
    return (Obj)(&T);
   default:
    return (Obj)NULL;
  }
}

static const Str_53 str_const_6
  = { 7, 50, 50, "STRING given ~s, which was not a string or symbol." };

/* Translated from STRING-ARG-INVALID(T) = T */

Obj string_arg_invalid (Obj x)
{
  error_one_arg((Obj)(&str_const_6),            /* "STRING given ~s, which was not a string or symbol." */
      x);
  return (Obj)NULL;
}

static const Str_5 str_const_7
  = { 7, 3, 3, "NIL" };

/* Translated from COERCE-TO-STRING(T) = STRING */

unsigned char *coerce_to_string (Obj thing)
{
  Obj coerced_string;
  sint32 temp;
  Obj g;

  coerced_string = (Obj)NULL;
  switch (TYPE_TAG(thing,temp)) {
   case 0:
   case 11:
    g = thing;
    coerced_string = ((g!=NULL) ? ((Sym *)g)->symbol_name : (Obj)(&str_const_7));   /* "NIL" */
    break;
   case 7:
    coerced_string = thing;
    break;
   default:
    string_arg_invalid(thing);
    break;
  }
  return ((Str *)coerced_string)->body;
}

/* Translated from BOUNDED-STRING-COMPARE(STRING STRING FIXNUM T FIXNUM T) = FIXNUM */

sint32 bounded_string_compare (unsigned char *a, unsigned char *b, sint32 start1, 
        Obj end1, sint32 start2, Obj end2)
{
  sint32 e1, e2;
  unsigned char c1, c2;

  if (end1==NULL) 
    e1 = (sint32)(StrHDR(a)->fill_length);
  else 
    e1 = UNBOXFIX(end1);
  if (end2==NULL) 
    e2 = (sint32)(StrHDR(b)->fill_length);
  else 
    e2 = UNBOXFIX(end2);
  while (!((start1>=e1) || (start2>=e2))) {
    c1 = (a[start1]);
    c2 = (b[start2]);
    if (c1!=c2) {
      if (c1<c2) 
        return -1;
      else 
        return 1;
    }
  }

  return 0;
}

/* Translated from STRING-EQUAL(T T) = T */

Obj string_equal (Obj string_or_symbol_1, Obj string_or_symbol_2)
{
  unsigned char *string1;
  unsigned char *string2;
  sint32 length1, length2, index;
  unsigned char arg_temp, g, if_result_temp, g_1, if_result_temp_1;

  string1 = coerce_to_string(string_or_symbol_1);
  string2 = coerce_to_string(string_or_symbol_2);
  length1 = (sint32)(StrHDR(string1)->fill_length);
  length2 = (sint32)(StrHDR(string2)->fill_length);
  if (length1==length2) {
    index = 0;
    for (;index<length1;index = (index+1)) {
      g = (string1[index]);
      if (('a'<=g) && (g<='z')) 
        if_result_temp = (unsigned char)(((sint32)g)-32);
      else 
        if_result_temp = g;
      arg_temp = if_result_temp;
      g_1 = (string2[index]);
      if (('a'<=g_1) && (g_1<='z')) 
        if_result_temp_1 = (unsigned char)(((sint32)g_1)-32);
      else 
        if_result_temp_1 = g_1;
      if (arg_temp!=if_result_temp_1) 
        return (Obj)NULL;
    }
    return (Obj)(&T);
  }
  else 
    return (Obj)NULL;
}

/* Translated from STRING-LESSP(T T) = T */

Obj string_lessp (Obj string_or_symbol_1, Obj string_or_symbol_2)
{
  unsigned char *string1;
  unsigned char *string2;
  sint32 length1, length2, min_length, index;
  unsigned char char1, g, char2, g_1;

  string1 = coerce_to_string(string_or_symbol_1);
  string2 = coerce_to_string(string_or_symbol_2);
  length1 = (sint32)(StrHDR(string1)->fill_length);
  length2 = (sint32)(StrHDR(string2)->fill_length);
  if (length1<length2) 
    min_length = length1;
  else 
    min_length = length2;
  index = 0;
  for (;index<min_length;index = (index+1)) {
    g = (string1[index]);
    if (('a'<=g) && (g<='z')) 
      char1 = (unsigned char)(((sint32)g)-32);
    else 
      char1 = g;
    g_1 = (string2[index]);
    if (('a'<=g_1) && (g_1<='z')) 
      char2 = (unsigned char)(((sint32)g_1)-32);
    else 
      char2 = g_1;
    if (char1<char2) 
      return (Obj)(&T);
    else if (char1>char2) 
      return (Obj)NULL;
  }
  return (length1<length2) ? ((Obj)(&T)) : (Obj)NULL;
}

/* Translated from STRING-GREATERP(T T) = T */

Obj string_greaterp (Obj string_or_symbol_1, Obj string_or_symbol_2)
{
  unsigned char *string1;
  unsigned char *string2;
  sint32 length1, length2, min_length, index;
  unsigned char char1, g, char2, g_1;

  string1 = coerce_to_string(string_or_symbol_1);
  string2 = coerce_to_string(string_or_symbol_2);
  length1 = (sint32)(StrHDR(string1)->fill_length);
  length2 = (sint32)(StrHDR(string2)->fill_length);
  if (length1<length2) 
    min_length = length1;
  else 
    min_length = length2;
  index = 0;
  for (;index<min_length;index = (index+1)) {
    g = (string1[index]);
    if (('a'<=g) && (g<='z')) 
      char1 = (unsigned char)(((sint32)g)-32);
    else 
      char1 = g;
    g_1 = (string2[index]);
    if (('a'<=g_1) && (g_1<='z')) 
      char2 = (unsigned char)(((sint32)g_1)-32);
    else 
      char2 = g_1;
    if (char1>char2) 
      return (Obj)(&T);
    else if (char1<char2) 
      return (Obj)NULL;
  }
  return (length1>length2) ? ((Obj)(&T)) : (Obj)NULL;
}

/* Translated from NCONC-1(T T) = T */

Obj nconc_1 (Obj list1, Obj list2)
{
  Obj cons_1, next_cdr;

  if (list1==NULL) 
    return list2;
  else {
    cons_1 = list1;
    next_cdr = CDR(cons_1);
    for (;next_cdr!=NULL;next_cdr = CDR(cons_1)) 
      cons_1 = next_cdr;
    CDR(cons_1) = list2;
    return list1;
  }
}

/* Translated from NRECONC(T T) = T */

Obj nreconc (Obj list1, Obj list2)
{
  Obj current_cdr, next_cdr;

  if (list1==NULL) 
    return list2;
  else {
    current_cdr = CDR(list1);
    CDR(list1) = list2;
    while (current_cdr!=NULL) {
      next_cdr = CDR(current_cdr);
      CDR(current_cdr) = list1;
      list1 = current_cdr;
      current_cdr = next_cdr;
    }

    return list1;
  }
}

static const Str_57 str_const_8
  = { 7, 53, 53, "Write-string-into-string overflow while appending ~s." };

/* Translated from WRITE-STRING-INTO-STRING(STRING STRING) = STRING */

unsigned char *write_string_into_string (unsigned char *string_to_append, 
        unsigned char *output_string)
{
  sint32 append_fill, output_fill, new_length, arg_temp;
  Str *string;

  append_fill = (sint32)(StrHDR(string_to_append)->fill_length);
  output_fill = (sint32)(StrHDR(output_string)->fill_length);
  new_length = (append_fill+output_fill);
  arg_temp = new_length;
  if (arg_temp>(sint32)(StrHDR(output_string)->length)) 
    error_one_arg((Obj)(&str_const_8),          /* "Write-string-into-string overflow while appending ..." */
        ObjStrHDR(string_to_append));
  memcpy((void *)(output_string+output_fill),(void *)(string_to_append+0),
      append_fill);
  (void)output_string;
  string = StrHDR(output_string);
  (string->body[new_length]) = '\000';
  string->fill_length = new_length;
  return output_string;
}

static const Str_49 str_const_9
  = { 7, 45, 45, "Write-char-into-string overflow to string ~s." };

/* Translated from WRITE-CHAR-INTO-STRING(CHARACTER STRING) = STRING */

unsigned char *write_char_into_string (unsigned char char_1, unsigned char *output_string)
{
  sint32 current_fill, arg_temp;
  Str *string;
  sint32 fill_1;

  current_fill = (sint32)(StrHDR(output_string)->fill_length);
  arg_temp = (sint32)(StrHDR(output_string)->length);
  if (!(arg_temp>current_fill)) 
    error_one_arg((Obj)(&str_const_9),          /* "Write-char-into-string overflow to string ~s." */
        ObjStrHDR(output_string));
  string = StrHDR(output_string);
  fill_1 = (current_fill+1);
  (string->body[fill_1]) = '\000';
  string->fill_length = fill_1;
  (output_string[current_fill]) = char_1;
  return output_string;
}

static const Str_9 str_const_10
  = { 7, 5, 5, "1.0.1" };

/* Translated from LISP-IMPLEMENTATION-VERSION() = STRING */

unsigned char *lisp_implementation_version (void)
{
  return ((Str *)(&str_const_10))->body;        /* "1.0.1" */
}

static const Str_9 str_const_11
  = { 7, 8, 8, "ThinLisp" };

/* Translated from LISP-IMPLEMENTATION-TYPE() = STRING */

unsigned char *lisp_implementation_type (void)
{
  return ((Str *)(&str_const_11))->body;        /* "ThinLisp" */
}

/* Translated from SYMS-TL-TL-BASICS() = VOID */

void syms_tl_tl_basics (void)
{
  return;
}


/* Translated from INIT-TL-TL-BASICS() = VOID */

void init_tl_tl_basics (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  return;
}

