/***
 *
 * Module:      tl/c/tl-util.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/tl-util.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "tl-util.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

/* Translated from PAIRLIS(LIST LIST &OPTIONAL LIST) = LIST */

Obj pairlis (Obj new_keys, Obj new_data, Obj alist)
{
  Obj key, tl_loop_list_, data, tl_loop_list__1;

  key = (Obj)NULL;
  tl_loop_list_ = new_keys;
  data = (Obj)NULL;
  tl_loop_list__1 = new_data;
  while (tl_loop_list_!=NULL) {
    key = CAR(tl_loop_list_);
    tl_loop_list_ = CDR(tl_loop_list_);
    if (tl_loop_list__1==NULL) 
      goto end_loop;
    data = CAR(tl_loop_list__1);
    tl_loop_list__1 = CDR(tl_loop_list__1);
    alist = alloc_cons(alloc_cons(key,data,-1),alist,-1);
  }

 end_loop:
  return alist;
}

/* Translated from COMPUTE-NEW-PLIST(T T T) = T */

Obj compute_new_plist (Obj plist, Obj property, Obj new_value)
{
  Obj temp;
  Thread_state *ts;
  Obj cons_1;

  ts = THREAD_STATE;
  temp = BOXFIX(0);
  bind_global(&current_region,ts,temp);
  cons_1 = plist;
  while (cons_1!=NULL) {
    if (CAR(cons_1)==property) {
      CAR(CDR(cons_1)) = new_value;
      goto exit_nil;
    }
    cons_1 = CDR(CDR(cons_1));
  }

  plist = alloc_cons(property,alloc_cons(new_value,plist,0),0);
 exit_nil:
  unbind_global(&current_region,ts);
  return plist;
}

/* Translated from FILL-LIST(LIST T) = LIST */

Obj fill_list (Obj sequence, Obj elt_1)
{
  Obj cons_1;

  cons_1 = sequence;
  for (;cons_1!=NULL;cons_1 = CDR(cons_1)) 
    CAR(cons_1) = elt_1;
  return sequence;
}

/* Translated from FILL-SIMPLE-VECTOR(SIMPLE-VECTOR T) = SIMPLE-VECTOR */

Obj *fill_simple_vector (Obj *sequence, Obj elt_1)
{
  sint32 index, tl_loop_bind_;

  index = 0;
  tl_loop_bind_ = (sint32)(SvHDR(sequence)->length);
  for (;!(index>=tl_loop_bind_);index = (index+1)) 
    (sequence[index]) = elt_1;
  return sequence;
}

/* Translated from FILL-ARRAY-UNSIGNED-BYTE-8((ARRAY (INTEGER 0 255)) (INTEGER 0 255)) = (ARRAY (INTEGER 0 255)) */

uint8 *fill_array_unsigned_byte_8 (uint8 *sequence, uint8 elt_1)
{
  sint32 index, tl_loop_bind_;

  index = 0;
  tl_loop_bind_ = (sint32)(((Sa_uint8 *)(Obj)(((uint32)sequence)-(uint32)(
      &(((Sa_uint8 *)NULL)->body[0]))))->fill_length);
  for (;!(index>=tl_loop_bind_);index = (index+1)) 
    (sequence[index]) = elt_1;
  return sequence;
}

/* Translated from FILL-ARRAY-UNSIGNED-BYTE-16((ARRAY (INTEGER 0 65535)) (INTEGER 0 65535)) = (ARRAY (INTEGER 0 65535)) */

uint16 *fill_array_unsigned_byte_16 (uint16 *sequence, uint16 elt_1)
{
  sint32 index, tl_loop_bind_;

  index = 0;
  tl_loop_bind_ = (sint32)(((Sa_uint16 *)(Obj)(((uint32)sequence)-(uint32)(
      &(((Sa_uint16 *)NULL)->body[0]))))->fill_length);
  for (;!(index>=tl_loop_bind_);index = (index+1)) 
    (sequence[index]) = elt_1;
  return sequence;
}

/* Translated from FILL-ARRAY-SIGNED-BYTE-16((ARRAY (INTEGER -32768 32767)) (INTEGER -32768 32767)) = (ARRAY (INTEGER -32768 32767)) */

sint16 *fill_array_signed_byte_16 (sint16 *sequence, sint16 elt_1)
{
  sint32 index, tl_loop_bind_;

  index = 0;
  tl_loop_bind_ = (sint32)(((Sa_sint16 *)(Obj)(((uint32)sequence)-(uint32)(
      &(((Sa_sint16 *)NULL)->body[0]))))->fill_length);
  for (;!(index>=tl_loop_bind_);index = (index+1)) 
    (sequence[index]) = elt_1;
  return sequence;
}

/* Translated from FILL-ARRAY-DOUBLE-FLOAT((ARRAY DOUBLE-FLOAT) DOUBLE-FLOAT) = (ARRAY DOUBLE-FLOAT) */

double *fill_array_double_float (double *sequence, double elt_1)
{
  sint32 index, tl_loop_bind_;

  index = 0;
  tl_loop_bind_ = (sint32)(((Sa_double *)(Obj)(((uint32)sequence)-(uint32)(
      &(((Sa_double *)NULL)->body[0]))))->length);
  for (;!(index>=tl_loop_bind_);index = (index+1)) 
    (sequence[index]) = elt_1;
  return sequence;
}

static const Str_45 str_const_1
  = { 7, 42, 42, "Unrecognized sequence-type of ~s for fill." };

/* Translated from FILL(T T) = T */

Obj fill (Obj sequence, Obj elt_1)
{
  sint32 temp;

  switch (TYPE_TAG(sequence,temp)) {
   case 0:
    break;
   case 2:
    fill_list(sequence,elt_1);
    break;
   case 6:
    fill_simple_vector(((Sv *)sequence)->body,elt_1);
    break;
   case 7:
    memset((void *)(((Str *)sequence)->body+0),UNBOXCHAR(elt_1),(sint32)(
        ((Str *)sequence)->fill_length));
    (void)sequence;
    break;
   case 8:
    fill_array_unsigned_byte_8(((Sa_uint8 *)sequence)->body,(uint8)UNBOXFIX(elt_1));
    break;
   case 9:
    fill_array_unsigned_byte_16(((Sa_uint16 *)sequence)->body,(uint16)UNBOXFIX(elt_1));
    break;
   case 18:
    fill_array_signed_byte_16(((Sa_sint16 *)sequence)->body,(sint16)UNBOXFIX(elt_1));
    break;
   case 10:
    fill_array_double_float(((Sa_double *)sequence)->body,((Ldouble *)elt_1)->body);
    break;
   default:
    error_one_arg((Obj)(&str_const_1),          /* "Unrecognized sequence-type of ~s for fill." */
        sequence);
    break;
  }
  return sequence;
}

/* Translated from SEARCH-LIST-FUNCTION(T T COMPILED-FUNCTION FIXNUM T FIXNUM T) = T */

Obj search_list_function (Obj pattern_list, Obj source_list, Obj test_func, 
        sint32 start1, Obj end1, sint32 start2, Obj end2)
{
  Obj pattern;
  sint32 index, tl_loop_bind_;
  Obj arg_temp, temp, temp_1, source_cons, tl_loop_iter_flag_, src_elt, 
        tl_loop_list_, pat_elt, tl_loop_list__1;
  int block_temp;

  pattern = nthcdr(start1,pattern_list);
  index = start2;
  temp = end2;
  if (temp==NULL) 
    temp = BOXFIX(length(source_list));
  arg_temp = temp;
  temp_1 = end1;
  if (temp_1==NULL) 
    temp_1 = BOXFIX(length(pattern_list));
  tl_loop_bind_ = UNBOXFIX((Obj)((((sint32)arg_temp)-(sint32)temp_1)+1));   /* Fixnum subtract */
  source_cons = nthcdr(start2,source_list);
  tl_loop_iter_flag_ = (Obj)(&T);
  for (;!(index>tl_loop_bind_);index = (index+1)) {
    if (tl_loop_iter_flag_==NULL) 
      source_cons = CDR(source_cons);
    if (source_cons==NULL) 
      goto end_loop;
    src_elt = (Obj)NULL;
    tl_loop_list_ = source_cons;
    pat_elt = (Obj)NULL;
    tl_loop_list__1 = pattern;
    while (tl_loop_list_!=NULL) {
      src_elt = CAR(tl_loop_list_);
      tl_loop_list_ = CDR(tl_loop_list_);
      if (tl_loop_list__1==NULL) 
        goto end_loop_1;
      pat_elt = CAR(tl_loop_list__1);
      tl_loop_list__1 = CDR(tl_loop_list__1);
      if (((Func_2 *)test_func)->c_function(src_elt,pat_elt)==NULL) {
        block_temp = 0;
        goto exit_nil;
      }
    }

   end_loop_1:
    block_temp = 1;
    goto exit_nil;
   exit_nil:
    if (block_temp) 
      return BOXFIX(index);
    tl_loop_iter_flag_ = (Obj)NULL;
  }
 end_loop:
  return (Obj)NULL;
}

/* Translated from GENERIC-SEARCH(T T COMPILED-FUNCTION FIXNUM T FIXNUM T) = T */

Obj generic_search (Obj pattern, Obj source, Obj test_func, sint32 start1, 
        Obj end1, sint32 start2, Obj end2)
{
  sint32 pattern_length;
  Obj temp;
  sint32 source_length;
  Obj temp_1;
  sint32 index, tl_loop_bind_, index2;
  int block_temp;

  if (((pattern==NULL) || (IMMED_TAG(pattern)==2)) && ((source  /* Consp */
      ==NULL) || (IMMED_TAG(source)==2)))       /* Consp */
    return search_list_function(pattern,source,test_func,start1,end1,start2,
        end2);
  else {
    temp = end1;
    if (temp==NULL) 
      temp = BOXFIX(length(pattern));
    pattern_length = UNBOXFIX(temp);
    temp_1 = end2;
    if (temp_1==NULL) 
      temp_1 = BOXFIX(length(source));
    source_length = UNBOXFIX(temp_1);
    index = start2;
    tl_loop_bind_ = (source_length-pattern_length);
    for (;!(index>tl_loop_bind_);index = (index+1)) {
      index2 = start1;
      for (;!(index2>=pattern_length);index2 = (index2+1)) {
        if (((Func_2 *)test_func)->c_function(generic_aref(pattern,index2),
            generic_aref(source,index+index2))==NULL) {
          block_temp = 0;
          goto exit_nil;
        }
      }
      block_temp = 1;
      goto exit_nil;
     exit_nil:
      if (block_temp) 
        return BOXFIX(index);
    }
    return (Obj)NULL;
  }
}

/* Translated from FLET-SEARCH-PREDICATE-IN-SEARCH-TEST-0(T T) = T */

Obj flet_search_predicate_in_search_test_0 (Obj x, Obj y)
{
  return eql(x,y);
}

static const Str_41 str_const_2
  = { 7, 38, 38, "FLET-SEARCH-PREDICATE-IN-SEARCH-TEST-0" };

/* Translated from SEARCH-TEST(T T) = * */

Obj search_test (Obj some_seq, Obj some_big_seq)
{
  Obj arg_temp, arg_temp_1, arg_temp_2, temp;

  arg_temp = some_seq;
  arg_temp_1 = some_big_seq;
  arg_temp_2 = (Obj)(&(tl_tl_util_funcs[0]));   /* #'FLET-SEARCH-PREDICATE-IN-SEARCH-TEST-0 */
  temp = generic_search(arg_temp,arg_temp_1,arg_temp_2,0,(Obj)NULL,0,(Obj)NULL);
  Values_count = 1;
  return temp;
}

/* Translated from GENERIC-SORT-VECTOR(T COMPILED-FUNCTION) = T */

Obj generic_sort_vector (Obj vector, Obj less_than_predicate)
{
  sint32 end_point, lesser_index;
  Obj lesser_elt;
  sint32 greater_index;
  Obj greater_elt;

  end_point = (length(vector)-1);
  for (;!(end_point<1);end_point = (end_point-1)) {
    lesser_index = 0;
    lesser_elt = (Obj)NULL;
    greater_index = 0;
    greater_elt = (Obj)NULL;
    for (;!(lesser_index>=end_point);lesser_index = (lesser_index+1)) {
      lesser_elt = generic_aref(vector,lesser_index);
      greater_index = (lesser_index+1);
      greater_elt = generic_aref(vector,greater_index);
      if (((Func_2 *)less_than_predicate)->c_function(greater_elt,lesser_elt)
          !=NULL) {
        generic_set_aref(vector,greater_index,lesser_elt);
        generic_set_aref(vector,lesser_index,greater_elt);
      }
    }
  }
  return vector;
}

/* Translated from QUICK-SORT-LIST(T FIXNUM T) = T */

Obj quick_sort_list (Obj l, sint32 n, Obj predicate)
{
  Obj first_1, cdr_1, second_1;
  sint32 half_n;
  Obj l_tail, l2, l1;
  int if_result_temp;
  Obj if_result_temp_1, if_result_temp_2;

  switch (n) {
   case 0:
   case 1:
    break;
   case 2:
    first_1 = CAR(l);
    cdr_1 = CDR(l);
    second_1 = CAR(cdr_1);
    if (((Func_2 *)predicate)->c_function(first_1,second_1)==NULL) {
      CAR(l) = second_1;
      CAR(cdr_1) = first_1;
    }
    break;
   default:
    half_n = (n>>1);
    l_tail = nthcdr(half_n-1,l);
    l2 = quick_sort_list(CDR(l_tail),n-half_n,predicate);
    CDR(l_tail) = (Obj)NULL;
    l1 = quick_sort_list(l,half_n,predicate);
    l = (Obj)NULL;
    while (1) {
      if (((Func_2 *)predicate)->c_function(CAR(l2),CAR(l1))==NULL) {
        if (l==NULL) 
          if_result_temp_1 = (l = l1);
        else 
          if_result_temp_1 = (CDR(l_tail) = l1);
        l_tail = if_result_temp_1;
        if ((l1 = ((l1!=NULL) ? CDR(l1) : (Obj)NULL))==NULL) 
          if_result_temp = ((CDR(l_tail) = l2)!=NULL);
        else 
          if_result_temp = 0;
      }
      else {
        if (l==NULL) 
          if_result_temp_2 = (l = l2);
        else 
          if_result_temp_2 = (CDR(l_tail) = l2);
        l_tail = if_result_temp_2;
        if ((l2 = ((l2!=NULL) ? CDR(l2) : (Obj)NULL))==NULL) 
          if_result_temp = ((CDR(l_tail) = l1)!=NULL);
        else 
          if_result_temp = 0;
      }
      if (if_result_temp) 
        goto end_loop;
    }

   end_loop:
    break;
  }
  return l;
}

/* Translated from GENERIC-POSITION(T T FIXNUM T COMPILED-FUNCTION) = T */

Obj generic_position (Obj item, Obj sequence, sint32 start, Obj end, Obj test_func)
{
  Obj elt_cons;
  sint32 index, tl_loop_bind_;
  Obj elt_1;

  if ((sequence==NULL) || (IMMED_TAG(sequence)==2)) {   /* Consp */
    elt_cons = nthcdr(start,sequence);
    index = start;
    if (end!=NULL) 
      tl_loop_bind_ = UNBOXFIX(end);
    else 
      tl_loop_bind_ = length(sequence);
    elt_1 = (Obj)NULL;
    for (;!(index>=tl_loop_bind_);index = (index+1)) {
      elt_1 = CAR(elt_cons);
      if (((Func_2 *)test_func)->c_function(item,elt_1)!=NULL) 
        return BOXFIX(index);
      elt_cons = CDR(elt_cons);
    }
    return (Obj)NULL;
  }
  else {
    index = start;
    if (end!=NULL) 
      tl_loop_bind_ = UNBOXFIX(end);
    else 
      tl_loop_bind_ = length(sequence);
    for (;!(index>=tl_loop_bind_);index = (index+1)) {
      if (((Func_2 *)test_func)->c_function(item,generic_aref(sequence,
          index))!=NULL) 
        return BOXFIX(index);
    }
    return (Obj)NULL;
  }
}

static const Str_49 str_const_3
  = { 7, 48, 48, "REMOVE only handles lists, not this sequence: ~s" };

/* Translated from NON-LIST-REMOVE-ERROR(T) = VOID */

void non_list_remove_error (Obj sequence)
{
  error_one_arg((Obj)(&str_const_3),            /* "REMOVE only handles lists, not this sequence: ~s" */
      sequence);
  return;
}

/* Translated from LIST-LENGTH(LIST) = T */

Obj list_length (Obj lis)
{
  sint32 n;
  Obj fast, slow;

  n = 0;
  fast = lis;
  slow = lis;
  while (1) {
    if (fast==NULL) 
      return BOXFIX(n);
    if ((fast = CDR(fast))==NULL) 
      return BOXFIX(n+1);
    if ((fast==slow) && (n>0)) 
      return (Obj)NULL;
    n = (n+2);
    fast = CDR(fast);
    slow = CDR(slow);
  }

  return (Obj)NULL;
}

/* Translated from TREE-EQUAL-TEST(T T T) = T */

Obj tree_equal_test (Obj a, Obj b, Obj test)
{
  if ((IMMED_TAG(a)==2) && (IMMED_TAG(b)==2)) {     /* Consp, Consp */
    if (tree_equal_test(CAR(a),CAR(b),test)!=NULL) 
      return tree_equal_test(CDR(a),CDR(b),test);
    else 
      return (Obj)NULL;
  }
  else 
    return ((Func_2 *)test)->c_function(a,b);
}

/* Translated from COPY-TREE(T) = T */

Obj copy_tree (Obj tree)
{
  Obj arg_temp;

  if (IMMED_TAG(tree)==2) {                     /* Consp */
    arg_temp = copy_tree(CAR(tree));
    return alloc_cons(arg_temp,copy_tree(CDR(tree)),-1);
  }
  else 
    return tree;
}

/* Translated from NSUBST-EQL-IDENT(T T T) = T */

Obj nsubst_eql_ident (Obj new_1, Obj old, Obj tree)
{
  if (eql(old,tree)!=NULL) 
    return new_1;
  else 
    return nsubst_eql_ident_aux(new_1,old,tree);
}

/* Translated from NSUBST-EQL-IDENT-AUX(T T T) = * */

Obj nsubst_eql_ident_aux (Obj new_1, Obj old, Obj tree)
{
  Obj temp;

  if (IMMED_TAG(tree)==2) {                     /* Consp */
    if (eql(old,CAR(tree))!=NULL) 
      CAR(tree) = new_1;
    else 
      nsubst_eql_ident_aux(new_1,old,CAR(tree));
    if (eql(old,CDR(tree))!=NULL) {
      temp = (CDR(tree) = new_1);
      Values_count = 1;
      return temp;
    }
    else 
      return nsubst_eql_ident_aux(new_1,old,CDR(tree));
  }
  else {
    Values_count = 1;
    return (Obj)NULL;
  }
}

static const Str_45 str_const_4
  = { 7, 42, 42, "unrecognized sequence type for sequence ~s" };

/* Translated from COPY-SEQ(T) = T */

Obj copy_seq (Obj sequence)
{
  Obj item, tl_loop_list_, tl_loopvar_, tl_loopvar__1, tl_loopvar__2;
  sint32 temp;
  unsigned char *g;

  if ((sequence==NULL) || (IMMED_TAG(sequence)==2)) {   /* Consp */
    item = (Obj)NULL;
    tl_loop_list_ = sequence;
    tl_loopvar_ = (Obj)NULL;
    tl_loopvar__1 = (Obj)NULL;
    tl_loopvar__2 = (Obj)NULL;
    while (tl_loop_list_!=NULL) {
      item = CAR(tl_loop_list_);
      tl_loop_list_ = CDR(tl_loop_list_);
      tl_loopvar__2 = alloc_cons(item,(Obj)NULL,-1);
      if (tl_loopvar__1!=NULL) {
        CDR(tl_loopvar__1) = tl_loopvar__2;
        (void)tl_loopvar__1;
      }
      else 
        tl_loopvar_ = tl_loopvar__2;
      tl_loopvar__1 = tl_loopvar__2;
    }

    return tl_loopvar_;
  }
  else 
    switch (TYPE_TAG(sequence,temp)) {
     case 6:
      return alloc_simple_vector(length(sequence),-1,6);
     case 7:
      g = (((Str *)alloc_string(length(sequence),-1,7))->body);
      memset((void *)(g+0),'\000',(sint32)(StrHDR(g)->fill_length));
      (void)g;
      return ObjStrHDR(g);
     case 10:
      return alloc_double_array(length(sequence),-1,10);
     case 8:
      return alloc_uint8_array(length(sequence),-1,8);
     case 9:
      return alloc_uint16_array(length(sequence),-1,9);
     default:
      return error_one_arg((Obj)(&str_const_4),     /* "unrecognized sequence type for sequence ~s" */
          sequence);
    }
}

/* Translated from SUBSTITUTE(T T T) = * */

Obj substitute (Obj new_item, Obj old_item, Obj sequence)
{
  Obj item, tl_loop_list_, tl_loopvar_, tl_loopvar__1, tl_loopvar__2, new_sequence;
  sint32 index, tl_loop_bind_;
  Obj item_1;

  if ((sequence==NULL) || (IMMED_TAG(sequence)==2)) {   /* Consp */
    item = (Obj)NULL;
    tl_loop_list_ = sequence;
    tl_loopvar_ = (Obj)NULL;
    tl_loopvar__1 = (Obj)NULL;
    tl_loopvar__2 = (Obj)NULL;
    while (tl_loop_list_!=NULL) {
      item = CAR(tl_loop_list_);
      tl_loop_list_ = CDR(tl_loop_list_);
      tl_loopvar__2 = alloc_cons((eql(item,old_item)!=NULL) ? new_item 
          : item,(Obj)NULL,-1);
      if (tl_loopvar__1!=NULL) {
        CDR(tl_loopvar__1) = tl_loopvar__2;
        (void)tl_loopvar__1;
      }
      else 
        tl_loopvar_ = tl_loopvar__2;
      tl_loopvar__1 = tl_loopvar__2;
    }

    Values_count = 1;
    return tl_loopvar_;
  }
  else {
    new_sequence = copy_seq(sequence);
    index = 0;
    tl_loop_bind_ = length(sequence);
    item_1 = (Obj)NULL;
    for (;!(index>=tl_loop_bind_);index = (index+1)) {
      item_1 = generic_aref(sequence,index);
      generic_set_aref(new_sequence,index,(eql(item_1,old_item)!=NULL) 
          ? new_item : item_1);
    }
    Values_count = 1;
    return new_sequence;
  }
}

/* Translated from VECTORP(T) = T */

Obj vectorp (Obj object)
{
  sint32 temp;

  switch (TYPE_TAG(object,temp)) {
   case 6:
    return (Obj)(&T);
   case 7:
    return (Obj)(&T);
   case 10:
    return (Obj)(&T);
   case 8:
    return (Obj)(&T);
   case 9:
    return (Obj)(&T);
   default:
    return (Obj)NULL;
  }
}

/* Translated from EQUALP(T T) = T */

Obj equalp (Obj a, Obj b)
{
  sint32 temp, temp_1, temp_2, temp_3, temp_4, a_length, index;

  switch (TYPE_TAG(a,temp)) {
   case 1:
    switch (TYPE_TAG(b,temp_1)) {
     case 1:
      return (((sint32)a)==(sint32)b) ? ((Obj)(&T)) : (Obj)NULL;
     case 5:
      return (((double)UNBOXFIX(a))==(((Ldouble *)b)->body)) ? ((Obj)(&T)) 
          : (Obj)NULL;
     default:
      return (Obj)NULL;
    }
   case 5:
    switch (TYPE_TAG(b,temp_2)) {
     case 1:
      return (((Ldouble *)a)->body==(double)UNBOXFIX(b)) ? ((Obj)(&T)) 
          : (Obj)NULL;
     case 5:
      return (((Ldouble *)a)->body==(((Ldouble *)b)->body)) ? ((Obj)(&T)) 
          : (Obj)NULL;
     default:
      return (Obj)NULL;
    }
   case 3:
    switch (TYPE_TAG(b,temp_3)) {
     case 3:
      return (a==b) ? ((Obj)(&T)) : (Obj)NULL;
     default:
      return (Obj)NULL;
    }
   case 2:
    if ((IMMED_TAG(b)==2) && (equalp(CAR(a),CAR(b))!=NULL))     /* Consp */
      return equalp(CDR(a),CDR(b));
    else 
      return (Obj)NULL;
   case 7:
    switch (TYPE_TAG(b,temp_4)) {
     case 7:
      return string_equal(a,b);
     default:
      return (Obj)NULL;
    }
   case 6:
    if (vectorp(b)!=NULL) {
      a_length = (sint32)(((Sv *)a)->length);
      if (a_length==length(b)) {
        index = 0;
        for (;!(index>=a_length);index = (index+1)) {
          if (equalp(((Sv *)a)->body[index],generic_aref(b,index))==NULL) 
            return (Obj)NULL;
        }
        return (Obj)(&T);
      }
      else 
        return (Obj)NULL;
    }
    else 
      return (Obj)NULL;
   case 8:
    if (vectorp(b)!=NULL) {
      a_length = (sint32)(((Sa_uint8 *)a)->fill_length);
      if (a_length==length(b)) {
        index = 0;
        for (;!(index>=a_length);index = (index+1)) {
          if (equalp(BOXFIX(((Sa_uint8 *)a)->body[index]),generic_aref(b,
              index))==NULL) 
            return (Obj)NULL;
        }
        return (Obj)(&T);
      }
      else 
        return (Obj)NULL;
    }
    else 
      return (Obj)NULL;
   case 9:
    if (vectorp(b)!=NULL) {
      a_length = (sint32)(((Sa_uint16 *)a)->fill_length);
      if (a_length==length(b)) {
        index = 0;
        for (;!(index>=a_length);index = (index+1)) {
          if (equalp(BOXFIX(((Sa_uint16 *)a)->body[index]),generic_aref(b,
              index))==NULL) 
            return (Obj)NULL;
        }
        return (Obj)(&T);
      }
      else 
        return (Obj)NULL;
    }
    else 
      return (Obj)NULL;
   case 10:
    if (vectorp(b)!=NULL) {
      a_length = (sint32)(((Sa_double *)a)->length);
      if (a_length==length(b)) {
        index = 0;
        for (;!(index>=a_length);index = (index+1)) {
          if (equalp(alloc_ldouble(((Sa_double *)a)->body[index],-1,5),
              generic_aref(b,index))==NULL) 
            return (Obj)NULL;
        }
        return (Obj)(&T);
      }
      else 
        return (Obj)NULL;
    }
    else 
      return (Obj)NULL;
   default:
    return (a==b) ? ((Obj)(&T)) : (Obj)NULL;
  }
}

static const Str_45 str_const_5
  = { 7, 44, 44, "Unhandled type in copy-optmized-constant: ~s" };

/* Translated from COPY-OPTIMIZED-CONSTANT(T) = T */

Obj copy_optimized_constant (Obj arg)
{
  Obj temp;
  Thread_state *ts;
  Obj temp_1;
  sint32 temp_2;
  Obj new_list, copied_cons, next_consP, arg_temp, g;
  sint32 temp_3;
  Obj temp_4;
  sint32 temp_5;
  Obj temp_6;
  sint32 length_1;
  Obj *sv;
  Obj *new_sv;
  sint32 index;
  Obj *arg_temp_1;
  sint32 arg_temp_2, temp_7;
  Obj temp_8;
  sint32 length_2;
  Obj new_string;
  unsigned char *g_1;
  unsigned char *g_2;

  ts = THREAD_STATE;
  temp = BOXFIX(0);
  bind_global(&current_region,ts,temp);
  switch (TYPE_TAG(arg,temp_2)) {
   case 0:
    temp_1 = arg;
    break;
   case 1:
    temp_1 = arg;
    break;
   case 3:
    temp_1 = arg;
    break;
   case 11:
    temp_1 = arg;
    break;
   case 2:
    new_list = copy_list(arg);
    copied_cons = new_list;
    next_consP = (Obj)NULL;
    for (;1;copied_cons = next_consP) {
      next_consP = CDR(copied_cons);
      if (!(IMMED_TAG(next_consP)==2))          /* Consp */
        goto end_loop;
      arg_temp = copied_cons;
      g = CAR(copied_cons);
      switch (TYPE_TAG(g,temp_3)) {
       case 0:
        temp_4 = g;
        break;
       case 1:
        temp_4 = g;
        break;
       case 3:
        temp_4 = g;
        break;
       case 11:
        temp_4 = g;
        break;
       default:
        temp_4 = copy_optimized_constant(g);
        break;
      }
      CAR(arg_temp) = temp_4;
    }
   end_loop:
    arg_temp = copied_cons;
    switch (TYPE_TAG(next_consP,temp_5)) {
     case 0:
      temp_6 = next_consP;
      break;
     case 1:
      temp_6 = next_consP;
      break;
     case 3:
      temp_6 = next_consP;
      break;
     case 11:
      temp_6 = next_consP;
      break;
     default:
      temp_6 = copy_optimized_constant(next_consP);
      break;
    }
    CDR(arg_temp) = temp_6;
    temp_1 = new_list;
    goto exit_nil;
   exit_nil:
    break;
   case 6:
    length_1 = (sint32)(((Sv *)arg)->length);
    sv = (((Sv *)arg)->body);
    new_sv = (((Sv *)alloc_simple_vector(length_1,0,6))->body);
    index = 0;
    for (;!(index>=length_1);index = (index+1)) {
      arg_temp_1 = new_sv;
      arg_temp_2 = index;
      g = (sv[index]);
      switch (TYPE_TAG(g,temp_7)) {
       case 0:
        temp_8 = g;
        break;
       case 1:
        temp_8 = g;
        break;
       case 3:
        temp_8 = g;
        break;
       case 11:
        temp_8 = g;
        break;
       default:
        temp_8 = copy_optimized_constant(g);
        break;
      }
      (arg_temp_1[arg_temp_2]) = temp_8;
    }
    temp_1 = ObjSvHDR(new_sv);
    break;
   case 7:
    length_2 = (sint32)(((Str *)arg)->fill_length);
    new_string = alloc_string(length_2,0,7);
    g_1 = (((Str *)new_string)->body);
    g_2 = (((Str *)arg)->body);
    memcpy((void *)(g_1+0),(void *)(g_2+0),length_2);
    temp_1 = new_string;
    break;
   default:
    temp_1 = error_one_arg((Obj)(&str_const_5),     /* "Unhandled type in copy-optmized-constant: ~s" */
        arg);
    break;
  }
  unbind_global(&current_region,ts);
  return temp_1;
}

static const Str_89 str_const_6
  = { 7, 88, 88, "Bad memory region name ~s.  Expected :dynamic, :static, or ~@\n               :temporary." };

/* Translated from REGION-NUMBER-OF-NAME(T) = FIXNUM */

sint32 region_number_of_name (Obj region_name)
{
  if (region_name==(Obj)(tl_tl_util_symbols+0))     /* DYNAMIC */
    return 0;
  else if (region_name==(Obj)(tl_tl_util_symbols+1))    /* STATIC */
    return 1;
  else if (region_name==(Obj)(tl_tl_util_symbols+2))    /* TEMPORARY */
    return 2;
  else 
    return (sint32)error_one_arg((Obj)(&str_const_6),   /* "Bad memory region name ~s.  Expected :dynamic, :st..." */
        region_name);
}

/* Translated from REALLOC-REGION-UP-TO-LIMIT(T FIXNUM) = VOID */

void realloc_region_up_to_limit (Obj region_name, sint32 target_size)
{
  sint32 region_number, size;

  region_number = region_number_of_name(region_name);
  size = region_number_bytes_size(region_number);
  if (size<target_size) {
    malloc_block_into_region(region_number,target_size-size,1);
    return;
  }
  else 
    return;
}

/* Translated from REGION-BYTES-SIZE(T) = FIXNUM */

sint32 region_bytes_size (Obj region_name)
{
  return region_number_bytes_size(region_number_of_name(region_name));
}

/* Translated from REGION-BYTES-USED(T) = FIXNUM */

sint32 region_bytes_used (Obj region_name)
{
  return region_number_bytes_used(region_number_of_name(region_name));
}

/* Translated from REGION-BYTES-AVAILABLE(T) = FIXNUM */

sint32 region_bytes_available (Obj region_name)
{
  return region_number_bytes_available(region_number_of_name(region_name));
}

/* Translated from GENERIC-SXHASH(T) = FIXNUM */

sint32 generic_sxhash (Obj object)
{
  sint32 temp;

  switch (TYPE_TAG(object,temp)) {
   case 1:
    return (sint32)abs((int)UNBOXFIX(object));
   case 5:
    return sxhash_double_float(((Ldouble *)object)->body);
   case 7:
    return sxhash_string(((Str *)object)->body);
   case 0:
   case 11:
    return (sint32)(((uint32)object)>>3);
   case 12:
    return (sint32)(((uint32)object)>>3);
   case 3:
    return (sint32)UNBOXCHAR(object);
   case 2:
    return sxhash_cons_tree(object);
   case 9:
    return sxhash_array_16(((Sa_uint16 *)object)->body);
   default:
    return 0;
  }
}

/* Translated from SXHASH-ARRAY-16((ARRAY (INTEGER 0 65535))) = FIXNUM */

sint32 sxhash_array_16 (uint16 *array_16)
{
  sint32 hash, index, tl_loop_bind_;

  hash = 0;
  index = 0;
  tl_loop_bind_ = (sint32)(((Sa_uint16 *)(Obj)(((uint32)array_16)-(uint32)(
      &(((Sa_uint16 *)NULL)->body[0]))))->fill_length);
  for (;!(index>=tl_loop_bind_);index = (index+1)) 
    hash = ((((hash<<1)&65535)+(hash>>15))^(sint32)(array_16[index]));
  return hash;
}

Obj Sdecompose_float_bufferS = (Obj)(&Unbound);

/* Translated from SXHASH-DOUBLE-FLOAT(DOUBLE-FLOAT) = FIXNUM */

sint32 sxhash_double_float (double double_float)
{
  sint32 hash, index;

  memcpy(((Sa_uint16 *)GET_GLOBAL(Sdecompose_float_bufferS))->body,&double_float,
      sizeof(double));
  hash = 0;
  index = 0;
  for (;!(index>3);index = (index+1)) 
    hash = ((((hash<<4)&268435455)+(hash>>24))^(sint32)(((Sa_uint16 *)GET_GLOBAL(Sdecompose_float_bufferS))->body[index]));
  return hash;
}

/* Translated from SXHASH-CONS-TREE(CONS) = FIXNUM */

sint32 sxhash_cons_tree (Obj cons_tree)
{
  sint32 hash;
  Obj next_cons;
  sint32 arg_temp;
  Obj g;

  hash = 0;
  next_cons = cons_tree;
  for (;next_cons!=NULL;next_cons = CDR(next_cons)) {
    arg_temp = (((hash<<5)&268435455)+(hash>>23));
    hash = (arg_temp^generic_sxhash(CAR(next_cons)));
    g = CDR(next_cons);
    if (!(IMMED_TAG(g)==2)) {                   /* Consp */
      arg_temp = (((hash<<5)&268435455)+(hash>>23));
      return arg_temp^generic_sxhash(CDR(next_cons));
    }
  }
  return hash;
}

static const Str_9 str_const_7
  = { 7, 7, 7, "KEYWORD" };

static const Str_9 str_const_8
  = { 7, 7, 7, "DYNAMIC" };

static const Str_9 str_const_9
  = { 7, 6, 6, "STATIC" };

static const Str_13 str_const_10
  = { 7, 9, 9, "TEMPORARY" };

Sym tl_tl_util_symbols[3];

Func tl_tl_util_funcs[1];

/* Translated from SYMS-TL-TL-UTIL() = VOID */

void syms_tl_tl_util (void)
{
  Obj cached_keyword_package;

  cached_keyword_package = find_package_1((Obj)(&str_const_7));     /* "KEYWORD" */
  init_symbol_into_package((Obj)(&(tl_tl_util_symbols[0])),(Obj)(&str_const_8),     /* "DYNAMIC" */
      7469,cached_keyword_package);
  (tl_tl_util_symbols[0]).external = 1;
  (tl_tl_util_symbols[0]).symbol_value = (Obj)(&(tl_tl_util_symbols[0]));
  init_symbol_into_package((Obj)(&(tl_tl_util_symbols[1])),(Obj)(&str_const_9),     /* "STATIC" */
      3241,cached_keyword_package);
  (tl_tl_util_symbols[1]).external = 1;
  (tl_tl_util_symbols[1]).symbol_value = (Obj)(&(tl_tl_util_symbols[1]));
  init_symbol_into_package((Obj)(&(tl_tl_util_symbols[2])),(Obj)(&str_const_10),    /* "TEMPORARY" */
      26713,cached_keyword_package);
  (tl_tl_util_symbols[2]).external = 1;
  (tl_tl_util_symbols[2]).symbol_value = (Obj)(&(tl_tl_util_symbols[2]));
  return;
}


/* Translated from INIT-TL-TL-UTIL() = VOID */

void init_tl_tl_util (void)
{
  (tl_tl_util_funcs[0]).type = 12;
  (tl_tl_util_funcs[0]).arg_count = 2;
  (tl_tl_util_funcs[0]).optional_arguments = 0;
  (tl_tl_util_funcs[0]).sets_values_count = 0;
  (tl_tl_util_funcs[0]).default_arguments = (Obj)NULL;
  (tl_tl_util_funcs[0]).closure_environment = (Obj)NULL;
  (tl_tl_util_funcs[0]).name = (Obj)(&str_const_2);     /* "FLET-SEARCH-PREDICATE-IN-SEARCH-TEST-0" */
  (tl_tl_util_funcs[0]).c_function = (Obj (*)(Obj))flet_search_predicate_in_search_test_0;
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  if (Sdecompose_float_bufferS==(Obj)(&Unbound)) 
    Sdecompose_float_bufferS = alloc_uint16_array(4,0,9);
  return;
}

