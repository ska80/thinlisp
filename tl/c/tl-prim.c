/***
 *
 * Module:      tl/c/tl-prim.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/tl-prim.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "tl-prim.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

Obj current_system_being_loaded = (Obj)(&Unbound);

Obj all_systems = (Obj)(&Unbound);

Obj SfeaturesS = (Obj)(&Unbound);

static Obj cons_const[2]
  = 
#if defined(NO_ADDRESS_CONSTANTS)
  {(Obj)NULL, (Obj)NULL}
#else
  {(Obj)(tl_tl_prim_symbols+0), (Obj)NULL}      /* TL */
#endif
  ;

Obj S = (Obj)(&Unbound);

/* Translated from EVAL-FEATURE(T) = * */

Obj eval_feature (Obj feature_form)
{
  Obj loop_var, g, temp;

  loop_var = (Obj)NULL;
  if ((feature_form!=NULL) && ((IMMED_TAG(feature_form)==0) && (STD_TAG(feature_form)
      ==11))) {                                 /* SYMBOL-P */
    for (loop_var = GET_GLOBAL(SfeaturesS);loop_var!=NULL;loop_var = CDR(loop_var)) {
      if (CAR(loop_var)==feature_form) {
        Values_count = 1;
        return feature_form;
      }
    }
    Values_count = 1;
    return (Obj)NULL;
  }
  else if (!(IMMED_TAG(feature_form)==2)) {     /* Consp */
    Values_count = 1;
    return (Obj)NULL;
  }
  else {
    g = CAR(feature_form);
    if (g==(Obj)(tl_tl_prim_symbols+1)) {       /* AND */
      for (loop_var = CDR(feature_form);loop_var!=NULL;loop_var = CDR(loop_var)) {
        if (eval_feature(CAR(loop_var))==NULL) {
          Values_count = 1;
          return (Obj)NULL;
        }
      }
      Values_count = 1;
      return feature_form;
    }
    else if (g==(Obj)(tl_tl_prim_symbols+2)) {  /* OR */
      for (loop_var = CDR(feature_form);loop_var!=NULL;loop_var = CDR(loop_var)) {
        if (eval_feature(CAR(loop_var))!=NULL) {
          Values_count = 1;
          return feature_form;
        }
      }
      Values_count = 1;
      return (Obj)NULL;
    }
    else if (g==(Obj)(tl_tl_prim_symbols+3)) {  /* NOT */
      temp = ((eval_feature(CAR(CDR(feature_form)))==NULL) ? ((Obj)(&T)) 
          : (Obj)NULL);
      Values_count = 1;
      return temp;
    }
    else {
      Values_count = 1;
      return (Obj)NULL;
    }
  }
}

Obj lambda_list_keywords = (Obj)(&Unbound);

static Obj cons_const_1[16]
  = 
#if defined(NO_ADDRESS_CONSTANTS)
  {(Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, 
      (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, (Obj)NULL, 
      (Obj)NULL, (Obj)NULL, (Obj)NULL}
#else
  {(Obj)(tl_tl_prim_symbols+4), (Obj)(((uint32)(&(cons_const_1[2])))    /* &OPTIONAL */
      +2), (Obj)(tl_tl_prim_symbols+5), (Obj)(((uint32)(&(  /* &REST */
      cons_const_1[4])))+2), (Obj)(tl_tl_prim_symbols+6), (Obj)((   /* &KEY */
      (uint32)(&(cons_const_1[6])))+2), (Obj)(tl_tl_prim_symbols+7),    /* &AUX */
      (Obj)(((uint32)(&(cons_const_1[8])))+2), (Obj)(tl_tl_prim_symbols
      +8), (Obj)(((uint32)(&(cons_const_1[10])))+2), (Obj)(tl_tl_prim_symbols   /* &BODY */
      +9), (Obj)(((uint32)(&(cons_const_1[12])))+2), (Obj)(tl_tl_prim_symbols   /* &WHOLE */
      +10), (Obj)(((uint32)(&(cons_const_1[14])))   /* &ALLOW-OTHER-KEYS */
      +2), (Obj)(tl_tl_prim_symbols+11), (Obj)NULL}     /* &ENVIRONMENT */
#endif
  ;

/* Translated from GETF(T T &OPTIONAL T) = T */

Obj getf (Obj property_list, Obj property, Obj default_1)
{
  Obj g;

  g = property_list;
  for (;g!=NULL;g = CDR(CDR(g))) {
    if (CAR(g)==property) 
      return CAR(CDR(g));
  }
  return default_1;
}

/* Translated from GET(T T &OPTIONAL T) = T */

Obj get (Obj symbol, Obj property, Obj default_1)
{
  Obj g;

  if (symbol!=NULL) 
    g = (((Sym *)symbol)->symbol_plist);
  else 
    g = GET_GLOBAL(symbol_plist_of_nil);
  for (;g!=NULL;g = CDR(CDR(g))) {
    if (CAR(g)==property) 
      return CAR(CDR(g));
  }
  return default_1;
}

/* Translated from SET-GET(T T T) = T */

Obj set_get (Obj symbol, Obj property, Obj new_value)
{
  Obj original_plist, plist, plist_1;

  if (symbol!=NULL) {
    original_plist = (((Sym *)symbol)->symbol_plist);
    plist = original_plist;
    for (;plist!=NULL;plist = CDR(CDR(plist))) {
      if (CAR(plist)==property) {
        CAR(CDR(plist)) = new_value;
        goto exit_nil;
      }
    }
    ((Sym *)symbol)->symbol_plist = alloc_cons(property,alloc_cons(new_value,
        original_plist,-1),-1);
   exit_nil:
;
  }
  else {
    plist_1 = GET_GLOBAL(symbol_plist_of_nil);
    for (;plist_1!=NULL;plist_1 = CDR(CDR(plist_1))) {
      if (CAR(plist_1)==property) {
        CAR(CDR(plist_1)) = new_value;
        goto exit_nil_1;
      }
    }
    SET_GLOBAL(symbol_plist_of_nil,alloc_cons(property,alloc_cons(new_value,
        GET_GLOBAL(symbol_plist_of_nil),-1),-1));
   exit_nil_1:
;
  }
  return new_value;
}

/* Translated from CAAR(LIST) = T */

Obj caar (Obj list)
{
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CAR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CDAR(LIST) = T */

Obj cdar (Obj list)
{
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CDR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CADR(LIST) = T */

Obj cadr (Obj list)
{
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CAR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CDDR(LIST) = T */

Obj cddr (Obj list)
{
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CDR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CAAAR(LIST) = T */

Obj caaar (Obj list)
{
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CAR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CDAAR(LIST) = T */

Obj cdaar (Obj list)
{
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CDR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CADAR(LIST) = T */

Obj cadar (Obj list)
{
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CAR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CDDAR(LIST) = T */

Obj cddar (Obj list)
{
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CDR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CAADR(LIST) = T */

Obj caadr (Obj list)
{
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CAR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CDADR(LIST) = T */

Obj cdadr (Obj list)
{
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CDR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CADDR(LIST) = T */

Obj caddr (Obj list)
{
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CAR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CDDDR(LIST) = T */

Obj cdddr (Obj list)
{
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CDR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CAAAAR(LIST) = T */

Obj caaaar (Obj list)
{
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CAR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CDAAAR(LIST) = T */

Obj cdaaar (Obj list)
{
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CDR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CADAAR(LIST) = T */

Obj cadaar (Obj list)
{
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CAR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CDDAAR(LIST) = T */

Obj cddaar (Obj list)
{
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CDR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CAADAR(LIST) = T */

Obj caadar (Obj list)
{
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CAR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CDADAR(LIST) = T */

Obj cdadar (Obj list)
{
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CDR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CADDAR(LIST) = T */

Obj caddar (Obj list)
{
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CAR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CDDDAR(LIST) = T */

Obj cdddar (Obj list)
{
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CDR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CAAADR(LIST) = T */

Obj caaadr (Obj list)
{
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CAR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CDAADR(LIST) = T */

Obj cdaadr (Obj list)
{
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CDR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CADADR(LIST) = T */

Obj cadadr (Obj list)
{
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CAR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CDDADR(LIST) = T */

Obj cddadr (Obj list)
{
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CDR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CAADDR(LIST) = T */

Obj caaddr (Obj list)
{
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CAR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CDADDR(LIST) = T */

Obj cdaddr (Obj list)
{
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CAR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CDR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CADDDR(LIST) = T */

Obj cadddr (Obj list)
{
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CAR(list);
  else 
    return (Obj)NULL;
}

/* Translated from CDDDDR(LIST) = T */

Obj cddddr (Obj list)
{
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    list = CDR(list);
  else 
    return (Obj)NULL;
  if (list!=NULL) 
    return CDR(list);
  else 
    return (Obj)NULL;
}

/* Translated from FIRST(LIST) = T */

Obj first (Obj list)
{
  return (list!=NULL) ? CAR(list) : (Obj)NULL;
}

/* Translated from REST(LIST) = T */

Obj rest (Obj list)
{
  return (list!=NULL) ? CDR(list) : (Obj)NULL;
}

/* Translated from SECOND(LIST) = T */

Obj second (Obj list)
{
  Obj list_1;

  list_1 = list;
  if (list_1!=NULL) 
    list_1 = CDR(list_1);
  else 
    return (Obj)NULL;
  if (list_1!=NULL) 
    return CAR(list_1);
  else 
    return (Obj)NULL;
}

/* Translated from THIRD(LIST) = T */

Obj third (Obj list)
{
  Obj list_1;

  list_1 = list;
  if (list_1!=NULL) 
    list_1 = CDR(list_1);
  else 
    return (Obj)NULL;
  if (list_1!=NULL) 
    list_1 = CDR(list_1);
  else 
    return (Obj)NULL;
  if (list_1!=NULL) 
    return CAR(list_1);
  else 
    return (Obj)NULL;
}

/* Translated from FOURTH(LIST) = T */

Obj fourth (Obj list)
{
  Obj list_1, block_temp;

  list_1 = list;
  if (list_1!=NULL) 
    list_1 = CDR(list_1);
  else {
    block_temp = (Obj)NULL;
    goto exit_cdddr;
  }
  if (list_1!=NULL) 
    list_1 = CDR(list_1);
  else {
    block_temp = (Obj)NULL;
    goto exit_cdddr;
  }
  block_temp = ((list_1!=NULL) ? CDR(list_1) : (Obj)NULL);
 exit_cdddr:
  return (block_temp!=NULL) ? CAR(block_temp) : (Obj)NULL;
}

/* Translated from FIFTH(LIST) = T */

Obj fifth (Obj list)
{
  Obj list_1, list_2;

  list_2 = list;
  if (list_2!=NULL) 
    list_2 = CDR(list_2);
  else {
    list_1 = (Obj)NULL;
    goto exit_cdddr;
  }
  if (list_2!=NULL) 
    list_2 = CDR(list_2);
  else {
    list_1 = (Obj)NULL;
    goto exit_cdddr;
  }
  if (list_2!=NULL) 
    list_1 = CDR(list_2);
  else 
    list_1 = (Obj)NULL;
 exit_cdddr:
  if (list_1!=NULL) 
    list_1 = CDR(list_1);
  else 
    return (Obj)NULL;
  if (list_1!=NULL) 
    return CAR(list_1);
  else 
    return (Obj)NULL;
}

/* Translated from SIXTH(LIST) = T */

Obj sixth (Obj list)
{
  Obj list_1, list_2;

  list_2 = list;
  if (list_2!=NULL) 
    list_2 = CDR(list_2);
  else {
    list_1 = (Obj)NULL;
    goto exit_cdddr;
  }
  if (list_2!=NULL) 
    list_2 = CDR(list_2);
  else {
    list_1 = (Obj)NULL;
    goto exit_cdddr;
  }
  if (list_2!=NULL) 
    list_1 = CDR(list_2);
  else 
    list_1 = (Obj)NULL;
 exit_cdddr:
  if (list_1!=NULL) 
    list_1 = CDR(list_1);
  else 
    return (Obj)NULL;
  if (list_1!=NULL) 
    list_1 = CDR(list_1);
  else 
    return (Obj)NULL;
  if (list_1!=NULL) 
    return CAR(list_1);
  else 
    return (Obj)NULL;
}

/* Translated from SEVENTH(LIST) = T */

Obj seventh (Obj list)
{
  Obj list_1, list_2, block_temp;

  list_2 = list;
  if (list_2!=NULL) 
    list_2 = CDR(list_2);
  else {
    list_1 = (Obj)NULL;
    goto exit_cdddr;
  }
  if (list_2!=NULL) 
    list_2 = CDR(list_2);
  else {
    list_1 = (Obj)NULL;
    goto exit_cdddr;
  }
  if (list_2!=NULL) 
    list_1 = CDR(list_2);
  else 
    list_1 = (Obj)NULL;
 exit_cdddr:
  if (list_1!=NULL) 
    list_1 = CDR(list_1);
  else {
    block_temp = (Obj)NULL;
    goto exit_cdddr_1;
  }
  if (list_1!=NULL) 
    list_1 = CDR(list_1);
  else {
    block_temp = (Obj)NULL;
    goto exit_cdddr_1;
  }
  block_temp = ((list_1!=NULL) ? CDR(list_1) : (Obj)NULL);
 exit_cdddr_1:
  return (block_temp!=NULL) ? CAR(block_temp) : (Obj)NULL;
}

/* Translated from EIGHTH(LIST) = T */

Obj eighth (Obj list)
{
  Obj list_1, list_2, list_3;

  list_3 = list;
  if (list_3!=NULL) 
    list_3 = CDR(list_3);
  else {
    list_2 = (Obj)NULL;
    goto exit_cdddr;
  }
  if (list_3!=NULL) 
    list_3 = CDR(list_3);
  else {
    list_2 = (Obj)NULL;
    goto exit_cdddr;
  }
  if (list_3!=NULL) 
    list_2 = CDR(list_3);
  else 
    list_2 = (Obj)NULL;
 exit_cdddr:
  if (list_2!=NULL) 
    list_2 = CDR(list_2);
  else {
    list_1 = (Obj)NULL;
    goto exit_cdddr_1;
  }
  if (list_2!=NULL) 
    list_2 = CDR(list_2);
  else {
    list_1 = (Obj)NULL;
    goto exit_cdddr_1;
  }
  if (list_2!=NULL) 
    list_1 = CDR(list_2);
  else 
    list_1 = (Obj)NULL;
 exit_cdddr_1:
  if (list_1!=NULL) 
    list_1 = CDR(list_1);
  else 
    return (Obj)NULL;
  if (list_1!=NULL) 
    return CAR(list_1);
  else 
    return (Obj)NULL;
}

/* Translated from NINTH(LIST) = T */

Obj ninth (Obj list)
{
  Obj list_1, list_2, list_3;

  list_3 = list;
  if (list_3!=NULL) 
    list_3 = CDR(list_3);
  else {
    list_2 = (Obj)NULL;
    goto exit_cdddr;
  }
  if (list_3!=NULL) 
    list_3 = CDR(list_3);
  else {
    list_2 = (Obj)NULL;
    goto exit_cdddr;
  }
  if (list_3!=NULL) 
    list_2 = CDR(list_3);
  else 
    list_2 = (Obj)NULL;
 exit_cdddr:
  if (list_2!=NULL) 
    list_2 = CDR(list_2);
  else {
    list_1 = (Obj)NULL;
    goto exit_cdddr_1;
  }
  if (list_2!=NULL) 
    list_2 = CDR(list_2);
  else {
    list_1 = (Obj)NULL;
    goto exit_cdddr_1;
  }
  if (list_2!=NULL) 
    list_1 = CDR(list_2);
  else 
    list_1 = (Obj)NULL;
 exit_cdddr_1:
  if (list_1!=NULL) 
    list_1 = CDR(list_1);
  else 
    return (Obj)NULL;
  if (list_1!=NULL) 
    list_1 = CDR(list_1);
  else 
    return (Obj)NULL;
  if (list_1!=NULL) 
    return CAR(list_1);
  else 
    return (Obj)NULL;
}

/* Translated from TENTH(LIST) = T */

Obj tenth (Obj list)
{
  Obj list_1, list_2, list_3, block_temp;

  list_3 = list;
  if (list_3!=NULL) 
    list_3 = CDR(list_3);
  else {
    list_2 = (Obj)NULL;
    goto exit_cdddr;
  }
  if (list_3!=NULL) 
    list_3 = CDR(list_3);
  else {
    list_2 = (Obj)NULL;
    goto exit_cdddr;
  }
  if (list_3!=NULL) 
    list_2 = CDR(list_3);
  else 
    list_2 = (Obj)NULL;
 exit_cdddr:
  if (list_2!=NULL) 
    list_2 = CDR(list_2);
  else {
    list_1 = (Obj)NULL;
    goto exit_cdddr_1;
  }
  if (list_2!=NULL) 
    list_2 = CDR(list_2);
  else {
    list_1 = (Obj)NULL;
    goto exit_cdddr_1;
  }
  if (list_2!=NULL) 
    list_1 = CDR(list_2);
  else 
    list_1 = (Obj)NULL;
 exit_cdddr_1:
  if (list_1!=NULL) 
    list_1 = CDR(list_1);
  else {
    block_temp = (Obj)NULL;
    goto exit_cdddr_2;
  }
  if (list_1!=NULL) 
    list_1 = CDR(list_1);
  else {
    block_temp = (Obj)NULL;
    goto exit_cdddr_2;
  }
  block_temp = ((list_1!=NULL) ? CDR(list_1) : (Obj)NULL);
 exit_cdddr_2:
  return (block_temp!=NULL) ? CAR(block_temp) : (Obj)NULL;
}

/* Translated from LAST(LIST) = LIST */

Obj last (Obj list)
{
  Obj current_cons, next_consP;

  if (list!=NULL) {
    current_cons = list;
    next_consP = CDR(current_cons);
    while (next_consP!=NULL) {
      current_cons = next_consP;
      next_consP = CDR(current_cons);
    }

    return current_cons;
  }
  else 
    return (Obj)NULL;
}

static const Str_45 str_const_1
  = { 7, 42, 42, "REVERSE argument was not a string or list." };

/* Translated from REVERSE(T) = T */

Obj reverse (Obj sequence)
{
  sint32 temp;

  switch (TYPE_TAG(sequence,temp)) {
   case 0:
   case 2:
    return reverse_list(sequence);
   case 7:
    return ObjStrHDR(reverse_string(((Str *)sequence)->body));
   default:
    return (error((char *)(((Str *)(&str_const_1))->body)),     /* "REVERSE argument was not a string or list." */
        (Obj)NULL);
  }
}

/* Translated from REVERSE-LIST(LIST) = LIST */

Obj reverse_list (Obj list)
{
  Obj current_cons, reversed_list;

  if (list!=NULL) {
    current_cons = list;
    reversed_list = (Obj)NULL;
    while (current_cons!=NULL) {
      reversed_list = alloc_cons(CAR(current_cons),reversed_list,-1);
      current_cons = CDR(current_cons);
    }

    return reversed_list;
  }
  else 
    return (Obj)NULL;
}

/* Translated from REVERSE-STRING(STRING) = STRING */

unsigned char *reverse_string (unsigned char *string)
{
  sint32 length_1;
  unsigned char *new_string;
  unsigned char *g;
  sint32 index, reverse_index;

  length_1 = (sint32)(StrHDR(string)->fill_length);
  g = (((Str *)alloc_string(length_1,-1,7))->body);
  memset((void *)(g+0),'\000',(sint32)(StrHDR(g)->fill_length));
  (void)g;
  new_string = g;
  index = 0;
  reverse_index = (length_1-1);
  while (index<length_1) {
    (new_string[reverse_index]) = (string[index]);
    index = (index+1);
    reverse_index = (reverse_index-1);
  }

  return new_string;
}

/* Translated from NREVERSE(LIST) = LIST */

Obj nreverse (Obj list)
{
  Obj current_cdr, next_cdr;

  if (list!=NULL) {
    current_cdr = CDR(list);
    CDR(list) = (Obj)NULL;
    while (current_cdr!=NULL) {
      next_cdr = CDR(current_cdr);
      CDR(current_cdr) = list;
      list = current_cdr;
      current_cdr = next_cdr;
    }

    return list;
  }
  else 
    return (Obj)NULL;
}

static const Str_9 str_const_2
  = { 7, 7, 7, "KEYWORD" };

static const Str_5 str_const_3
  = { 7, 3, 3, "AND" };

static const Str_5 str_const_4
  = { 7, 2, 2, "OR" };

static const Str_5 str_const_5
  = { 7, 3, 3, "NOT" };

static const Str_13 str_const_6
  = { 7, 9, 9, "&OPTIONAL" };

static const Str_9 str_const_7
  = { 7, 5, 5, "&REST" };

static const Str_5 str_const_8
  = { 7, 4, 4, "&KEY" };

static const Str_5 str_const_9
  = { 7, 4, 4, "&AUX" };

static const Str_9 str_const_10
  = { 7, 5, 5, "&BODY" };

static const Str_9 str_const_11
  = { 7, 6, 6, "&WHOLE" };

static const Str_21 str_const_12
  = { 7, 17, 17, "&ALLOW-OTHER-KEYS" };

static const Str_13 str_const_13
  = { 7, 12, 12, "&ENVIRONMENT" };

Sym tl_tl_prim_symbols[12];

/* Translated from SYMS-TL-TL-PRIM() = VOID */

void syms_tl_tl_prim (void)
{
  Obj cached_keyword_package, cached_tl_package;

  cached_keyword_package = find_package_1((Obj)(&str_const_2));     /* "KEYWORD" */
  init_symbol_into_package((Obj)(&(tl_tl_prim_symbols[0])),(Obj)(&str_const),   /* "TL" */
      228,cached_keyword_package);
  (tl_tl_prim_symbols[0]).external = 1;
  (tl_tl_prim_symbols[0]).symbol_value = (Obj)(&(tl_tl_prim_symbols[0]));
  cached_tl_package = find_package_1((Obj)(&str_const));    /* "TL" */
  init_symbol_into_package((Obj)(&(tl_tl_prim_symbols[1])),(Obj)(&str_const_3),     /* "AND" */
      476,cached_tl_package);
  (tl_tl_prim_symbols[1]).external = 1;
  init_symbol_into_package((Obj)(&(tl_tl_prim_symbols[2])),(Obj)(&str_const_4),     /* "OR" */
      204,cached_tl_package);
  (tl_tl_prim_symbols[2]).external = 1;
  init_symbol_into_package((Obj)(&(tl_tl_prim_symbols[3])),(Obj)(&str_const_5),     /* "NOT" */
      498,cached_tl_package);
  (tl_tl_prim_symbols[3]).external = 1;
  init_symbol_into_package((Obj)(&(tl_tl_prim_symbols[4])),(Obj)(&str_const_6),     /* "&OPTIONAL" */
      6174,cached_tl_package);
  (tl_tl_prim_symbols[4]).external = 1;
  init_symbol_into_package((Obj)(&(tl_tl_prim_symbols[5])),(Obj)(&str_const_7),     /* "&REST" */
      278,cached_tl_package);
  (tl_tl_prim_symbols[5]).external = 1;
  init_symbol_into_package((Obj)(&(tl_tl_prim_symbols[6])),(Obj)(&str_const_8),     /* "&KEY" */
      207,cached_tl_package);
  (tl_tl_prim_symbols[6]).external = 1;
  init_symbol_into_package((Obj)(&(tl_tl_prim_symbols[7])),(Obj)(&str_const_9),     /* "&AUX" */
      198,cached_tl_package);
  (tl_tl_prim_symbols[7]).external = 1;
  init_symbol_into_package((Obj)(&(tl_tl_prim_symbols[8])),(Obj)(&str_const_10),    /* "&BODY" */
      413,cached_tl_package);
  (tl_tl_prim_symbols[8]).external = 1;
  init_symbol_into_package((Obj)(&(tl_tl_prim_symbols[9])),(Obj)(&str_const_11),    /* "&WHOLE" */
      529,cached_tl_package);
  (tl_tl_prim_symbols[9]).external = 1;
  init_symbol_into_package((Obj)(&(tl_tl_prim_symbols[10])),(Obj)(&str_const_12),   /* "&ALLOW-OTHER-KEYS" */
      2151,cached_tl_package);
  (tl_tl_prim_symbols[10]).external = 1;
  init_symbol_into_package((Obj)(&(tl_tl_prim_symbols[11])),(Obj)(&str_const_13),   /* "&ENVIRONMENT" */
      53428,cached_tl_package);
  (tl_tl_prim_symbols[11]).external = 1;
  return;
}


/* Translated from INIT-TL-TL-PRIM() = VOID */

void init_tl_tl_prim (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  if (current_system_being_loaded==(Obj)(&Unbound)) 
    current_system_being_loaded = (Obj)NULL;
  if (all_systems==(Obj)(&Unbound)) 
    all_systems = (Obj)NULL;
#if defined(NO_ADDRESS_CONSTANTS)
    (cons_const[0]) = (Obj)(tl_tl_prim_symbols+0);  /* TL */
#endif
  if (SfeaturesS==(Obj)(&Unbound)) 
    SfeaturesS = (Obj)(((uint32)(&(cons_const[0])))+2);
  if (S==(Obj)(&Unbound)) 
    S = (Obj)NULL;
#if defined(NO_ADDRESS_CONSTANTS)
  {
    hook_up_cdrs(cons_const_1,8,(Obj)NULL);
    (cons_const_1[0]) = (Obj)(tl_tl_prim_symbols+4);    /* &OPTIONAL */
    (cons_const_1[2]) = (Obj)(tl_tl_prim_symbols+5);    /* &REST */
    (cons_const_1[4]) = (Obj)(tl_tl_prim_symbols+6);    /* &KEY */
    (cons_const_1[6]) = (Obj)(tl_tl_prim_symbols+7);    /* &AUX */
    (cons_const_1[8]) = (Obj)(tl_tl_prim_symbols+8);    /* &BODY */
    (cons_const_1[10]) = (Obj)(tl_tl_prim_symbols+9);   /* &WHOLE */
    (cons_const_1[12]) = (Obj)(tl_tl_prim_symbols+10);  /* &ALLOW-OTHER-KEYS */
    (cons_const_1[14]) = (Obj)(tl_tl_prim_symbols+11);  /* &ENVIRONMENT */
  }
#endif
  lambda_list_keywords = (Obj)(((uint32)(&(cons_const_1[0])))+2);
  return;
}

