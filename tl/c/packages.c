/***
 *
 * Module:      tl/c/packages.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/packages.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "packages.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

Obj SpackageS = (Obj)(&Unbound);

/* Translated from SXHASH-STRING(STRING) = FIXNUM */

sint32 sxhash_string (unsigned char *text_string)
{
  sint32 hash, index, tl_loop_bind_;

  hash = 0;
  index = 0;
  tl_loop_bind_ = (sint32)(StrHDR(text_string)->fill_length);
  for (;!(index>=tl_loop_bind_);index = (index+1)) 
    hash = ((((hash<<1)&65535)+(hash>>15))^(sint32)(text_string[index]));
  return hash;
}

/* Translated from INIT-SYMBOL(SYMBOL T FIXNUM) = SYMBOL */

Obj init_symbol (Obj symbol, Obj string, sint32 string_hash)
{
  ((Sym *)symbol)->type = 11;
  ((Sym *)symbol)->local_value = 1;
  ((Sym *)symbol)->external = 0;
  ((Sym *)symbol)->balance = 0;
  ((Sym *)symbol)->imported = 0;
  ((Sym *)symbol)->name_hash = string_hash;
  ((Sym *)symbol)->symbol_name = string;
  ((Sym *)symbol)->symbol_value = (Obj)(&Unbound);
  ((Sym *)symbol)->symbol_plist = (Obj)NULL;
  ((Sym *)symbol)->symbol_package = (Obj)NULL;
  ((Sym *)symbol)->symbol_function = (Obj)(&Unbound);
  ((Sym *)symbol)->left_branch = (Obj)(&Unbound);
  ((Sym *)symbol)->right_branch = (Obj)(&Unbound);
  return symbol;
}

/* Translated from INIT-SYMBOL-INTO-PACKAGE(SYMBOL T FIXNUM PACKAGE) = SYMBOL */

Obj init_symbol_into_package (Obj symbol, Obj string, sint32 string_hash, 
        Obj package)
{
  init_symbol(symbol,string,string_hash);
  if (package!=NULL) 
    insert_symbol_into_package(symbol,package);
  return symbol;
}

static const Str_65 str_const_1
  = { 7, 62, 62, "Can\'t insert ~a in ~a, a symbol with that name already exists." };

/* Translated from INSERT-SYMBOL-INTO-PACKAGE(SYMBOL PACKAGE) = VOID */

void insert_symbol_into_package (Obj symbol, Obj package)
{
  sint32 hash_number;
  unsigned char *name;
  Obj current_symbol, last_symbol;
  sint32 current_hash, compare_result;

  if (((Pkg *)package)->root_symbol!=(Obj)(&Unbound)) {
    hash_number = (sint32)(((Sym *)symbol)->name_hash);
    name = (((Str *)(((Sym *)symbol)->symbol_name))->body);
    current_symbol = (((Pkg *)package)->root_symbol);
    last_symbol = (Obj)NULL;
    current_hash = 0;
    while (1) {
      last_symbol = current_symbol;
      current_hash = (sint32)(((Sym *)current_symbol)->name_hash);
      if (hash_number<current_hash) {
        current_symbol = (((Sym *)current_symbol)->left_branch);
        if (current_symbol==(Obj)(&Unbound)) {
          ((Sym *)last_symbol)->left_branch = symbol;
          ((Sym *)symbol)->symbol_package = package;
          return;
        }
      }
      else if (hash_number>current_hash) {
        current_symbol = (((Sym *)current_symbol)->right_branch);
        if (current_symbol==(Obj)(&Unbound)) {
          ((Sym *)last_symbol)->right_branch = symbol;
          ((Sym *)symbol)->symbol_package = package;
          return;
        }
      }
      else {
        compare_result = strcmp((char *)name,(char *)(((Str *)(((Sym *)current_symbol)->symbol_name))->body));
        if (compare_result<0) {
          current_symbol = (((Sym *)current_symbol)->left_branch);
          if (current_symbol==(Obj)(&Unbound)) {
            ((Sym *)last_symbol)->left_branch = symbol;
            ((Sym *)symbol)->symbol_package = package;
            return;
          }
        }
        else if (compare_result>0) {
          current_symbol = (((Sym *)current_symbol)->right_branch);
          if (current_symbol==(Obj)(&Unbound)) {
            ((Sym *)last_symbol)->right_branch = symbol;
            ((Sym *)symbol)->symbol_package = package;
            return;
          }
        }
        else 
          error_two_args((Obj)(&str_const_1),   /* "Can't insert ~a in ~a, a symbol with that name alr..." */
              symbol,package);
      }
    }

    return;
  }
  else {
    ((Sym *)symbol)->symbol_package = package;
    ((Pkg *)package)->root_symbol = symbol;
    return;
  }
}

/* Translated from FIND-SYMBOL-IN-SINGLE-PACKAGE(STRING FIXNUM PACKAGE) = T */

Obj find_symbol_in_single_package (unsigned char *string, sint32 string_hash, 
        Obj package)
{
  Obj current_symbol;
  sint32 current_hash, compare_result;

  current_symbol = (((Pkg *)package)->root_symbol);
  current_hash = 0;
  if (current_symbol==(Obj)(&Unbound)) 
    return BOXFIX(0);
  while (current_symbol!=(Obj)(&Unbound)) {
    current_hash = (sint32)(((Sym *)current_symbol)->name_hash);
    if (string_hash<current_hash) 
      current_symbol = (((Sym *)current_symbol)->left_branch);
    else if (string_hash>current_hash) 
      current_symbol = (((Sym *)current_symbol)->right_branch);
    else {
      compare_result = strcmp((char *)string,(char *)(((Str *)(((Sym *)current_symbol)->symbol_name))->body));
      if (compare_result<0) 
        current_symbol = (((Sym *)current_symbol)->left_branch);
      else if (compare_result>0) 
        current_symbol = (((Sym *)current_symbol)->right_branch);
      else 
        return current_symbol;
    }
  }

  return BOXFIX(0);
}

Obj all_packages = (Obj)(&Unbound);

/* Translated from LIST-ALL-PACKAGES() = T */

Obj list_all_packages (void)
{
  return GET_GLOBAL(all_packages);
}

static const Str_5 str_const_2
  = { 7, 3, 3, "NIL" };

static const Str_37 str_const_3
  = { 7, 35, 35, "FIND-PACKAGE given bad argument ~a." };

/* Translated from FIND-PACKAGE-1(T) = T */

Obj find_package_1 (Obj string_or_symbol_or_package)
{
  sint32 temp;
  unsigned char *name;
  Obj package, tl_loop_list_;
  unsigned char *g;

  switch (TYPE_TAG(string_or_symbol_or_package,temp)) {
   case 7:
    name = (((Str *)string_or_symbol_or_package)->body);
    package = (Obj)NULL;
    tl_loop_list_ = GET_GLOBAL(all_packages);
    while (tl_loop_list_!=NULL) {
      package = CAR(tl_loop_list_);
      tl_loop_list_ = CDR(tl_loop_list_);
      g = (((Str *)(((Pkg *)package)->name))->body);
      if (strcmp((char *)g,(char *)name)==0) 
        return package;
    }

    return (Obj)NULL;
   case 0:
   case 11:
    return find_package_1((string_or_symbol_or_package!=NULL) ? ((Sym *)string_or_symbol_or_package)->symbol_name 
        : (Obj)(&str_const_2));                 /* "NIL" */
   case 13:
    return string_or_symbol_or_package;
   default:
    return error_one_arg((Obj)(&str_const_3),   /* "FIND-PACKAGE given bad argument ~a." */
        string_or_symbol_or_package);
  }
}

static const Str_41 str_const_4
  = { 7, 39, 39, "No package with name ~a could be found." };

/* Translated from FIND-PACKAGE-OR-ERROR-1(T) = PACKAGE */

Obj find_package_or_error_1 (Obj name)
{
  Obj find_result, if_result_temp;

  find_result = find_package_1(name);
  if (find_result!=NULL) 
    if_result_temp = find_result;
  else 
    if_result_temp = error_one_arg((Obj)(&str_const_4),     /* "No package with name ~a could be found." */
        name);
  return if_result_temp;
}

static const Str_101 str_const_5
  = { 7, 98, 98, "Use list for ~a differs from compile-time list: ~@\n                   compile-time = ~s, new = ~s." };

/* Translated from MAKE-PACKAGE-1(STRING T) = T */

Obj make_package_1 (unsigned char *name, Obj use)
{
  Obj temp;
  Thread_state *ts;
  Obj temp_1, name_to_use, use_list, used, tl_loop_list_, used_package, 
        tl_loopvar_, tl_loopvar__1, tl_loopvar__2, found_packageP, used_package_cons, 
        new_package_cons;
  int block_temp;
  Obj new_package;

  ts = THREAD_STATE;
  temp = BOXFIX(0);
  bind_global(&current_region,ts,temp);
  name_to_use = ObjStrHDR(string_upcase_function(name,0,(Obj)NULL));
  used = (Obj)NULL;
  tl_loop_list_ = use;
  used_package = (Obj)NULL;
  tl_loopvar_ = (Obj)NULL;
  tl_loopvar__1 = (Obj)NULL;
  tl_loopvar__2 = (Obj)NULL;
  while (tl_loop_list_!=NULL) {
    used = CAR(tl_loop_list_);
    tl_loop_list_ = CDR(tl_loop_list_);
    used_package = find_package_or_error_1(used);
    tl_loopvar__2 = alloc_cons(used_package,(Obj)NULL,0);
    if (tl_loopvar__1!=NULL) {
      CDR(tl_loopvar__1) = tl_loopvar__2;
      (void)tl_loopvar__1;
    }
    else 
      tl_loopvar_ = tl_loopvar__2;
    tl_loopvar__1 = tl_loopvar__2;
  }

  use_list = tl_loopvar_;
  goto exit_nil;
 exit_nil:
  found_packageP = find_package_1(name_to_use);
  if (found_packageP!=NULL) {
    used_package_cons = (((Pkg *)find_package_or_error_1(found_packageP))->used_packages);
    new_package_cons = use_list;
    for (;(used_package_cons!=NULL) && (new_package_cons!=NULL);new_package_cons 
        = CDR(new_package_cons)) {
      if (CAR(used_package_cons)!=CAR(new_package_cons)) {
        block_temp = 0;
        goto exit_nil_1;
      }
      used_package_cons = CDR(used_package_cons);
    }
    if ((used_package_cons!=NULL) || (new_package_cons!=NULL)) {
      block_temp = 0;
      goto exit_nil_1;
    }
    block_temp = 1;
    goto exit_nil_1;
   exit_nil_1:
    if (!block_temp) 
      error_three_args((Obj)(&str_const_5),     /* "Use list for ~a differs from compile-time list: ~@..." */
          name_to_use,((Pkg *)found_packageP)->used_packages,use);
    temp_1 = found_packageP;
  }
  else {
    new_package = alloc_package(name_to_use,use_list,0,13);
    SET_GLOBAL(all_packages,alloc_cons(new_package,GET_GLOBAL(all_packages),
        0));
    temp_1 = new_package;
  }
  unbind_global(&current_region,ts);
  return temp_1;
}

/* Translated from FIND-SYMBOL-IN-PACKAGE(STRING FIXNUM PACKAGE) = * */

Obj find_symbol_in_package (unsigned char *string, sint32 string_hash, 
        Obj package)
{
  Obj found_symbol, used_package, tl_loop_list_, found_inherited_symbol, 
        temp;

  found_symbol = find_symbol_in_single_package(string,string_hash,package);
  if (found_symbol==BOXFIX(0)) {
    used_package = (Obj)NULL;
    tl_loop_list_ = (((Pkg *)package)->used_packages);
    found_inherited_symbol = (Obj)NULL;
    while (tl_loop_list_!=NULL) {
      used_package = CAR(tl_loop_list_);
      tl_loop_list_ = CDR(tl_loop_list_);
      found_inherited_symbol = find_symbol_in_single_package(string,string_hash,
          used_package);
      if (found_inherited_symbol!=BOXFIX(0)) {
        if (((Sym *)found_inherited_symbol)->imported) 
          temp = (((Sym *)found_inherited_symbol)->symbol_value);
        else 
          temp = found_inherited_symbol;
        (Values_buffer[0]) = (Obj)(tl_packages_symbols+0);  /* INHERITED */
        Values_count = 2;
        return temp;
      }
    }

    temp = (Obj)NULL;
    (Values_buffer[0]) = (Obj)NULL;
    Values_count = 2;
    return temp;
  }
  else {
    if (((Sym *)found_symbol)->imported) 
      temp = (((Sym *)found_symbol)->symbol_value);
    else 
      temp = found_symbol;
    (Values_buffer[0]) = (((Sym *)found_symbol)->external ? ((Obj)(tl_packages_symbols
        +1)) : (Obj)(tl_packages_symbols+2));   /* EXTERNAL, INTERNAL */
    Values_count = 2;
    return temp;
  }
}

/* Translated from INTERN-STRING-IN-PACKAGE(STRING FIXNUM PACKAGE) = * */

Obj intern_string_in_package (unsigned char *string, sint32 hash_number, 
        Obj package)
{
  Obj symbol, foundP, temp, temp_1;
  Thread_state *ts;
  Obj temp_2, new_symbol, temp_3;

  symbol = find_symbol_in_package(string,hash_number,package);
  foundP = ((Values_count>1) ? (Values_buffer[0]) : (Obj)NULL);
  if (foundP!=NULL) {
    temp = symbol;
    (Values_buffer[0]) = foundP;
    Values_count = 2;
    return temp;
  }
  else {
    ts = THREAD_STATE;
    temp_1 = BOXFIX(0);
    bind_global(&current_region,ts,temp_1);
    new_symbol = alloc_symbol(1,11);
    init_symbol(new_symbol,ObjStrHDR(string),hash_number);
    ((Sym *)new_symbol)->symbol_package = package;
    insert_symbol_into_package(new_symbol,package);
    temp_3 = new_symbol;
    (Values_buffer[0]) = (Obj)(tl_packages_symbols+2);  /* INTERNAL */
    Values_count = 2;
    temp_2 = temp_3;
    unbind_global(&current_region,ts);
    return temp_2;
  }
}

static const Str_113 str_const_6
  = { 7, 110, 110, "The symbol ~a cannot be imported into ~a, a symbol with ~\n                    that name is already accessible." };

/* Translated from IMPORT(T &OPTIONAL T) = T */

Obj import (Obj symbol_or_symbol_list, Obj package_arg)
{
  Obj temp;
  Thread_state *ts;
  Obj temp_1, symbol_list;
  Obj temp_list[2];
  Obj package, symbol, tl_loop_list_, symbol_name;
  sint32 symbol_hash;
  Obj found_symbol, foundP, import_symbol;

  ts = THREAD_STATE;
  temp = BOXFIX(0);
  bind_global(&current_region,ts,temp);
  if ((symbol_or_symbol_list==NULL) || (IMMED_TAG(symbol_or_symbol_list)
      ==2))                                     /* Consp */
    symbol_list = symbol_or_symbol_list;
  else {
    (temp_list[0]) = symbol_or_symbol_list;
    (temp_list[1]) = (Obj)NULL;
    symbol_list = (Obj)(((uint32)temp_list)+2);
  }
  package = find_package_or_error_1(package_arg);
  symbol = (Obj)NULL;
  tl_loop_list_ = symbol_list;
  symbol_name = (Obj)NULL;
  symbol_hash = 0;
  while (tl_loop_list_!=NULL) {
    symbol = CAR(tl_loop_list_);
    tl_loop_list_ = CDR(tl_loop_list_);
    symbol_name = ((symbol!=NULL) ? ((Sym *)symbol)->symbol_name : (Obj)(
        &str_const_2));                         /* "NIL" */
    symbol_hash = (sint32)(((Sym *)symbol)->name_hash);
    found_symbol = find_symbol_in_package(((Str *)symbol_name)->body,symbol_hash,
        package);
    foundP = ((Values_count>1) ? (Values_buffer[0]) : (Obj)NULL);
    if ((foundP!=NULL) && (found_symbol!=symbol)) 
      error_two_args((Obj)(&str_const_6),       /* "The symbol ~a cannot be imported into ~a, a symbol..." */
          symbol,package);
    if (((Sym *)symbol)->symbol_package==NULL) 
      insert_symbol_into_package(symbol,package);
    else {
      import_symbol = alloc_symbol(1,11);
      init_symbol(import_symbol,symbol_name,symbol_hash);
      ((Sym *)import_symbol)->imported = 1;
      ((Sym *)import_symbol)->symbol_value = symbol;
      insert_symbol_into_package(import_symbol,package);
    }
  }

  temp_1 = (Obj)(&T);
  unbind_global(&current_region,ts);
  return temp_1;
}

static const Str_105 str_const_7
  = { 7, 103, 103, "The symbol ~a cannot be exported from ~a, it is not ~\n                    accessible from that package." };

static const Str_57 str_const_8
  = { 7, 56, 56, "Bad second value ~a from find-symbol received by export." };

/* Translated from EXPORT(T &OPTIONAL T) = T */

Obj export (Obj symbol_or_symbol_list, Obj package_arg)
{
  Obj temp;
  Thread_state *ts;
  Obj temp_1, symbol_list;
  Obj temp_list[2];
  Obj package, symbol, tl_loop_list_, symbol_name;
  sint32 symbol_hash;
  Obj found_symbol, foundP, import_symbol;

  ts = THREAD_STATE;
  temp = BOXFIX(0);
  bind_global(&current_region,ts,temp);
  if ((symbol_or_symbol_list==NULL) || (IMMED_TAG(symbol_or_symbol_list)
      ==2))                                     /* Consp */
    symbol_list = symbol_or_symbol_list;
  else {
    (temp_list[0]) = symbol_or_symbol_list;
    (temp_list[1]) = (Obj)NULL;
    symbol_list = (Obj)(((uint32)temp_list)+2);
  }
  package = find_package_or_error_1(package_arg);
  symbol = (Obj)NULL;
  tl_loop_list_ = symbol_list;
  symbol_name = (Obj)NULL;
  symbol_hash = 0;
  while (tl_loop_list_!=NULL) {
    symbol = CAR(tl_loop_list_);
    tl_loop_list_ = CDR(tl_loop_list_);
    symbol_name = ((symbol!=NULL) ? ((Sym *)symbol)->symbol_name : (Obj)(
        &str_const_2));                         /* "NIL" */
    symbol_hash = (sint32)(((Sym *)symbol)->name_hash);
    found_symbol = find_symbol_in_package(((Str *)symbol_name)->body,symbol_hash,
        package);
    foundP = ((Values_count>1) ? (Values_buffer[0]) : (Obj)NULL);
    if (!((foundP!=NULL) && (found_symbol==symbol))) 
      error_two_args((Obj)(&str_const_7),       /* "The symbol ~a cannot be exported from ~a, it is no..." */
          symbol,package);
    if (!(foundP==(Obj)(tl_packages_symbols+1))) {  /* EXTERNAL */
      if (foundP==(Obj)(tl_packages_symbols+2))     /* INTERNAL */
        ((Sym *)symbol)->external = 1;
      else if (foundP==(Obj)(tl_packages_symbols+0)) {  /* INHERITED */
        import_symbol = alloc_symbol(1,11);
        init_symbol(import_symbol,symbol_name,symbol_hash);
        ((Sym *)import_symbol)->imported = 1;
        ((Sym *)import_symbol)->symbol_value = symbol;
        insert_symbol_into_package(import_symbol,package);
        ((Sym *)import_symbol)->external = 1;
      }
      else 
        error_one_arg((Obj)(&str_const_8),      /* "Bad second value ~a from find-symbol received by e..." */
            foundP);
    }
  }

  temp_1 = (Obj)(&T);
  unbind_global(&current_region,ts);
  return temp_1;
}

Obj Skeyword_packageS = (Obj)(&Unbound);

static const Str_9 str_const_9
  = { 7, 7, 7, "KEYWORD" };

Obj Sgensym_counterS = (Obj)(&Unbound);

static const Str_5 str_const_10
  = { 7, 1, 1, "G" };

static const Str_9 str_const_11
  = { 7, 8, 8, "~a~v,\'0d" };

/* Translated from MAKE-GENSYMED-SYMBOL(T) = SYMBOL */

Obj make_gensymed_symbol (Obj string_or_counterP)
{
  Obj prefix;
  sint32 counter;
  Obj temp;
  Thread_state *ts;
  Obj temp_1;
  sint32 counter_length;
  Obj new_name;
  unsigned char *g;
  Obj arg_temp;
  Obj temp_list[6];

  prefix = (Obj)(&str_const_10);                /* "G" */
  counter = UNBOXFIX(GET_GLOBAL(Sgensym_counterS));
  if (IMMED_TAG(string_or_counterP)==1)         /* Fixnump */
    counter = UNBOXFIX(string_or_counterP);
  else if ((string_or_counterP!=NULL) && ((IMMED_TAG(string_or_counterP)
      ==0) && (STD_TAG(string_or_counterP)==7))) {  /* STRING-P */
    prefix = string_or_counterP;
    SET_GLOBAL(Sgensym_counterS,(Obj)((((sint32)GET_GLOBAL(Sgensym_counterS))
        +(sint32)BOXFIX(1))-1));                /* Fixnum add */
  }
  else 
    SET_GLOBAL(Sgensym_counterS,(Obj)((((sint32)GET_GLOBAL(Sgensym_counterS))
        +(sint32)BOXFIX(1))-1));                /* Fixnum add */
  ts = THREAD_STATE;
  temp = BOXFIX(0);
  bind_global(&current_region,ts,temp);
  counter_length = (((sint32)log10((double)counter))+1);
  g = (((Str *)alloc_string(counter_length+(sint32)(((Str *)prefix)->fill_length),
      0,7))->body);
  memset((void *)(g+0),'\000',(sint32)(StrHDR(g)->fill_length));
  (void)g;
  new_name = ObjStrHDR(g);
  generic_set_fill_pointer(new_name,0);
  arg_temp = new_name;
  (temp_list[0]) = prefix;
  (temp_list[2]) = BOXFIX(counter_length);
  (temp_list[4]) = BOXFIX(counter);
  format_function(arg_temp,((Str *)(&str_const_11))->body,  /* "~a~v,'0d" */
      hook_up_cdrs(temp_list,3,NULL));
  arg_temp = alloc_symbol(1,11);
  temp_1 = init_symbol(arg_temp,new_name,sxhash_string(((Str *)new_name)->body));
  unbind_global(&current_region,ts);
  return temp_1;
}

static const Str_5 str_const_12
  = { 7, 1, 1, ":" };

static const Str_5 str_const_13
  = { 7, 2, 2, "::" };

/* Translated from WRITE-SYMBOL(SYMBOL T T) = VOID */

void write_symbol (Obj symbol, Obj case_1, Obj streamP)
{
  Obj name_string, home_package, arg_temp;
  unsigned char *arg_temp_1;
  sint32 arg_temp_2;
  int temp;
  unsigned char *name;
  sint32 length_1;
  Obj stream;
  sint32 temp_1;
  Obj stringP;
  sint32 index;
  unsigned char g, g_1;
  unsigned char *g_2;
  sint32 g_3;
  Str *string;
  sint32 fill_1, index_1;
  unsigned char arg_temp_3, if_result_temp;
  sint32 index_2;
  unsigned char g_4;
  sint32 g_5, fill_2;
  Obj first_1;
  sint32 index_3;
  unsigned char char_1;
  Obj alpha_charP, temp_2;
  sint32 g_6, fill_3;

  if (symbol!=NULL) 
    name_string = (((Sym *)symbol)->symbol_name);
  else 
    name_string = (Obj)(&str_const_2);          /* "NIL" */
  if (GET_GLOBAL(Sprint_escapeS)!=NULL) {
    if ((((symbol!=NULL) && ((IMMED_TAG(symbol)==0) && (STD_TAG(symbol)
        ==11))) && (symbol!=NULL)) && (((Sym *)symbol)->symbol_package  /* SYMBOL-P */
        ==GET_GLOBAL(Skeyword_packageS))) 
      write_string_function(((Str *)(&str_const_12))->body,streamP,     /* ":" */
          0,(Obj)NULL);
    else {
      home_package = (((Sym *)symbol)->symbol_package);
      if (home_package!=GET_GLOBAL(SpackageS)) {
        arg_temp_1 = (((Str *)name_string)->body);
        arg_temp_2 = sxhash_string(((Str *)name_string)->body);
        arg_temp = find_symbol_in_package(arg_temp_1,arg_temp_2,find_package_or_error_1(
            GET_GLOBAL(SpackageS)));
        temp = (arg_temp!=symbol);
      }
      else 
        temp = 0;
      if (temp) {
        write_string_function(((Str *)(((Pkg *)home_package)->name))->body,
            streamP,0,(Obj)NULL);
        write_string_function(((Sym *)symbol)->external ? ((Str *)(&str_const_12))->body    /* ":" */
            : (((Str *)(&str_const_13))->body),streamP,0,(Obj)NULL);    /* "::" */
      }
    }
  }
  name = (((Str *)name_string)->body);
  length_1 = (sint32)(((Str *)name_string)->fill_length);
  switch (TYPE_TAG(streamP,temp_1)) {
   case 0:
   case 11:
    if (streamP==NULL) 
      stream = GET_GLOBAL(Sstandard_outputS);
    else if (streamP==(Obj)(&T)) 
      stream = GET_GLOBAL(Sterminal_ioS);
    else 
      stream = get_string_or_file_stream_for_output(streamP,length_1);
    break;
   case 16:
    stream = streamP;
    break;
   case 7:
    stream = streamP;
    break;
   default:
    stream = get_string_or_file_stream_for_output(streamP,length_1);
    break;
  }
  stringP = (((stream!=NULL) && ((IMMED_TAG(stream)==0) && (STD_TAG(stream)
      ==7))) ? ((Obj)(&T)) : (Obj)NULL);        /* STRING-P */
  if (case_1==(Obj)(tl_format_symbols+0)) {     /* UPCASE */
    if (stringP!=NULL) {
      index = 0;
      for (;index<length_1;index = (index+1)) {
        g_1 = (name[index]);
        if (('a'<=g_1) && (g_1<='z')) 
          g = (unsigned char)(((sint32)g_1)-32);
        else 
          g = g_1;
        g_2 = (((Str *)stream)->body);
        g_3 = (sint32)(StrHDR(g_2)->fill_length);
        string = StrHDR(g_2);
        fill_1 = (g_3+1);
        (string->body[fill_1]) = '\000';
        string->fill_length = fill_1;
        (g_2[g_3]) = g;
      }
      return;
    }
    else {
      index_1 = 0;
      for (;index_1<length_1;index_1 = (index_1+1)) {
        g = (name[index_1]);
        if (('a'<=g) && (g<='z')) 
          if_result_temp = (unsigned char)(((sint32)g)-32);
        else 
          if_result_temp = g;
        arg_temp_3 = if_result_temp;
        putc((char)arg_temp_3,((File_strm *)stream)->output);
      }
      return;
    }
  }
  else if (case_1==(Obj)(tl_format_symbols+1)) {    /* DOWNCASE */
    index_2 = 0;
    for (;index_2<length_1;index_2 = (index_2+1)) {
      g_4 = (name[index_2]);
      if (('A'<=g_4) && (g_4<='Z')) 
        g = (unsigned char)(((sint32)g_4)+32);
      else 
        g = g_4;
      if (stringP!=NULL) {
        g_5 = (sint32)(((Str *)stream)->fill_length);
        string = (Str *)stream;
        fill_2 = (g_5+1);
        (string->body[fill_2]) = '\000';
        string->fill_length = fill_2;
        (((Str *)stream)->body[g_5]) = g;
      }
      else 
        putc((char)g,((File_strm *)stream)->output);
      (void)g;
    }
    return;
  }
  else {
    first_1 = (Obj)(&T);
    index_3 = 0;
    for (;!(index_3>=length_1);index_3 = (index_3+1)) {
      char_1 = (name[index_3]);
      if ('a'<=char_1) 
        temp_2 = ((char_1<='z') ? ((Obj)(&T)) : (Obj)NULL);
      else 
        temp_2 = (Obj)NULL;
      if (temp_2==NULL) {
        if ('A'<=char_1) 
          temp_2 = ((char_1<='Z') ? ((Obj)(&T)) : (Obj)NULL);
        else 
          temp_2 = (Obj)NULL;
      }
      alpha_charP = temp_2;
      if (first_1!=NULL) {
        if (alpha_charP!=NULL) {
          if (('a'<=char_1) && (char_1<='z')) 
            if_result_temp = (unsigned char)(((sint32)char_1)-32);
          else 
            if_result_temp = char_1;
          char_1 = if_result_temp;
          first_1 = (Obj)NULL;
        }
      }
      else if (alpha_charP==NULL) 
        first_1 = (Obj)(&T);
      else {
        if (('A'<=char_1) && (char_1<='Z')) 
          if_result_temp = (unsigned char)(((sint32)char_1)+32);
        else 
          if_result_temp = char_1;
        char_1 = if_result_temp;
      }
      if (stringP!=NULL) {
        g_6 = (sint32)(((Str *)stream)->fill_length);
        string = (Str *)stream;
        fill_3 = (g_6+1);
        (string->body[fill_3]) = '\000';
        string->fill_length = fill_3;
        (((Str *)stream)->body[g_6]) = char_1;
      }
      else 
        putc((char)char_1,((File_strm *)stream)->output);
      (void)char_1;
    }
    return;
  }
}

static const Str_13 str_const_14
  = { 7, 9, 9, "INHERITED" };

static const Str_9 str_const_15
  = { 7, 8, 8, "EXTERNAL" };

static const Str_9 str_const_16
  = { 7, 8, 8, "INTERNAL" };

Sym tl_packages_symbols[3];

/* Translated from SYMS-TL-PACKAGES() = VOID */

void syms_tl_packages (void)
{
  Obj cached_keyword_package;

  cached_keyword_package = find_package_1((Obj)(&str_const_9));     /* "KEYWORD" */
  init_symbol_into_package((Obj)(&(tl_packages_symbols[0])),(Obj)(&str_const_14),   /* "INHERITED" */
      29270,cached_keyword_package);
  (tl_packages_symbols[0]).external = 1;
  (tl_packages_symbols[0]).symbol_value = (Obj)(&(tl_packages_symbols[0]));
  init_symbol_into_package((Obj)(&(tl_packages_symbols[1])),(Obj)(&str_const_15),   /* "EXTERNAL" */
      14646,cached_keyword_package);
  (tl_packages_symbols[1]).external = 1;
  (tl_packages_symbols[1]).symbol_value = (Obj)(&(tl_packages_symbols[1]));
  init_symbol_into_package((Obj)(&(tl_packages_symbols[2])),(Obj)(&str_const_16),   /* "INTERNAL" */
      15030,cached_keyword_package);
  (tl_packages_symbols[2]).external = 1;
  (tl_packages_symbols[2]).symbol_value = (Obj)(&(tl_packages_symbols[2]));
  return;
}


/* Translated from INIT-TL-PACKAGES() = VOID */

void init_tl_packages (void)
{
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  if (Skeyword_packageS==(Obj)(&Unbound)) 
    Skeyword_packageS = find_package_1((Obj)(&str_const_9));    /* "KEYWORD" */
  if (Sgensym_counterS==(Obj)(&Unbound)) 
    Sgensym_counterS = BOXFIX(1);
  return;
}

