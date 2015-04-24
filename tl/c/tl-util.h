/***
 *
 * Module:      tl/c/tl-util.h
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/tl-util.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */


typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[5];
} Str_5;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[45];
} Str_45;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[41];
} Str_41;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[49];
} Str_49;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[89];
} Str_89;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[9];
} Str_9;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[13];
} Str_13;

extern Sym tl_tl_util_symbols[3];

extern Func tl_tl_util_funcs[1];

extern Obj SpackageS;

extern Obj current_region;

extern Func tl_tl_util_funcs[];

extern Obj copy_list(Obj);
extern Obj eql(Obj, Obj);
extern Obj error_one_arg(Obj, Obj);
extern Obj find_package_1(Obj);
extern Obj generic_aref(Obj, sint32);
extern Obj generic_set_aref(Obj, sint32, Obj);
extern Obj init_symbol_into_package(Obj, Obj, sint32, Obj);
extern sint32 length(Obj);
extern Obj nsubst_eql_ident_aux(Obj, Obj, Obj);
extern Obj nthcdr(sint32, Obj);
extern Obj string_equal(Obj, Obj);
extern sint32 sxhash_array_16(uint16 *);
extern sint32 sxhash_cons_tree(Obj);
extern sint32 sxhash_double_float(double);
extern sint32 sxhash_string(unsigned char *);
