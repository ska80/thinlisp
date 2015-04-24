/***
 *
 * Module:      tl/c/tl-prim.h
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/tl-prim.lisp.
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
  unsigned char body[9];
} Str_9;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[13];
} Str_13;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[21];
} Str_21;

extern Sym tl_tl_prim_symbols[12];

extern Obj SpackageS;

extern Obj symbol_plist_of_nil;

extern Obj find_package_1(Obj);
extern Obj init_symbol_into_package(Obj, Obj, sint32, Obj);
extern Obj reverse_list(Obj);
extern unsigned char * reverse_string(unsigned char *);
