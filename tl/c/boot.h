/***
 *
 * Module:      tl/c/boot.h
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/boot.lisp.
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
  unsigned char body[9];
} Str_9;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[17];
} Str_17;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[21];
} Str_21;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[13];
} Str_13;

extern Sym tl_boot_symbols[23];

extern Obj SpackageS;

extern Obj all_systems;

extern Obj current_system_being_loaded;

extern Obj find_package_1(Obj);
extern Obj init_symbol_into_package(Obj, Obj, sint32, Obj);
extern Obj set_get(Obj, Obj, Obj);
