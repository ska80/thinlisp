/***
 *
 * Module:      tl/c/tl-extension.h
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/tl-extension.lisp.
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
  unsigned char body[125];
} Str_125;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[77];
} Str_77;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[113];
} Str_113;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[121];
} Str_121;

extern Obj SpackageS;

extern Obj find_package_1(Obj);
extern Obj generic_fceiling_one(Obj);
extern Obj generic_ffloor_one(Obj);
extern Obj memq(Obj, Obj);
extern sint32 two_arg_gcdf(sint32, sint32);
extern unsigned char write_char(unsigned char, Obj);
extern sint32 write_fixnum(sint32, sint32, sint32, Obj);
extern unsigned char * write_string_function(unsigned char *, Obj, sint32, 
    Obj);
