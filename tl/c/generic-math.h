/***
 *
 * Module:      tl/c/generic-math.h
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/generic-math.lisp.
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
  unsigned char body[49];
} Str_49;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[33];
} Str_33;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[61];
} Str_61;

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
  unsigned char body[9];
} Str_9;

extern Obj SpackageS;

extern Obj error_three_args(Obj, Obj, Obj, Obj);
extern Obj error_two_args(Obj, Obj, Obj);
extern Obj find_package_1(Obj);
extern sint32 fixnum_floor_first(sint32, sint32);
extern double fmod(double, double);
extern void integer_divide_error(Obj, Obj);
extern sint32 mod_fixnums(sint32, sint32);
extern double mod_float(double, double);
