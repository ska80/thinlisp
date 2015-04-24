/***
 *
 * Module:      tl/c/apply.h
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/apply.lisp.
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
  unsigned char body[49];
} Str_49;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[57];
} Str_57;

extern Obj SpackageS;

extern Obj error_one_arg(Obj, Obj);
extern Obj error_three_args(Obj, Obj, Obj, Obj);
extern Obj error_two_args(Obj, Obj, Obj);
extern Obj find_package_1(Obj);
extern sint32 length(Obj);
extern Obj nthcdr(sint32, Obj);
