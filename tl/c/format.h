/***
 *
 * Module:      tl/c/format.h
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/format.lisp.
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
  unsigned char body[13];
} Str_13;

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
  unsigned char body[25];
} Str_25;

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
  unsigned char body[49];
} Str_49;

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
  unsigned char body[29];
} Str_29;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[65];
} Str_65;

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
  unsigned char body[77];
} Str_77;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[57];
} Str_57;

extern Sym tl_format_symbols[4];

extern Func tl_format_funcs[1];

extern Obj SpackageS;

extern Obj current_region;

extern Func tl_format_funcs[];

extern void bad_control_directive_error(Obj);
extern void bad_stream_error(Obj);
extern sint32 discard_format_arglist(unsigned char *, sint32, sint32);
extern sint32 find_end_of_conditional(unsigned char *, sint32, sint32);
extern Obj find_package_1(Obj);
extern sint32 fixnum_floor_first(sint32, sint32);
extern Obj format_error(Obj, Obj);
extern Obj init_symbol_into_package(Obj, Obj, sint32, Obj);
extern Obj last(Obj);
extern sint32 length(Obj);
extern sint32 mod_fixnums(sint32, sint32);
extern Obj princ(Obj, Obj);
extern void unsupported_control_char_error(Obj);
extern Obj write_list(Obj, Obj);
extern void write_symbol(Obj, Obj, Obj);
