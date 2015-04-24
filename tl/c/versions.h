/***
 *
 * Module:      tl/c/versions.h
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/versions.lisp.
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
  unsigned char body[45];
} Str_45;

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
  unsigned char body[17];
} Str_17;

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
  unsigned char body[25];
} Str_25;

typedef struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[29];
} Str_29;

extern Sym tl_versions_symbols[37];

extern Obj SpackageS;

extern Obj current_region;

extern Sym tl_boot_symbols[];

extern void collect_all_used_systems(Obj);
extern Obj find_package_1(Obj);
extern Obj find_package_or_error_1(Obj);
extern Obj format_function(Obj, unsigned char *, Obj);
extern Obj get(Obj, Obj, Obj);
extern sint32 get_platform_code(void);
extern Obj init_symbol_into_package(Obj, Obj, sint32, Obj);
extern Obj intern_string_in_package(unsigned char *, sint32, Obj);
extern Obj memq(Obj, Obj);
extern Obj nreverse(Obj);
extern Obj set_get(Obj, Obj, Obj);
extern sint32 sxhash_string(unsigned char *);
