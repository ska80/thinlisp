/***
 *
 * Module:      tl/c/format.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/format.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "format.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

static const Str_9 str_const_1
  = { 7, 7, 7, "newline" };

static const Str_9 str_const_2
  = { 7, 5, 5, "space" };

static const Str_9 str_const_3
  = { 7, 6, 6, "rubout" };

static const Str_5 str_const_4
  = { 7, 4, 4, "page" };

static const Str_5 str_const_5
  = { 7, 3, 3, "tab" };

static const Str_13 str_const_6
  = { 7, 9, 9, "backspace" };

static const Str_9 str_const_7
  = { 7, 6, 6, "return" };

static const Str_5 str_const_8
  = { 7, 4, 4, "null" };

/* Translated from CHAR-NAME(CHARACTER) = T */

Obj char_name (unsigned char character)
{
  switch ((sint32)character) {
   case 10:
    return (Obj)(&str_const_1);                 /* "newline" */
   case 32:
    return (Obj)(&str_const_2);                 /* "space" */
   case 127:
    return (Obj)(&str_const_3);                 /* "rubout" */
   case 12:
    return (Obj)(&str_const_4);                 /* "page" */
   case 9:
    return (Obj)(&str_const_5);                 /* "tab" */
   case 8:
    return (Obj)(&str_const_6);                 /* "backspace" */
   case 13:
    return (Obj)(&str_const_7);                 /* "return" */
   case 0:
    return (Obj)(&str_const_8);                 /* "null" */
   default:
    return (Obj)NULL;
  }
}

/* Translated from COPY-STRING(STRING FIXNUM) = T */

Obj copy_string (unsigned char *string, sint32 length_1)
{
  Obj new_string;
  unsigned char *g;
  unsigned char *g_1;

  g = (((Str *)alloc_string(length_1,-1,7))->body);
  memset((void *)(g+0),'\000',(sint32)(StrHDR(g)->fill_length));
  (void)g;
  new_string = ObjStrHDR(g);
  g_1 = (((Str *)new_string)->body);
  memcpy((void *)(g_1+0),(void *)(string+0),length_1);
  return new_string;
}

/* Translated from NSTRING-UPCASE-FUNCTION(STRING FIXNUM T) = STRING */

unsigned char *nstring_upcase_function (unsigned char *string, sint32 start, 
        Obj end)
{
  sint32 limit, index;
  unsigned char *arg_temp;
  sint32 arg_temp_1;
  unsigned char g, if_result_temp;

  if (end!=NULL) 
    limit = UNBOXFIX(end);
  else 
    limit = (sint32)(StrHDR(string)->fill_length);
  index = start;
  for (;!(index>=limit);index = (index+1)) {
    arg_temp = string;
    arg_temp_1 = index;
    g = (string[index]);
    if (('a'<=g) && (g<='z')) 
      if_result_temp = (unsigned char)(((sint32)g)-32);
    else 
      if_result_temp = g;
    (arg_temp[arg_temp_1]) = if_result_temp;
  }
  return string;
}

/* Translated from STRING-UPCASE-FUNCTION(STRING FIXNUM T) = STRING */

unsigned char *string_upcase_function (unsigned char *string, sint32 start, 
        Obj end)
{
  sint32 length_1, limit, index;
  unsigned char g;
  unsigned char *arg_temp;

  length_1 = (sint32)(StrHDR(string)->fill_length);
  if (end!=NULL) 
    limit = UNBOXFIX(end);
  else 
    limit = length_1;
  index = start;
  for (;!(index>=limit);index = (index+1)) {
    g = (string[index]);
    if (('a'<=g) && (g<='z')) {
      arg_temp = (((Str *)copy_string(string,length_1))->body);
      return nstring_upcase_function(arg_temp,index,BOXFIX(limit));
    }
  }
  return string;
}

/* Translated from NSTRING-DOWNCASE-FUNCTION(STRING FIXNUM T) = STRING */

unsigned char *nstring_downcase_function (unsigned char *string, sint32 start, 
        Obj end)
{
  sint32 limit, index;
  unsigned char *arg_temp;
  sint32 arg_temp_1;
  unsigned char g, if_result_temp;

  if (end!=NULL) 
    limit = UNBOXFIX(end);
  else 
    limit = (sint32)(StrHDR(string)->fill_length);
  index = start;
  for (;!(index>=limit);index = (index+1)) {
    arg_temp = string;
    arg_temp_1 = index;
    g = (string[index]);
    if (('A'<=g) && (g<='Z')) 
      if_result_temp = (unsigned char)(((sint32)g)+32);
    else 
      if_result_temp = g;
    (arg_temp[arg_temp_1]) = if_result_temp;
  }
  return string;
}

/* Translated from STRING-DOWNCASE-FUNCTION(STRING FIXNUM T) = STRING */

unsigned char *string_downcase_function (unsigned char *string, sint32 start, 
        Obj end)
{
  sint32 length_1, limit, index;
  unsigned char g;
  unsigned char *arg_temp;

  length_1 = (sint32)(StrHDR(string)->fill_length);
  if (end!=NULL) 
    limit = UNBOXFIX(end);
  else 
    limit = length_1;
  index = start;
  for (;!(index>=limit);index = (index+1)) {
    g = (string[index]);
    if (('A'<=g) && (g<='Z')) {
      arg_temp = (((Str *)copy_string(string,length_1))->body);
      return nstring_downcase_function(arg_temp,index,BOXFIX(limit));
    }
  }
  return string;
}

/* Translated from NSTRING-CAPITALIZE-FUNCTION(STRING FIXNUM T) = STRING */

unsigned char *nstring_capitalize_function (unsigned char *string, sint32 start, 
        Obj end)
{
  Obj word_start;
  sint32 limit, index;
  unsigned char char_1;
  unsigned char *arg_temp;
  sint32 arg_temp_1, g;
  int if_result_temp;
  unsigned char if_result_temp_1;

  word_start = (Obj)(&T);
  if (end!=NULL) 
    limit = UNBOXFIX(end);
  else 
    limit = (sint32)(StrHDR(string)->fill_length);
  index = start;
  for (;!(index>=limit);index = (index+1)) {
    char_1 = (string[index]);
    arg_temp = string;
    arg_temp_1 = index;
    g = (((sint32)char_1)-48);
    if ((g>=0) && (g<10)) 
      if_result_temp = (BOXFIX(g)!=NULL);
    else 
      if_result_temp = 0;
    if (if_result_temp || ((('a'<=char_1) && (char_1<='z')) || (('A'<=char_1)
         && (char_1<='Z')))) {
      if (word_start!=NULL) {
        word_start = (Obj)NULL;
        if (('a'<=char_1) && (char_1<='z')) 
          if_result_temp_1 = (unsigned char)(((sint32)char_1)-32);
        else 
          if_result_temp_1 = char_1;
      }
      else if (('A'<=char_1) && (char_1<='Z')) 
        if_result_temp_1 = (unsigned char)(((sint32)char_1)+32);
      else 
        if_result_temp_1 = char_1;
    }
    else {
      word_start = (Obj)(&T);
      if_result_temp_1 = char_1;
    }
    (arg_temp[arg_temp_1]) = if_result_temp_1;
  }
  return string;
}

Obj Sterminal_ioS = (Obj)(&Unbound);

Obj Sstandard_inputS = (Obj)(&Unbound);

Obj Sstandard_outputS = (Obj)(&Unbound);

Obj Serror_outputS = (Obj)(&Unbound);

Obj Squery_ioS = (Obj)(&Unbound);

Obj Sdebug_ioS = (Obj)(&Unbound);

Obj Strace_outputS = (Obj)(&Unbound);

Obj Sprint_caseS = (Obj)(&Unbound);

Obj Sprint_baseS = (Obj)(&Unbound);

Obj Sprint_escapeS = (Obj)(&Unbound);

Obj Sprint_prettyS = (Obj)(&Unbound);

Obj Sprint_lengthS = (Obj)(&Unbound);

Obj Sprint_circleS = (Obj)(&Unbound);

Obj Sprint_levelS = (Obj)(&Unbound);

Obj default_string_stream_size = BOXFIX(128);

/* Translated from GET-STRING-OR-FILE-STREAM-FOR-OUTPUT(T FIXNUM) = T */

Obj get_string_or_file_stream_for_output (Obj stream_arg, sint32 needed_space)
{
  sint32 temp;
  Obj string_list, first_string;
  sint32 arg_temp, arg_temp_1;
  int temp_1;
  unsigned char *new_string;
  unsigned char *g;
  Str *string;
  sint32 fill_1;

  switch (TYPE_TAG(stream_arg,temp)) {
   case 0:
   case 11:
    if (stream_arg==NULL) 
      return GET_GLOBAL(Sstandard_outputS);
    else if (stream_arg==(Obj)(&T)) 
      return GET_GLOBAL(Sterminal_ioS);
    else {
      bad_stream_error(stream_arg);
      return (Obj)NULL;
    }
   case 7:
   case 16:
    return stream_arg;
   case 15:
    string_list = (((String_strm *)stream_arg)->strings);
    first_string = ((string_list!=NULL) ? CAR(string_list) : (Obj)NULL);
    temp_1 = (first_string==NULL);
    if (!temp_1) {
      arg_temp_1 = (sint32)(((Str *)first_string)->length);
      arg_temp = (arg_temp_1-(sint32)(((Str *)first_string)->fill_length));
      temp_1 = (arg_temp<needed_space);
    }
    if (temp_1) {
      g = (((Str *)alloc_string((needed_space>128) ? needed_space : 128,
          0,7))->body);
      memset((void *)(g+0),'\000',(sint32)(StrHDR(g)->fill_length));
      (void)g;
      new_string = g;
      string = StrHDR(new_string);
      fill_1 = 0;
      (string->body[fill_1]) = '\000';
      string->fill_length = fill_1;
      ((String_strm *)stream_arg)->strings = alloc_cons(ObjStrHDR(new_string),
          string_list,0);
      first_string = ObjStrHDR(new_string);
    }
    return first_string;
   default:
    return (bad_stream_error(stream_arg),(Obj)NULL);
  }
}

/* Translated from LAST-STRING-CHAR?(STRING) = T */

Obj last_string_charP (unsigned char *string)
{
  sint32 length_1;

  length_1 = (sint32)(StrHDR(string)->fill_length);
  if (length_1>0) 
    return BOXCHAR(string[length_1-1]);
  else 
    return (Obj)NULL;
}

/* Translated from GET-LAST-STRING-STREAM-CHARACTER(TL-STRING-STREAM) = T */

Obj get_last_string_stream_character (Obj string_stream)
{
  Obj first_stringP, temp;

  temp = (((String_strm *)string_stream)->strings);
  first_stringP = ((temp!=NULL) ? CAR(temp) : (Obj)NULL);
  if (first_stringP!=NULL) 
    return last_string_charP(((Str *)first_stringP)->body);
  else 
    return (Obj)NULL;
}

/* Translated from LAST-STREAM-CHAR?(T) = T */

Obj last_stream_charP (Obj stream)
{
  if ((stream!=NULL) && ((IMMED_TAG(stream)==0) && (STD_TAG(stream)==7)))   /* STRING-P */
    return last_string_charP(((Str *)stream)->body);
  else if ((stream!=NULL) && ((IMMED_TAG(stream)==0) && (STD_TAG(stream)
      ==15)))                                   /* TL-STRING-STREAM-P */
    return get_last_string_stream_character(stream);
  else 
    return (Obj)NULL;
}

/* Translated from GET-OUTPUT-STREAM-STRING(TL-STRING-STREAM) = T */

Obj get_output_stream_string (Obj string_stream)
{
  Obj string_list;
  sint32 length_1;
  Obj g, string;
  unsigned char *result_string;
  unsigned char *g_1;
  sint32 current_end, this_length;
  unsigned char *g_2;

  string_list = (((String_strm *)string_stream)->strings);
  length_1 = 0;
  g = string_list;
  for (;g!=NULL;g = CDR(g)) {
    string = CAR(g);
    length_1 = (length_1+(sint32)(((Str *)string)->fill_length));
  }
  g_1 = (((Str *)alloc_string(length_1,0,7))->body);
  memset((void *)(g_1+0),'\000',(sint32)(StrHDR(g_1)->fill_length));
  (void)g_1;
  result_string = g_1;
  current_end = length_1;
  g = string_list;
  for (;g!=NULL;g = CDR(g)) {
    string = CAR(g);
    this_length = (sint32)(((Str *)string)->fill_length);
    current_end = (current_end-this_length);
    g_2 = (((Str *)string)->body);
    memcpy((void *)(result_string+current_end),(void *)(g_2+0),this_length);
    (void)result_string;
  }
  return ObjStrHDR(result_string);
}

/* Translated from FORCE-OUTPUT(&OPTIONAL T) = NULL */

Obj force_output (Obj output_stream)
{
  Obj stream;

  stream = get_string_or_file_stream_for_output(output_stream,0);
  if ((stream!=NULL) && ((IMMED_TAG(stream)==0) && (STD_TAG(stream)==16)))      /* FILE-STREAM-P */
    fflush(((File_strm *)stream)->output);
  return (Obj)NULL;
}

/* Translated from WRITE-STRING-FUNCTION(STRING T FIXNUM T) = STRING */

unsigned char *write_string_function (unsigned char *string, Obj streamP, 
        sint32 start, Obj end)
{
  sint32 end_index;
  Obj stream;
  sint32 current_fill;
  Str *string_1;
  sint32 fill_1;
  unsigned char *g;
  sint32 index;

  if (end==NULL) 
    end_index = (sint32)(StrHDR(string)->fill_length);
  else 
    end_index = UNBOXFIX(end);
  stream = get_string_or_file_stream_for_output(streamP,end_index-start);
  if ((stream!=NULL) && ((IMMED_TAG(stream)==0) && (STD_TAG(stream)==7))) {     /* STRING-P */
    current_fill = (sint32)(((Str *)stream)->fill_length);
    string_1 = (Str *)stream;
    fill_1 = (current_fill+(end_index-start));
    (string_1->body[fill_1]) = '\000';
    string_1->fill_length = fill_1;
    g = (((Str *)stream)->body);
    memcpy((void *)(g+current_fill),(void *)(string+start),end_index-start);
    (void)g;
  }
  else if ((end!=NULL) || (start!=0)) {
    index = start;
    for (;!(index>=end_index);index = (index+1)) 
      putc((char)(string[index]),((File_strm *)stream)->output);
    goto exit_nil;
   exit_nil:
;
  }
  else 
    fputs((char *)string,((File_strm *)stream)->output);
  return string;
}

/* Translated from WRITE-CHAR(CHARACTER &OPTIONAL T) = CHARACTER */

unsigned char write_char (unsigned char char_1, Obj streamP)
{
  Obj stringP;
  sint32 temp, g;
  Str *string;
  sint32 fill_1;

  stringP = (Obj)NULL;
  switch (TYPE_TAG(streamP,temp)) {
   case 7:
    stringP = (Obj)(&T);
    break;
   case 16:
    break;
   default:
    streamP = get_string_or_file_stream_for_output(streamP,1);
    stringP = (((streamP!=NULL) && ((IMMED_TAG(streamP)==0) && (STD_TAG(streamP)
        ==7))) ? ((Obj)(&T)) : (Obj)NULL);      /* STRING-P */
    break;
  }
  if (stringP!=NULL) {
    g = (sint32)(((Str *)streamP)->fill_length);
    string = (Str *)streamP;
    fill_1 = (g+1);
    (string->body[fill_1]) = '\000';
    string->fill_length = fill_1;
    (((Str *)streamP)->body[g]) = char_1;
  }
  else 
    putc((char)char_1,((File_strm *)streamP)->output);
  (void)char_1;
  return char_1;
}

/* Translated from WRITE-FIXNUM-IN-HEX(FIXNUM T) = VOID */

void write_fixnum_in_hex (sint32 fixnum, Obj streamP)
{
  unsigned char arg_temp;

  if (fixnum<0) {
    write_char('-',streamP);
    fixnum = (-fixnum);
  }
  if (fixnum>15) {
    write_fixnum_in_hex(fixnum>>4,streamP);
    fixnum = (fixnum&15);
  }
  arg_temp = (unsigned char)(((fixnum<10) ? 48 : 55)+fixnum);
  write_char(arg_temp,streamP);
  return;
}

/* Translated from WRITE-FIXNUM-IN-ARBITRARY-BASE(FIXNUM FIXNUM T) = VOID */

void write_fixnum_in_arbitrary_base (sint32 fixnum, sint32 base, Obj stream)
{
  unsigned char arg_temp;
  sint32 g;

  if (fixnum<0) {
    write_char('-',stream);
    write_fixnum_in_arbitrary_base(-fixnum,base,stream);
    return;
  }
  else if (fixnum>=base) {
    write_fixnum_in_arbitrary_base(fixnum_floor_first(fixnum,base),base,
        stream);
    g = mod_fixnums(fixnum,base);
    arg_temp = (unsigned char)(((g<10) ? 48 : 55)+g);
    write_char(arg_temp,stream);
    return;
  }
  else {
    arg_temp = (unsigned char)(((fixnum<10) ? 48 : 55)+fixnum);
    write_char(arg_temp,stream);
    return;
  }
}

Obj reversed_fixnum_with_commas_string = (Obj)(&Unbound);

/* Translated from WRITE-FIXNUM-WITH-COMMAS(FIXNUM FIXNUM FIXNUM CHARACTER T) = VOID */

void write_fixnum_with_commas (sint32 fixnum, sint32 base, sint32 comma_interval, 
        unsigned char comma_char, Obj stream)
{
  unsigned char *reversed_fixnum_with_commas;
  Str *string;
  sint32 fill_1, char_count, fill_2, fill_3, fill_4;
  unsigned char *arg_temp;
  sint32 arg_temp_1, g, g_1, index;

  reversed_fixnum_with_commas = (((Str *)GET_GLOBAL(reversed_fixnum_with_commas_string))->body);
  if (fixnum<0) {
    write_char('-',stream);
    fixnum = (-fixnum);
  }
  string = StrHDR(reversed_fixnum_with_commas);
  fill_1 = 0;
  (string->body[fill_1]) = '\000';
  string->fill_length = fill_1;
  char_count = 0;
  fill_2 = 0;
  while (!((fixnum<=0) && (fill_2>0))) {
    if (char_count>=comma_interval) {
      char_count = 0;
      string = StrHDR(reversed_fixnum_with_commas);
      fill_3 = (fill_2+1);
      (string->body[fill_3]) = '\000';
      string->fill_length = fill_3;
      (reversed_fixnum_with_commas[fill_2]) = comma_char;
      fill_2 = (fill_2+1);
    }
    string = StrHDR(reversed_fixnum_with_commas);
    fill_4 = (fill_2+1);
    (string->body[fill_4]) = '\000';
    string->fill_length = fill_4;
    arg_temp = reversed_fixnum_with_commas;
    arg_temp_1 = fill_2;
    g = mod_fixnums(fixnum,base);
    (arg_temp[arg_temp_1]) = (unsigned char)(((g<10) ? 48 : 55)+g);
    fixnum = ((fixnum<base) ? (fixnum-base) : fixnum_floor_first(fixnum,
        base));
    g = (char_count+1);
    g_1 = (fill_2+1);
    char_count = g;
    fill_2 = g_1;
  }

  goto exit_nil;
 exit_nil:
  index = (((sint32)(StrHDR(reversed_fixnum_with_commas)->fill_length))
      -1);
  for (;!(index<0);index = (index-1)) 
    write_char(reversed_fixnum_with_commas[index],stream);
  return;
}

/* Translated from WRITE-FIXNUM(FIXNUM FIXNUM FIXNUM T) = FIXNUM */

sint32 write_fixnum (sint32 fixnum, sint32 base, sint32 width, Obj streamP)
{
  Obj stream;
  sint32 g, temp;

  if (width==0) 
    g = 11;
  else 
    g = width;
  switch (TYPE_TAG(streamP,temp)) {
   case 0:
   case 11:
    if (streamP==NULL) 
      stream = GET_GLOBAL(Sstandard_outputS);
    else if (streamP==(Obj)(&T)) 
      stream = GET_GLOBAL(Sterminal_ioS);
    else 
      stream = get_string_or_file_stream_for_output(streamP,g);
    break;
   case 16:
    stream = streamP;
    break;
   case 7:
    stream = streamP;
    break;
   default:
    stream = get_string_or_file_stream_for_output(streamP,g);
    break;
  }
  if (base==10) {
    if ((stream!=NULL) && ((IMMED_TAG(stream)==0) && (STD_TAG(stream)==7)))     /* STRING-P */
      write_fixnum_into_str(fixnum,width,(Str *)stream);
    else 
      fprintf(((File_strm *)stream)->output,"%*ld",(int)width,(long)fixnum);
  }
  else if (base==16) 
    write_fixnum_in_hex(fixnum,stream);
  else 
    write_fixnum_in_arbitrary_base(fixnum,base,stream);
  return fixnum;
}

static const Str_5 str_const_9
  = { 7, 2, 2, "#<" };

static const Str_5 str_const_10
  = { 7, 2, 2, "0x" };

/* Translated from PRINT-RANDOM-OBJECT-WITH-TYPE-NAME(T STRING T T) = VOID */

void print_random_object_with_type_name (Obj object, unsigned char *name, 
        Obj extra_infoP, Obj streamP)
{
  sint32 arg_temp;

  write_string_function(((Str *)(&str_const_9))->body,streamP,  /* "#<" */
      0,(Obj)NULL);
  write_string_function(name,streamP,0,(Obj)NULL);
  write_char(' ',streamP);
  if (extra_infoP!=NULL) {
    princ(extra_infoP,streamP);
    write_char(' ',streamP);
  }
  write_string_function(((Str *)(&str_const_10))->body,streamP,     /* "0x" */
      0,(Obj)NULL);
  arg_temp = (sint32)object;
  write_fixnum_in_hex(arg_temp,streamP);
  write_char('>',streamP);
  return;
}

static const Str_5 str_const_11
  = { 7, 3, 3, "nil" };

static const Str_5 str_const_12
  = { 7, 3, 3, "Nil" };

static const Str_5 str_const_13
  = { 7, 3, 3, "NIL" };

static const Str_5 str_const_14
  = { 7, 2, 2, "#\\" };

static const Str_17 str_const_15
  = { 7, 13, 13, "Simple-Vector" };

static const Str_25 str_const_16
  = { 7, 22, 22, "Unsigned-Byte-8-Vector" };

static const Str_25 str_const_17
  = { 7, 23, 23, "Unsigned-Byte-16-Vector" };

static const Str_25 str_const_18
  = { 7, 21, 21, "Signed-Byte-16-Vector" };

static const Str_21 str_const_19
  = { 7, 19, 19, "Double-Float-Vector" };

static const Str_13 str_const_20
  = { 7, 12, 12, "WRITE-SYMBOL" };

static const Str_21 str_const_21
  = { 7, 17, 17, "Compiled-Function" };

static const Str_9 str_const_22
  = { 7, 7, 7, "Package" };

static const Str_21 str_const_23
  = { 7, 17, 17, "The-Unbound-Value" };

static const Str_17 str_const_24
  = { 7, 13, 13, "String-Stream" };

static const Str_13 str_const_25
  = { 7, 11, 11, "File-Stream" };

static const Str_13 str_const_26
  = { 7, 12, 12, "Unknown-Type" };

/* Translated from WRITE-FUNCTION(T T T T FIXNUM T T T T) = T */

Obj write_function (Obj object, Obj stream, Obj case_1, Obj escape, sint32 base, 
        Obj pretty, Obj level, Obj length_1, Obj circle)
{
  sint32 temp;
  unsigned char *arg_temp;
  unsigned char *if_result_temp;
  Obj nameP;
  double g;
  sint32 g_1;
  Obj g_2;
  sint32 g_3, index;
  unsigned char char_1;

  (void)pretty;                                 /* PRETTY was declared ignore */
  (void)level;                                  /* LEVEL was declared ignore */
  (void)length_1;                               /* LENGTH was declared ignore */
  (void)circle;                                 /* CIRCLE was declared ignore */
  switch (TYPE_TAG(object,temp)) {
   case 0:
    if (case_1==(Obj)(tl_format_symbols+1))     /* DOWNCASE */
      if_result_temp = (((Str *)(&str_const_11))->body);    /* "nil" */
    else if (case_1==(Obj)(tl_format_symbols+2))    /* CAPITALIZE */
      if_result_temp = (((Str *)(&str_const_12))->body);    /* "Nil" */
    else 
      if_result_temp = (((Str *)(&str_const_13))->body);    /* "NIL" */
    arg_temp = if_result_temp;
    write_string_function(arg_temp,stream,0,(Obj)NULL);
    break;
   case 1:
    write_fixnum(UNBOXFIX(object),base,0,stream);
    break;
   case 2:
    write_list(object,stream);
    break;
   case 3:
    if (escape!=NULL) {
      nameP = char_name(UNBOXCHAR(object));
      write_string_function(((Str *)(&str_const_14))->body,     /* "#\" */
          stream,0,(Obj)NULL);
      if (nameP!=NULL) 
        write_string_function(((Str *)nameP)->body,stream,0,(Obj)NULL);
      else 
        write_char(UNBOXCHAR(object),stream);
    }
    else 
      write_char(UNBOXCHAR(object),stream);
    break;
   case 5:
    g = (((Ldouble *)object)->body);
    g_1 = 0;
    g_2 = get_string_or_file_stream_for_output(stream,(g_1==0) ? 16 : g_1);
    if ((g_2!=NULL) && ((IMMED_TAG(g_2)==0) && (STD_TAG(g_2)==7)))      /* STRING-P */
      write_double_into_str(g,g_1,(Str *)g_2);
    else 
      fprintf(((File_strm *)g_2)->output,"%*g",(int)g_1,g);
    (void)g;
    break;
   case 6:
    print_random_object_with_type_name(object,((Str *)(&str_const_15))->body,   /* "Simple-Vector" */
        BOXFIX((sint32)(((Sv *)object)->length)),stream);
    break;
   case 7:
    if (escape!=NULL) {
      write_char('\"',stream);
      g_3 = (sint32)(((Str *)object)->fill_length);
      index = 0;
      for (;index<g_3;index = (index+1)) {
        char_1 = (((Str *)object)->body[index]);
        if ((char_1=='\"') || (char_1=='\\')) 
          write_char('\\',stream);
        write_char(char_1,stream);
      }
      write_char('\"',stream);
    }
    else 
      write_string_function(((Str *)object)->body,stream,0,(Obj)NULL);
    break;
   case 8:
    print_random_object_with_type_name(object,((Str *)(&str_const_16))->body,   /* "Unsigned-Byte-8-Vector" */
        BOXFIX((sint32)(((Sa_uint8 *)object)->fill_length)),stream);
    break;
   case 9:
    print_random_object_with_type_name(object,((Str *)(&str_const_17))->body,   /* "Unsigned-Byte-16-Vector" */
        BOXFIX((sint32)(((Sa_uint16 *)object)->fill_length)),stream);
    break;
   case 18:
    print_random_object_with_type_name(object,((Str *)(&str_const_18))->body,   /* "Signed-Byte-16-Vector" */
        BOXFIX((sint32)(((Sa_sint16 *)object)->fill_length)),stream);
    break;
   case 10:
    print_random_object_with_type_name(object,((Str *)(&str_const_19))->body,   /* "Double-Float-Vector" */
        BOXFIX((sint32)(((Sa_double *)object)->length)),stream);
    break;
   case 11:
    ((Func_3 *)(Obj)(&(tl_format_funcs[0])))->c_function(object,    /* #'WRITE-SYMBOL */
        case_1,stream);
    break;
   case 12:
    print_random_object_with_type_name(object,((Str *)(&str_const_21))->body,   /* "Compiled-Function" */
        ((Func *)object)->name,stream);
    break;
   case 13:
    print_random_object_with_type_name(object,((Str *)(&str_const_22))->body,   /* "Package" */
        ((Pkg *)object)->name,stream);
    break;
   case 14:
    print_random_object_with_type_name(object,((Str *)(&str_const_23))->body,   /* "The-Unbound-Value" */
        (Obj)NULL,stream);
    break;
   case 15:
    print_random_object_with_type_name(object,((Str *)(&str_const_24))->body,   /* "String-Stream" */
        (Obj)NULL,stream);
    break;
   case 16:
    print_random_object_with_type_name(object,((Str *)(&str_const_25))->body,   /* "File-Stream" */
        (Obj)NULL,stream);
    break;
   default:
    print_random_object_with_type_name(object,((Str *)(&str_const_26))->body,   /* "Unknown-Type" */
        (Obj)NULL,stream);
    break;
  }
  return object;
}

/* Translated from PRIN1(T &OPTIONAL T) = T */

Obj prin1 (Obj object, Obj streamP)
{
  Obj new_Sprint_escapeS, temp;
  Thread_state *ts;

  new_Sprint_escapeS = (Obj)(&T);
  ts = THREAD_STATE;
  bind_global(&Sprint_escapeS,ts,new_Sprint_escapeS);
  temp = write_function(object,streamP,GET_GLOBAL(Sprint_caseS),GET_GLOBAL(Sprint_escapeS),
      UNBOXFIX(GET_GLOBAL(Sprint_baseS)),GET_GLOBAL(Sprint_prettyS),GET_GLOBAL(Sprint_levelS),
      GET_GLOBAL(Sprint_lengthS),GET_GLOBAL(Sprint_circleS));
  unbind_global(&Sprint_escapeS,ts);
  return temp;
}

/* Translated from PRINC(T &OPTIONAL T) = T */

Obj princ (Obj object, Obj streamP)
{
  Obj new_Sprint_escapeS, temp;
  Thread_state *ts;

  new_Sprint_escapeS = (Obj)NULL;
  ts = THREAD_STATE;
  bind_global(&Sprint_escapeS,ts,new_Sprint_escapeS);
  temp = write_function(object,streamP,GET_GLOBAL(Sprint_caseS),GET_GLOBAL(Sprint_escapeS),
      UNBOXFIX(GET_GLOBAL(Sprint_baseS)),GET_GLOBAL(Sprint_prettyS),GET_GLOBAL(Sprint_levelS),
      GET_GLOBAL(Sprint_lengthS),GET_GLOBAL(Sprint_circleS));
  unbind_global(&Sprint_escapeS,ts);
  return temp;
}

/* Translated from TERPRI(&OPTIONAL T) = NULL */

Obj terpri (Obj streamP)
{
  write_char('\n',streamP);
  return (Obj)NULL;
}

/* Translated from WRITE-LINE-FUNCTION(T T FIXNUM T) = T */

Obj write_line_function (Obj string, Obj streamP, sint32 start, Obj end)
{
  write_string_function(((Str *)string)->body,streamP,start,end);
  write_char('\n',streamP);
  return string;
}

/* Translated from PRINT(T &OPTIONAL T) = T */

Obj print (Obj object, Obj streamP)
{
  terpri(streamP);
  prin1(object,streamP);
  write_char(' ',streamP);
  return object;
}

static const Str_5 str_const_27
  = { 7, 3, 3, " . " };

/* Translated from WRITE-LIST(T T) = T */

Obj write_list (Obj cons_1, Obj streamP)
{
  Obj current_cons, current_car, next_cons;

  write_char('(',streamP);
  current_cons = cons_1;
  current_car = CAR(current_cons);
  next_cons = CDR(current_cons);
  while (IMMED_TAG(next_cons)==2) {             /* Consp */
    write_function(current_car,streamP,GET_GLOBAL(Sprint_caseS),GET_GLOBAL(Sprint_escapeS),
        UNBOXFIX(GET_GLOBAL(Sprint_baseS)),GET_GLOBAL(Sprint_prettyS),GET_GLOBAL(Sprint_levelS),
        GET_GLOBAL(Sprint_lengthS),GET_GLOBAL(Sprint_circleS));
    write_char(' ',streamP);
    current_cons = next_cons;
    current_car = CAR(current_cons);
    next_cons = CDR(current_cons);
  }

  write_function(current_car,streamP,GET_GLOBAL(Sprint_caseS),GET_GLOBAL(Sprint_escapeS),
      UNBOXFIX(GET_GLOBAL(Sprint_baseS)),GET_GLOBAL(Sprint_prettyS),GET_GLOBAL(Sprint_levelS),
      GET_GLOBAL(Sprint_lengthS),GET_GLOBAL(Sprint_circleS));
  if (next_cons==NULL) 
    write_char(')',streamP);
  else {
    write_string_function(((Str *)(&str_const_27))->body,streamP,   /* " . " */
        0,(Obj)NULL);
    write_function(next_cons,streamP,GET_GLOBAL(Sprint_caseS),GET_GLOBAL(Sprint_escapeS),
        UNBOXFIX(GET_GLOBAL(Sprint_baseS)),GET_GLOBAL(Sprint_prettyS),GET_GLOBAL(Sprint_levelS),
        GET_GLOBAL(Sprint_lengthS),GET_GLOBAL(Sprint_circleS));
    write_char(')',streamP);
  }
  goto exit_nil;
 exit_nil:
  return cons_1;
}

Obj field_width_string_length = BOXFIX(256);

Obj field_width_string_list = (Obj)(&Unbound);

/* Translated from ALLOC-FIELD-WIDTH-STRING() = T */

Obj alloc_field_width_string (void)
{
  Obj this_cons, this_string, next_consP;
  unsigned char *g;
  Str *string;
  sint32 fill_1;

  this_cons = GET_GLOBAL(field_width_string_list);
  this_string = CAR(this_cons);
  next_consP = CDR(this_cons);
  if (next_consP==NULL) {
    g = (((Str *)alloc_string(256,0,7))->body);
    memset((void *)(g+0),'\000',(sint32)(StrHDR(g)->fill_length));
    (void)g;
    next_consP = alloc_cons(ObjStrHDR(g),(Obj)NULL,0);
    CDR(this_cons) = next_consP;
  }
  SET_GLOBAL(field_width_string_list,next_consP);
  string = (Str *)this_string;
  fill_1 = 0;
  (string->body[fill_1]) = '\000';
  string->fill_length = fill_1;
  return this_string;
}

static const Str_5 str_const_28
  = { 7, 2, 2, "()" };

/* Translated from WRITE-WITH-ARGLIST(T T T T T) = T */

Obj write_with_arglist (Obj stream, Obj object, Obj arglist, Obj atsign_modifierP, 
        Obj colon_modifierP)
{
  Obj current_stream, g, mincol_arg, colinc_arg, minpad_arg, padchar_arg;
  sint32 mincol;
  Obj temp;
  sint32 colinc;
  Obj temp_1;
  sint32 minpad;
  Obj temp_2;
  unsigned char padchar;
  Obj temp_3;
  sint32 length_1, pad, x;

  if (arglist!=NULL) 
    current_stream = alloc_field_width_string();
  else 
    current_stream = stream;
  if (object==NULL) 
    write_string_function((colon_modifierP!=NULL) ? ((Str *)(&str_const_28))->body      /* "()" */
        : (((Str *)(&str_const_11))->body),current_stream,  /* "nil" */
        0,(Obj)NULL);
  else 
    write_function(object,current_stream,GET_GLOBAL(Sprint_caseS),GET_GLOBAL(Sprint_escapeS),
        UNBOXFIX(GET_GLOBAL(Sprint_baseS)),GET_GLOBAL(Sprint_prettyS),GET_GLOBAL(Sprint_levelS),
        GET_GLOBAL(Sprint_lengthS),GET_GLOBAL(Sprint_circleS));
  if (arglist!=NULL) {
    g = arglist;
    mincol_arg = CAR(g);
    g = CDR(g);
    if (g!=NULL) 
      colinc_arg = CAR(g);
    else 
      colinc_arg = (Obj)NULL;
    g = ((g!=NULL) ? CDR(g) : (Obj)NULL);
    if (g!=NULL) 
      minpad_arg = CAR(g);
    else 
      minpad_arg = (Obj)NULL;
    g = ((g!=NULL) ? CDR(g) : (Obj)NULL);
    if (g!=NULL) 
      padchar_arg = CAR(g);
    else 
      padchar_arg = (Obj)NULL;
    temp = mincol_arg;
    if (temp==NULL) 
      temp = BOXFIX(0);
    mincol = UNBOXFIX(temp);
    temp_1 = colinc_arg;
    if (temp_1==NULL) 
      temp_1 = BOXFIX(1);
    colinc = UNBOXFIX(temp_1);
    temp_2 = minpad_arg;
    if (temp_2==NULL) 
      temp_2 = BOXFIX(0);
    minpad = UNBOXFIX(temp_2);
    temp_3 = padchar_arg;
    if (temp_3==NULL) 
      temp_3 = BOXCHAR(' ');
    padchar = UNBOXCHAR(temp_3);
    if (atsign_modifierP==NULL) 
      write_string_function(((Str *)current_stream)->body,stream,0,(Obj)NULL);
    length_1 = (sint32)(((Str *)current_stream)->fill_length);
    pad = 0;
    for (;!(((length_1+pad)>=mincol) && (pad>=minpad));pad = (pad+colinc)) {
      x = 0;
      for (;x<colinc;x = (x+1)) 
        write_char(padchar,stream);
    }
    goto exit_nil;
   exit_nil:
    if (atsign_modifierP!=NULL) 
      write_string_function(((Str *)current_stream)->body,stream,0,(Obj)NULL);
  }
  return object;
}

/* Translated from WRITE-FIXNUM-WITH-ARGLIST(T FIXNUM T T T) = VOID */

void write_fixnum_with_arglist (Obj stream, sint32 fixnum, Obj arglist, 
        Obj atsign_modifierP, Obj colon_modifierP)
{
  Obj current_stream, g, mincol_arg, padchar_arg, comma_char_arg, comma_interval_arg;
  sint32 mincol;
  Obj temp;
  unsigned char padchar;
  Obj temp_1;
  unsigned char comma_char;
  Obj temp_2;
  sint32 comma_interval;
  Obj temp_3;
  sint32 g_1, x;

  if (arglist!=NULL) 
    current_stream = alloc_field_width_string();
  else 
    current_stream = stream;
  if ((atsign_modifierP!=NULL) && (fixnum>=0)) 
    write_char('+',current_stream);
  g = arglist;
  if (g!=NULL) 
    mincol_arg = CAR(g);
  else 
    mincol_arg = (Obj)NULL;
  g = ((g!=NULL) ? CDR(g) : (Obj)NULL);
  if (g!=NULL) 
    padchar_arg = CAR(g);
  else 
    padchar_arg = (Obj)NULL;
  g = ((g!=NULL) ? CDR(g) : (Obj)NULL);
  if (g!=NULL) 
    comma_char_arg = CAR(g);
  else 
    comma_char_arg = (Obj)NULL;
  g = ((g!=NULL) ? CDR(g) : (Obj)NULL);
  if (g!=NULL) 
    comma_interval_arg = CAR(g);
  else 
    comma_interval_arg = (Obj)NULL;
  temp = mincol_arg;
  if (temp==NULL) 
    temp = BOXFIX(0);
  mincol = UNBOXFIX(temp);
  temp_1 = padchar_arg;
  if (temp_1==NULL) 
    temp_1 = BOXCHAR(' ');
  padchar = UNBOXCHAR(temp_1);
  temp_2 = comma_char_arg;
  if (temp_2==NULL) 
    temp_2 = BOXCHAR(',');
  comma_char = UNBOXCHAR(temp_2);
  temp_3 = comma_interval_arg;
  if (temp_3==NULL) 
    temp_3 = BOXFIX(3);
  comma_interval = UNBOXFIX(temp_3);
  if (colon_modifierP!=NULL) 
    write_fixnum_with_commas(fixnum,UNBOXFIX(GET_GLOBAL(Sprint_baseS)),
        comma_interval,comma_char,current_stream);
  else 
    write_fixnum(fixnum,UNBOXFIX(GET_GLOBAL(Sprint_baseS)),0,current_stream);
  if (mincol_arg!=NULL) {
    g_1 = (mincol-(sint32)(((Str *)current_stream)->fill_length));
    x = 0;
    for (;x<g_1;x = (x+1)) 
      write_char(padchar,stream);
    write_string_function(((Str *)current_stream)->body,stream,0,(Obj)NULL);
    return;
  }
  else 
    return;
}

static const Str_5 str_const_29
  = { 7, 2, 2, "  " };

static const Str_49 str_const_30
  = { 7, 47, 47, "TL:FORMAT doesn\'t support nested ~{ iterations." };

static const Str_45 str_const_31
  = { 7, 43, 43, "In FORMAT, a ~{ iteration had no closing ~}" };

static const Str_49 str_const_32
  = { 7, 48, 48, "In FORMAT, ~^ found outside of any ~{ iteration." };

static const Str_25 str_const_33
  = { 7, 24, 24, "In FORMAT, unmatched ~}." };

static const Str_29 str_const_34
  = { 7, 26, 26, "Only : args to ~; allowed." };

static const Str_21 str_const_35
  = { 7, 18, 18, "No ~] found for ~[" };

/* Translated from FORMAT-FUNCTION(T STRING T) = T */

Obj format_function (Obj stream_arg, unsigned char *control_string, Obj args)
{
  Obj stream, temp, cached_streamP, current_case_conversion;
  sint32 index;
  Obj iteration_start_indexP, iteration_maximum_loopsP, iteration_end_index, 
        iteration_cached_args, iteration_uses_sublistsP, iteration_sublists;
  sint32 control_string_length;
  Obj new_field_width_string_list, temp_1;
  Thread_state *ts;
  unsigned char next_char;
  Obj arglist_pool;
  Obj temp_list[14];
  Obj arglist, current_argP, colon_modifierP, atsign_modifierP, numericP;
  sint32 g;
  Obj if_result_temp;
  sint32 arg_temp;
  Obj new_cons, g_1;
  sint32 g_2;
  unsigned char down_char;
  Obj new_Sprint_escapeS;
  Thread_state *ts_1;
  Obj arg_temp_1, arg_temp_2, g_3, arg_temp_3, g_4, object, g_5, new_Sprint_baseS;
  Thread_state *ts_2;
  Obj new_cdr, list, block_temp, arg_temp_4;
  sint32 x;
  Obj temp_2;
  unsigned char white_char;
  int temp_3;
  unsigned char *field_width_string;
  unsigned char *arg_temp_5;
  unsigned char g_6, if_result_temp_1;
  Obj at_least_onceP;
  sint32 end_index;
  unsigned char char_1, next_char_1;
  Obj g_7;
  int temp_4;
  Obj g_8;
  sint32 clause_number;
  Obj g_9, g_10;
  sint32 clause_index;
  unsigned char char_2;
  sint32 directive_index;
  unsigned char next_char_2;
  Obj g_11;

  temp = stream_arg;
  if (temp==NULL) 
    temp = alloc_string_strm(0,15);
  stream = temp;
  cached_streamP = (Obj)NULL;
  current_case_conversion = (Obj)NULL;
  index = 0;
  iteration_start_indexP = (Obj)NULL;
  iteration_maximum_loopsP = (Obj)NULL;
  iteration_end_index = (Obj)NULL;
  iteration_cached_args = (Obj)NULL;
  iteration_uses_sublistsP = (Obj)NULL;
  iteration_sublists = (Obj)NULL;
  control_string_length = (sint32)(StrHDR(control_string)->fill_length);
  new_field_width_string_list = GET_GLOBAL(field_width_string_list);
  ts = THREAD_STATE;
  bind_global(&field_width_string_list,ts,new_field_width_string_list);
  while (!(index>=control_string_length)) {
    next_char = (control_string[index]);
    index = (index+1);
    if (next_char=='~') {
      if (index>=control_string_length) 
        bad_control_directive_error(ObjStrHDR(control_string));
      next_char = (control_string[index]);
      index = (index+1);
      (temp_list[0]) = (Obj)NULL;
      (temp_list[2]) = (Obj)NULL;
      (temp_list[4]) = (Obj)NULL;
      (temp_list[6]) = (Obj)NULL;
      (temp_list[8]) = (Obj)NULL;
      (temp_list[10]) = (Obj)NULL;
      (temp_list[12]) = (Obj)NULL;
      arglist_pool = hook_up_cdrs(temp_list,7,NULL);
      arglist = (Obj)NULL;
      current_argP = (Obj)NULL;
      colon_modifierP = (Obj)NULL;
      atsign_modifierP = (Obj)NULL;
      g = (((sint32)next_char)-48);
      if ((g>=0) && (g<10)) 
        numericP = BOXFIX(g);
      else 
        numericP = (Obj)NULL;
      while (1) {
        if (numericP!=NULL) {
          if (IMMED_TAG(current_argP)==1) {     /* Fixnump */
            arg_temp = (UNBOXFIX(current_argP)*10);
            if_result_temp = BOXFIX(arg_temp+UNBOXFIX(numericP));
          }
          else if (current_argP==NULL) 
            if_result_temp = numericP;
          else 
            if_result_temp = ((bad_control_directive_error(ObjStrHDR(control_string)),
                (Obj)NULL));
          current_argP = if_result_temp;
        }
        else 
          switch ((sint32)next_char) {
           case 44:
            new_cons = arglist_pool;
            arglist_pool = CDR(new_cons);
            CAR(new_cons) = current_argP;
            CDR(new_cons) = (Obj)NULL;
            if (arglist!=NULL) 
              CDR(last(arglist)) = new_cons;
            else 
              arglist = new_cons;
            current_argP = (Obj)NULL;
            break;
           case 118:
           case 86:
            g_1 = ((args!=NULL) ? CAR(args) : (Obj)NULL);
            args = ((args!=NULL) ? CDR(args) : (Obj)NULL);
            current_argP = g_1;
            break;
           case 35:
            current_argP = BOXFIX(length(args));
            break;
           case 39:
            current_argP = BOXCHAR(control_string[index]);
            index = (index+1);
            break;
           case 64:
            atsign_modifierP = (Obj)(&T);
            break;
           case 58:
            colon_modifierP = (Obj)(&T);
            break;
           default:
            if (current_argP!=NULL) {
              new_cons = arglist_pool;
              arglist_pool = CDR(new_cons);
              CAR(new_cons) = current_argP;
              CDR(new_cons) = (Obj)NULL;
              if (arglist!=NULL) 
                CDR(last(arglist)) = new_cons;
              else 
                arglist = new_cons;
            }
            goto exit_nil_1;
          }
        if (index>=control_string_length) 
          bad_control_directive_error(ObjStrHDR(control_string));
        next_char = (control_string[index]);
        index = (index+1);
        g_2 = (((sint32)next_char)-48);
        if ((g_2>=0) && (g_2<10)) 
          if_result_temp = BOXFIX(g_2);
        else 
          if_result_temp = (Obj)NULL;
        numericP = if_result_temp;
      }

      goto exit_nil_1;
     exit_nil_1:
      if (('A'<=next_char) && (next_char<='Z')) 
        down_char = (unsigned char)(((sint32)next_char)+32);
      else 
        down_char = next_char;
      switch ((sint32)down_char) {
       case 97:
       case 115:
        new_Sprint_escapeS = ((down_char=='s') ? ((Obj)(&T)) : (Obj)NULL);
        ts_1 = THREAD_STATE;
        bind_global(&Sprint_escapeS,ts_1,new_Sprint_escapeS);
        arg_temp_1 = stream;
        g_3 = ((args!=NULL) ? CAR(args) : (Obj)NULL);
        args = ((args!=NULL) ? CDR(args) : (Obj)NULL);
        arg_temp_2 = g_3;
        write_with_arglist(arg_temp_1,arg_temp_2,arglist,atsign_modifierP,
            colon_modifierP);
        unbind_global(&Sprint_escapeS,ts_1);
        break;
       case 99:
        g_4 = ((args!=NULL) ? CAR(args) : (Obj)NULL);
        args = ((args!=NULL) ? CDR(args) : (Obj)NULL);
        arg_temp_3 = g_4;
        write_function(arg_temp_3,stream,GET_GLOBAL(Sprint_caseS),atsign_modifierP,
            UNBOXFIX(GET_GLOBAL(Sprint_baseS)),GET_GLOBAL(Sprint_prettyS),
            GET_GLOBAL(Sprint_levelS),GET_GLOBAL(Sprint_lengthS),GET_GLOBAL(Sprint_circleS));
        break;
       case 100:
       case 120:
       case 111:
       case 98:
        g_5 = ((args!=NULL) ? CAR(args) : (Obj)NULL);
        args = ((args!=NULL) ? CDR(args) : (Obj)NULL);
        object = g_5;
        new_Sprint_escapeS = (Obj)NULL;
        switch ((sint32)down_char) {
         case 120:
          new_Sprint_baseS = BOXFIX(16);
          break;
         case 111:
          new_Sprint_baseS = BOXFIX(8);
          break;
         case 98:
          new_Sprint_baseS = BOXFIX(2);
          break;
         default:
          new_Sprint_baseS = BOXFIX(10);
          break;
        }
        ts_2 = THREAD_STATE;
        bind_global(&Sprint_baseS,ts_2,new_Sprint_baseS);
        bind_global(&Sprint_escapeS,ts_2,new_Sprint_escapeS);
        if (IMMED_TAG(object)==1)               /* Fixnump */
          write_fixnum_with_arglist(stream,UNBOXFIX(object),arglist,atsign_modifierP,
              colon_modifierP);
        else {
          if (((arglist!=NULL) ? CDR(arglist) : (Obj)NULL)!=NULL) {
            new_cdr = arglist;
            list = new_cdr;
            if (list!=NULL) 
              list = CDR(list);
            else {
              block_temp = (Obj)NULL;
              goto exit_cdddr;
            }
            if (list!=NULL) 
              list = CDR(list);
            else {
              block_temp = (Obj)NULL;
              goto exit_cdddr;
            }
            block_temp = ((list!=NULL) ? CDR(list) : (Obj)NULL);
           exit_cdddr:
            arglist = block_temp;
            CDR(CDR(CDR(new_cdr))) = (Obj)NULL;
            CAR(new_cdr) = (Obj)NULL;
            CAR(CDR(new_cdr)) = (Obj)NULL;
            arg_temp_4 = CDR(CDR(new_cdr));
            list = arglist;
            if (list!=NULL) 
              list = CDR(list);
            else {
              block_temp = (Obj)NULL;
              goto exit_cadr;
            }
            block_temp = ((list!=NULL) ? CAR(list) : (Obj)NULL);
           exit_cadr:
            CAR(arg_temp_4) = block_temp;
            CDR(arglist) = new_cdr;
          }
          write_with_arglist(stream,object,arglist,atsign_modifierP,colon_modifierP);
        }
        unbind_global(&Sprint_escapeS,ts_2);
        unbind_global(&Sprint_baseS,ts_2);
        break;
       case 38:
        if (BOXCHAR('\n')!=last_stream_charP(stream)) 
          write_char('\n',stream);
        if (arglist!=NULL) {
          g = UNBOXFIX((Obj)((((sint32)CAR(arglist))-(sint32)BOXFIX(1))
              +1));                             /* Fixnum subtract */
          x = 0;
          for (;x<g;x = (x+1)) 
            write_char('\n',stream);
        }
        break;
       case 116:
        write_string_function(((Str *)(&str_const_29))->body,   /* "  " */
            stream,0,(Obj)NULL);
        break;
       case 37:
        temp_2 = ((arglist!=NULL) ? CAR(arglist) : (Obj)NULL);
        if (temp_2==NULL) 
          temp_2 = BOXFIX(1);
        g = UNBOXFIX(temp_2);
        x = 0;
        for (;x<g;x = (x+1)) 
          write_char('\n',stream);
        break;
       case 126:
        write_char('~',stream);
        break;
       case 10:
        if (atsign_modifierP!=NULL) 
          write_char('\n',stream);
        if (colon_modifierP==NULL) {
          while (1) {
            if (index<control_string_length) {
              white_char = (control_string[index]);
              temp_3 = ((white_char!=' ') && (white_char!='\011'));
            }
            else 
              temp_3 = 0;
            if (temp_3) 
              goto end_loop;
            index = (index+1);
          }

         end_loop:
          goto exit_nil_2;
         exit_nil_2:
;
        }
        break;
       case 40:
        if ((current_case_conversion!=NULL) || (cached_streamP!=NULL)) 
          bad_control_directive_error(ObjStrHDR(control_string));
        cached_streamP = stream;
        stream = alloc_field_width_string();
        current_case_conversion = ((atsign_modifierP!=NULL) ? ((colon_modifierP
            !=NULL) ? ((Obj)(tl_format_symbols+0)) : (Obj)(tl_format_symbols    /* UPCASE */
            +3)) : ((colon_modifierP!=NULL)     /* CAP-FIRST-LOWER-REST */
            ? ((Obj)(tl_format_symbols+2)) : (Obj)(tl_format_symbols    /* CAPITALIZE */
            +1)));                              /* DOWNCASE */
        break;
       case 41:
        if (!((current_case_conversion!=NULL) && (cached_streamP!=NULL))) 
          bad_control_directive_error(ObjStrHDR(control_string));
        field_width_string = (((Str *)stream)->body);
        if (current_case_conversion==(Obj)(tl_format_symbols+0))    /* UPCASE */
          nstring_upcase_function(field_width_string,0,(Obj)NULL);
        else if (current_case_conversion==(Obj)(tl_format_symbols+1))   /* DOWNCASE */
          nstring_downcase_function(field_width_string,0,(Obj)NULL);
        else if (current_case_conversion==(Obj)(tl_format_symbols+2))   /* CAPITALIZE */
          nstring_capitalize_function(field_width_string,0,(Obj)NULL);
        else if (current_case_conversion==(Obj)(tl_format_symbols+3)) {     /* CAP-FIRST-LOWER-REST */
          if (((sint32)(StrHDR(field_width_string)->fill_length))>0) {
            arg_temp_5 = field_width_string;
            g_6 = (field_width_string[0]);
            if (('a'<=g_6) && (g_6<='z')) 
              if_result_temp_1 = (unsigned char)(((sint32)g_6)-32);
            else 
              if_result_temp_1 = g_6;
            (arg_temp_5[0]) = if_result_temp_1;
            nstring_downcase_function(field_width_string,1,(Obj)NULL);
          }
        }
        write_string_function(field_width_string,cached_streamP,0,(Obj)NULL);
        stream = cached_streamP;
        current_case_conversion = (Obj)NULL;
        cached_streamP = (Obj)NULL;
        break;
       case 123:
        if (iteration_start_indexP!=NULL) 
          format_error((Obj)(&str_const_30),    /* "TL:FORMAT doesn't support nested ~{ iterations." */
              ObjStrHDR(control_string));
        iteration_start_indexP = BOXFIX(index);
        iteration_maximum_loopsP = ((arglist!=NULL) ? CAR(arglist) : (Obj)NULL);
        if (atsign_modifierP!=NULL) {
          if (colon_modifierP!=NULL) {
            iteration_uses_sublistsP = (Obj)(&T);
            iteration_sublists = args;
          }
          else 
            iteration_uses_sublistsP = (Obj)NULL;
          iteration_cached_args = (Obj)NULL;
        }
        else {
          if (colon_modifierP!=NULL) {
            iteration_uses_sublistsP = (Obj)(&T);
            iteration_sublists = ((args!=NULL) ? CAR(args) : (Obj)NULL);
          }
          else {
            args = ((args!=NULL) ? CAR(args) : (Obj)NULL);
            iteration_uses_sublistsP = (Obj)NULL;
          }
          iteration_cached_args = ((args!=NULL) ? CDR(args) : (Obj)NULL);
        }
        at_least_onceP = (Obj)NULL;
        end_index = index;
        for (;!(end_index>=control_string_length);end_index = (end_index
            +1)) {
          char_1 = (control_string[end_index]);
          if (char_1=='~') {
            next_char_1 = (control_string[end_index+1]);
            if (next_char_1=='}') {
              block_temp = BOXFIX(end_index+2);
              goto exit_nil_3;
            }
            else if ((next_char_1==':') && ((control_string[end_index+2])
                =='}')) {
              at_least_onceP = (Obj)(&T);
              block_temp = BOXFIX(end_index+3);
              goto exit_nil_3;
            }
          }
        }
        block_temp = (Obj)NULL;
        goto exit_nil_3;
       exit_nil_3:
        iteration_end_index = block_temp;
        if (iteration_end_index==NULL) 
          format_error((Obj)(&str_const_31),    /* "In FORMAT, a ~{ iteration had no closing ~}" */
              ObjStrHDR(control_string));
        if (((iteration_maximum_loopsP!=NULL) && (((sint32)iteration_maximum_loopsP)
            <=(sint32)BOXFIX(0))) || (((iteration_uses_sublistsP!=NULL) 
            ? (iteration_sublists==NULL) : (args==NULL)) && (at_least_onceP
            ==NULL))) {
          args = iteration_cached_args;
          iteration_start_indexP = (Obj)NULL;
          index = UNBOXFIX(iteration_end_index);
        }
        else if (iteration_uses_sublistsP!=NULL) {
          g_7 = ((iteration_sublists!=NULL) ? CAR(iteration_sublists) : 
              (Obj)NULL);
          iteration_sublists = ((iteration_sublists!=NULL) ? CDR(iteration_sublists) 
              : (Obj)NULL);
          args = g_7;
        }
        break;
       case 94:
        if (iteration_start_indexP==NULL) 
          format_error((Obj)(&str_const_32),    /* "In FORMAT, ~^ found outside of any ~{ iteration." */
              ObjStrHDR(control_string));
        iteration_start_indexP = (Obj)NULL;
        index = UNBOXFIX(iteration_end_index);
        break;
       case 125:
        if (iteration_start_indexP==NULL) 
          format_error((Obj)(&str_const_33),    /* "In FORMAT, unmatched ~}." */
              ObjStrHDR(control_string));
        if (iteration_maximum_loopsP!=NULL) {
          g = UNBOXFIX((Obj)((((sint32)iteration_maximum_loopsP)-(sint32)
              BOXFIX(1))+1));                   /* Fixnum subtract */
          temp_4 = (((sint32)(iteration_maximum_loopsP = BOXFIX(g)))<=(sint32)
              BOXFIX(0));
        }
        else 
          temp_4 = 0;
        if (temp_4 || ((iteration_uses_sublistsP!=NULL) ? (iteration_sublists
            ==NULL) : (args==NULL))) {
          iteration_start_indexP = (Obj)NULL;
          args = iteration_cached_args;
        }
        else {
          index = UNBOXFIX(iteration_start_indexP);
          if (iteration_uses_sublistsP!=NULL) {
            g_8 = ((iteration_sublists!=NULL) ? CAR(iteration_sublists) 
                : (Obj)NULL);
            iteration_sublists = ((iteration_sublists!=NULL) ? CDR(iteration_sublists) 
                : (Obj)NULL);
            args = g_8;
          }
        }
        break;
       case 91:
        if (atsign_modifierP!=NULL) {
          if (((args!=NULL) ? CAR(args) : (Obj)NULL)!=NULL) 
            clause_number = 0;
          else {
            g_9 = ((args!=NULL) ? CAR(args) : (Obj)NULL);
            args = ((args!=NULL) ? CDR(args) : (Obj)NULL);
            (void)g_9;
            clause_number = 536870911;
          }
        }
        else if (colon_modifierP!=NULL) {
          g_10 = ((args!=NULL) ? CAR(args) : (Obj)NULL);
          args = ((args!=NULL) ? CDR(args) : (Obj)NULL);
          if (g_10!=NULL) 
            clause_number = 1;
          else 
            clause_number = 0;
        }
        else if (arglist!=NULL) 
          clause_number = UNBOXFIX((arglist!=NULL) ? CAR(arglist) : (Obj)NULL);
        else {
          g = UNBOXFIX((args!=NULL) ? CAR(args) : (Obj)NULL);
          args = ((args!=NULL) ? CDR(args) : (Obj)NULL);
          clause_number = g;
        }
        if (clause_number<0) 
          clause_number = 536870911;
        clause_index = index;
        for (;!(clause_index>=control_string_length);clause_index = (clause_index
            +1)) {
          if (clause_number==0) {
            index = clause_index;
            goto exit_nil_4;
          }
          char_2 = (control_string[clause_index]);
          if (char_2=='~') {
            clause_index = (clause_index+1);
            directive_index = discard_format_arglist(control_string,clause_index,
                control_string_length);
            next_char_2 = (control_string[directive_index]);
            switch ((sint32)next_char_2) {
             case 59:
              if ((control_string[clause_index])==':') {
                index = (directive_index+1);
                goto exit_nil_4;
              }
              else if (clause_index!=directive_index) 
                format_error((Obj)(&str_const_34),  /* "Only : args to ~; allowed." */
                    ObjStrHDR(control_string));
              else 
                clause_number = (clause_number-1);
              break;
             case 93:
              index = (directive_index+1);
              goto exit_nil_4;
             case 91:
              clause_index = find_end_of_conditional(control_string,directive_index
                  +1,control_string_length);
              break;
             default:
              clause_index = directive_index;
              break;
            }
          }
        }
        format_error((Obj)(&str_const_35),ObjStrHDR(control_string));   /* "No ~] found for ~[" */
        goto exit_nil_4;
       exit_nil_4:
        break;
       case 59:
        index = (find_end_of_conditional(control_string,index,control_string_length)
            +1);
        break;
       case 93:
        break;
       default:
        unsupported_control_char_error(BOXCHAR(next_char));
        break;
      }
    }
    else 
      write_char(next_char,stream);
  }

  goto exit_nil;
 exit_nil:
  if (stream_arg==NULL) {
    g_11 = get_output_stream_string(stream);
    temp_1 = g_11;
  }
  else 
    temp_1 = (Obj)NULL;
  unbind_global(&field_width_string_list,ts);
  return temp_1;
}

static const Str_49 str_const_36
  = { 7, 45, 45, "Twiddle did not have corresponding directive." };

/* Translated from DISCARD-FORMAT-ARGLIST(STRING FIXNUM FIXNUM) = FIXNUM */

sint32 discard_format_arglist (unsigned char *control_string, sint32 index, 
        sint32 length_1)
{
  unsigned char char_1;

  char_1 = (control_string[index]);
  while (!(index>=length_1)) {
    switch ((sint32)char_1) {
     case 44:
     case 118:
     case 86:
     case 35:
     case 64:
     case 58:
     case 48:
     case 49:
     case 50:
     case 51:
     case 52:
     case 53:
     case 54:
     case 55:
     case 56:
     case 57:
      index = (index+1);
      break;
     case 39:
      index = (index+2);
      break;
     default:
      return index;
    }
    char_1 = (control_string[index]);
  }

  format_error((Obj)(&str_const_36),            /* "Twiddle did not have corresponding directive." */
      ObjStrHDR(control_string));
  return 0;
}

static const Str_17 str_const_37
  = { 7, 13, 13, "Unmatched ~[." };

/* Translated from FIND-END-OF-CONDITIONAL(STRING FIXNUM FIXNUM) = FIXNUM */

sint32 find_end_of_conditional (unsigned char *control_string, sint32 conditional_body_index, 
        sint32 length_1)
{
  sint32 nesting, index;
  unsigned char char_1;
  Obj g;

  nesting = 1;
  index = conditional_body_index;
  for (;!(index>=length_1);index = (index+1)) {
    char_1 = (control_string[index]);
    if (char_1=='~') {
      index = discard_format_arglist(control_string,index+1,length_1);
      g = BOXCHAR(control_string[index]);
      if (g==BOXCHAR(']')) {
        nesting = (nesting-1);
        if (nesting==0) 
          return index;
      }
      else if (g==BOXCHAR('[')) 
        nesting = (nesting+1);
    }
  }
  return (sint32)format_error((Obj)(&str_const_37),     /* "Unmatched ~[." */
      ObjStrHDR(control_string));
}

/* Translated from ERROR-ONE-ARG(T T) = NULL */

Obj error_one_arg (Obj control_string, Obj arg)
{
  Obj temp;
  Thread_state *ts;
  unsigned char *arg_temp;
  Obj temp_list[2];

  ts = THREAD_STATE;
  temp = BOXFIX(0);
  bind_global(&current_region,ts,temp);
  arg_temp = (((Str *)control_string)->body);
  (temp_list[0]) = arg;
  (temp_list[1]) = (Obj)NULL;
  error((char *)(((Str *)format_function((Obj)NULL,arg_temp,(Obj)(((uint32)temp_list)
      +2)))->body));
  unbind_global(&current_region,ts);
  return (Obj)NULL;
}

/* Translated from ERROR-TWO-ARGS(T T T) = NULL */

Obj error_two_args (Obj control_string, Obj arg1, Obj arg2)
{
  Obj temp;
  Thread_state *ts;
  unsigned char *arg_temp;
  Obj temp_list[4];

  ts = THREAD_STATE;
  temp = BOXFIX(0);
  bind_global(&current_region,ts,temp);
  arg_temp = (((Str *)control_string)->body);
  (temp_list[0]) = arg1;
  (temp_list[2]) = arg2;
  error((char *)(((Str *)format_function((Obj)NULL,arg_temp,hook_up_cdrs(temp_list,
      2,NULL)))->body));
  unbind_global(&current_region,ts);
  return (Obj)NULL;
}

/* Translated from ERROR-THREE-ARGS(T T T T) = NULL */

Obj error_three_args (Obj control_string, Obj arg1, Obj arg2, Obj arg3)
{
  Obj temp;
  Thread_state *ts;
  unsigned char *arg_temp;
  Obj temp_list[6];

  ts = THREAD_STATE;
  temp = BOXFIX(0);
  bind_global(&current_region,ts,temp);
  arg_temp = (((Str *)control_string)->body);
  (temp_list[0]) = arg1;
  (temp_list[2]) = arg2;
  (temp_list[4]) = arg3;
  error((char *)(((Str *)format_function((Obj)NULL,arg_temp,hook_up_cdrs(temp_list,
      3,NULL)))->body));
  unbind_global(&current_region,ts);
  return (Obj)NULL;
}

static const Str_65 str_const_38
  = { 7, 64, 64, "This control string contained an ill-formed format directive: ~s" };

/* Translated from BAD-CONTROL-DIRECTIVE-ERROR(T) = VOID */

void bad_control_directive_error (Obj control_string)
{
  error_one_arg((Obj)(&str_const_38),           /* "This control string contained an ill-formed format..." */
      control_string);
  return;
}

static const Str_65 str_const_39
  = { 7, 61, 61, "The character ~s is not a supported format control character." };

/* Translated from UNSUPPORTED-CONTROL-CHAR-ERROR(T) = VOID */

void unsupported_control_char_error (Obj bad_char)
{
  error_one_arg((Obj)(&str_const_39),           /* "The character ~s is not a supported format control..." */
      bad_char);
  return;
}

static const Str_49 str_const_40
  = { 7, 46, 46, "The object ~s was not a valid stream argument." };

/* Translated from BAD-STREAM-ERROR(T) = VOID */

void bad_stream_error (Obj stream_arg)
{
  error_one_arg((Obj)(&str_const_40),           /* "The object ~s was not a valid stream argument." */
      stream_arg);
  return;
}

static const Str_33 str_const_41
  = { 7, 29, 29, "~a  The control string was ~s" };

/* Translated from FORMAT-ERROR(T T) = NULL */

Obj format_error (Obj description, Obj control_string)
{
  return error_two_args((Obj)(&str_const_41),   /* "~a  The control string was ~s" */
      description,control_string);
}

static const Str_77 str_const_42
  = { 7, 74, 74, "The extra value ~a ran off the end of a destructuring-bind-strict pattern." };

/* Translated from NOT-NULL-DESTRUCTURING-ERROR-1(T) = VOID */

void not_null_destructuring_error_1 (Obj shoulda_been_nil)
{
  error_one_arg((Obj)(&str_const_42),           /* "The extra value ~a ran off the end of a destructur..." */
      shoulda_been_nil);
  return;
}

static const Str_57 str_const_43
  = { 7, 54, 54, "TL make-array does not support multiple-dimensions: ~a" };

static const Str_57 str_const_44
  = { 7, 55, 55, "TL make-array dimension argument was not an integer: ~a" };

/* Translated from CHECK-MAKE-ARRAY-DIMENSIONS(T) = FIXNUM */

sint32 check_make_array_dimensions (Obj dimensions)
{
  if (IMMED_TAG(dimensions)==2) {               /* Consp */
    if (CDR(dimensions)!=NULL) 
      error_one_arg((Obj)(&str_const_43),       /* "TL make-array does not support multiple-dimensions..." */
          dimensions);
    else 
      dimensions = CAR(dimensions);
  }
  if (!(IMMED_TAG(dimensions)==1))              /* Fixnump */
    error_one_arg((Obj)(&str_const_44),         /* "TL make-array dimension argument was not an intege..." */
        dimensions);
  return UNBOXFIX(dimensions);
}

/* Translated from PM-PRINT(T) = T */

Obj pm_print (Obj object)
{
  prin1(object,(Obj)NULL);
  terpri((Obj)NULL);
  force_output((Obj)NULL);
  return (Obj)NULL;
}

static const Str_9 str_const_45
  = { 7, 7, 7, "KEYWORD" };

static const Str_9 str_const_46
  = { 7, 6, 6, "UPCASE" };

static const Str_9 str_const_47
  = { 7, 8, 8, "DOWNCASE" };

static const Str_13 str_const_48
  = { 7, 10, 10, "CAPITALIZE" };

static const Str_21 str_const_49
  = { 7, 20, 20, "CAP-FIRST-LOWER-REST" };

Sym tl_format_symbols[4];

Func tl_format_funcs[1];

/* Translated from SYMS-TL-FORMAT() = VOID */

void syms_tl_format (void)
{
  Obj cached_keyword_package;

  cached_keyword_package = find_package_1((Obj)(&str_const_45));    /* "KEYWORD" */
  init_symbol_into_package((Obj)(&(tl_format_symbols[0])),(Obj)(&str_const_46),     /* "UPCASE" */
      3167,cached_keyword_package);
  (tl_format_symbols[0]).external = 1;
  (tl_format_symbols[0]).symbol_value = (Obj)(&(tl_format_symbols[0]));
  init_symbol_into_package((Obj)(&(tl_format_symbols[1])),(Obj)(&str_const_47),     /* "DOWNCASE" */
      15423,cached_keyword_package);
  (tl_format_symbols[1]).external = 1;
  (tl_format_symbols[1]).symbol_value = (Obj)(&(tl_format_symbols[1]));
  init_symbol_into_package((Obj)(&(tl_format_symbols[2])),(Obj)(&str_const_48),     /* "CAPITALIZE" */
      61541,cached_keyword_package);
  (tl_format_symbols[2]).external = 1;
  (tl_format_symbols[2]).symbol_value = (Obj)(&(tl_format_symbols[2]));
  init_symbol_into_package((Obj)(&(tl_format_symbols[3])),(Obj)(&str_const_49),     /* "CAP-FIRST-LOWER-REST" */
      49547,cached_keyword_package);
  (tl_format_symbols[3]).external = 1;
  (tl_format_symbols[3]).symbol_value = (Obj)(&(tl_format_symbols[3]));
  return;
}


/* Translated from INIT-TL-FORMAT() = VOID */

void init_tl_format (void)
{
  unsigned char *g;
  unsigned char *g_1;

  (tl_format_funcs[0]).type = 12;
  (tl_format_funcs[0]).arg_count = 3;
  (tl_format_funcs[0]).optional_arguments = 0;
  (tl_format_funcs[0]).sets_values_count = 0;
  (tl_format_funcs[0]).default_arguments = (Obj)NULL;
  (tl_format_funcs[0]).closure_environment = (Obj)NULL;
  (tl_format_funcs[0]).name = (Obj)(&str_const_20);     /* "WRITE-SYMBOL" */
  (tl_format_funcs[0]).c_function = (Obj (*)(Obj))write_symbol;
  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  if (Sterminal_ioS==(Obj)(&Unbound)) 
    Sterminal_ioS = alloc_file_strm(stdin,stdout,NULL,NULL,0,16);
  if (Sstandard_inputS==(Obj)(&Unbound)) 
    Sstandard_inputS = GET_GLOBAL(Sterminal_ioS);
  if (Sstandard_outputS==(Obj)(&Unbound)) 
    Sstandard_outputS = GET_GLOBAL(Sterminal_ioS);
  if (Serror_outputS==(Obj)(&Unbound)) 
    Serror_outputS = alloc_file_strm(NULL,stderr,NULL,NULL,0,16);
  if (Squery_ioS==(Obj)(&Unbound)) 
    Squery_ioS = GET_GLOBAL(Sterminal_ioS);
  if (Sdebug_ioS==(Obj)(&Unbound)) 
    Sdebug_ioS = GET_GLOBAL(Sterminal_ioS);
  if (Strace_outputS==(Obj)(&Unbound)) 
    Strace_outputS = GET_GLOBAL(Serror_outputS);
  if (Sprint_caseS==(Obj)(&Unbound)) 
    Sprint_caseS = (Obj)(tl_format_symbols+0);  /* UPCASE */
  if (Sprint_baseS==(Obj)(&Unbound)) 
    Sprint_baseS = BOXFIX(10);
  if (Sprint_escapeS==(Obj)(&Unbound)) 
    Sprint_escapeS = (Obj)(&T);
  if (Sprint_prettyS==(Obj)(&Unbound)) 
    Sprint_prettyS = (Obj)NULL;
  if (Sprint_lengthS==(Obj)(&Unbound)) 
    Sprint_lengthS = (Obj)NULL;
  if (Sprint_circleS==(Obj)(&Unbound)) 
    Sprint_circleS = (Obj)NULL;
  if (Sprint_levelS==(Obj)(&Unbound)) 
    Sprint_levelS = (Obj)NULL;
  g = (((Str *)alloc_string(64,0,7))->body);
  memset((void *)(g+0),'\000',(sint32)(StrHDR(g)->fill_length));
  (void)g;
  reversed_fixnum_with_commas_string = ObjStrHDR(g);
  if (field_width_string_list==(Obj)(&Unbound)) {
    g_1 = (((Str *)alloc_string(256,0,7))->body);
    memset((void *)(g_1+0),'\000',(sint32)(StrHDR(g_1)->fill_length));
    (void)g_1;
    field_width_string_list = alloc_cons(ObjStrHDR(g_1),(Obj)NULL,0);
  }
  return;
}

