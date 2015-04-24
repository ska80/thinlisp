/***
 *
 * Module:      tl/c/input.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/input.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "input.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

static const Str_45 str_const_1
  = { 7, 42, 42, "Non-EOF error while reading from stream ~s" };

/* Translated from ANALYZE-FILE-STREAM-ERROR(FILE-STREAM T T) = T */

Obj analyze_file_stream_error (Obj file_stream, Obj eof_error_p, Obj eof_value)
{
  if (feof(((File_strm *)file_stream)->input)!=0) 
    return error_or_value(file_stream,eof_error_p,eof_value);
  else 
    return error_one_arg((Obj)(&str_const_1),   /* "Non-EOF error while reading from stream ~s" */
        file_stream);
}

static const Str_49 str_const_2
  = { 7, 47, 47, "End of File  error while reading from stream ~s" };

/* Translated from ERROR-OR-VALUE(T T T) = T */

Obj error_or_value (Obj stream, Obj eof_error_p, Obj eof_value)
{
  if (eof_error_p!=NULL) 
    return error_one_arg((Obj)(&str_const_2),   /* "End of File  error while reading from stream ~s" */
        stream);
  else 
    return eof_value;
}

static const Str_109 str_const_3
  = { 7, 108, 108, "Stream ~s was neither a file-stream ~\n                              nor a string-stream in generic-read-char" };

/* Translated from GENERIC-READ-CHAR(T T T) = T */

Obj generic_read_char (Obj stream, Obj eof_error_p, Obj eof_value)
{
  sint32 temp;
  Obj g;
  sint32 g_1, g_2;

  switch (TYPE_TAG(stream,temp)) {
   case 16:
    g = stream;
    g_1 = (sint32)getc(((File_strm *)g)->input);
    if (g_1==(-1)) 
      return analyze_file_stream_error(g,eof_error_p,eof_value);
    else 
      return BOXCHAR((unsigned char)g_1);
   case 15:
    g = stream;
    g_2 = (((String_strm *)g)->input_index);
    if (g_2<(((String_strm *)g)->input_index_bounds)) {
      ((String_strm *)g)->input_index = (g_2+1);
      return BOXCHAR(((String_strm *)g)->input_string[g_2]);
    }
    else 
      return error_or_value(g,eof_error_p,eof_value);
   default:
    return error_one_arg((Obj)(&str_const_3),   /* "Stream ~s was neither a file-stream ~
            ..." */
        stream);
  }
}

Obj Sinput_string_bufferS = (Obj)(&Unbound);

Obj Sinput_string_buffer_sizeS = (Obj)(&Unbound);

static const Str_41 str_const_4
  = { 7, 37, 37, "Input buffer overflow reading from ~s" };

/* Translated from READ-LINE-FROM-FILE-STREAM(&OPTIONAL FILE-STREAM T T) = * */

Obj read_line_from_file_stream (Obj file_stream, Obj eof_error_p, Obj eof_value)
{
  Obj fgets_result, temp;
  sint32 strlen_value;
  Obj missing_new_lineP;
  Str *string;
  Obj temp_1;

  fgets_result = ObjStrHDR((unsigned char *)fgets((char *)(((Str *)GET_GLOBAL(Sinput_string_bufferS))->body),
      (int)UNBOXFIX(GET_GLOBAL(Sinput_string_buffer_sizeS)),((File_strm *)file_stream)->input));
  if (((Str *)fgets_result)->body==NULL) {
    temp = analyze_file_stream_error(file_stream,eof_error_p,eof_value);
    (Values_buffer[0]) = (Obj)(&T);
    Values_count = 2;
    return temp;
  }
  else {
    strlen_value = (sint32)strlen((char *)(((Str *)GET_GLOBAL(Sinput_string_bufferS))->body));
    missing_new_lineP = (Obj)(&T);
    if (strlen_value>UNBOXFIX(GET_GLOBAL(Sinput_string_buffer_sizeS))) 
      error_one_arg((Obj)(&str_const_4),        /* "Input buffer overflow reading from ~s" */
          file_stream);
    else if ((strlen_value>0) && ((((Str *)GET_GLOBAL(Sinput_string_bufferS))->body[strlen_value
        -1])=='\n')) {
      missing_new_lineP = (Obj)NULL;
      strlen_value = (strlen_value-1);
    }
    string = (Str *)GET_GLOBAL(Sinput_string_bufferS);
    (string->body[strlen_value]) = '\000';
    string->fill_length = strlen_value;
    temp_1 = GET_GLOBAL(Sinput_string_bufferS);
    (Values_buffer[0]) = missing_new_lineP;
    Values_count = 2;
    return temp_1;
  }
}

/* Translated from READ-LINE-FROM-STRING-STREAM(&OPTIONAL TL-STRING-STREAM T T) = * */

Obj read_line_from_string_stream (Obj string_stream, Obj eof_error_p, Obj eof_value)
{
  unsigned char *input_string;
  unsigned char *final_result;
  sint32 input_index_bounds;
  unsigned char next_char;
  Obj missing_newlineP;
  sint32 input_index, buffer_index, g, g_1;
  Str *string;
  Obj temp;

  input_string = (((String_strm *)string_stream)->input_string);
  final_result = (((Str *)GET_GLOBAL(Sinput_string_bufferS))->body);
  input_index_bounds = (((String_strm *)string_stream)->input_index_bounds);
  next_char = '\000';
  missing_newlineP = (Obj)(&T);
  input_index = (((String_strm *)string_stream)->input_index);
  buffer_index = 0;
  while (1) {
    if (!(((input_index<input_index_bounds) && (buffer_index<UNBOXFIX(GET_GLOBAL(Sinput_string_buffer_sizeS))))
         && (missing_newlineP!=NULL))) 
      goto end_loop;
    next_char = (input_string[input_index]);
    (((Str *)GET_GLOBAL(Sinput_string_bufferS))->body[buffer_index]) = next_char;
    missing_newlineP = ((next_char!='\n') ? ((Obj)(&T)) : (Obj)NULL);
    g = (input_index+1);
    g_1 = (buffer_index+1);
    input_index = g;
    buffer_index = g_1;
  }

 end_loop:
  if (missing_newlineP==NULL) 
    buffer_index = (buffer_index-1);
  else if (buffer_index==0) 
    final_result = (((Str *)error_or_value(string_stream,eof_error_p,eof_value))->body);
  else if ((buffer_index==UNBOXFIX(GET_GLOBAL(Sinput_string_buffer_sizeS)))
       && (input_index!=input_index_bounds)) 
    error_one_arg((Obj)(&str_const_4),          /* "Input buffer overflow reading from ~s" */
        string_stream);
  ((String_strm *)string_stream)->input_index = input_index;
  string = (Str *)GET_GLOBAL(Sinput_string_bufferS);
  (string->body[buffer_index]) = '\000';
  string->fill_length = buffer_index;
  temp = ObjStrHDR(final_result);
  (Values_buffer[0]) = missing_newlineP;
  Values_count = 2;
  return temp;
}

static const Str_109 str_const_5
  = { 7, 108, 108, "Stream ~s was neither a file-stream ~\n                              nor a string-stream in generic-read-line" };

/* Translated from GENERIC-READ-LINE(T T T) = * */

Obj generic_read_line (Obj stream, Obj eof_error_p, Obj eof_value)
{
  sint32 temp;
  Obj temp_1;

  switch (TYPE_TAG(stream,temp)) {
   case 16:
    return read_line_from_file_stream(stream,eof_error_p,eof_value);
   case 15:
    return read_line_from_string_stream(stream,eof_error_p,eof_value);
   default:
    temp_1 = error_one_arg((Obj)(&str_const_5),     /* "Stream ~s was neither a file-stream ~
            ..." */
        stream);
    Values_count = 1;
    return temp_1;
  }
}

static const Str_5 str_const_6
  = { 7, 2, 2, "ab" };

static const Str_5 str_const_7
  = { 7, 1, 1, "a" };

/* Translated from CREATE-FILE(STRING &OPTIONAL T) = T */

Obj create_file (unsigned char *filename, Obj binaryP)
{
  Obj g;
  FILE *temp;
  Obj g_1;
  sint32 temp_1;
  Obj g_2;
  FILE *temp_2;
  Obj g_3;
  sint32 temp_3;

  if (binaryP!=NULL) {
    temp = fopen((char *)filename,(char *)(((Str *)(&str_const_6))->body));     /* "ab" */
    g = ((temp==NULL) ? ((Obj)NULL) : alloc_file_strm(temp,temp,(char *)filename,
        (char *)(((Str *)(&str_const_6))->body),-1,16));    /* "ab" */
    g_1 = (Obj)NULL;
    if (!((g==NULL) || (g==GET_GLOBAL(Sterminal_ioS)))) {
      temp_1 = (sint32)fclose(((File_strm *)g)->input);
      if (g_1!=NULL) 
        delete_named_file(((File_strm *)g)->filename);
    }
    return (Obj)(&T);
  }
  else {
    temp_2 = fopen((char *)filename,(char *)(((Str *)(&str_const_7))->body));   /* "a" */
    g_2 = ((temp_2==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_2,temp_2,
        (char *)filename,(char *)(((Str *)(&str_const_7))->body),   /* "a" */
        -1,16));
    g_3 = (Obj)NULL;
    if (!((g_2==NULL) || (g_2==GET_GLOBAL(Sterminal_ioS)))) {
      temp_3 = (sint32)fclose(((File_strm *)g_2)->input);
      if (g_3!=NULL) 
        delete_named_file(((File_strm *)g_2)->filename);
    }
    return (Obj)(&T);
  }
}

static const Str_5 str_const_8
  = { 7, 2, 2, "rb" };

static const Str_69 str_const_9
  = { 7, 67, 67, "File named ~s could not be opened for input, perhaps does not exist" };

/* Translated from OPEN-FOR-BINARY-INPUT(STRING T) = T */

Obj open_for_binary_input (unsigned char *filename, Obj if_does_not_exist)
{
  FILE *temp;
  Obj file_stream;
  FILE *temp_1;
  Obj file_stream_1;
  FILE *temp_2;
  FILE *temp_3;

  if (if_does_not_exist==(Obj)NULL) {
    temp = fopen((char *)filename,(char *)(((Str *)(&str_const_8))->body));     /* "rb" */
    return (temp==NULL) ? ((Obj)NULL) : alloc_file_strm(temp,temp,(char *)filename,
        (char *)(((Str *)(&str_const_8))->body),-1,16);     /* "rb" */
  }
  else if (if_does_not_exist==(Obj)(tl_input_symbols+0)) {  /* ERROR */
    temp_1 = fopen((char *)filename,(char *)(((Str *)(&str_const_8))->body));   /* "rb" */
    file_stream = ((temp_1==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_1,
        temp_1,(char *)filename,(char *)(((Str *)(&str_const_8))->body),    /* "rb" */
        -1,16));
    if (file_stream!=NULL) 
      return file_stream;
    else 
      return error_one_arg((Obj)(&str_const_9),     /* "File named ~s could not be opened for input, perha..." */
          ObjStrHDR(filename));
  }
  else if (if_does_not_exist==(Obj)(tl_input_symbols+1)) {  /* CREATE */
    temp_2 = fopen((char *)filename,(char *)(((Str *)(&str_const_8))->body));   /* "rb" */
    file_stream_1 = ((temp_2==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_2,
        temp_2,(char *)filename,(char *)(((Str *)(&str_const_8))->body),    /* "rb" */
        -1,16));
    if (file_stream_1!=NULL) 
      return file_stream_1;
    else {
      create_file(filename,(Obj)(&T));
      temp_3 = fopen((char *)filename,(char *)(((Str *)(&str_const_8))->body));     /* "rb" */
      return (temp_3==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_3,temp_3,
          (char *)filename,(char *)(((Str *)(&str_const_8))->body),     /* "rb" */
          -1,16);
    }
  }
  else 
    return (Obj)NULL;
}

static const Str_5 str_const_10
  = { 7, 1, 1, "r" };

static const Str_53 str_const_11
  = { 7, 52, 52, "Unrecognized value ~s for keyword :IF-DOES-NOT-EXIST" };

/* Translated from OPEN-FOR-TEXT-INPUT(STRING T) = T */

Obj open_for_text_input (unsigned char *filename, Obj if_does_not_exist)
{
  FILE *temp;
  Obj file_stream;
  FILE *temp_1;
  Obj file_stream_1;
  FILE *temp_2;
  FILE *temp_3;

  if (if_does_not_exist==(Obj)NULL) {
    temp = fopen((char *)filename,(char *)(((Str *)(&str_const_10))->body));    /* "r" */
    return (temp==NULL) ? ((Obj)NULL) : alloc_file_strm(temp,temp,(char *)filename,
        (char *)(((Str *)(&str_const_10))->body),-1,16);    /* "r" */
  }
  else if (if_does_not_exist==(Obj)(tl_input_symbols+0)) {  /* ERROR */
    temp_1 = fopen((char *)filename,(char *)(((Str *)(&str_const_10))->body));  /* "r" */
    file_stream = ((temp_1==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_1,
        temp_1,(char *)filename,(char *)(((Str *)(&str_const_10))->body),   /* "r" */
        -1,16));
    if (file_stream!=NULL) 
      return file_stream;
    else 
      return error_one_arg((Obj)(&str_const_9),     /* "File named ~s could not be opened for input, perha..." */
          ObjStrHDR(filename));
  }
  else if (if_does_not_exist==(Obj)(tl_input_symbols+1)) {  /* CREATE */
    temp_2 = fopen((char *)filename,(char *)(((Str *)(&str_const_10))->body));  /* "r" */
    file_stream_1 = ((temp_2==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_2,
        temp_2,(char *)filename,(char *)(((Str *)(&str_const_10))->body),   /* "r" */
        -1,16));
    if (file_stream_1!=NULL) 
      return file_stream_1;
    else {
      create_file(filename,(Obj)NULL);
      temp_3 = fopen((char *)filename,(char *)(((Str *)(&str_const_10))->body));    /* "r" */
      return (temp_3==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_3,temp_3,
          (char *)filename,(char *)(((Str *)(&str_const_10))->body),    /* "r" */
          -1,16);
    }
  }
  else 
    return error_one_arg((Obj)(&str_const_11),  /* "Unrecognized value ~s for keyword :IF-DOES-NOT-EXI..." */
        if_does_not_exist);
}

static const Str_25 str_const_12
  = { 7, 22, 22, "File ~s already exists" };

static const Str_5 str_const_13
  = { 7, 3, 3, "rb+" };

static const Str_5 str_const_14
  = { 7, 2, 2, "wb" };

static const Str_125 str_const_15
  = { 7, 121, 121, { 'W','h','i','l','e',' ','a','t','t','e','m','p','t',
      'i','n','g',' ','t','o',' ','o','p','e','n',' ','f','i','l','e',' ',
      '~','s',' ','f','o','r',' ','o','u','t','p','u','t','~','%','~','\n',
      ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
      ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
      'u','n','r','e','c','o','g','n','i','z','e','d',' ',':','I','F','-',
      'E','X','I','S','T','S',' ','k','e','y','w','o','r','d',' ','v','a',
      'l','u','e',' ','~','s', '\000' } };

static const Str_125 str_const_16
  = { 7, 123, 123, { 'W','h','i','l','e',' ','a','t','t','e','m','p','t',
      'i','n','g',' ','t','o',' ','o','p','e','n',' ','f','i','l','e',' ',
      '~','s',' ','f','o','r',' ','o','u','t','p','u','t','~','%','~','\n',
      ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
      ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
      ' ',' ','u','n','r','e','c','o','g','n','i','z','e','d',' ',':','I',
      'F','-','E','X','I','S','T','S',' ','k','e','y','w','o','r','d',' ',
      'v','a','l','u','e',' ','~','s', '\000' } };

static const Str_25 str_const_17
  = { 7, 22, 22, "File ~s does not exist" };

/* Translated from OPEN-FOR-BINARY-OUTPUT(STRING T T) = T */

Obj open_for_binary_output (unsigned char *filename, Obj if_exists, Obj if_does_not_exist)
{
  Obj file_stream, g, file_stream_1;
  FILE *temp;
  sint32 temp_1;
  FILE *temp_2;
  Obj file_stream_2;
  FILE *temp_3;
  FILE *temp_4;
  FILE *temp_5;
  FILE *temp_6;
  Obj file_stream_3;
  sint32 temp_7;
  FILE *temp_8;
  FILE *temp_9;
  FILE *temp_10;

  if (if_does_not_exist==(Obj)(tl_input_symbols+1)) {   /* CREATE */
    if ((if_exists==(Obj)(tl_input_symbols+0)) || (if_exists    /* ERROR */
        ==(Obj)NULL)) {
      g = ObjStrHDR(filename);
      temp = fopen((char *)(((Str *)g)->body),(char *)(((Str *)(&str_const_10))->body));    /* "r" */
      file_stream_1 = ((temp==NULL) ? ((Obj)NULL) : alloc_file_strm(temp,
          temp,(char *)(((Str *)g)->body),(char *)(((Str *)(&str_const_10))->body),     /* "r" */
          -1,16));
      if (file_stream_1!=NULL) {
        temp_1 = (sint32)fclose(((File_strm *)file_stream_1)->input);
        if (0) 
          delete_named_file(((File_strm *)file_stream_1)->filename);
      }
      if (file_stream_1!=NULL) 
        file_stream = g;
      else 
        file_stream = (Obj)NULL;
      if (file_stream!=NULL) {
        if (if_exists==(Obj)(tl_input_symbols+0))   /* ERROR */
          return error_one_arg((Obj)(&str_const_12),    /* "File ~s already exists" */
              ObjStrHDR(filename));
        else 
          return (Obj)NULL;
      }
      else {
        temp_2 = fopen((char *)filename,(char *)(((Str *)(&str_const_6))->body));   /* "ab" */
        return (temp_2==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_2,temp_2,
            (char *)filename,(char *)(((Str *)(&str_const_6))->body),   /* "ab" */
            -1,16);
      }
    }
    else if (if_exists==(Obj)(tl_input_symbols+2)) {    /* OVERWRITE */
      temp_3 = fopen((char *)filename,(char *)(((Str *)(&str_const_13))->body));    /* "rb+" */
      file_stream_2 = ((temp_3==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_3,
          temp_3,(char *)filename,(char *)(((Str *)(&str_const_13))->body),     /* "rb+" */
          -1,16));
      if (file_stream_2!=NULL) 
        return file_stream_2;
      else {
        create_file(filename,(Obj)(&T));
        temp_4 = fopen((char *)filename,(char *)(((Str *)(&str_const_13))->body));  /* "rb+" */
        return (temp_4==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_4,temp_4,
            (char *)filename,(char *)(((Str *)(&str_const_13))->body),  /* "rb+" */
            -1,16);
      }
    }
    else if (if_exists==(Obj)(tl_input_symbols+3)) {    /* SUPERSEDE */
      temp_5 = fopen((char *)filename,(char *)(((Str *)(&str_const_14))->body));    /* "wb" */
      return (temp_5==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_5,temp_5,
          (char *)filename,(char *)(((Str *)(&str_const_14))->body),    /* "wb" */
          -1,16);
    }
    else if (if_exists==(Obj)(tl_input_symbols+4)) {    /* APPEND */
      temp_6 = fopen((char *)filename,(char *)(((Str *)(&str_const_6))->body));     /* "ab" */
      return (temp_6==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_6,temp_6,
          (char *)filename,(char *)(((Str *)(&str_const_6))->body),     /* "ab" */
          -1,16);
    }
    else 
      return error_two_args((Obj)(&str_const_15),   /* "While attempting to open file ~s for output~%~
   ..." */
          ObjStrHDR(filename),if_exists);
  }
  else if ((if_does_not_exist==(Obj)(tl_input_symbols+0)) || (if_does_not_exist     /* ERROR */
      ==(Obj)NULL)) {
    g = ObjStrHDR(filename);
    temp = fopen((char *)(((Str *)g)->body),(char *)(((Str *)(&str_const_10))->body));  /* "r" */
    file_stream_3 = ((temp==NULL) ? ((Obj)NULL) : alloc_file_strm(temp,
        temp,(char *)(((Str *)g)->body),(char *)(((Str *)(&str_const_10))->body),   /* "r" */
        -1,16));
    if (file_stream_3!=NULL) {
      temp_7 = (sint32)fclose(((File_strm *)file_stream_3)->input);
      if (0) 
        delete_named_file(((File_strm *)file_stream_3)->filename);
    }
    if (file_stream_3!=NULL) 
      file_stream = g;
    else 
      file_stream = (Obj)NULL;
    if (file_stream!=NULL) {
      if (if_exists==(Obj)(tl_input_symbols+0))     /* ERROR */
        return error_one_arg((Obj)(&str_const_12),  /* "File ~s already exists" */
            ObjStrHDR(filename));
      else if (if_exists==(Obj)NULL) 
        return (Obj)NULL;
      else if (if_exists==(Obj)(tl_input_symbols+2)) {  /* OVERWRITE */
        temp_8 = fopen((char *)filename,(char *)(((Str *)(&str_const_13))->body));  /* "rb+" */
        return (temp_8==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_8,temp_8,
            (char *)filename,(char *)(((Str *)(&str_const_13))->body),  /* "rb+" */
            -1,16);
      }
      else if (if_exists==(Obj)(tl_input_symbols+3)) {  /* SUPERSEDE */
        temp_9 = fopen((char *)filename,(char *)(((Str *)(&str_const_14))->body));  /* "wb" */
        return (temp_9==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_9,temp_9,
            (char *)filename,(char *)(((Str *)(&str_const_14))->body),  /* "wb" */
            -1,16);
      }
      else if (if_exists==(Obj)(tl_input_symbols+4)) {  /* APPEND */
        temp_10 = fopen((char *)filename,(char *)(((Str *)(&str_const_6))->body));  /* "ab" */
        return (temp_10==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_10,
            temp_10,(char *)filename,(char *)(((Str *)(&str_const_6))->body),   /* "ab" */
            -1,16);
      }
      else 
        return error_two_args((Obj)(&str_const_16),     /* "While attempting to open file ~s for output~%~
   ..." */
            ObjStrHDR(filename),if_exists);
    }
    else if (if_does_not_exist==(Obj)(tl_input_symbols+0))      /* ERROR */
      return error_one_arg((Obj)(&str_const_17),    /* "File ~s does not exist" */
          ObjStrHDR(filename));
    else 
      return (Obj)NULL;
  }
  else 
    return (Obj)NULL;
}

static const Str_5 str_const_18
  = { 7, 2, 2, "r+" };

static const Str_5 str_const_19
  = { 7, 1, 1, "w" };

/* Translated from OPEN-FOR-TEXT-OUTPUT(STRING T T) = T */

Obj open_for_text_output (unsigned char *filename, Obj if_exists, Obj if_does_not_exist)
{
  Obj file_stream, g, file_stream_1;
  FILE *temp;
  sint32 temp_1;
  FILE *temp_2;
  Obj file_stream_2;
  FILE *temp_3;
  FILE *temp_4;
  FILE *temp_5;
  FILE *temp_6;
  Obj file_stream_3;
  sint32 temp_7;
  FILE *temp_8;
  FILE *temp_9;
  FILE *temp_10;

  if (if_does_not_exist==(Obj)(tl_input_symbols+1)) {   /* CREATE */
    if ((if_exists==(Obj)(tl_input_symbols+0)) || (if_exists    /* ERROR */
        ==(Obj)NULL)) {
      g = ObjStrHDR(filename);
      temp = fopen((char *)(((Str *)g)->body),(char *)(((Str *)(&str_const_10))->body));    /* "r" */
      file_stream_1 = ((temp==NULL) ? ((Obj)NULL) : alloc_file_strm(temp,
          temp,(char *)(((Str *)g)->body),(char *)(((Str *)(&str_const_10))->body),     /* "r" */
          -1,16));
      if (file_stream_1!=NULL) {
        temp_1 = (sint32)fclose(((File_strm *)file_stream_1)->input);
        if (0) 
          delete_named_file(((File_strm *)file_stream_1)->filename);
      }
      if (file_stream_1!=NULL) 
        file_stream = g;
      else 
        file_stream = (Obj)NULL;
      if (file_stream!=NULL) {
        if (if_exists==(Obj)(tl_input_symbols+0))   /* ERROR */
          return error_one_arg((Obj)(&str_const_12),    /* "File ~s already exists" */
              ObjStrHDR(filename));
        else 
          return (Obj)NULL;
      }
      else {
        temp_2 = fopen((char *)filename,(char *)(((Str *)(&str_const_7))->body));   /* "a" */
        return (temp_2==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_2,temp_2,
            (char *)filename,(char *)(((Str *)(&str_const_7))->body),   /* "a" */
            -1,16);
      }
    }
    else if (if_exists==(Obj)(tl_input_symbols+2)) {    /* OVERWRITE */
      temp_3 = fopen((char *)filename,(char *)(((Str *)(&str_const_18))->body));    /* "r+" */
      file_stream_2 = ((temp_3==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_3,
          temp_3,(char *)filename,(char *)(((Str *)(&str_const_18))->body),     /* "r+" */
          -1,16));
      if (file_stream_2!=NULL) 
        return file_stream_2;
      else {
        create_file(filename,(Obj)NULL);
        temp_4 = fopen((char *)filename,(char *)(((Str *)(&str_const_18))->body));  /* "r+" */
        return (temp_4==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_4,temp_4,
            (char *)filename,(char *)(((Str *)(&str_const_18))->body),  /* "r+" */
            -1,16);
      }
    }
    else if (if_exists==(Obj)(tl_input_symbols+3)) {    /* SUPERSEDE */
      temp_5 = fopen((char *)filename,(char *)(((Str *)(&str_const_19))->body));    /* "w" */
      return (temp_5==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_5,temp_5,
          (char *)filename,(char *)(((Str *)(&str_const_19))->body),    /* "w" */
          -1,16);
    }
    else if (if_exists==(Obj)(tl_input_symbols+4)) {    /* APPEND */
      temp_6 = fopen((char *)filename,(char *)(((Str *)(&str_const_7))->body));     /* "a" */
      return (temp_6==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_6,temp_6,
          (char *)filename,(char *)(((Str *)(&str_const_7))->body),     /* "a" */
          -1,16);
    }
    else 
      return error_two_args((Obj)(&str_const_15),   /* "While attempting to open file ~s for output~%~
   ..." */
          ObjStrHDR(filename),if_exists);
  }
  else if ((if_does_not_exist==(Obj)(tl_input_symbols+0)) || (if_does_not_exist     /* ERROR */
      ==(Obj)NULL)) {
    g = ObjStrHDR(filename);
    temp = fopen((char *)(((Str *)g)->body),(char *)(((Str *)(&str_const_10))->body));  /* "r" */
    file_stream_3 = ((temp==NULL) ? ((Obj)NULL) : alloc_file_strm(temp,
        temp,(char *)(((Str *)g)->body),(char *)(((Str *)(&str_const_10))->body),   /* "r" */
        -1,16));
    if (file_stream_3!=NULL) {
      temp_7 = (sint32)fclose(((File_strm *)file_stream_3)->input);
      if (0) 
        delete_named_file(((File_strm *)file_stream_3)->filename);
    }
    if (file_stream_3!=NULL) 
      file_stream = g;
    else 
      file_stream = (Obj)NULL;
    if (file_stream!=NULL) {
      if (if_exists==(Obj)(tl_input_symbols+0))     /* ERROR */
        return error_one_arg((Obj)(&str_const_12),  /* "File ~s already exists" */
            ObjStrHDR(filename));
      else if (if_exists==(Obj)NULL) 
        return (Obj)NULL;
      else if (if_exists==(Obj)(tl_input_symbols+2)) {  /* OVERWRITE */
        temp_8 = fopen((char *)filename,(char *)(((Str *)(&str_const_18))->body));  /* "r+" */
        return (temp_8==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_8,temp_8,
            (char *)filename,(char *)(((Str *)(&str_const_18))->body),  /* "r+" */
            -1,16);
      }
      else if (if_exists==(Obj)(tl_input_symbols+3)) {  /* SUPERSEDE */
        temp_9 = fopen((char *)filename,(char *)(((Str *)(&str_const_19))->body));  /* "w" */
        return (temp_9==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_9,temp_9,
            (char *)filename,(char *)(((Str *)(&str_const_19))->body),  /* "w" */
            -1,16);
      }
      else if (if_exists==(Obj)(tl_input_symbols+4)) {  /* APPEND */
        temp_10 = fopen((char *)filename,(char *)(((Str *)(&str_const_7))->body));  /* "a" */
        return (temp_10==NULL) ? ((Obj)NULL) : alloc_file_strm(temp_10,
            temp_10,(char *)filename,(char *)(((Str *)(&str_const_7))->body),   /* "a" */
            -1,16);
      }
      else 
        return error_two_args((Obj)(&str_const_16),     /* "While attempting to open file ~s for output~%~
   ..." */
            ObjStrHDR(filename),if_exists);
    }
    else if (if_does_not_exist==(Obj)(tl_input_symbols+0))      /* ERROR */
      return error_one_arg((Obj)(&str_const_17),    /* "File ~s does not exist" */
          ObjStrHDR(filename));
    else 
      return (Obj)NULL;
  }
  else 
    return (Obj)NULL;
}

static const Str_105 str_const_20
  = { 7, 104, 104, "Stream ~s was neither a file-stream ~\n                              nor a string-stream in generic-close" };

/* Translated from GENERIC-CLOSE(T T) = T */

Obj generic_close (Obj stream, Obj abort_1)
{
  sint32 temp, temp_1;

  switch (TYPE_TAG(stream,temp)) {
   case 0:
    break;
   case 16:
    if (!((stream==NULL) || (stream==GET_GLOBAL(Sterminal_ioS)))) {
      temp_1 = (sint32)fclose(((File_strm *)stream)->input);
      if (abort_1!=NULL) 
        delete_named_file(((File_strm *)stream)->filename);
    }
    break;
   case 15:
    break;
   default:
    error_one_arg((Obj)(&str_const_20),         /* "Stream ~s was neither a file-stream ~
            ..." */
        stream);
    break;
  }
  return (Obj)(&T);
}

/* Translated from READ-FROM-STRING-1(T T T T T) = (VALUES T FIXNUM) */

Obj read_from_string_1 (Obj string, Obj eof_error_p, Obj eof_value, Obj start, 
        Obj end)
{
  Obj temp;

  (void)string;                                 /* STRING was declared ignore */
  (void)eof_error_p;                            /* EOF-ERROR-P was declared ignore */
  (void)start;                                  /* START was declared ignore */
  (void)end;                                    /* END was declared ignore */
  temp = eof_value;
  (Values_buffer[0]) = BOXFIX(0);
  Values_count = 2;
  return temp;
}

static const Str_9 str_const_21
  = { 7, 7, 7, "KEYWORD" };

static const Str_9 str_const_22
  = { 7, 5, 5, "ERROR" };

static const Str_9 str_const_23
  = { 7, 6, 6, "CREATE" };

static const Str_13 str_const_24
  = { 7, 9, 9, "OVERWRITE" };

static const Str_13 str_const_25
  = { 7, 9, 9, "SUPERSEDE" };

static const Str_9 str_const_26
  = { 7, 6, 6, "APPEND" };

Sym tl_input_symbols[5];

/* Translated from SYMS-TL-INPUT() = VOID */

void syms_tl_input (void)
{
  Obj cached_keyword_package;

  cached_keyword_package = find_package_1((Obj)(&str_const_21));    /* "KEYWORD" */
  init_symbol_into_package((Obj)(&(tl_input_symbols[0])),(Obj)(&str_const_22),  /* "ERROR" */
      1860,cached_keyword_package);
  (tl_input_symbols[0]).external = 1;
  (tl_input_symbols[0]).symbol_value = (Obj)(&(tl_input_symbols[0]));
  init_symbol_into_package((Obj)(&(tl_input_symbols[1])),(Obj)(&str_const_23),  /* "CREATE" */
      3713,cached_keyword_package);
  (tl_input_symbols[1]).external = 1;
  (tl_input_symbols[1]).symbol_value = (Obj)(&(tl_input_symbols[1]));
  init_symbol_into_package((Obj)(&(tl_input_symbols[2])),(Obj)(&str_const_24),  /* "OVERWRITE" */
      31017,cached_keyword_package);
  (tl_input_symbols[2]).external = 1;
  (tl_input_symbols[2]).symbol_value = (Obj)(&(tl_input_symbols[2]));
  init_symbol_into_package((Obj)(&(tl_input_symbols[3])),(Obj)(&str_const_25),  /* "SUPERSEDE" */
      25409,cached_keyword_package);
  (tl_input_symbols[3]).external = 1;
  (tl_input_symbols[3]).symbol_value = (Obj)(&(tl_input_symbols[3]));
  init_symbol_into_package((Obj)(&(tl_input_symbols[4])),(Obj)(&str_const_26),  /* "APPEND" */
      3692,cached_keyword_package);
  (tl_input_symbols[4]).external = 1;
  (tl_input_symbols[4]).symbol_value = (Obj)(&(tl_input_symbols[4]));
  return;
}


/* Translated from INIT-TL-INPUT() = VOID */

void init_tl_input (void)
{
  unsigned char *g;

  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  g = (((Str *)alloc_string(16384,0,7))->body);
  memset((void *)(g+0),'\000',(sint32)(StrHDR(g)->fill_length));
  (void)g;
  Sinput_string_bufferS = ObjStrHDR(g);
  Sinput_string_buffer_sizeS = BOXFIX(16383);
  return;
}

