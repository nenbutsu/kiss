/*  -*- coding: utf-8 -*-
  stream.c --- defines the stream mechanism of ISLisp processor KISS.

  Copyright (C) 2017, 2018, 2019 Yuji Minejima <yuji@minejima.jp>

  This file is part of ISLisp processor KISS.

  KISS is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  KISS is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

*/
#include "kiss.h"

kiss_file_stream_t Kiss_Standard_Input;
kiss_file_stream_t Kiss_Standard_Output;
kiss_file_stream_t Kiss_Error_Output;

void kiss_init_streams(void) {
     Kiss_Standard_Input.type      = KISS_STREAM;
     Kiss_Standard_Input.gc_ptr    = NULL;
     Kiss_Standard_Input.flags     = KISS_INPUT_STREAM | KISS_CHARACTER_STREAM | KISS_FILE_STREAM;
     Kiss_Standard_Input.file_ptr  = stdin;
     Kiss_Standard_Input.column    = 0;

     Kiss_Standard_Output.type     = KISS_STREAM;
     Kiss_Standard_Output.gc_ptr   = NULL;
     Kiss_Standard_Output.flags    = KISS_OUTPUT_STREAM | KISS_CHARACTER_STREAM | KISS_FILE_STREAM;
     Kiss_Standard_Output.file_ptr = stdout;
     Kiss_Standard_Output.column   = 0;
    
    
     Kiss_Error_Output.type        = KISS_STREAM;
     Kiss_Error_Output.gc_ptr      = NULL;
     Kiss_Error_Output.flags       = KISS_OUTPUT_STREAM | KISS_CHARACTER_STREAM | KISS_FILE_STREAM;
     Kiss_Error_Output.file_ptr    = stderr;
     Kiss_Error_Output.column      = 0;
}

static kiss_file_stream_t* kiss_make_file_stream(FILE* fp) {
     kiss_file_stream_t* p = Kiss_GC_Malloc(sizeof(kiss_file_stream_t));
     p->type = KISS_STREAM;
     p->flags = KISS_FILE_STREAM;
     p->file_ptr = fp;
     p->column = 0;
     p->pos =0;
     return p;
}

static kiss_string_stream_t* kiss_make_string_stream(kiss_string_t* str) {
     kiss_string_stream_t* p = Kiss_GC_Malloc(sizeof(kiss_string_stream_t));
     p->type = KISS_STREAM;
     p->flags = KISS_STRING_STREAM;
     p->list = KISS_NIL; // in case of gc
     p->list = kiss_str_to_chars(str); // might gc
     return p;
}

/* function: (open-stream-p obj) -> boolean
   Returns t if obj is an open stream; otherwise, returns nil. */
kiss_obj* kiss_open_stream_p(kiss_obj* obj) {
     if (!KISS_IS_STREAM(obj)) { return KISS_NIL; }
     if (KISS_IS_STRING_STREAM(obj)) { return KISS_T; }
     assert(KISS_IS_FILE_STREAM(obj));
     return ((kiss_file_stream_t*)obj)->file_ptr ? KISS_T : KISS_NIL;
}

/*function: (close stream) -> implementation defined
  The function close closes the stream STREAM. If STREAM is closed it may no longer
  be used in input or output operations. Closing a file stream ends the association
  between the stream and its file. If the STREAM was already closed this function
  performs nothing. The result value is implementation defined.
  An error shall be signaled if STREAM is not a stream (error-id. domain-error). */
kiss_obj* kiss_close(kiss_obj* obj) {
     Kiss_Stream(obj);
     if (KISS_IS_STRING_STREAM(obj)) {
	  return KISS_T;
     }
     assert(KISS_IS_FILE_STREAM(obj));
     kiss_file_stream_t* fs = (kiss_file_stream_t*)obj;
     if (fs->file_ptr) {
	  if (fclose(fs->file_ptr) == EOF) {
	       Kiss_System_Error();
	  }
	  fs->file_ptr = NULL;
     }
     return KISS_T;
}

static FILE * kiss_fopen(const kiss_obj* const filename, const char* const opentype) {
     char* name = kiss_wcstombs(Kiss_String(filename)->str);
     FILE* fp = fopen(name, opentype);
     free(name);
     return fp;
}

static int kiss_is_character_class(kiss_obj* obj) {
     if (KISS_IS_SYMBOL(obj) && wcscmp(((kiss_symbol_t*)obj)->name, L"<character>") == 0) {
          return 1;
     }
     if (!KISS_IS_ILOS_OBJ(obj)) { return 0; }
     kiss_obj* class = kiss_c_funcall(L"kiss::class", kiss_c_list(1, kiss_symbol(L"<character>")));
     return obj == class ? 1 : 0;
}

/* function: (open-input-file filename [element-class]) -> <stream>
   Opens a file for input only.
   An error shall be signaled if FILENAME is not a string. 
   The corresponding file is opened in an implementation-defined way.
   These functions return an instance of the <stream> class connected to the file
   specified by FILENAME.
   The ELEMENT-CLASS can be either the class <character> (the default) or a positive integer
   that is a number of bits in a byte to be used for a binary stream.
   All implementations must support a value of 8 (denoting integer byte values from 0 to 255),
   but some implementations might support other byte sizes as well. */
kiss_obj* kiss_open_input_file(const kiss_obj* const filename, const kiss_obj* const rest) {
     FILE* fp = kiss_fopen(filename, "r");
     if (fp == NULL) { Kiss_System_Error(); }
     else {
	  kiss_file_stream_t* stream = kiss_make_file_stream(fp);

          fwide(fp, 1); // wide oriented
	  if (rest == KISS_NIL || kiss_is_character_class(kiss_car(rest))) {
	       stream->flags |= (KISS_INPUT_STREAM | KISS_CHARACTER_STREAM);
	  } else if (kiss_num_eq(kiss_car(rest), kiss_make_fixnum(8)) == KISS_T) {
	       stream->flags |= (KISS_INPUT_STREAM | KISS_BYTE_STREAM);
	  } else {
               kiss_close((kiss_obj*)stream);
	       Kiss_Err(L"Invalid stream element-class ~S", kiss_car(rest));
	  }
	  return (kiss_obj*)stream;
     }
}

/* function: (open-output-file filename [element-class]) -> <stream>
   Opens a file for output only.
   An error shall be signaled if FILENAME is not a string. 
   The corresponding file is opened in an implementation-defined way.
   These functions return an instance of the <stream> class connected to the file
   specified by FILENAME.
   The ELEMENT-CLASS can be either the class <character> (the default) or a positive integer
   that is a number of bits in a byte to be used for a binary stream.
   All implementations must support a value of 8 (denoting integer byte values from 0 to 255),
   but some implementations might support other byte sizes as well. */
kiss_obj* kiss_open_output_file(kiss_obj* filename, kiss_obj* rest) {
     FILE* fp = kiss_fopen(filename, "w");
     if (fp == NULL) { Kiss_System_Error(); }
     else {
	  kiss_file_stream_t* stream = kiss_make_file_stream(fp);

          fwide(fp, 1); // wide oriented
	  if (rest == KISS_NIL || kiss_is_character_class(kiss_car(rest))) {
	       stream->flags |= (KISS_OUTPUT_STREAM | KISS_CHARACTER_STREAM);
	  } else if (kiss_num_eq(kiss_car(rest), kiss_make_fixnum(8)) == KISS_T) {
	       stream->flags |= (KISS_OUTPUT_STREAM | KISS_BYTE_STREAM);
	  } else {
               kiss_close((kiss_obj*)stream);
	       Kiss_Err(L"Invalid stream element-class ~S", kiss_car(rest));
	  }
	  return (kiss_obj*)stream;
     }
}

/* function: (open-io-file filename [element-class]) -> <stream>
   Opens a file for both input and output.
   An error shall be signaled if FILENAME is not a string. 
   The corresponding file is opened in an implementation-defined way.
   These functions return an instance of the <stream> class connected to the file
   specified by FILENAME.
   The ELEMENT-CLASS can be either the class <character> (the default) or a positive integer
   that is a number of bits in a byte to be used for a binary stream.
   All implementations must support a value of 8 (denoting integer byte values from 0 to 255),
   but some implementations might support other byte sizes as well. */
kiss_obj* kiss_open_io_file(kiss_obj* filename, kiss_obj* rest) {
     FILE* fp = kiss_fopen(filename, "r+");
     if (fp == NULL) { Kiss_System_Error(); }
     else {
	  kiss_file_stream_t* stream = kiss_make_file_stream(fp);

          fwide(fp, 1); // wide oriented
	  if (rest == KISS_NIL || kiss_is_character_class(kiss_car(rest))) {
	       stream->flags |= (KISS_OUTPUT_STREAM | KISS_INPUT_STREAM | KISS_CHARACTER_STREAM);
	  } else if (kiss_num_eq(kiss_car(rest), kiss_make_fixnum(8)) == KISS_T) {
	       stream->flags |= (KISS_OUTPUT_STREAM | KISS_INPUT_STREAM | KISS_BYTE_STREAM);
	  } else {
               kiss_close((kiss_obj*)stream);
	       Kiss_Err(L"Invalid stream element-class ~S", kiss_car(rest));
	  }
	  return (kiss_obj*)stream;
     }
}

/* function: (finish-output stream) -> <null>
   Completes any pending output to the destination designated by STREAM.
   Waits until the pending output is complete and then returns nil.
   For instance, pending output might be stored in a buffer; in this case
   finish-output forces the buffer to be written to the STREAM's destination.
   An error shall be signaled if STREAM is not a stream that can handle output operations
   (error-id. domain-error). */
kiss_obj* kiss_finish_output (kiss_obj* obj) {
     if (!KISS_IS_OUTPUT_STREAM(Kiss_Stream(obj))) {
	  Kiss_Err(L"output stream expected ~S", obj);
     }
     if (KISS_IS_FILE_STREAM(obj)) {
	  kiss_file_stream_t* stream = Kiss_Open_File_Stream(obj);
	  if (fflush(stream->file_ptr) == EOF) {
	       Kiss_System_Error();
	  }
     }
     return KISS_NIL;
}


/* function: (create-string-input-stream string) -> <stream>
   Creates and returns an input stream from the STRING. An error shall be
   signaled if STRING is not a string (error-id. domain-error ). */
kiss_obj* kiss_create_string_input_stream(kiss_obj* string) {
     kiss_string_t* str = Kiss_String(string);
     kiss_string_stream_t* p = kiss_make_string_stream(str);
     p->flags |= (KISS_INPUT_STREAM | KISS_CHARACTER_STREAM);
     return (kiss_obj*)p;
}

/* function: (create-string-output-stream) -> <stream>
   Creates and returns a string output stream. The output
   to a string stream can be retrieved by get-output-stream-string. */
kiss_obj* kiss_create_string_output_stream(void) {
     kiss_string_t* str = kiss_make_string(L"");
     kiss_string_stream_t* p = kiss_make_string_stream(str);
     p->flags |= (KISS_OUTPUT_STREAM | KISS_CHARACTER_STREAM);
     return (kiss_obj*)p;
}

/* function: (get-output-stream-string stream) -> <string> 
   Returns a string containing all characters written to STREAM since the
   last call to this function or since the creation of the STREAM, if
   this function has not been called with STREAM before. An error shall
   be signaled if STREAM is not a stream created with
   create-string-output-stream (error-id. domain-error ).*/
kiss_obj* kiss_get_output_stream_string(kiss_obj* stream) {
     kiss_string_stream_t* string_stream = Kiss_String_Output_Stream(stream);
     kiss_string_t* string = kiss_chars_to_str(kiss_reverse(string_stream->list));
     string_stream->list = KISS_NIL;
     return (kiss_obj*)string;
}

kiss_obj* kiss_c_read_char(const kiss_obj* const in, const kiss_obj* const eos_err_p, const kiss_obj* const eos_val) {
     if (KISS_IS_FILE_STREAM(Kiss_Input_Char_Stream(in))) {
          kiss_file_stream_t* file_stream = (kiss_file_stream_t*)in;
          FILE* fp = file_stream->file_ptr;
          wint_t c = getwc(fp);
          if (c == WEOF) goto eos;
          return kiss_make_char(c);
     } else if (KISS_IS_STRING_STREAM(in)) {
	  kiss_string_stream_t* string_stream = (kiss_string_stream_t*)in;
	  if (string_stream->list == KISS_NIL) {
	       goto eos;
	  } else {
	       kiss_obj* c = KISS_CAR(string_stream->list);
	       string_stream->list = KISS_CDR(string_stream->list);
	       return c;
	  }
     } else {
	  fwprintf(stderr, L"kiss_c_read_char: unknown input stream type = %d", KISS_OBJ_TYPE(in));
	  exit(EXIT_FAILURE);
     }
eos:
     if (eos_err_p != KISS_NIL) {
	  Kiss_End_Of_Stream_Error(in);
     }
     return (kiss_obj*)eos_val;
}

/* function: (read-char [input-stream [eos-error-p [eos-value]]]) -> <object>
   Reads a single character from INPUT-STREAM and returns the corresponding
   character object. */
kiss_obj* kiss_read_char(const kiss_obj* args) {
     kiss_obj* in = kiss_standard_input();
     kiss_obj* eos_err_p = KISS_T;
     kiss_obj* eos_val = KISS_NIL;
     if (KISS_IS_CONS(args)) {
	  in = KISS_CAR(args);
	  args = KISS_CDR(args);
	  if (KISS_IS_CONS(args)) {
	       eos_err_p = KISS_CAR(args);
	       args = KISS_CDR(args);
	       if (KISS_IS_CONS(args)) {
		    eos_val = KISS_CAR(args);
	       }
	  }
     }
     return kiss_c_read_char(in, eos_err_p, eos_val);
}

/* Reads a line of characters from input-stream and returns them as a string
   (without the newline character at the end of the line).
   If an end-of-stream is reached before the next newline character
   and a non-empty line has been read prior to the end-of-stream, that line is returned. */
kiss_obj* kiss_c_read_line(kiss_obj* in, kiss_obj* eos_err_p, kiss_obj* eos_val) {
     kiss_obj* p = KISS_NIL;
     kiss_obj* c = kiss_c_read_char(in, eos_err_p, KISS_NIL);
     if (c == KISS_NIL) { return eos_val; }
     while (c != KISS_NIL) {
	  if (kiss_C_wchar_t(c) == L'\n') {
	       break;
	  }
	  kiss_push(c, &p);
	  c = kiss_c_read_char(in, KISS_NIL, KISS_NIL);
     }
     return (kiss_obj*)kiss_chars_to_str(kiss_nreverse(p));
}


/* (read-line [input-stream [eos-error-p [eos-value]]]) -> <object> */
kiss_obj* kiss_read_line(kiss_obj* args) {
     kiss_obj* in = kiss_standard_input();
     kiss_obj* eos_err_p = KISS_T;
     kiss_obj* eos_val = KISS_NIL;
     if (KISS_IS_CONS(args)) {
	  in = KISS_CAR(args);
	  args = KISS_CDR(args);
	  if (KISS_IS_CONS(args)) {
	       eos_err_p = KISS_CAR(args);
	       args = KISS_CDR(args);
	       if (KISS_IS_CONS(args)) {
		    eos_val = KISS_CAR(args);
	       }
	  }
     }
     return kiss_c_read_line(in, eos_err_p, eos_val);
}


kiss_obj* kiss_c_preview_char(const kiss_obj* const in, const kiss_obj* const eos_err_p, const kiss_obj* const eos_val) {
     if (KISS_IS_FILE_STREAM(Kiss_Input_Char_Stream(in))) {
          kiss_file_stream_t* file_stream = Kiss_Open_File_Stream(in);
          FILE* fp = file_stream->file_ptr;
          wint_t c = getwc(fp);
          if (c == WEOF) goto eos;
          ungetwc(c, fp);
          return kiss_make_char(c);
     } else if (KISS_IS_STRING_STREAM(in)) {
	  kiss_string_stream_t* string_stream = (kiss_string_stream_t*)in;
	  if (string_stream->list == KISS_NIL) {
	       goto eos;
	  } else {
	       kiss_obj* c = KISS_CAR(string_stream->list);
	       return c;
	  }
     } else {
	  fwprintf(stderr, L"kiss_c_preview_char: unknown input stream type = %d", KISS_OBJ_TYPE(in));
	  exit(EXIT_FAILURE);

     }
eos:
     if (eos_err_p != KISS_NIL) {
	  Kiss_End_Of_Stream_Error(in);
     }
     return (kiss_obj*)eos_val;
}

/* function: (preview-char [input-stream [eos-error-p [eos-value]]]) -> <object>
   Returns the next character of input-stream, if any.
   The character is not consumed; the next attempt to peek at or read a character from
   the stream sees that same character.
*/
kiss_obj* kiss_preview_char(kiss_obj* args) {
     kiss_obj* in = kiss_standard_input();
     kiss_obj* eos_err_p = KISS_T;
     kiss_obj* eos_val = KISS_NIL;
     if (KISS_IS_CONS(args)) {
	  in = KISS_CAR(args);
	  args = KISS_CDR(args);
	  if (KISS_IS_CONS(args)) {
	       eos_err_p = KISS_CAR(args);
	       args = KISS_CDR(args);
	       if (KISS_IS_CONS(args)) {
		    eos_val = KISS_CAR(args);
	       }
	  }
     }
     return kiss_c_preview_char(in, eos_err_p, eos_val);
}

static size_t kiss_next_column(size_t column, size_t width) {
     return (column / width) * width + width;
}

/* function: (format-char output-stream char) -> <null> */
kiss_obj* kiss_format_char(kiss_obj* output, kiss_obj* character) {
     wchar_t c = Kiss_Character(character);
     if (KISS_IS_FILE_STREAM(Kiss_Output_Char_Stream(output))) {
	  kiss_file_stream_t* out = Kiss_Open_File_Stream(output);
	  FILE* fp = out->file_ptr;
	  if (c == L'\n') {
	       out->column = 0;
	  } else if (c == L'\t'){
	       size_t column = out->column;
	       size_t width = Kiss_Fixnum(kiss_dynamic(kiss_symbol(L"*tab-width*")));
	       out->column = kiss_next_column(column, width);
	  }
	  if (putwc(c, fp) == WEOF) {
	       Kiss_System_Error();
	  } else {
	       out->column++;
	       out->pos++;
	  }
     } else if (KISS_IS_STRING_STREAM(output)) {
	  kiss_string_stream_t* out = (kiss_string_stream_t*)output;
	  if (c == L'\n') {
	       out->column = 0;
	  } else if (c == L'\t'){
	       size_t column = out->column;
	       size_t width = Kiss_Fixnum(kiss_dynamic(kiss_symbol(L"*tab-width*")));	
       out->column = kiss_next_column(column, width);
	  } else {
	       out->column += 1;
	  }
	  kiss_push(character, &(out->list));
     } else {
	  fwprintf(stderr, L"kiss_format_char: unknown stream type = %d", KISS_OBJ_TYPE(output));
	  exit(EXIT_FAILURE);
     }
     return KISS_NIL;
}

/* function: (stream-ready-p input-stream) -> boolean
   Returns t if an attempt to obtain the next element from the stream will not cause the
   processor to have to wait; otherwise, returns nil.
   An error shall be signaled if stream is not a stream that can handle input operations
   (error-id. domain-error). */
kiss_obj* kiss_stream_ready_p(kiss_obj* obj) {
     Kiss_Stream(obj);
     if (!KISS_IS_INPUT_STREAM(obj)) {
	  Kiss_Err(L"input stream expected ~S", obj);
     }
     if (KISS_IS_FILE_STREAM(obj)) {
	  kiss_file_stream_t* f = (kiss_file_stream_t*)obj;
	  if (f->file_ptr == NULL) {
	       Kiss_Err(L"file stream is closed ~S", obj);
	  }
	  if (isatty(fileno(f->file_ptr))) {
	       return KISS_NIL;
	  } else {
	       return KISS_T;
	  }
	  
     } else {
	  return KISS_T;
     }
}

kiss_obj* kiss_c_read_byte(kiss_obj* in, kiss_obj* eos_err_p, kiss_obj* eos_val) {
     if (KISS_IS_FILE_STREAM(Kiss_Input_Byte_Stream(in))) {
	  FILE* fp = Kiss_Open_File_Stream(in)->file_ptr;
	  int c = fgetc(fp);
	  if (c == EOF) {
	       if (ferror(fp)) { Kiss_System_Error(); }
	       goto eos;
	  } else {
	       ((kiss_file_stream_t*)in)->pos++;
	       return (kiss_obj*)kiss_make_fixnum(c);
	  }
     } else {
	  fwprintf(stderr, L"kiss_c_read_byte: unknown input stream type = %d", KISS_OBJ_TYPE(in));
	  exit(EXIT_FAILURE);
     }
eos:
     if (eos_err_p != KISS_NIL) {
	  Kiss_End_Of_Stream_Error(in);
     }
     return eos_val;
}

/* function: (read-byte input-stream [eos-error-p [eos-value]]) -> <integer>
   Reads a byte from the input-stream and returns it.
   The number of bits in a byte is determined by the stream element type of the
   input-stream ;see open-input-file. */
kiss_obj* kiss_read_byte(kiss_obj* in, kiss_obj* args) {
     kiss_obj* eos_err_p = KISS_T;
     kiss_obj* eos_val = KISS_NIL;
     if (KISS_IS_CONS(args)) {
	  eos_err_p = KISS_CAR(args);
	  args = KISS_CDR(args);
	  if (KISS_IS_CONS(args)) {
	       eos_val = KISS_CAR(args);
	  }
     }
     return kiss_c_read_byte(in, eos_err_p, eos_val);
}

/* (write-byte z output-stream) -> <integer>
   Writes z to the output-stream and returns it.
   An error shall be signaled if z is not an integer in the range appropriate to
   the stream element type of output-stream or if output-stream is not a stream
   capable of handling output operations (error-id. domain-error). */
kiss_obj* kiss_write_byte(kiss_obj* z, kiss_obj* output) {
     if (KISS_IS_FILE_STREAM(Kiss_Output_Byte_Stream(output))) {
	  FILE* fp = Kiss_Open_File_Stream(output)->file_ptr;
	  kiss_C_integer i = Kiss_Fixnum(z);
	  if (i > CHAR_MAX || i < CHAR_MIN) {
	       Kiss_Err(L"out of 8-bit integer range ~S", z);
	  }
	  if (fputc(i, fp) == EOF) {
	       Kiss_System_Error();
	  } else {
	       ((kiss_file_stream_t*)output)->pos++;
	  }
	  return z;
     } else {
	  fwprintf(stderr, L"kiss_write_byte: unknown output stream type = %d", KISS_OBJ_TYPE(output));
	  exit(EXIT_FAILURE);
     }
}

kiss_obj* kiss_load(const kiss_obj* const filename) {
     kiss_obj* in = kiss_open_input_file(filename, KISS_NIL);
     kiss_obj* form = kiss_c_read(in, KISS_NIL, KISS_EOS);
     while (form != KISS_EOS) {
	  kiss_eval(form);
	  form = kiss_c_read(in, KISS_NIL, KISS_EOS);
     }
     return KISS_T;
}
