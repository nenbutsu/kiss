/*  -*- coding: utf-8 -*-
  stream.c --- defines the stream mechanism of ISLisp processor KISS.

  Copyright (C) 2017 Yuji Minejima.

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
     Kiss_Standard_Input.gc_flag   = 0;
     Kiss_Standard_Input.gc_next   = NULL;
     Kiss_Standard_Input.flags     = KISS_INPUT_STREAM | KISS_CHARACTER_STREAM | KISS_FILE_STREAM;
     Kiss_Standard_Input.file_ptr  = stdin;
     Kiss_Standard_Input.column    = 0;

     Kiss_Standard_Output.type     = KISS_STREAM;
     Kiss_Standard_Output.gc_flag   = 0;
     Kiss_Standard_Output.gc_next   = NULL;
     Kiss_Standard_Output.flags    = KISS_OUTPUT_STREAM | KISS_CHARACTER_STREAM | KISS_FILE_STREAM;
     Kiss_Standard_Output.file_ptr = stdout;
     Kiss_Standard_Output.column   = 0;
    
    
     Kiss_Error_Output.type        = KISS_STREAM;
     Kiss_Error_Output.gc_flag     = 0;
     Kiss_Error_Output.gc_next     = NULL;
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
     return p;
}

static kiss_string_stream_t* kiss_make_string_stream(kiss_string_t* str) {
     kiss_string_stream_t* p = Kiss_GC_Malloc(sizeof(kiss_string_stream_t));
     p->type = KISS_STREAM;
     p->flags = KISS_STRING_STREAM;
     p->list = KISS_NIL;
     p->list = kiss_str_to_chars(str); // might gc
     return p;
}

/* function: (streamp obj ) -> boolean
   Returns t if obj is a stream (instance of class <stream>); otherwise,
   returns nil. obj may be any ISLISP object. streamp is unaffected by
   whether its argument, if an instance of the class <stream>, is open or
   closed.
   Example: (streamp (standard-input)) => t
            (streamp '()) => nil */
kiss_obj* kiss_streamp(kiss_obj* obj) {
     if (KISS_IS_STREAM(obj)) { return KISS_T; }
     else                     { return KISS_NIL; }
}

/* function: (open-stream-p obj) -> boolean
   Returns t if obj is an open stream; otherwise, returns nil. */
kiss_obj* kiss_open_stream_p(kiss_obj* obj) {
     if (!KISS_IS_STREAM(obj)) { return KISS_NIL; }
     if (KISS_IS_STRING_STREAM(obj)) { return KISS_T; }
     assert(KISS_IS_FILE_STREAM(obj));

     kiss_file_stream_t* f = (kiss_file_stream_t*)obj;
     if (f->file_ptr) {
	  return KISS_T;
     } else {
	  return KISS_NIL;
     }
}

/* function: (input-stream-p obj) -> boolean
   Returns t if obj is a stream that can handle input operations;
   otherwise, returns nil. */
kiss_obj* kiss_input_stream_p(kiss_obj* p) {
     if (KISS_IS_INPUT_STREAM(p)) { return KISS_T; }
     else                         { return KISS_NIL; }
}

/* function: (output-stream-p obj ) -> boolean
   Returns t if obj is a stream that can handle output operations;
   otherwise, returns nil. */
kiss_obj* kiss_output_stream_p(kiss_obj* p) {
     if (KISS_IS_OUTPUT_STREAM(p)) { return KISS_T; }
     else                          { return KISS_NIL; }
}


/* function: (standard-input) -> <stream> */
kiss_obj* kiss_standard_input(void)  {
     return kiss_dynamic(kiss_symbol(L"*kiss::standard-input*"));
}

/* function: (standard-output) -> <stream> */
kiss_obj* kiss_standard_output(void) {
     return kiss_dynamic(kiss_symbol(L"*kiss::standard-output*"));
}

/* function: (error-output) -> <stream> */
kiss_obj* kiss_error_output(void)    {
     return kiss_dynamic(kiss_symbol(L"*kiss::error-output*"));
}

/*function: (close stream) -> implementation defined
  The function close closes the stream stream. If stream is closed it may no longer
  be used in input or output operations. Closing a file stream ends the association
  between the stream and its file. If the stream was already closed this function
  performs nothing. The result value is implementation defined.
  An error shall be signaled if stream is not a stream (error-id. domain-error).
*/
kiss_obj* kiss_close(kiss_obj* obj) {
     Kiss_Stream(obj);
     if (KISS_IS_STRING_STREAM(obj)) {
	  return KISS_T;
     }
     assert(KISS_IS_FILE_STREAM(obj));
     kiss_file_stream_t* file_stream = (kiss_file_stream_t*)obj;
     if (file_stream->file_ptr) {
	  int result = fclose(file_stream->file_ptr);
	  if (result == EOF) {
	       Kiss_System_Error();
	  }
	  file_stream->file_ptr = NULL;
     }
     return KISS_T;
}

char* kiss_wcstombs(const wchar_t* src) {
     mbstate_t state;
     memset (&state, '\0', sizeof (state));
     size_t len = sizeof(char) * MB_LEN_MAX * wcslen(src) + 1;
     char* str = Kiss_Malloc(len);
     size_t result = wcsrtombs(str, &src, len, &state);
     if (result == -1) {
	  Kiss_System_Error();
     }
     return str;
}


/* function: (open-input-file filename [element-class]) -> <stream>
   open-input-file opens a file for input only.
   An error shall be signaled if filename is not a string. 
   The corresponding file is opened in an implementation-defined way.
   These functions return an instance of the <stream> class connected to the file
   specified by filename.
   The element-class can be either the class <character> (the default) or a positive integer
   that is a number of bits in a byte to be used for a binary stream.
   All implementations must support a value of 8 (denoting integer byte values from 0 to 255),
   but some implementations might support other byte sizes as well.
*/
kiss_obj* kiss_open_input_file(kiss_obj* filename, kiss_obj* rest) {
     char* name = kiss_wcstombs(Kiss_String(filename)->str);

     FILE* fp = fopen(name, "r");
     free(name);
     if (fp == NULL) { Kiss_System_Error(); }
     else {
	  kiss_file_stream_t* stream = kiss_make_file_stream(fp);

	  if (rest == KISS_NIL) {
	       stream->flags |= (KISS_INPUT_STREAM | KISS_CHARACTER_STREAM);
	  } else if (Kiss_Integer(kiss_car(rest))->i == 8) {
	       stream->flags |= (KISS_INPUT_STREAM | KISS_BYTE_STREAM);
	  } else {
	       Kiss_Err(L"only 8 bit-binary-element-class is supported ~S", kiss_car(rest));
	  }
	  return (kiss_obj*)stream;
     }
}

/* function: (open-output-file filename [element-class]) -> <stream>
   open-output-file opens a file for output only.
   An error shall be signaled if filename is not a string. 
   The corresponding file is opened in an implementation-defined way.
   These functions return an instance of the <stream> class connected to the file
   specified by filename.
   The element-class can be either the class <character> (the default) or a positive integer
   that is a number of bits in a byte to be used for a binary stream.
   All implementations must support a value of 8 (denoting integer byte values from 0 to 255),
   but some implementations might support other byte sizes as well.
*/
kiss_obj* kiss_open_output_file(kiss_obj* filename, kiss_obj* rest) {
     char* name = kiss_wcstombs(Kiss_String(filename)->str);

     FILE* fp = fopen(name, "w");
     free(name);
     if (fp == NULL) { Kiss_System_Error(); }
     else {
	  kiss_file_stream_t* stream = kiss_make_file_stream(fp);

	  if (rest == KISS_NIL) {
	       stream->flags |= (KISS_OUTPUT_STREAM | KISS_CHARACTER_STREAM);
	  } else if (Kiss_Integer(kiss_car(rest))->i == 8) {
	       stream->flags |= (KISS_OUTPUT_STREAM | KISS_BYTE_STREAM);
	  } else {
	       Kiss_Err(L"only 8 bit-binary-element-class is supported ~S", kiss_car(rest));
	  }
	  return (kiss_obj*)stream;
     }
}

/* function: (open-io-file filename [element-class]) -> <stream>
   open-io-file opens a file for both input and output.
   An error shall be signaled if filename is not a string. 
   The corresponding file is opened in an implementation-defined way.
   These functions return an instance of the <stream> class connected to the file
   specified by filename.
   The element-class can be either the class <character> (the default) or a positive integer
   that is a number of bits in a byte to be used for a binary stream.
   All implementations must support a value of 8 (denoting integer byte values from 0 to 255),
   but some implementations might support other byte sizes as well.
*/
kiss_obj* kiss_open_io_file(kiss_obj* filename, kiss_obj* rest) {
     char* name = kiss_wcstombs(Kiss_String(filename)->str);

     FILE* fp = fopen(name, "r+");
     free(name);
     if (fp == NULL) { Kiss_System_Error(); }
     else {
	  kiss_file_stream_t* stream = kiss_make_file_stream(fp);

	  if (rest == KISS_NIL) {
	       stream->flags |= (KISS_OUTPUT_STREAM | KISS_INPUT_STREAM | KISS_CHARACTER_STREAM);
	  } else if (Kiss_Integer(kiss_car(rest))->i == 8) {
	       stream->flags |= (KISS_OUTPUT_STREAM | KISS_INPUT_STREAM | KISS_BYTE_STREAM);
	  } else {
	       Kiss_Err(L"only 8 bit-binary-element-class is supported ~S", kiss_car(rest));
	  }
	  return (kiss_obj*)stream;
     }
}

/* function: (finish-output stream) -> <null>
   Completes any pending output to the destination designated by stream.
   Waits until the pending output is complete and then returns nil.
   For instance, pending output might be stored in a buffer; in this case
   finish-output forces the buffer to be written to the stream’s destination.
   An error shall be signaled if stream is not a stream that can handle output operations
   (error-id. domain-error).
*/
kiss_obj* kiss_finish_output (kiss_obj* obj) {
     if (!KISS_IS_OUTPUT_STREAM(Kiss_Stream(obj))) {
	  Kiss_Err(L"output stream expected ~S", obj);
     }
     if (KISS_IS_FILE_STREAM(obj)) {
	  kiss_file_stream_t* stream = (kiss_file_stream_t*)obj;
	  int result = fflush(stream->file_ptr);
	  if (result == EOF) {
	       Kiss_System_Error();
	  }
     }
     return KISS_NIL;
}


/* function: (create-string-input-stream string) -> <stream>
   Creates and returns an input stream from the string. An error shall be
   signaled if string is not a string (error-id. domain-error ). */
kiss_obj* kiss_create_string_input_stream(kiss_obj* string) {
     kiss_string_t* str = Kiss_String(string);
     kiss_string_stream_t* p = kiss_make_string_stream(str);
     p->flags |= (KISS_INPUT_STREAM | KISS_CHARACTER_STREAM);
     return (kiss_obj*)p;
}

/* function: (create-string-output-stream) -> <stream>
   This function creates and returns a string output stream. The output
   to a string stream can be retrieved by get-output-stream-string. */
kiss_obj* kiss_create_string_output_stream(void) {
     kiss_string_t* str = kiss_make_string(L"");
     kiss_string_stream_t* p = kiss_make_string_stream(str);
     p->flags |= (KISS_OUTPUT_STREAM | KISS_CHARACTER_STREAM);
     return (kiss_obj*)p;
}

/* function: (get-output-stream-string stream) -> <string> 
   Returns a string containing all characters written to stream since the
   last call to this function or since the creation of the stream, if
   this function has not been called with stream before. An error shall
   be signaled if stream is not a stream created with
   create-string-output-stream (error-id. domain-error ).*/
kiss_obj* kiss_get_output_stream_string(kiss_obj* stream) {
     kiss_string_stream_t* p = Kiss_String_Output_Stream(stream);
     return (kiss_obj*)kiss_chars_to_str(kiss_reverse(p->list));
}

kiss_obj* kiss_c_read_char(kiss_obj* in, kiss_obj* eos_err_p, kiss_obj* eos_val) {
     if (KISS_IS_FILE_STREAM(Kiss_Input_Char_Stream(in))) {
	  FILE* fp = ((kiss_file_stream_t*)in)->file_ptr;
	  wint_t c = fgetwc(fp);
	  if (c == WEOF) {
	       if (ferror(fp)) { Kiss_System_Error(); }
	       goto eos;
	  } else {
	       return (kiss_obj*)kiss_make_character(c);
	  }
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
     } else {
	  return eos_val;
     }
}


/* function: (read-char [input-stream [eos-error-p [eos-value]]]) -> <object>
   read-char reads a single character from input-stream and returns the corresponding
   character object.
*/
kiss_obj* kiss_read_char(kiss_obj* args) {
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


kiss_obj* kiss_c_preview_char(kiss_obj* in, kiss_obj* eos_err_p, kiss_obj* eos_val) {
     if (KISS_IS_FILE_STREAM(Kiss_Input_Char_Stream(in))) {
	  FILE* fp = ((kiss_file_stream_t*)in)->file_ptr;
	  wint_t c = fgetwc(fp);
	  if (c == WEOF) {
	       if (ferror(fp)) { Kiss_System_Error(); }
	  } else {
	       ungetwc(c, fp);
	       return (kiss_obj*)kiss_make_character(c);
	  }
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
     } else {
	  return eos_val;
     }
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
     wint_t c;
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
     kiss_character_t* c = Kiss_Character(character);
     if (KISS_IS_FILE_STREAM(Kiss_Output_Char_Stream(output))) {
	  kiss_file_stream_t* out = (kiss_file_stream_t*)output;
	  FILE* fp = out->file_ptr;
	  if (c->c == L'\n') {
	       out->column = 0;
	  } else if (c->c == L'\t'){
	       size_t column = out->column;
	       size_t width = Kiss_Integer(kiss_dynamic(kiss_symbol(L"*column-width*")))->i;
	       out->column = kiss_next_column(column, width);
	  } else {
	       out->column += 1;
	  }
	  if (fputwc(c->c, fp) == WEOF) {
	       Kiss_System_Error();
	  }
     } else if (KISS_IS_STRING_STREAM(output)) {
	  kiss_string_stream_t* out = (kiss_string_stream_t*)output;
	  if (c->c == L'\n') {
	       out->column = 0;
	  } else if (c->c == L'\t'){
	       size_t column = out->column;
	       size_t width = Kiss_Integer(kiss_dynamic(kiss_symbol(L"*column-width*")))->i;
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

