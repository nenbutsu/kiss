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
    Kiss_Standard_Input.flags     = KISS_INPUT_STREAM | KISS_CHARACTER_STREAM | KISS_FILE_STREAM;
    Kiss_Standard_Input.file_ptr  = stdin;
    Kiss_Standard_Input.column    = 0;
    Kiss_Standard_Input.gc_next   = NULL;
    Kiss_Standard_Input.gc_flag   = 0;

    Kiss_Standard_Output.type     = KISS_STREAM;
    Kiss_Standard_Output.flags    = KISS_OUTPUT_STREAM | KISS_CHARACTER_STREAM | KISS_FILE_STREAM;
    Kiss_Standard_Output.file_ptr = stdout;
    Kiss_Standard_Output.column   = 0;
    Kiss_Standard_Output.gc_next  = NULL;
    Kiss_Standard_Output.gc_flag  = 0;
    
    
    Kiss_Error_Output.type        = KISS_STREAM;
    Kiss_Error_Output.flags       = KISS_OUTPUT_STREAM | KISS_CHARACTER_STREAM | KISS_FILE_STREAM;
    Kiss_Error_Output.file_ptr    = stderr;
    Kiss_Error_Output.column      = 0;
    Kiss_Error_Output.gc_next     = NULL;
    Kiss_Error_Output.gc_flag     = 0;
}

static kiss_file_stream_t* kiss_make_file_stream(FILE* fp) {
    kiss_file_stream_t* p = Kiss_GC_Malloc(sizeof(kiss_file_stream_t));
    p->type = KISS_STREAM;
    p->flags = KISS_FILE_STREAM;
    p->file_ptr = fp;
    p->column = 0;
    return p;
}

static kiss_string_stream_t* kiss_make_string_stream(kiss_string_t* str)
{
    kiss_string_stream_t* p = Kiss_GC_Malloc(sizeof(kiss_string_stream_t));
    p->type = KISS_STREAM;
    p->flags = KISS_STRING_STREAM;
    p->list = KISS_NIL;
    p->list = kiss_str_to_chars(str); // might gc
    return p;
}

/* function: (streamp obj ) → boolean
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

/* function: (open-stream-p obj ) → boolean
   Returns t if obj is an open stream; otherwise, returns nil. */
kiss_obj* kiss_open_streamp(kiss_obj* obj) {
    
}

/* function: (create-string-input-stream string) → <stream>
   Creates and returns an input stream from the string. An error shall be
   signaled if string is not a string (error-id. domain-error ). */
kiss_obj* kiss_create_string_input_stream(kiss_obj* string) {
    kiss_string_t* str = Kiss_String(string);
    kiss_string_stream_t* p = kiss_make_string_stream(str);
    p->flags = KISS_INPUT_STREAM | KISS_CHARACTER_STREAM | p->flags;
    return (kiss_obj*)p;
}

/* function: (create-string-output-stream) → <stream>
   This function creates and returns a string output stream. The output
   to a string stream can be retrieved by get-output-stream-string. */
kiss_obj* kiss_create_string_output_stream(void) {
    kiss_string_t* str = kiss_make_string(L"");
    kiss_string_stream_t* p = kiss_make_string_stream(str);
    p->flags = KISS_OUTPUT_STREAM | KISS_CHARACTER_STREAM | p->flags;
    return (kiss_obj*)p;
}

/* function: (get-output-stream-string stream) → <string> 
   Returns a string containing all characters written to stream since the
   last call to this function or since the creation of the stream, if
   this function has not been called with stream before. An error shall
   be signaled if stream is not a stream created with
   create-string-output-stream (error-id. domain-error ).*/
kiss_obj* kiss_get_output_stream_string(kiss_obj* stream) {
    kiss_string_stream_t* p = Kiss_String_Output_Stream(stream);
    return (kiss_obj*)kiss_chars_to_str(kiss_reverse(p->list));
}

/* function: (standard-input) -> <stream> */
kiss_obj* kiss_standard_input(void)  { return (kiss_obj*)&Kiss_Standard_Input; }

/* function: (standard-output) -> <stream> */
kiss_obj* kiss_standard_output(void) {
     return (kiss_obj*)&Kiss_Standard_Output;
}

/* function: (error-output) -> <stream> */
kiss_obj* kiss_error_output(void)    { return (kiss_obj*)&Kiss_Error_Output; }

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

kiss_obj* kiss_char_stream_p(kiss_obj* p) {
    if (KISS_IS_CHARACTER_STREAM(p)) { return KISS_T; }
    else                             { return KISS_NIL; }
}

kiss_obj* kiss_input_char_stream_p(kiss_obj* p) {
    if (kiss_input_stream_p(p) == KISS_T && kiss_char_stream_p(p) == KISS_T) {
	return KISS_T;
    } else {
	return KISS_NIL;
    }
}

kiss_obj* kiss_output_char_stream_p(kiss_obj* p) {
    if (kiss_output_stream_p(p) == KISS_T && kiss_char_stream_p(p) == KISS_T) {
	return KISS_T;
    } else {
	return KISS_NIL;
    }
}

kiss_obj* kiss_cread_char(kiss_obj* in, kiss_obj* eos_err_p, kiss_obj* eos_val)
{
    kiss_stream_t* input = Kiss_Input_Char_Stream(in);

    if (KISS_IS_FILE_STREAM(input)) {
	FILE* fp = ((kiss_file_stream_t*)input)->file_ptr;
	wint_t c = fgetwc(fp);
	if (c == WEOF) {
	    if (ferror(fp)) { Kiss_System_Error(); }
	    goto eos;
	} else {
	    return (kiss_obj*)kiss_make_character(c);
	}
    } else {
	kiss_string_stream_t* string_stream = (kiss_string_stream_t*)in;
	if (string_stream->list == KISS_NIL) {
	    goto eos;
	} else {
	    kiss_obj* c = KISS_CAR(string_stream->list);
	    string_stream->list = KISS_CDR(string_stream->list);
	    return c;
	}
    }
eos:
    if (eos_err_p != KISS_NIL) {
	Kiss_End_Of_Stream_Error(in);
    } else {
	return eos_val;
    }
}



kiss_obj* kiss_cpreview_char(kiss_obj* in, kiss_obj* eos_err_p, kiss_obj* eos_val) {
    kiss_stream_t* input = Kiss_Input_Char_Stream(in);

    if (KISS_IS_FILE_STREAM(input)) {
	FILE* fp = ((kiss_file_stream_t*)input)->file_ptr;
	wint_t c = fgetwc(fp);
	if (c == WEOF) {
	    if (ferror(fp)) { Kiss_System_Error(); }
	} else {
	    ungetc(c, fp);
	    return (kiss_obj*)kiss_make_character(c);
	}
    } else {
	kiss_string_stream_t* string_stream = (kiss_string_stream_t*)in;
	if (string_stream->list == KISS_NIL) {
	    goto eos;
	} else {
	    kiss_obj* c = KISS_CAR(string_stream->list);
	    return c;
	}
    }
eos:
    if (eos_err_p != KISS_NIL) {
	Kiss_End_Of_Stream_Error(in);
    } else {
	return eos_val;
    }
}

/*
  function: (read-char [input-stream [eos-error-p [eos-value]]]) -> <object>
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
    return kiss_cread_char(in, eos_err_p, eos_val);
}

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
    return kiss_cpreview_char(in, eos_err_p, eos_val);
}

/* function: (format-char output-stream char) -> <null> */
kiss_obj* kiss_format_char(kiss_obj* output_stream, kiss_obj* character) {
    kiss_stream_t* out = Kiss_Output_Char_Stream(output_stream);
    kiss_character_t* c = Kiss_Character(character);
    if (KISS_IS_FILE_STREAM(out)) {
	FILE* fp;
	fp = ((kiss_file_stream_t*)out)->file_ptr;
	if (c->c == L'\n') {
	     ((kiss_file_stream_t*)out)->column = 0;
	} else if (c->c == L'\t'){
	     size_t column = ((kiss_file_stream_t*)out)->column;
	     ((kiss_file_stream_t*)out)->column = 7 * ((column / 7) + 1) + (column / 7);
		  /* assume tab is 8 column-position.
		     column 0 represents the left margin. */
	} else {
	     ((kiss_file_stream_t*)out)->column += 1;
	}
	if (fputwc(c->c, fp) == WEOF) {
	    if (ferror(fp)) { Kiss_System_Error(); }
	}
    } else {
	kiss_string_stream_t* string_stream = (kiss_string_stream_t*)out;
	kiss_push(character, &(string_stream->list));
    }
    return KISS_NIL;
}

kiss_obj* kiss_open_input_file(kiss_obj* filename, kiss_obj* rest) {
    kiss_string_t* str = Kiss_String(filename);
    char buff[1024];
    wcstombs(buff, str->str, 1024);
    FILE* fp = fopen(buff, "r");
    if (fp == NULL) { Kiss_System_Error(); }
    else {
	kiss_file_stream_t* stream = kiss_make_file_stream(fp);
	stream->flags = KISS_INPUT_STREAM | KISS_CHARACTER_STREAM | stream->flags;
	return (kiss_obj*)stream;
    }
}

