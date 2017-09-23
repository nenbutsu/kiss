/*  -*- coding: utf-8 -*-
  error.c --- defines the error mechanism of ISLisp processor KISS.

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

static int is_condition_working(void) {
     return kiss_featurep(kiss_symbol(L"condition")) == KISS_T;
}

_Noreturn
void Kiss_System_Error (void) {
     perror(NULL);
     kiss_obj* msg = (kiss_obj*)kiss_make_string(L"system error");
     if (is_condition_working()) {
	  kiss_c_funcall(L"kiss::signal-simple-error", kiss_c_list(3, msg, KISS_NIL, KISS_NIL));
     } else {
	  kiss_throw(kiss_c_list(2, kiss_symbol(L"quote"), kiss_symbol(L"kiss::error")), msg);
     }
     exit(EXIT_FAILURE); // not reach here
}

_Noreturn
void Kiss_Err(const wchar_t* const str, ...) {
     va_list args;
     const wchar_t* p;
     kiss_obj* const out = kiss_create_string_output_stream();
     kiss_obj* string;
     kiss_obj* rest = KISS_NIL;

     va_start(args, str);
     for (p = str; *p != L'\0'; p++) {
	  if (*p == L'~' && *(p+1) == L'S') {
	       if (is_condition_working()) {
		    kiss_push(va_arg(args, kiss_obj*), &rest);
		    kiss_format_object(out, (kiss_obj*)kiss_make_string(L"~S"), KISS_NIL);
	       } else {
		    kiss_format_object(out, va_arg(args, kiss_obj*), KISS_NIL);
	       }
	       p++;
	  } else {
	       kiss_format_char(out, kiss_make_character(*p));
	  }
     }
     va_end(args);

     string = kiss_get_output_stream_string(out);
     if (is_condition_working()) {
	  rest = kiss_nreverse(rest);
	  kiss_c_funcall(L"kiss::signal-simple-error",
			kiss_c_list(3, (kiss_obj*)string, rest, KISS_NIL));
     } else {
	  kiss_throw(kiss_c_list(2, kiss_symbol(L"quote"), kiss_symbol(L"kiss::error")), string);
     }
     exit(EXIT_FAILURE); // not reach here
}


_Noreturn
void Kiss_Domain_Error(const kiss_obj* const obj, const wchar_t* const domain) {
     if (is_condition_working()) {
          if (*domain == L'<') {
               kiss_c_funcall(L"kiss::assure", kiss_c_list(2, kiss_symbol(domain), obj));
          } else {
               kiss_c_funcall(L"kiss::signal-simple-domain-error",
                              kiss_c_list(3, obj, kiss_make_string(domain), KISS_NIL));
          }
     } else {
          Kiss_Err(L"~S is not ~S", obj, kiss_make_string(domain));
     }
     exit(EXIT_FAILURE); // not reach here.
}


kiss_obj* Kiss_Valid_Sequence_Index(const kiss_obj* const sequence, const kiss_obj* const index)
{
     const size_t n = kiss_c_length(sequence);
     const kiss_ptr_int i = Kiss_Fixnum(index);
     if (i < 0 || i >= n) {
          Kiss_Err(L"Invalid sequence index ~S for ~S", index, sequence);
     }
     return (kiss_obj*)index;
}

kiss_stream_t* Kiss_Input_Char_Stream(const kiss_obj* const obj) {
    if (!KISS_IS_INPUT_STREAM(obj) || !KISS_IS_CHARACTER_STREAM(obj)) {
	Kiss_Err(L"Input character stream expected ~S", obj);
    }
    return (kiss_stream_t*)obj;
}

kiss_stream_t* Kiss_Output_Char_Stream(const kiss_obj* const obj) {
    if (!KISS_IS_OUTPUT_STREAM(obj) || !KISS_IS_CHARACTER_STREAM(obj)){
	Kiss_Err(L"Output character stream expected ~S", obj);
    }
    return (kiss_stream_t*)obj;
}

kiss_stream_t* Kiss_Input_Byte_Stream(const kiss_obj* const obj) {
    if (!KISS_IS_INPUT_STREAM(obj) || !KISS_IS_BYTE_STREAM(obj)) {
	Kiss_Err(L"Input byte stream expected ~S", obj);
    }
    return (kiss_stream_t*)obj;
}

kiss_stream_t* Kiss_Output_Byte_Stream(const kiss_obj* const obj) {
    if (!KISS_IS_OUTPUT_STREAM(obj) || !KISS_IS_BYTE_STREAM(obj)){
	Kiss_Err(L"Output byte stream expected ~S", obj);
    }
    return (kiss_stream_t*)obj;
}


kiss_file_stream_t* Kiss_Open_File_Stream(const kiss_obj* const obj) {
     Kiss_Stream(obj);
     if (KISS_IS_FILE_STREAM(obj) || ((kiss_file_stream_t*)obj)->file_ptr) {
	  return (kiss_file_stream_t*)obj;
     }
     Kiss_Err(L"Open file stream expected ~S", obj);
     exit(EXIT_FAILURE); // not reach here
}

kiss_string_stream_t* Kiss_String_Output_Stream(const kiss_obj* const obj) {
    if (!KISS_IS_OUTPUT_STREAM(obj) || !KISS_IS_STRING_STREAM(obj)) {
	Kiss_Err(L"String output stream expected ~S", obj);
    }
    return (kiss_string_stream_t*)obj;
}

/* lambda-list ::= (identifier* [:rest identifier]) */
kiss_obj* Kiss_Lambda_List(const kiss_obj* const list) {
     kiss_obj* stack = KISS_NIL;
     for (const kiss_obj* p = Kiss_Proper_List(list); KISS_IS_CONS(p); p = KISS_CDR(p)) {
          const kiss_symbol_t* name = Kiss_Symbol(KISS_CAR(p));
          if (name == &KISS_Samp_rest || name == &KISS_Skw_rest) {
               if (kiss_c_length(p) != 2) {
                    Kiss_Err(L"~S must be followed by one variable name ~S",
                             name == &KISS_Samp_rest ? &KISS_Samp_rest : &KISS_Skw_rest, list);
               }
               p = KISS_CDR(p);
               name = Kiss_Symbol(KISS_CAR(p));
          }
          if (name == &KISS_Samp_rest || name == &KISS_Skw_rest) {
               Kiss_Err(L"~S cannot be used as a variable name ~S",
                        name == &KISS_Samp_rest ? &KISS_Samp_rest : &KISS_Skw_rest, list);
          }
          if (kiss_member((kiss_obj*)name, stack) != KISS_NIL) {
               Kiss_Err(L"Same variable name ~S occurs more than once", name);
          }
          kiss_push((kiss_obj*)name, &stack);
     }
     return (kiss_obj*)list;
}
                                                                                
/* lambda-expression :: = (lambda LAMBDA-LIST form*) */
kiss_obj* Kiss_Lambda_Expression(const kiss_obj* const p) {
    Kiss_Proper_List(p);
    if (kiss_c_length(p) < 2 || KISS_CAR(p) != KISS_LAMBDA) {
	Kiss_Err(L"Invalid lambda expression ~S", p);
    }
    Kiss_Lambda_List(kiss_cadr(p));
    return (kiss_obj*)p;
}

// signaling errors
_Noreturn
void Kiss_Cannot_Parse_Number_Error(const kiss_obj* const str) {
     Kiss_Err(L"Cannot parse number ~S", str);
     exit(EXIT_FAILURE);
}

_Noreturn
void Kiss_Division_By_Zero_Error(const kiss_obj* const i) {
     Kiss_Err(L"Division by zero: ~S", i);
     exit(EXIT_FAILURE);
}

_Noreturn
void Kiss_End_Of_Stream_Error(const kiss_obj* const stream) {
     Kiss_Err(L"End of stream ~S", stream);
     exit(EXIT_FAILURE);
}

_Noreturn
void Kiss_Unbound_Variable_Error(const kiss_obj* const name) {
     if (is_condition_working()) {
	  kiss_c_funcall(L"kiss::signal-unbound-variable", kiss_c_list(2, name, KISS_NIL));
     } else {
	  Kiss_Err(L"Unbound variable ~S", name);
     }
     exit(EXIT_FAILURE);
}

_Noreturn
void Kiss_Catcher_Not_Found_Error(const kiss_obj* const tag) {
     if (is_condition_working()) {
	  kiss_c_funcall(L"kiss::signal-catcher-not-found", kiss_c_list(2, tag, KISS_NIL));
     } else {
	  Kiss_Err(L"Catcher not found for ~S", tag);
     }
     exit(EXIT_FAILURE);
}

_Noreturn
void Kiss_Block_Not_Found_Error(const kiss_obj* const tag) {
     if (is_condition_working()) {
	  kiss_c_funcall(L"kiss::signal-block-not-found", kiss_c_list(2, tag, KISS_NIL));
     } else {
	  Kiss_Err(L"Block not found ~S", tag);
     }
     exit(EXIT_FAILURE);
}

_Noreturn
void Kiss_Tagbody_Not_Found_Error(const kiss_obj* const tag) {
     if (is_condition_working()) {
	  kiss_c_funcall(L"kiss::signal-tagbody-not-found", kiss_c_list(2, tag, KISS_NIL));
     } else {
	  Kiss_Err(L"Tagbody not found ~S", tag);
     }
     exit(EXIT_FAILURE);
}

