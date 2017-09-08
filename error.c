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

_Noreturn void Kiss_System_Error (void) {
     perror(NULL);
     kiss_obj* msg = (kiss_obj*)kiss_make_string(L"system error");
     if (is_condition_working()) {
	  kiss_cfuncall(L"kiss::signal-simple-error", kiss_c_list(3, msg, KISS_NIL, KISS_NIL));
     } else {
	  kiss_throw(kiss_c_list(2, kiss_symbol(L"quote"), kiss_symbol(L"kiss::error")), msg);
     }
     exit(EXIT_FAILURE); // not reach here
}

_Noreturn void Kiss_Err(const wchar_t* const str, ...) {
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
	       kiss_format_char(out, (kiss_obj*)kiss_make_character(*p));
	  }
     }
     va_end(args);

     string = kiss_get_output_stream_string(out);
     if (is_condition_working()) {
	  rest = kiss_nreverse(rest);
	  kiss_cfuncall(L"kiss::signal-simple-error",
			kiss_c_list(3, (kiss_obj*)string, rest, KISS_NIL));
     } else {
	  kiss_throw(kiss_c_list(2, kiss_symbol(L"quote"), kiss_symbol(L"kiss::error")), string);
     }
     exit(EXIT_FAILURE); // not reach here
}


/* assure primitive type */
void kiss_primitive_assure(const kiss_type t, const kiss_obj* const obj) {
     if (t != KISS_OBJ_TYPE(obj)) {
	  if (is_condition_working()) {
	       kiss_cfuncall(L"kiss::assure", kiss_c_list(2, kiss_type_to_class_name(t), obj));
	  } else {
	       Kiss_Err(L"~S expected ~S", kiss_type_to_class_name(t), obj);
	  }
     }
}

inline kiss_cons_t* Kiss_Cons(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_CONS, obj);
     return (kiss_cons_t*)obj;
}

inline kiss_integer_t* Kiss_Integer(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_INTEGER, obj);
     return (kiss_integer_t*)obj;
}

inline kiss_float_t* Kiss_Float(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_FLOAT, obj);
     return (kiss_float_t*)obj;
}

inline kiss_character_t* Kiss_Character(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_CHARACTER, obj);
     return (kiss_character_t*)obj;
}

inline kiss_symbol_t* Kiss_Symbol(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_SYMBOL, obj);
     return (kiss_symbol_t*)obj;
}

inline kiss_string_t* Kiss_String(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_STRING, obj);
    return (kiss_string_t*)obj;
}

inline kiss_stream_t* Kiss_Stream(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_STREAM, obj);
     return (kiss_stream_t*)obj;
}

inline kiss_general_vector_t* Kiss_General_Vector(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_GENERAL_VECTOR, obj);
    return (kiss_general_vector_t*)obj;
}

inline kiss_general_array_t* Kiss_General_Array_S(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_GENERAL_ARRAY, obj);
    return (kiss_general_array_t*)obj;
}

inline kiss_function_t* Kiss_Function(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_FUNCTION, obj);
     return (kiss_function_t*)obj;
}



/* assure non-primitive type */

inline kiss_obj* Kiss_Number(const kiss_obj* const obj) {
     if (!KISS_IS_INTEGER(obj) && !KISS_IS_FLOAT(obj)) {
	  if (is_condition_working()) {
	       kiss_cfuncall(L"kiss::assure", kiss_c_list(2, kiss_symbol(L"<number>"), obj));
	  } else {
	       Kiss_Err(L"Number expected ~S", obj);
	  }
     }
     return (kiss_obj*)obj;
}

inline kiss_obj* Kiss_List(const kiss_obj* const obj) {
     if (obj != KISS_NIL && !KISS_IS_CONS(obj)) {
	  if (is_condition_working()) {
	       kiss_cfuncall(L"kiss::assure", kiss_c_list(2, kiss_symbol(L"<list>"), obj));
	  } else {
	       Kiss_Err(L"List expected ~S", obj);
	  }
     }
     return (kiss_obj*)obj;
}

inline kiss_integer_t* Kiss_Non_Negative_Integer(const kiss_obj* const obj) {
    const long int i = Kiss_Integer(obj)->i;
    if (i < 0) {
	 Kiss_Err(L"Non negative integer expected ~S", obj);
    }
    return (kiss_integer_t*)obj;
}

inline kiss_integer_t* Kiss_Non_Zero_Integer(const kiss_obj* const obj) {
    const long int i = Kiss_Integer(obj)->i;
    if (i == 0) {
	 Kiss_Err(L"Non zero integer expected ~S", obj);
    }
    return (kiss_integer_t*)obj;
}

inline kiss_obj* Kiss_General_Array(const kiss_obj* const obj) {
     if (!KISS_IS_GENERAL_VECTOR(obj) && !KISS_IS_GENERAL_ARRAY(obj)) {
          Kiss_Err(L"<general-vector> or <general-array*> expected ~S", obj);
     }
     return (kiss_obj*)obj;
}

inline kiss_obj* Kiss_Basic_Array(const kiss_obj* const obj) {
     if (!KISS_IS_GENERAL_VECTOR(obj) && !KISS_IS_GENERAL_ARRAY(obj) && !KISS_IS_STRING(obj)) {
          if (is_condition_working()) {
               kiss_cfuncall(L"kiss::assure", kiss_c_list(2, kiss_symbol(L"<basic-array>"), obj));
          } else {
	       Kiss_Err(L"<basic-array> expected ~S", obj);
          }
     }
     return (kiss_obj*)obj;
}

kiss_obj* Kiss_Valid_Sequence_Index(const kiss_obj* const sequence, const kiss_obj* const index) {
     const size_t n = kiss_c_length(sequence);
     const long int i = Kiss_Integer(index)->i;
     if (i < 0 || i >= n) {
          Kiss_Err(L"Invalid sequence index ~S ~S", sequence, index);
     }
     return (kiss_obj*)index;
}

/* Proper list is a list terminated by the empty list. (The empty list is a proper list.) */
kiss_obj* Kiss_Proper_List(const kiss_obj* const obj) {
     const kiss_obj* p = obj;
     while (KISS_IS_CONS(p)) { p = KISS_CDR(p); }
     if (p != KISS_NIL) {
          Kiss_Err(L"Proper list expected ~S", obj);
     }
     return (kiss_obj*)obj;
}

kiss_cons_t* Kiss_Proper_List_2(kiss_obj* obj) {
    Kiss_Proper_List(obj);
    if (kiss_c_length(obj) != 2) {
	Kiss_Err(L"Proper list of length 2 expected ~S", obj);
    }
    return (kiss_cons_t*)obj;
}

kiss_oo_obj_t* Kiss_Object(kiss_obj* obj) {
    if (!KISS_IS_OBJECT(obj)) { Kiss_Err(L"Object expected ~S", obj); }
    return (kiss_oo_obj_t*)obj;
}

kiss_stream_t* Kiss_Input_Char_Stream(kiss_obj* obj) {
    if (!KISS_IS_INPUT_STREAM(obj) || !KISS_IS_CHARACTER_STREAM(obj)) {
	Kiss_Err(L"Input character stream expected ~S", obj);
    }
    return (kiss_stream_t*)obj;
}

kiss_stream_t* Kiss_Output_Char_Stream(kiss_obj* obj) {
    if (!KISS_IS_OUTPUT_STREAM(obj) || !KISS_IS_CHARACTER_STREAM(obj)){
	Kiss_Err(L"Output character stream expected ~S", obj);
    }
    return (kiss_stream_t*)obj;
}

kiss_stream_t* Kiss_Input_Byte_Stream(kiss_obj* obj) {
    if (!KISS_IS_INPUT_STREAM(obj) || !KISS_IS_BYTE_STREAM(obj)) {
	Kiss_Err(L"Input byte stream expected ~S", obj);
    }
    return (kiss_stream_t*)obj;
}

kiss_stream_t* Kiss_Output_Byte_Stream(kiss_obj* obj) {
    if (!KISS_IS_OUTPUT_STREAM(obj) || !KISS_IS_BYTE_STREAM(obj)){
	Kiss_Err(L"Output byte stream expected ~S", obj);
    }
    return (kiss_stream_t*)obj;
}


kiss_file_stream_t* Kiss_Open_File_Stream(kiss_obj* obj) {
     Kiss_Stream(obj);
     if (KISS_IS_FILE_STREAM(obj) || ((kiss_file_stream_t*)obj)->file_ptr) {
	  return (kiss_file_stream_t*)obj;
     }
     Kiss_Err(L"Open file stream expected ~S", obj);
     exit(EXIT_FAILURE); // not reach here
}

kiss_string_stream_t* Kiss_String_Output_Stream(kiss_obj* obj) {
    if (!KISS_IS_OUTPUT_STREAM(obj) || !KISS_IS_STRING_STREAM(obj)) {
	Kiss_Err(L"String output stream expected ~S", obj);
    }
    return (kiss_string_stream_t*)obj;
}

kiss_obj* Kiss_Sequence(const kiss_obj* const obj) {
     if (!KISS_IS_SEQUENCE(obj)) {
          Kiss_Err(L"Sequence expected ~S", obj);
     }
     return (kiss_obj*)obj;
}

kiss_function_t* Kiss_Macro(kiss_obj* obj) {
/* implementation of macro currently uses function_t */
    if (!KISS_IS_MACRO(obj)) { Kiss_Err(L"Macro expected ~S", obj); }
    return (kiss_function_t*)obj;
}

kiss_cfunction_t* Kiss_CFunction(kiss_obj* obj) {
    if (!KISS_IS_CFUNCTION(obj)) { Kiss_Err(L"C function expected ~S", obj); }
    return (kiss_cfunction_t*)obj;
}

kiss_cfunction_t* Kiss_CMacro(kiss_obj* obj) {
    if (!KISS_IS_CMACRO(obj)) { Kiss_Err(L"C macro expected ~S", obj); }
    return (kiss_cfunction_t*)obj;
}

/* lambda-list ::= (identifier* [:rest identifier]) */
kiss_obj* Kiss_Lambda_List(kiss_obj* list) {
    kiss_obj* p;
    kiss_obj* stack = KISS_NIL;
    for (p = Kiss_Proper_List(list); KISS_IS_CONS(p); p = KISS_CDR(p)) {
	kiss_symbol_t* name = Kiss_Symbol(KISS_CAR(p));
	if (name == &KISS_Samp_rest) {
	    if (kiss_c_length(p) != 2) {
		Kiss_Err(L":rest must be followed by one variable name ~S",
			   list);
	    }
	    p = KISS_CDR(p);
	    name = Kiss_Symbol(KISS_CAR(p));
	}
	if (name == &KISS_Samp_rest) {
	    Kiss_Err(L":rest cannot be used as a variable name ~S", list);
	}
	if (kiss_member((kiss_obj*)name, stack) != KISS_NIL) {
	    Kiss_Err(L"Same variable name ~S occurs more than once", name);
	}
	kiss_push((kiss_obj*)name, &stack);
    }
    return list;
}
                                                                                
/* lambda-expression :: = (lambda LAMBDA-LIST form*) */
kiss_obj* Kiss_Lambda_Expression(kiss_obj* p) {
    p = Kiss_Proper_List(p);
    if (kiss_c_length(p) < 2 || KISS_CAR(p) != KISS_LAMBDA) {
	Kiss_Err(L"Illegal lambda expression ~S", p);
    }
    Kiss_Lambda_List(kiss_cadr(p));
    return p;
}

// -----------
void Kiss_Cannot_Parse_Number_Error(kiss_obj* str) {
     Kiss_Err(L"Cannot parse number ~S", str);
}

void Kiss_Division_By_Zero_Error(kiss_obj* i) {
     Kiss_Err(L"Division by zero: ~S", i);
}

void Kiss_End_Of_Stream_Error(kiss_obj* stream) {
     Kiss_Err(L"End of stream ~S", stream);
}

void Kiss_Unbound_Variable_Error(kiss_obj* name) {
     if (is_condition_working()) {
	  kiss_cfuncall(L"kiss::signal-unbound-variable", kiss_c_list(2, name, KISS_NIL));
     } else {
	  Kiss_Err(L"Unbound variable ~S", name);
     }
}

void Kiss_Catcher_Not_Found_Error(kiss_obj* tag) {
     if (is_condition_working()) {
	  kiss_cfuncall(L"kiss::signal-catcher-not-found", kiss_c_list(2, tag, KISS_NIL));
     } else {
	  Kiss_Err(L"Catcher not found for ~S", tag);
     }
}

void Kiss_Block_Not_Found_Error(kiss_obj* tag) {
     if (is_condition_working()) {
	  kiss_cfuncall(L"kiss::signal-block-not-found", kiss_c_list(2, tag, KISS_NIL));
     } else {
	  Kiss_Err(L"Block not found ~S", tag);
     }
}

void Kiss_Tagbody_Not_Found_Error(kiss_obj* tag) {
     if (is_condition_working()) {
	  kiss_cfuncall(L"kiss::signal-tagbody-not-found", kiss_c_list(2, tag, KISS_NIL));
     } else {
	  Kiss_Err(L"Tagbody not found ~S", tag);
     }
}

