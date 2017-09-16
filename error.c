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


/* assure primitive type */
static inline void kiss_primitive_assure(const kiss_type t, const kiss_obj* const obj) {
     if (t != KISS_OBJ_TYPE(obj)) {
	  if (is_condition_working()) {
	       kiss_c_funcall(L"kiss::assure", kiss_c_list(2, kiss_type_to_class_name(t), obj));
	  } else {
	       Kiss_Err(L"~S expected ~S", kiss_type_to_class_name(t), obj);
	  }
     }
}

inline kiss_cons_t* Kiss_Cons(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_CONS, obj);
     return (kiss_cons_t*)obj;
}

inline kiss_ptr_int Kiss_Fixnum(const kiss_obj* const obj) {
     if (KISS_IS_FIXNUM(obj)) {
          return kiss_ptr_int(obj);
     } else if (KISS_IS_BIGNUM(obj)) {
          kiss_bignum_t* z = (kiss_bignum_t*)obj;
          if (mpz_cmp_si(z->mpz, KISS_PTR_INT_MAX) <= 0 &&
              mpz_cmp_si(z->mpz, KISS_PTR_INT_MIN) >= 0)
          {
               return mpz_get_si(z->mpz);
          }
     }
     const kiss_type t = KISS_FIXNUM;
     if (is_condition_working()) {
          kiss_c_funcall(L"kiss::assure", kiss_c_list(2, kiss_type_to_class_name(t), obj));
     } else {
          Kiss_Err(L"~S expected ~S", kiss_type_to_class_name(t), obj);
     }
     exit(EXIT_FAILURE); // not reach here
}

inline kiss_bignum_t* Kiss_Bignum(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_BIGNUM, obj);
     return (kiss_bignum_t*)obj;
}

inline kiss_float_t* Kiss_Float(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_FLOAT, obj);
     return (kiss_float_t*)obj;
}

inline wchar_t Kiss_Character(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_CHARACTER, obj);
     return kiss_wchar(obj);
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

kiss_function_t* Kiss_Macro(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_MACRO, obj);
     return (kiss_function_t*)obj;
}

kiss_cfunction_t* Kiss_CFunction(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_CFUNCTION, obj);
     return (kiss_cfunction_t*)obj;
}

kiss_cfunction_t* Kiss_CMacro(const kiss_obj* const obj) {
     kiss_primitive_assure(KISS_CMACRO, obj);
     return (kiss_cfunction_t*)obj;
}



/* assure non-primitive type */

inline kiss_obj* Kiss_Integer(const kiss_obj* const obj) {
     if (!KISS_IS_FIXNUM(obj) && !KISS_IS_BIGNUM(obj)) {
	  if (is_condition_working()) {
	       kiss_c_funcall(L"kiss::assure", kiss_c_list(2, kiss_symbol(L"<integer>"), obj));
	  } else {
	       Kiss_Err(L"Integer expected ~S", obj);
	  }
     }
     return (kiss_obj*)obj;
}


inline kiss_obj* Kiss_Number(const kiss_obj* const obj) {
     if (!KISS_IS_INTEGER(obj) && !KISS_IS_FLOAT(obj)) {
	  if (is_condition_working()) {
	       kiss_c_funcall(L"kiss::assure", kiss_c_list(2, kiss_symbol(L"<number>"), obj));
	  } else {
	       Kiss_Err(L"Number expected ~S", obj);
	  }
     }
     return (kiss_obj*)obj;
}

inline kiss_obj* Kiss_List(const kiss_obj* const obj) {
     if (obj != KISS_NIL && !KISS_IS_CONS(obj)) {
	  if (is_condition_working()) {
	       kiss_c_funcall(L"kiss::assure", kiss_c_list(2, kiss_symbol(L"<list>"), obj));
	  } else {
	       Kiss_Err(L"List expected ~S", obj);
	  }
     }
     return (kiss_obj*)obj;
}

inline long int Kiss_Non_Negative_Integer(const kiss_obj* const obj) {
    const kiss_ptr_int i = Kiss_Fixnum(obj);
    if (i < 0) {
	 Kiss_Err(L"Non negative integer expected ~S", obj);
    }
    return i;
}

inline long int Kiss_Non_Zero_Integer(const kiss_obj* const obj) {
    const kiss_ptr_int i = Kiss_Fixnum(obj);
    if (i == 0) {
	 Kiss_Err(L"Non zero integer expected ~S", obj);
    }
    return i;
}

inline kiss_obj* Kiss_General_Array(const kiss_obj* const obj) {
     if (!KISS_IS_GENERAL_VECTOR(obj) && !KISS_IS_GENERAL_ARRAY(obj)) {
          Kiss_Err(L"general array(<general-vector> or <general-array*>) expected ~S", obj);
     }
     return (kiss_obj*)obj;
}

inline kiss_obj* Kiss_Basic_Array(const kiss_obj* const obj) {
     if (!KISS_IS_GENERAL_VECTOR(obj) && !KISS_IS_GENERAL_ARRAY(obj) && !KISS_IS_STRING(obj)) {
          if (is_condition_working()) {
               kiss_c_funcall(L"kiss::assure", kiss_c_list(2, kiss_symbol(L"<basic-array>"), obj));
          } else {
	       Kiss_Err(L"<basic-array> expected ~S", obj);
          }
     }
     return (kiss_obj*)obj;
}

inline kiss_obj* Kiss_Valid_Sequence_Index(const kiss_obj* const sequence, const kiss_obj* const index)
{
     const size_t n = kiss_c_length(sequence);
     const kiss_ptr_int i = Kiss_Fixnum(index);
     if (i < 0 || i >= n) {
          Kiss_Err(L"Invalid sequence index ~S ~S", sequence, index);
     }
     return (kiss_obj*)index;
}

kiss_obj* Kiss_Sequence(const kiss_obj* const obj) {
     if (!KISS_IS_SEQUENCE(obj)) {
          Kiss_Err(L"Sequence expected ~S", obj);
     }
     return (kiss_obj*)obj;
}

/* Proper list is a list terminated by the empty list. (The empty list is a proper list.) */
inline kiss_obj* Kiss_Proper_List(const kiss_obj* const obj) {
     const kiss_obj* p = obj;
     while (KISS_IS_CONS(p)) { p = KISS_CDR(p); }
     if (p != KISS_NIL) {
          Kiss_Err(L"Proper list expected ~S", obj);
     }
     return (kiss_obj*)obj;
}

inline kiss_cons_t* Kiss_Proper_List_2(const kiss_obj* const obj) {
    Kiss_Proper_List(obj);
    if (kiss_c_length(obj) != 2) {
	Kiss_Err(L"Proper list of length 2 expected ~S", obj);
    }
    return (kiss_cons_t*)obj;
}

inline kiss_oo_obj_t* Kiss_Object(const kiss_obj* const obj) {
    if (!KISS_IS_OBJECT(obj)) { Kiss_Err(L"ILOS object expected ~S", obj); }
    return (kiss_oo_obj_t*)obj;
}

inline kiss_stream_t* Kiss_Input_Char_Stream(const kiss_obj* const obj) {
    if (!KISS_IS_INPUT_STREAM(obj) || !KISS_IS_CHARACTER_STREAM(obj)) {
	Kiss_Err(L"Input character stream expected ~S", obj);
    }
    return (kiss_stream_t*)obj;
}

inline kiss_stream_t* Kiss_Output_Char_Stream(const kiss_obj* const obj) {
    if (!KISS_IS_OUTPUT_STREAM(obj) || !KISS_IS_CHARACTER_STREAM(obj)){
	Kiss_Err(L"Output character stream expected ~S", obj);
    }
    return (kiss_stream_t*)obj;
}

inline kiss_stream_t* Kiss_Input_Byte_Stream(const kiss_obj* const obj) {
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

