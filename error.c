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

static int condition_working_p(void) {
     return kiss_featurep(kiss_symbol(L"condition")) == KISS_T;
}

void Kiss_System_Error (void) {
    perror(NULL);
    abort();
}

/* Called in signal-condition from Lisp world by the name kiss::err */
kiss_obj* kiss_err(kiss_obj* error_string, kiss_obj* rest) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_string_t* s = Kiss_String(error_string);
    kiss_obj* out = kiss_create_string_output_stream();
    wchar_t* p;
    for (p = s->str; *p != L'\0'; p++) {
	if (*p == L'~' && *(p+1) == L'S') {
	    kiss_format_object(out, kiss_car(rest), KISS_T);
	    rest = kiss_cdr(rest);
	    p++;
	} else {
	    kiss_format_char(out, (kiss_obj*)kiss_make_character(*p));
	}
    }
    kiss_throw(kiss_clist(2, kiss_symbol(L"quote"), kiss_symbol(L"kiss::error")),
	       kiss_get_output_stream_string(out));
}

void Kiss_Err(wchar_t* str, ...) {
    va_list args;
    wchar_t* p;
    kiss_obj* out = kiss_create_string_output_stream();
    kiss_obj* string;

    va_start(args, str);
    for (p = str; *p != L'\0'; p++) {
	if (*p == L'~' && *(p+1) == L'S') {
	    kiss_format_object(out, va_arg(args, kiss_obj*), KISS_T);
	    p++;
	} else {
	    kiss_format_char(out, (kiss_obj*)kiss_make_character(*p));
	}
    }
    va_end(args);

    string = kiss_get_output_stream_string(out);

    kiss_throw(kiss_clist(2, kiss_symbol(L"quote"), kiss_symbol(L"kiss::error")), string);
}


/* assure primitive type */
kiss_obj* kiss_assure(kiss_type t, kiss_obj* obj) {
     if (t == obj->type) {
	  return obj;
     } else {
	  if (condition_working_p()) {
	       kiss_cfuncall(L"kiss::assure", kiss_clist(2, kiss_type_to_class_name(t), obj));
	  } else {
	       Kiss_Err(L"~S expected ~S", kiss_type_to_class_name(t), obj);
	  }
     }
}

kiss_cons_t* Kiss_Cons(kiss_obj* obj) {
     kiss_assure(KISS_CONS, obj);
     return (kiss_cons_t*)obj;
}

kiss_integer_t* Kiss_Integer(kiss_obj* obj) {
     kiss_assure(KISS_INTEGER, obj);
     return (kiss_integer_t*)obj;
}

kiss_float_t* Kiss_Float(kiss_obj* obj) {
     kiss_assure(KISS_FLOAT, obj);
     return (kiss_float_t*)obj;
}

kiss_character_t* Kiss_Character(kiss_obj* obj) {
     kiss_assure(KISS_CHARACTER, obj);
     return (kiss_character_t*)obj;
}

kiss_symbol_t* Kiss_Symbol(kiss_obj* obj) {
     kiss_assure(KISS_SYMBOL, obj);
     return (kiss_symbol_t*)obj;
}

kiss_string_t* Kiss_String(kiss_obj* obj) {
     kiss_assure(KISS_STRING, obj);
    return (kiss_string_t*)obj;
}

kiss_general_vector_t* Kiss_General_Vector(kiss_obj* obj) {
     kiss_assure(KISS_GENERAL_VECTOR, obj);
    return (kiss_general_vector_t*)obj;
}

kiss_function_t* Kiss_Function(kiss_obj* obj) {
     kiss_assure(KISS_FUNCTION, obj);
     return (kiss_function_t*)obj;
}



/* assure non-primitive type */

kiss_obj* Kiss_Number(kiss_obj* obj) {
  if (!KISS_IS_INTEGER(obj) && !KISS_IS_FLOAT(obj)) {
      Kiss_Err(L"Number expected ~S", obj);
  }
  return obj;
}

kiss_obj* Kiss_List(kiss_obj* obj) {
     if (obj != KISS_NIL && !KISS_IS_CONS(obj)) {
	  if (condition_working_p()) {
	       kiss_cfuncall(L"kiss::assure", kiss_clist(2, kiss_symbol(L"<list>"), obj));
	  } else {
	       Kiss_Err(L"List expected ~S", obj);
	  }
     }
     return obj;
}

kiss_integer_t* Kiss_Non_Negative_Integer(kiss_obj* obj) {
    kiss_integer_t* i = Kiss_Integer(obj);
    if (i->i < 0) {
	 if (condition_working_p()) {
	      kiss_cfuncall(L"kiss::signal-non-negative-integer",
			    kiss_clist(2, (kiss_obj*)obj, KISS_NIL));
	 } else {
	      Kiss_Err(L"Non negative integer expected ~S", obj);
	 }
    }
    return i;
}

kiss_integer_t* Kiss_Non_Zero_Integer(kiss_obj* obj) {
    kiss_integer_t* i = Kiss_Integer(obj);
    if (i->i == 0) { Kiss_Err(L"Non zero integer expected ~S", obj); }
    return i;
}

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
     if (condition_working_p()) {
	  kiss_cfuncall(L"kiss::signal-unbound-variable", kiss_clist(2, name, KISS_NIL));
     } else {
	  Kiss_Err(L"Unbound variable ~S", name);
     }
}

void Kiss_Catcher_Not_Found_Error(kiss_obj* tag) {
     if (condition_working_p()) {
	  kiss_cfuncall(L"kiss::signal-catcher-not-found", kiss_clist(2, tag, KISS_NIL));
     } else {
	  Kiss_Err(L"Catcher not found for ~S", tag);
     }
}

void Kiss_Block_Not_Found_Error(kiss_obj* tag) {
     if (condition_working_p()) {
	  kiss_cfuncall(L"kiss::signal-block-not-found", kiss_clist(2, tag, KISS_NIL));
     } else {
	  Kiss_Err(L"Block not found ~S", tag);
     }
}

void Kiss_Tagbody_Not_Found_Error(kiss_obj* tag) {
     if (condition_working_p()) {
	  kiss_cfuncall(L"kiss::signal-tagbody-not-found", kiss_clist(2, tag, KISS_NIL));
     } else {
	  Kiss_Err(L"Tagbody not found ~S", tag);
     }
}

void Kiss_Index_Out_Of_Range_Error(kiss_obj* sequence, kiss_obj* index) {
     if (condition_working_p()) {
	  kiss_cfuncall(L"kiss::signal-index-out-of-range",
			kiss_clist(3, sequence, index, KISS_NIL));
     } else {
	  Kiss_Err(L"Index out of range: ~S ~S", sequence, index);
     }
}




kiss_obj* Kiss_Check_Sequence_Index_Range(kiss_obj* sequence, kiss_obj* index) {
    size_t n = kiss_clength(sequence);
    kiss_integer_t* i = Kiss_Integer(index);
    if (i->i < 0 || i->i >= n) {
	Kiss_Index_Out_Of_Range_Error(sequence, index);
    }
    return sequence;
}

kiss_obj* Kiss_Proper_List(kiss_obj* obj) {
    /* Proper list is a list terminated by the empty list. (The empty
       list is a proper list.) */
    kiss_obj* p = obj;
    while (KISS_IS_CONS(p)) { p = KISS_CDR(p); }
    if (p != KISS_NIL) {
	Kiss_Err(L"Proper list expected ~S", obj);
    }
    return obj;
}

kiss_cons_t* Kiss_Proper_List_2(kiss_obj* obj) {
    Kiss_Proper_List(obj);
    if (kiss_clength(obj) != 2) {
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

kiss_string_stream_t* Kiss_String_Output_Stream(kiss_obj* obj) {
    if (!KISS_IS_OUTPUT_STREAM(obj) || !KISS_IS_STRING_STREAM(obj)) {
	Kiss_Err(L"String output stream expected ~S", obj);
    }
    return (kiss_string_stream_t*)obj;
}

kiss_obj* Kiss_Sequence(kiss_obj* obj) {
     if (!KISS_IS_LIST(obj) && !KISS_IS_STRING(obj))
     {
	  Kiss_Err(L"Sequence expected ~S", obj);
     }
     return obj;
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
	    if (kiss_clength(p) != 2) {
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
    if (kiss_clength(p) < 2 || KISS_CAR(p) != KISS_LAMBDA) {
	Kiss_Err(L"Illegal lambda expression ~S", p);
    }
    Kiss_Lambda_List(kiss_cadr(p));
    return p;
}
