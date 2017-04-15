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

void Kiss_System_Error (void) {
    perror(NULL);
    abort();
}

void Kiss_Err(char* str, ...) {
    va_list args;
    char* p;
    kiss_obj* out = kiss_create_string_output_stream();
    kiss_obj* string;

    va_start(args, str);
    for (p = str; *p != '\0'; p++) {
	if (*p == '~' && *(p+1) == 'S') {
	    kiss_format_object(out, va_arg(args, kiss_obj*), KISS_T);
	    p++;
	} else {
	    kiss_format_char(out, (kiss_obj*)kiss_make_character(*p));
	}
    }
    va_end(args);

    string = kiss_get_output_stream_string(out);

    if (kiss_featurep(kiss_symbol("condition")) == KISS_T) {
	 kiss_cfuncall("error", kiss_clist(2, (kiss_obj*)kiss_make_string("~A"), string))
    } else {
	 kiss_throw(kiss_clist(2, kiss_symbol("quote"), kiss_symbol("kiss::error-tag")), string);
    }
}

/* function : (error error-string obj*) ¨ <object>
   An error shall be signaled. error-string and the objs are advice to the implementation
   about how the error message might be textually described (using format), but whether or
   not that advice is used is implementation defined.
   This is equivalent to:
   (signal-condition
     (create (class <simple-error>)
             'format-string error-string
             'format-arguments obj*)
     nil) */
kiss_obj* kiss_error(kiss_obj* error_string, kiss_obj* rest) {
    /* This version of error has limited capability and will be relieved by
       the fullfledged error defined later in lisp world. */
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_string_t* s = Kiss_String(error_string);
    kiss_obj* out = kiss_create_string_output_stream();
    char* p;
    for (p = s->str; *p != '\0'; p++) {
	if (*p == '~' && *(p+1) == 'S') {
	    kiss_format_object(out, kiss_car(rest), KISS_T);
	    rest = kiss_cdr(rest);
	    p++;
	} else {
	    kiss_format_char(out, (kiss_obj*)kiss_make_character(*p));
	}
    }
    kiss_throw(kiss_clist(2,
			  kiss_symbol("quote"),
			  kiss_symbol("kiss::error-tag")),
	       kiss_get_output_stream_string(out));
}

void Kiss_Cannot_Parse_Number_Error(kiss_obj* str) {
     Kiss_Err("Cannot parse number: ~S", str);
}

void Kiss_Division_By_Zero_Error(kiss_obj* i) {
     Kiss_Err("Division by zero: ~S", i);
}

void Kiss_Cannot_Chage_Constant_Error(kiss_obj* obj) {
     Kiss_Err("Cannot chage constant: ~S", obj);
}

void Kiss_End_Of_Stream_Error(kiss_obj* stream) {
    if (kiss_featurep(kiss_symbol("condition")) == KISS_T) {
	kiss_cfuncall("kiss::signal-end-of-stream",
		      kiss_clist(2, stream, KISS_NIL));
    } else {
	Kiss_Err("End of stream ~S", stream);
    }
}

void Kiss_Unbound_Variable_Error(kiss_obj* name) {
    if (kiss_featurep(kiss_symbol("condition")) == KISS_T) {
	kiss_cfuncall("kiss::signal-unbound-variable", kiss_clist(2, name, KISS_NIL));
    } else {
	Kiss_Err("Unbound variable ~S", name);
    }
}

void Kiss_Catcher_Not_Found_Error(kiss_obj* tag) {
    if (kiss_featurep(kiss_symbol("condition")) == KISS_T) {
	kiss_cfuncall("kiss::signal-catcher-not-found", kiss_clist(2, tag, KISS_NIL));
    } else {
	Kiss_Err("Catcher not found for ~S", tag);
    }
}

void Kiss_Block_Not_Found_Error(kiss_obj* tag) {
    if (kiss_featurep(kiss_symbol("condition")) == KISS_T) {
	kiss_cfuncall("kiss::signal-block-not-found",
		      kiss_clist(2, tag, KISS_NIL));
    } else {
	Kiss_Err("Block not found ~S", tag);
    }
}

void Kiss_Tagbody_Not_Found_Error(kiss_obj* tag) {
    if (kiss_featurep(kiss_symbol("condition")) == KISS_T) {
	kiss_cfuncall("kiss::signal-tagbody-not-found",
		      kiss_clist(2, tag, KISS_NIL));
    } else {
	Kiss_Err("Tagbody not found ~S", tag);
    }
}

void Kiss_Index_Out_Of_Range_Error(kiss_obj* sequence, kiss_obj* index) {
    if (kiss_featurep(kiss_symbol("condition")) == KISS_T) {
	kiss_cfuncall("kiss::signal-index-out-of-range",
		      kiss_clist(3, sequence, index, KISS_NIL));
    } else {
	Kiss_Err("Index out of range: ~S ~S", sequence, index);
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

/* assure certain object */
kiss_cons_t* Kiss_Cons(kiss_obj* obj) {
    if (!KISS_IS_CONS(obj)) {
	if (kiss_featurep(kiss_symbol("condition")) == KISS_T) {
	    kiss_cfuncall("kiss::assure",
			  kiss_clist(2, kiss_symbol("<cons>"), obj));
	} else {
	    Kiss_Err("Cons expected ~S", obj);
	}
    }
    return (kiss_cons_t*)obj;
}

kiss_obj* Kiss_List(kiss_obj* obj) {
    if (obj != KISS_NIL && !KISS_IS_CONS(obj)) {
	if (kiss_featurep(kiss_symbol("condition")) == KISS_T) {
	    kiss_cfuncall("kiss::assure",
			  kiss_clist(2, kiss_symbol("<list>"), obj));
	} else {
	    Kiss_Err("List expected ~S", obj);
	}
    }
    return obj;
}

kiss_obj* Kiss_Proper_List(kiss_obj* obj) {
    /* Proper list is a list terminated by the empty list. (The empty
       list is a proper list.) */
    kiss_obj* p = obj;
    while (KISS_IS_CONS(p)) { p = KISS_CDR(p); }
    if (p != KISS_NIL) {
	Kiss_Err("Proper list expected ~S", obj);
    }
    return obj;
}

kiss_cons_t* Kiss_Proper_List_2(kiss_obj* obj) {
    Kiss_Proper_List(obj);
    if (kiss_clength(obj) != 2) {
	Kiss_Err("Proper list of length 2 expected ~S", obj);
    }
    return (kiss_cons_t*)obj;
}

kiss_integer_t* Kiss_Integer(kiss_obj* obj) {
    if (!KISS_IS_INTEGER(obj)) { Kiss_Err("Integer expected ~S", obj); }
    return (kiss_integer_t*)obj;
}

kiss_obj* Kiss_Number(kiss_obj* obj) {
  if (!KISS_IS_INTEGER(obj) && !KISS_IS_FLOAT(obj)) {
      Kiss_Err("Number expected ~S", obj);
  }
  return obj;
}

kiss_float_t* Kiss_Float(kiss_obj* obj) {
    if (!KISS_IS_FLOAT(obj)) { Kiss_Err("Float expected ~S", obj); }
    return (kiss_float_t*)obj;
}

kiss_integer_t* Kiss_Non_Negative_Integer(kiss_obj* obj) {
    kiss_integer_t* i = Kiss_Integer(obj);
    if (i->i < 0) { Kiss_Err("Non negative integer expected ~S", obj); }
    return i;
}

kiss_integer_t* Kiss_Non_Zero_Integer(kiss_obj* obj) {
    kiss_integer_t* i = Kiss_Integer(obj);
    if (i->i == 0) { Kiss_Err("Non zero integer expected ~S", obj); }
    return i;
}

kiss_character_t* Kiss_Character(kiss_obj* obj) {
    if (!KISS_IS_CHARACTER(obj)) { Kiss_Err("Character expected ~S", obj); }
    return (kiss_character_t*)obj;
}

kiss_symbol_t* Kiss_Symbol(kiss_obj* obj) {
    if (!KISS_IS_SYMBOL(obj)) { Kiss_Err("Symbol expected ~S", obj); }
    return (kiss_symbol_t*)obj;
}

kiss_object_t* Kiss_Object(kiss_obj* obj) {
    if (!KISS_IS_OBJECT(obj)) { Kiss_Err("Object expected ~S", obj); }
    return (kiss_object_t*)obj;
}

kiss_string_t* Kiss_String(kiss_obj* obj) {
    if (!KISS_IS_STRING(obj)) { Kiss_Err("String expected ~S", obj); }
    return (kiss_string_t*)obj;
}

kiss_general_vector_t* Kiss_General_Vector(kiss_obj* obj) {
    if (!KISS_IS_GENERAL_VECTOR(obj)) {
	Kiss_Err("General vector expected ~S", obj);
    }
    return (kiss_general_vector_t*)obj;
}

kiss_stream_t* Kiss_Input_Char_Stream(kiss_obj* obj) {
    if (!KISS_IS_INPUT_STREAM(obj) || !KISS_IS_CHARACTER_STREAM(obj)) {
	Kiss_Err("Input character stream expected ~S", obj);
    }
    return (kiss_stream_t*)obj;
}

kiss_stream_t* Kiss_Output_Char_Stream(kiss_obj* obj) {
    if (!KISS_IS_OUTPUT_STREAM(obj) || !KISS_IS_CHARACTER_STREAM(obj)){
	Kiss_Err("Output character stream expected ~S", obj);
    }
    return (kiss_stream_t*)obj;
}

kiss_string_stream_t* Kiss_String_Output_Stream(kiss_obj* obj) {
    if (!KISS_IS_OUTPUT_STREAM(obj) || !KISS_IS_STRING_STREAM(obj)) {
	Kiss_Err("String output stream expected ~S", obj);
    }
    return (kiss_string_stream_t*)obj;
}

kiss_obj* Kiss_Sequence(kiss_obj* obj) {
    if (!KISS_IS_LIST(obj) && !KISS_IS_STRING(obj) &&
	!KISS_IS_GENERAL_VECTOR(obj))
    {
	Kiss_Err("Sequence expected ~S", obj);
    }
    return obj;
}

kiss_function_t* Kiss_Function(kiss_obj* obj) {
    if (!KISS_IS_FUNCTION(obj)) { Kiss_Err("Function expected ~S", obj); }
    return (kiss_function_t*)obj;
}

kiss_function_t* Kiss_Macro(kiss_obj* obj) {
/* implementation of macro currently uses function_t */
    if (!KISS_IS_MACRO(obj)) { Kiss_Err("Macro expected ~S", obj); }
    return (kiss_function_t*)obj;
}

kiss_cfunction_t* Kiss_CFunction(kiss_obj* obj) {
    if (!KISS_IS_CFUNCTION(obj)) { Kiss_Err("C function expected ~S", obj); }
    return (kiss_cfunction_t*)obj;
}

kiss_cfunction_t* Kiss_CMacro(kiss_obj* obj) {
    if (!KISS_IS_CMACRO(obj)) { Kiss_Err("C macro expected ~S", obj); }
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
		Kiss_Err(":rest must be followed by one variable name ~S",
			   list);
	    }
	    p = KISS_CDR(p);
	    name = Kiss_Symbol(KISS_CAR(p));
	}
	if (name == &KISS_Samp_rest) {
	    Kiss_Err(":rest cannot be used as a variable name ~S", list);
	}
	if (kiss_member((kiss_obj*)name, stack) != KISS_NIL) {
	    Kiss_Err("Same variable name ~S occurs more than once", name);
	}
	kiss_push((kiss_obj*)name, &stack);
    }
    return list;
}
                                                                                
/* lambda-expression :: = (lambda LAMBDA-LIST form*) */
kiss_obj* Kiss_Lambda_Expression(kiss_obj* p) {
    p = Kiss_Proper_List(p);
    if (kiss_clength(p) < 2 || KISS_CAR(p) != KISS_LAMBDA) {
	Kiss_Err("Illegal lambda expression ~S", p);
    }
    Kiss_Lambda_List(kiss_cadr(p));
    return p;
}

