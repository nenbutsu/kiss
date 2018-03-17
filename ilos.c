/*  -*- coding: utf-8 -*-
  ilos.c --- defines ILOS mechanism of ISLisp processor KISS.

  Copyright (C) 2017, 2018 Yuji Minejima <yuji@minejima.jp>

  This file is part of ISLisp processor KISS.

  KISS is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  KISS is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details. */
#include "kiss.h"

/* Spec. p.14 Figure 1. Class Inheritance  
   https://nenbutsu.github.io/ISLispHyperDraft/islisp-v23.html#figure_1

    <object>
       |
       +--> <basic-array>
       |        |
       |        +--> <basic-array*>
       |        |        |
       |        |        +--> <general-array*>
       |        |
       |        +--> <basic-vector>
       |                 |
       |                 +--> <general-vector>
       |                 +--> <string>
       |
       +--> <built-in-class>
       +--> <character>
       +--> <function>
       |        |
       |        +--> <generic-function>
       |                 |
       |                 +--> <standard-generic-function>
       +--> <list>
       |        |
       |        +--> <cons>
       |        +--> <null> cpl = (<null> <symbol> <list> <object>)
       |                 ^
       |                 |
       |        +--------+
       |        |
       +--> <symbol>
       |
       +--> <number>
       |        |
       |        +--> <float>
       |        +--> <integer>
       |
       +--> <serious-condition>
       |        |
       |        +--> <error>
       |        |       |
       |        |       +--> <arithmetic-error>
       |        |       |           |
       |        |       |           +--> <division-by-zero>
       |        |       |           +--> <floating-point-overflow>
       |        |       |           +--> <floating-point-underflow>
       |        |       |
       |        |       +--> <control-error>
       |        |       +--> <parse-error>
       |        |       +--> <program-error>
       |        |       |           |
       |        |       |           +--> <domain-error>
       |        |       |           +--> <undefined-entity>
       |        |       |                     |
       |        |       |                     +--> <unbound-variable>
       |        |       |                     +--> <undefined-function>
       |        |       +--> <simple-error>
       |        |       +--> <stream-error>
       |        |                   |
       |        |                   +--> <end-of-stream>
       |        +--> <storage-exhausted>
       |
       +--> <standard-class>
       +--> <standard-object>
       +--> <stream>
 */

// <object>
kiss_ilos_class_t KISS_ILOS_CLASS_built_in_class;

kiss_ilos_class_t KISS_ILOS_CLASS_object = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_object,                     // name
     KISS_T,                                              // abstractp
     KISS_NIL,                                            // cpl
};


// <built-in-class>
kiss_cons_t KISS_ILOS_cpl01 = {
     KISS_CONS,
     NULL, // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_object,
     KISS_NIL,
};

kiss_ilos_class_t KISS_ILOS_CLASS_built_in_class = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_built_in_class,             // name
     KISS_T,                                              // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01,                         // cpl
};

// <character>
kiss_ilos_class_t KISS_ILOS_CLASS_character = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_character,                  // name
     KISS_NIL,                                            // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01,                         // cpl
};

// <symbol>
kiss_ilos_class_t KISS_ILOS_CLASS_symbol = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_symbol,                     // name
     KISS_NIL,                                            // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01,                         // cpl
};

// <list>
kiss_ilos_class_t KISS_ILOS_CLASS_list = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_list,                       // name
     KISS_T,                                              // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01,                         // cpl
};

// <cons>
kiss_cons_t KISS_ILOS_cpl02 = {
     KISS_CONS,
     NULL, // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_list,
     (kiss_obj*)&KISS_ILOS_cpl01,
};

kiss_ilos_class_t KISS_ILOS_CLASS_cons = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_cons,                       // name
     KISS_NIL,                                            // abstractp
     (kiss_obj*)&KISS_ILOS_cpl02,                         // cpl
};

// <null>
kiss_cons_t KISS_ILOS_cpl03 = {
     KISS_CONS,
     NULL, // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_symbol,
     (kiss_obj*)&KISS_ILOS_cpl02,
};

kiss_ilos_class_t KISS_ILOS_CLASS_null = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_null,                       // name
     KISS_T,                                              // abstractp
     (kiss_obj*)&KISS_ILOS_cpl03,                         // cpl
};

// <standard-class>
kiss_ilos_class_t KISS_ILOS_CLASS_standard_class = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_standard_class,             // name
     KISS_T,                                              // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01,                         // cpl
};

// <standard-object> spec. p.13
// The class named <standard-object> is an instance of the class
// <standard-class> and is a superclass of every class that is an
// instance of <standard-class> except itself. 
kiss_ilos_class_t KISS_ILOS_CLASS_standard_object = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_standard_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_standard_object,            // name
     KISS_T,                                              // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01,                         // cpl
};

// <stream>
kiss_ilos_class_t KISS_ILOS_CLASS_stream = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_stream,                     // name
     KISS_NIL,                                            // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01,                         // cpl
};

// <function>
kiss_ilos_class_t KISS_ILOS_CLASS_function = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_function,                   // name
     KISS_NIL,                                            // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01,                         // cpl
};

// <generic-function>
kiss_cons_t KISS_ILOS_cpl04 = {
     KISS_CONS,
     NULL, // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_function,
     (kiss_obj*)&KISS_ILOS_cpl02,
};

kiss_ilos_class_t KISS_ILOS_CLASS_generic_function = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_generic_function,           // name
     KISS_T,                                              // abstractp
     (kiss_obj*)&KISS_ILOS_cpl04,                         // cpl
};

// <standard-generic-function>
kiss_cons_t KISS_ILOS_cpl05 = {
     KISS_CONS,
     NULL, // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_generic_function,
     (kiss_obj*)&KISS_ILOS_cpl04,
};

kiss_ilos_class_t KISS_ILOS_CLASS_standard_generic_function = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_standard_generic_function,  // name
     KISS_NIL,                                            // abstractp
     (kiss_obj*)&KISS_ILOS_cpl05,                         // cpl
};

// <standard-method>
kiss_ilos_class_t KISS_ILOS_CLASS_standard_method = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_standard_method,            // name
     KISS_NIL,                                            // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01,                         // cpl
};

// <number>
kiss_ilos_class_t KISS_ILOS_CLASS_number = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_number,                     // name
     KISS_T,                                              // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01,                         // cpl
};

// <integer>
kiss_cons_t KISS_ILOS_cpl06 = {
     KISS_CONS,
     NULL, // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_number,
     (kiss_obj*)&KISS_ILOS_cpl01,
};

kiss_ilos_class_t KISS_ILOS_CLASS_integer = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_integer,                    // name
     KISS_NIL,                                            // abstractp
     (kiss_obj*)&KISS_ILOS_cpl06,                         // cpl
};

// <float>
kiss_ilos_class_t KISS_ILOS_CLASS_float = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_float,                      // name
     KISS_NIL,                                            // abstractp
     (kiss_obj*)&KISS_ILOS_cpl06,                         // cpl
};

// <basic-array>
kiss_ilos_class_t KISS_ILOS_CLASS_basic_array = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_basic_array,                // name
     KISS_T,                                              // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01,                         // cpl
};

// <basic-array*>
kiss_cons_t KISS_ILOS_cpl07 = {
     KISS_CONS,
     NULL, // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_basic_array,
     (kiss_obj*)&KISS_ILOS_cpl01,
};

kiss_ilos_class_t KISS_ILOS_CLASS_basic_array_s = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_basic_array_s,              // name
     KISS_T,                                              // abstractp
     (kiss_obj*)&KISS_ILOS_cpl07,                         // cpl
};

// <general-array*>
kiss_cons_t KISS_ILOS_cpl08 = {
     KISS_CONS,
     NULL, // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_basic_array_s,
     (kiss_obj*)&KISS_ILOS_cpl07,
};

kiss_ilos_class_t KISS_ILOS_CLASS_general_array_s = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_general_array_s,            // name
     KISS_NIL,                                            // abstractp
     (kiss_obj*)&KISS_ILOS_cpl08,                         // cpl
};

// <basic-vector>
kiss_ilos_class_t KISS_ILOS_CLASS_basic_vector = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_basic_vector,               // name
     KISS_T,                                              // abstractp
     (kiss_obj*)&KISS_ILOS_cpl07,                         // cpl
};

// <general-vector>
kiss_cons_t KISS_ILOS_cpl09 = {
     KISS_CONS,
     NULL, // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_basic_vector,
     (kiss_obj*)&KISS_ILOS_cpl07,
};

kiss_ilos_class_t KISS_ILOS_CLASS_general_vector = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_general_vector,             // name
     KISS_NIL,                                            // abstractp
     (kiss_obj*)&KISS_ILOS_cpl09,                         // cpl
};

// <string>
kiss_ilos_class_t KISS_ILOS_CLASS_string = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_string,                     // name
     KISS_NIL,                                            // abstractp
     (kiss_obj*)&KISS_ILOS_cpl09,                         // cpl
};

// <hash-table>
kiss_ilos_class_t KISS_ILOS_CLASS_hash_table = {
     KISS_ILOS_CLASS,                                     // type
     NULL,                                                // gc_ptr
     (kiss_ilos_class_t*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL,                                            // slots
     (kiss_symbol_t*)&KISS_Sc_hash_table,                // name
     KISS_T,                                              // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01,                         // cpl
};

// ---------------------------------
kiss_ilos_obj_t* kiss_make_ilos_obj(const kiss_ilos_class_t* const class) {
    kiss_ilos_obj_t* obj = Kiss_GC_Malloc(sizeof(kiss_ilos_obj_t));
    obj->type  = KISS_ILOS_OBJ;
    obj->class = (kiss_ilos_class_t*)class;
    obj->slots = KISS_NIL;
    return obj;
}

kiss_obj* kiss_compute_cpl(const kiss_obj* const supers);
extern kiss_ilos_class_t KISS_ILOS_CLASS_standard_class;

kiss_ilos_class_t* kiss_make_ilos_class(const kiss_symbol_t* const name,
                                        const kiss_obj* const supers)
{
    kiss_ilos_class_t* class = Kiss_GC_Malloc(sizeof(kiss_ilos_class_t));
    class->type       = KISS_ILOS_CLASS;
    class->class      = &KISS_ILOS_CLASS_standard_class;
    class->slot_specs = KISS_NIL;
    class->name       = (kiss_symbol_t*)name;
    class->abstractp  = KISS_NIL;
    class->cpl        = kiss_cons((kiss_obj*)class, kiss_compute_cpl(supers));
    return class;
}

extern kiss_symbol_t KISS_Sstandard;

kiss_generic_function_t* kiss_make_generic_function(const kiss_symbol_t* const name) {
    kiss_generic_function_t* gf = Kiss_GC_Malloc(sizeof(kiss_generic_function_t));
    gf->type               = KISS_GENERIC_FUNCTION;
    gf->name               = (kiss_symbol_t*)name;
    gf->lambda_list        = KISS_NIL;
    gf->method_combination = (kiss_obj*)&KISS_Sstandard;
    gf->around_methods     = KISS_NIL;
    gf->before_methods     = KISS_NIL;
    gf->primary_methods    = KISS_NIL;
    gf->after_methods      = KISS_NIL;
    return gf;
}

kiss_method_t* kiss_make_method(void) {
     kiss_method_t* method = Kiss_GC_Malloc(sizeof(kiss_method_t));
     method->type         = KISS_METHOD;
     method->specializers = KISS_NIL;
     method->qualifier    = KISS_NIL;
     method->fun          = NULL;
     return method;
}

/* special operator: (class class-name) -> <class>
   Returns the class object that corresponds to the class named CLASS-NAME.
   On error, signal <undefined-entity> see spec. p.119 */
kiss_obj* kiss_class(const kiss_obj* const name) {
     kiss_ilos_class_t* class = Kiss_Symbol(name)->class;
     if (class) {
          return (kiss_obj*)Kiss_Class((kiss_obj*)class);
     } else {
          Kiss_Err(L"Undefined Class: ~S", name);
     }
}

/* function: (class-of obj) -> <class>
   Returns the class of which the given OBJ is a direct instance.
   OBJ may be any ISLISP object. */
kiss_obj* kiss_class_of(const kiss_obj* const obj) {
	  switch (KISS_OBJ_TYPE(obj)) {
	  case KISS_CONS:
	       return (kiss_obj*)&KISS_ILOS_CLASS_cons;
	  case KISS_SYMBOL:
               if (obj == KISS_NIL) {
                    return (kiss_obj*)&KISS_ILOS_CLASS_null;
               } else {
                    return (kiss_obj*)&KISS_ILOS_CLASS_symbol;
               }
	  case KISS_CHARACTER:
               return (kiss_obj*)&KISS_ILOS_CLASS_character;
	  case KISS_FIXNUM:
          case KISS_BIGNUM:
	       return (kiss_obj*)&KISS_ILOS_CLASS_integer;
	  case KISS_FLOAT:
	       return (kiss_obj*)&KISS_ILOS_CLASS_float;
	  case KISS_STRING:
	       return (kiss_obj*)&KISS_ILOS_CLASS_string;
	  case KISS_GENERAL_VECTOR:
	       return (kiss_obj*)&KISS_ILOS_CLASS_general_vector;
	  case KISS_GENERAL_ARRAY_S:
               return (kiss_obj*)&KISS_ILOS_CLASS_general_array_s;
	  case KISS_HASH_TABLE:
               return (kiss_obj*)&KISS_ILOS_CLASS_hash_table;
	  case KISS_STREAM:
               return (kiss_obj*)&KISS_ILOS_CLASS_stream;
	  case KISS_LFUNCTION:
	  case KISS_LMACRO:
	  case KISS_CFUNCTION:
	  case KISS_CMACRO:
	  case KISS_CSPECIAL:
               return (kiss_obj*)&KISS_ILOS_CLASS_function;
          case KISS_GENERIC_FUNCTION:
               return (kiss_obj*)&KISS_ILOS_CLASS_standard_generic_function;
	  case KISS_ILOS_OBJ:
          case KISS_ILOS_CLASS:
               return (kiss_obj*)((kiss_ilos_obj_t*)obj)->class;
	  case KISS_CATCHER:
	  case KISS_BLOCK:
	  case KISS_CLEANUP:
	  case KISS_TAGBODY:
          case KISS_METHOD:
          case KISS_GF_INVOCATION:
	       fwprintf(stderr, L"class-of: unexpected internal built-in primitive type = %d\n",
                        KISS_OBJ_TYPE(obj));
	       exit(EXIT_FAILURE);
	  default:
	       fwprintf(stderr, L"class-of: unknown primitive object type = %d\n",
                        KISS_OBJ_TYPE(obj));
	       exit(EXIT_FAILURE);
	  }
}

kiss_obj* kiss_slotref(const kiss_obj* const obj, const kiss_obj* const name) {
     const kiss_obj* const binding = kiss_assoc(name, Kiss_ILOS_Obj(obj)->slots);
     if (binding == KISS_NIL) {
          Kiss_Err(L"Unbound slot: ~S", name);
     } else {
          return kiss_cdr(binding);
     }
}

kiss_obj* kiss_slot_bound_p(const kiss_obj* const obj, const kiss_obj* const name) {
     const kiss_obj* const binding = kiss_assoc(name, Kiss_ILOS_Obj(obj)->slots);
     return kiss_consp(binding);
}

kiss_obj* kiss_set_slotref(const kiss_obj* const value, kiss_obj* const obj, kiss_obj* const name)
{
     kiss_ilos_obj_t* const ilos_obj = Kiss_ILOS_Obj(obj);
     kiss_obj* slots = ilos_obj->slots;
     kiss_obj* binding = kiss_assoc(name, slots);
     if (binding == KISS_NIL) {
          binding = kiss_cons(name, value);
          ilos_obj->slots = kiss_cons(binding, slots);
     } else {
          kiss_set_cdr(value, binding);
     }
     return (kiss_obj*)value;
}

kiss_symbol_t KISS_Skw_abstractp;
kiss_symbol_t KISS_Skw_metaclass;

/* defining operator: (defclass class-name (sc-name*) (slot-spec*) class-opt*) -> <symbol>
   class-name ::= identifier
   sc-name    ::= identifier
   slot-spec  ::= slot-name | (slot-name slot-opt*)
   slot-name  ::= identifier
   slot-opt   ::= :reader reader-function-name |
                  :writer writer-function-name |
                  :accessor reader-function-name |
                  :boundp boundp-function-name |
                  :initform form |
                  :initarg initarg-name
   initarg-name         ::= identifier
   reader-function-name ::= identifier
   writer-function-name ::= identifier
   boundp-function-name ::= identifier
   class-opt ::= (:metaclass class-name) | (:abstractp abstract-flag)
   abstractp ::= t | nil */
kiss_obj* kiss_defclass(const kiss_obj* const name, const kiss_obj* const supers,
                        const kiss_obj* const slot_specs, const kiss_obj* const class_opts)
{
     kiss_symbol_t* const symbol = Kiss_Symbol(name);
     Kiss_List(supers);
     Kiss_List(slot_specs);
     kiss_ilos_class_t* class = kiss_make_ilos_class(symbol, supers);

     kiss_obj* abstractp = kiss_assoc((kiss_obj*)&KISS_Skw_abstractp, (kiss_obj*)class_opts);
     if (KISS_IS_CONS(abstractp)) {
          abstractp = KISS_CADR(abstractp);
     }
     class->abstractp = abstractp;

     kiss_obj* metaclass = kiss_assoc((kiss_obj*)&KISS_Skw_metaclass, class_opts);
     if (KISS_IS_CONS(metaclass)) {
          metaclass = KISS_CADR(metaclass);
          Kiss_Class(metaclass);
     } else {
          metaclass = (kiss_obj*)&KISS_ILOS_CLASS_standard_class;
     }
     class->class = (kiss_ilos_class_t*)metaclass;

     symbol->class = class;
     return (kiss_obj*)name;
}


/* function: (subclassp subclass superclass) -> boolean
   Returns t if the class SUBCLASS is a subclass of the class SUPERCLASS;
   otherwise, returns nil. An error shall be signaled if either SUBCLASS or
   SUPERCLASS is not a class object (error-id. domain-error). */
kiss_obj* kiss_subclassp(const kiss_obj* const sub, const kiss_obj* const super) {
     kiss_ilos_class_t* a = Kiss_Class(sub);
     Kiss_Class(super);
     kiss_obj* cpl = a->cpl;
     return kiss_member(super, cpl) != KISS_NIL ? KISS_T : KISS_NIL;
}

/* spec. p. 51
   Let C1, . . . , Cn be the direct superclasses of C in the order defined in
   the defclass defining form for C. Let P1, . . ., Pn be the class precedence
   lists for C1, . . . , Cn, respectively. Define P . Q on class precedence
   lists P and Q to be the two lists appended. Then the class precedence
   list for C is C . P1 . . . . Pn with duplicate classes removed by
   repeated application of the following rule: If a class appears twice in
   the resulting class precedence list, the leftmost occurrence is removed. */
kiss_obj* kiss_compute_cpl(const kiss_obj* const supers) {
     kiss_obj* head = kiss_cons(KISS_NIL, KISS_NIL);
     kiss_obj* tail = head;
     for (const kiss_obj* p = supers; KISS_IS_CONS(p); p = KISS_CDR(p)) {
          kiss_ilos_class_t* s = Kiss_Class(KISS_CAR(p));
          kiss_obj* cpl = s->cpl;
          kiss_set_cdr(kiss_cons(cpl, KISS_NIL), tail);
          tail = KISS_CDR(tail);
     }
     kiss_obj* list = kiss_append(KISS_CDR(head));
     tail = head;
     for (const kiss_obj* p = list; KISS_IS_CONS(p); p = KISS_CDR(p)) {
          kiss_obj* obj = KISS_CAR(p);
          if (kiss_member(obj, KISS_CDR(p)) == KISS_NIL) {
               kiss_set_cdr(kiss_cons(obj, KISS_NIL), tail);
               tail = KISS_CDR(tail);
          }
     }
     return KISS_CDR(head);
}

/* function: (instancep obj class) -> boolean
   Returns t if OBJ is an instance (directly or otherwise) of the class CLASS; 
   otherwise, returns nil. OBJ may be any ISLISP object. 
   An error shall be signaled if CLASS is not a class object (error-id. domain-error ). */
kiss_obj* kiss_instancep(const kiss_obj* const obj, const kiss_obj* const class) {
     Kiss_Class(class);
     kiss_obj* c = kiss_class_of(obj);
     return c == class || kiss_subclassp(c, class) ? KISS_T : KISS_NIL;
}

/* function: (generic-function-p obj ) -> boolean
   Returns t if OBJ is a generic function; otherwise, returns nil.
   OBJ may be any ISLISP object. */
kiss_obj* kiss_generic_function_p(const kiss_obj* const obj) {
     return KISS_IS_GENERIC_FUNCTION(obj) ? KISS_T : KISS_NIL;
}

static kiss_string_t setf_prefix = {
     KISS_STRING,   /* type */
     NULL,          /* gc_ptr */
     L"kiss::set-", /* name */
     10,            /* size */
};

kiss_symbol_t* kiss_make_setf_name(kiss_symbol_t* symbol) {
     kiss_obj* args = kiss_c_list(2, &setf_prefix, kiss_make_string(symbol->name));
     kiss_obj* name = kiss_string_append(args);
     return (kiss_symbol_t*)kiss_intern(name);
}

extern kiss_symbol_t KISS_Ssetf;
kiss_symbol_t* Kiss_Func_Spec(const kiss_obj* const obj) {
     if (KISS_IS_SYMBOL(obj)) { return (kiss_symbol_t*)obj; }
     if (!KISS_IS_CONS(obj)) {
          Kiss_Err(L"Invalid func-spec: ~S", obj);
     }
     if (KISS_CAR(obj) != (kiss_obj*)&KISS_Ssetf ||
         !KISS_IS_CONS(KISS_CDR(obj)) ||
         !KISS_IS_SYMBOL(KISS_CAR(KISS_CDR(obj))) ||
         KISS_CDDR(obj) != KISS_NIL)
     {
          Kiss_Err(L"Invalid func-spec: ~S", obj);
     }
     return kiss_make_setf_name((kiss_symbol_t*)KISS_CADR(obj));
}

/* defining operator: (defgeneric func-spec lambda-list {option | method-desc}*) -> <symbol>
   func-spec   ::= identifier | (setf identifier)
   lambda-list ::= (var* [&rest var]) | (var* [:rest var])
   option      ::= (:method-combination {identifier | keyword}) |
                   (:generic-function-class class-name)
   method-desc ::= (:method method-qualifier* parameter-profile form*)
   method-qualifier  ::= identifier | keyword
   parameter-profile ::= ({var | (var parameter-specializer-name)}*
                          [{&rest | :rest} var])
   parameter-specializer-name ::= class-name
   class-name ::= identifier
   var ::= identifier                                                              */
kiss_obj* kiss_defgeneric(const kiss_obj* const func_spec,
                          const kiss_obj* const lambda_list, const kiss_obj* const rest)
{
     kiss_symbol_t* name = Kiss_Func_Spec(func_spec);
     kiss_generic_function_t* gf = kiss_make_generic_function(name);
     gf->lambda_list = Kiss_Lambda_List(lambda_list);
     
     name->fun = (kiss_obj*)gf;
     return (kiss_obj*)name;
}

int kiss_compare_specializers(const kiss_obj* q1, const kiss_obj* const q2) {
     const kiss_obj* p2 = q2;
     for (const kiss_obj* p1 = q1; KISS_IS_CONS(p1); p1 = KISS_CDR(p1)) {
          kiss_obj* c1 = KISS_CAR(p1);
          if (!KISS_IS_CONS(p2)) {
               Kiss_Err(L"kiss_compare_specializers: two specializers of different length: ~S, ~S",
                        q1, q2);
          }
          kiss_obj* c2 = KISS_CAR(p2);
          if (kiss_subclassp(c1, c2) == KISS_T) {
               return -1;
          } else if (c1 != c2) {
               return 1;
          }
          p2 = KISS_CDR(p2);
     }
     return 0;
}

extern kiss_symbol_t KISS_Skw_around, KISS_Skw_before, KISS_Skw_after;

void kiss_add_method(kiss_generic_function_t* const gf, const kiss_method_t* const method) {
     kiss_obj* qualifier = method->qualifier;
     kiss_obj* list = KISS_NIL;
     if (qualifier == KISS_NIL) {
          list = gf->primary_methods;
     } else if (qualifier == (kiss_obj*)&KISS_Skw_around) {
          list = gf->around_methods;
     } else if (qualifier == (kiss_obj*)&KISS_Skw_before) {
          list = gf->before_methods;
     } else if (qualifier == (kiss_obj*)&KISS_Skw_after) {
          list = gf->after_methods;
     } else {
          Kiss_Err(L"unknown method qualifier: ~S", qualifier);
     }

     kiss_obj* head = kiss_cons(KISS_NIL, KISS_NIL);
     kiss_obj* tail = head;
     for (const kiss_obj* p = list; KISS_IS_CONS(p); p = KISS_CDR(p)) {
          kiss_method_t* m = Kiss_Method(KISS_CAR(p));
          int result = kiss_compare_specializers(method->specializers, m->specializers);
          if (result < 0) {
               kiss_set_cdr(kiss_cons((kiss_obj*)method, p), tail);
               break;
          } else if (result == 0) {
               kiss_set_cdr(kiss_cons((kiss_obj*)method, KISS_CDR(p)), tail);
               break;
          } else {
               if (KISS_CDR(p) == KISS_NIL) {
                    kiss_set_cdr(kiss_cons((kiss_obj*)method, KISS_NIL), tail);
                    break;
               } else {
                    kiss_set_cdr(kiss_cons(KISS_CAR(p), KISS_NIL), tail);
                    tail = KISS_CDR(tail);
               }
          }
     }

     head = KISS_CDR(head);
     if (qualifier == KISS_NIL) {
          gf->primary_methods = head;
     } else if (qualifier == (kiss_obj*)&KISS_Skw_around) {
          gf->around_methods = head;
     } else if (qualifier == (kiss_obj*)&KISS_Skw_before) {
          gf->before_methods = head;
     } else {
          gf->after_methods = head;
     }
     return;
}

/* local function: (call-next-method) -> <object> */
/* local function: (next-method-p) -> boolean */
extern kiss_symbol_t KISS_Scall_next_method;
extern kiss_symbol_t KISS_Snext_method_p;
kiss_lexical_environment_t Kiss_Null_Lexical_Env = {
    KISS_NIL, /* vars */
    KISS_NIL, /* funcs */
    KISS_NIL, /* jumpers */
};

kiss_obj* kiss_next_method_p(void) {
     kiss_environment_t* env = Kiss_Get_Environment();
     kiss_obj* gf_invocations = env->dynamic_env.gf_invocations;
     if (!KISS_IS_CONS(gf_invocations)) {
          Kiss_Err(L"next-method-p: internal error: gf_invocations are empty");
     }
     kiss_gf_invocation_t* gf_inv = KISS_CAR(gf_invocations);
     return KISS_IS_CONS(gf_inv->next_methods) ? KISS_T : KISS_NIL;
}
kiss_cfunction_t KISS_CFnext_method_p = {
     KISS_CFUNCTION,                 /* type */
     &KISS_Snext_method_p,           /* name */
     (kiss_cf_t*)kiss_next_method_p, /* C function name */
     0,                              /* minimum argument number */
     0,                              /* maximum argument number */
};

kiss_obj* kiss_call_next_method(void) {
     kiss_environment_t* env = Kiss_Get_Environment();
     kiss_obj* gf_invocations = env->dynamic_env.gf_invocations;
     if (!KISS_IS_CONS(gf_invocations)) {
          Kiss_Err(L"call-next-method: internal error: gf_invocations are empty");
     }
     kiss_gf_invocation_t* gf_inv = KISS_CAR(gf_invocations);
     kiss_obj* list = gf_inv->next_methods;
     if (!KISS_IS_CONS(list)) {
          Kiss_Err(L"Next method doesn't exist");
     }
     kiss_method_t* next_method = Kiss_Method(KISS_CAR(list));
     gf_inv->next_methods = KISS_CDR(list);
     return kiss_funcall(next_method->fun, gf_inv->args);
}

kiss_cfunction_t KISS_CFcall_next_method = {
     KISS_CFUNCTION,                    /* type */
     &KISS_Scall_next_method,           /* name */
     (kiss_cf_t*)kiss_call_next_method, /* C function name */
     0,                                 /* minimum argument number */
     0,                                 /* maximum argument number */
};

static kiss_obj* kiss_false(void) { return KISS_NIL; }
kiss_cfunction_t KISS_CFfalse = {
    KISS_CFUNCTION,         /* type */
    NULL,                   /* name */
    (kiss_cf_t*)kiss_false, /* C function name */
    0,                      /* minimum argument number */
    0,                      /* maximum argument number */
};


kiss_obj* kiss_next_method_error(void) {
    Kiss_Err(L"Next method doesn't exist");
}
kiss_cfunction_t KISS_CFnext_method_error = {
    KISS_CFUNCTION,                     /* type */
    NULL,                               /* name */
    (kiss_cf_t*)kiss_next_method_error, /* C function name */
    0,                                  /* minimum argument number */
    0,                                  /* maximum argument number */
};

kiss_cons_t KISS_LEX_ENV_call_next_method_error = {
     KISS_CONS,                            // type
     NULL,                                 // gc_ptr
     (kiss_obj*)&KISS_Scall_next_method,   // car
     (kiss_obj*)&KISS_CFnext_method_error, // cdr
};

kiss_cons_t KISS_LEX_ENV_next_method_p_false = {
     KISS_CONS,                       // type
     NULL,                            // gc_ptr
     (kiss_obj*)&KISS_Snext_method_p, // car
     (kiss_obj*)&KISS_CFfalse,        // cdr
};

kiss_cons_t KISS_LEX_ENV_call_next_method = {
     KISS_CONS,                           // type
     NULL,                                // gc_ptr
     (kiss_obj*)&KISS_Scall_next_method,  // car
     (kiss_obj*)&KISS_CFcall_next_method, // cdr
};

kiss_cons_t KISS_LEX_ENV_next_method_p = {
     KISS_CONS,                        // type
     NULL,                             // gc_ptr
     (kiss_obj*)&KISS_Snext_method_p,  // car
     (kiss_obj*)&KISS_CFnext_method_p, // cdr
};

kiss_cons_t KISS_LEX_ENV_funcs02 = {
     KISS_CONS,                                     // type
     NULL,                                          // gc_ptr
     (kiss_obj*)&KISS_LEX_ENV_next_method_p_false,  // car
     KISS_NIL,                                      // cdr
};

kiss_cons_t KISS_LEX_ENV_funcs01 = {
     KISS_CONS,                                        // type
     NULL,                                             // gc_ptr
     (kiss_obj*)&KISS_LEX_ENV_call_next_method_error,  // car
     (kiss_obj*)&KISS_LEX_ENV_funcs02,                 // cdr
};

kiss_cons_t KISS_LEX_ENV_funcs12 = {
     KISS_CONS,                               // type
     NULL,                                    // gc_ptr
     (kiss_obj*)&KISS_LEX_ENV_next_method_p,  // car
     KISS_NIL,                                // cdr
};

kiss_cons_t KISS_LEX_ENV_funcs11 = {
     KISS_CONS,                                 // type
     NULL,                                      // gc_ptr
     (kiss_obj*)&KISS_LEX_ENV_call_next_method, // car
     (kiss_obj*)&KISS_LEX_ENV_funcs12,          // cdr
};

kiss_ilos_class_t KISS_ILOS_CLASS_object;

kiss_obj* kiss_collect_specilizers(const kiss_obj* const parameter_profile) {
     kiss_obj* head = (kiss_obj*)kiss_cons(KISS_NIL, KISS_NIL);
     kiss_obj* tail = head;
     for (const kiss_obj* p = parameter_profile; KISS_IS_CONS(p); p = KISS_CDR(p)) {
          kiss_obj* obj = KISS_CAR(p);
          kiss_ilos_class_t* class = &KISS_ILOS_CLASS_object;
          if (KISS_IS_CONS(obj)) {
               class = Kiss_Class((kiss_obj*)Kiss_Symbol(kiss_cadr(obj)));
          } else if (obj == (kiss_obj*)&KISS_Skw_rest || obj == (kiss_obj*)&KISS_Samp_rest) {
               break;
          }
          kiss_set_cdr(kiss_cons((kiss_obj*)class, KISS_NIL), tail);
     }
     return (kiss_obj*)KISS_CDR(head);
}

kiss_obj* kiss_collect_lambda_list(const kiss_obj* const parameter_profile) {
     kiss_obj* head = (kiss_obj*)kiss_cons(KISS_NIL, KISS_NIL);
     kiss_obj* tail = head;
     for (const kiss_obj* p = parameter_profile; KISS_IS_CONS(p); p = KISS_CDR(p)) {
          kiss_obj* obj = KISS_CAR(p);
          kiss_obj* var = KISS_IS_CONS(obj) ? KISS_CAR(obj) : obj;
          Kiss_Symbol(var);
          kiss_set_cdr(kiss_cons(var, KISS_NIL), tail);
     }
     return Kiss_Lambda_List(KISS_CDR(head));
}

/* defining operator: (defmethod func-spec method-qualifier* parameter-profile form*) -> <symbol>
   func-spec ::= identifier | (setf identifier)
   method-qualifier ::= identifier | keyword
   parameter-profile ::= ({var | (var parameter-specializer-name)}* [{&rest | :rest} var])
   parameter-specializer-name ::= class-name
   class-name ::= identifier
   var ::= identifier                                                          */
kiss_obj* kiss_defmethod(const kiss_obj* const func_spec, const kiss_obj* const rest) {
     kiss_symbol_t* name = Kiss_Func_Spec(func_spec);
     const kiss_obj* p = rest;
     const kiss_obj* qualifier = kiss_car(rest);
     if (KISS_IS_CONS(qualifier)) {
          qualifier = KISS_NIL;
     } else {
          Kiss_Symbol(qualifier);
          if (qualifier != (kiss_obj*)&KISS_Skw_around &&
              qualifier != (kiss_obj*)&KISS_Skw_before &&
              qualifier != (kiss_obj*)&KISS_Skw_after  &&
              qualifier != KISS_NIL)
          {
               Kiss_Err(L"Invalid method qualifier: ~S", qualifier);
          }
          p = kiss_cdr(rest);
     }

     // skip other qualifiers
     for (; KISS_IS_CONS(p) && KISS_IS_SYMBOL(KISS_CAR(p)); p = KISS_CDR(p)) {}
     
     const kiss_obj* parameter_profile = Kiss_List(kiss_car(p));
     const kiss_obj* specializers = kiss_collect_specilizers(parameter_profile);
     const kiss_obj* lambda_list  = kiss_collect_lambda_list(parameter_profile);
     const kiss_obj* const body = kiss_cdr(p);
     kiss_generic_function_t* gf = Kiss_Generic_Function(name->fun);
     kiss_method_t* method = kiss_make_method();
     method->specializers = (kiss_obj*)specializers;
     method->qualifier = (kiss_obj*)qualifier;
     method->fun = kiss_lambda((kiss_obj*)lambda_list, (kiss_obj*)body);
     kiss_function_t* fun = (kiss_function_t*)method->fun;
     fun->lexical_env = Kiss_Null_Lexical_Env;
     if (qualifier == (kiss_obj*)&KISS_Skw_before || qualifier == (kiss_obj*)&KISS_Skw_after) {
          fun->lexical_env.funs = (kiss_obj*)&KISS_LEX_ENV_funcs01;
     } else {
          fun->lexical_env.funs = (kiss_obj*)&KISS_LEX_ENV_funcs11;
     }

     kiss_add_method(gf, method);
     return (kiss_obj*)func_spec;
}

