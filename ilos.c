/*  -*- coding: utf-8 -*-
  ilos.c --- defines ILOS mechanism of ISLisp processor KISS.

  Copyright (C) 2017, 2018 Yuji Minejima (yuji@minejima.jp).

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

kiss_ilos_obj_t* kiss_make_ilos_obj(const kiss_obj* const class) {
    kiss_ilos_obj_t* obj = Kiss_GC_Malloc(sizeof(kiss_ilos_obj_t));
    obj->type  = KISS_ILOS_OBJ;
    obj->class = (kiss_obj*)class;
    obj->slots = KISS_NIL;
    return obj;
}

kiss_obj* kiss_compute_cpl(const kiss_obj* const class, const kiss_obj* const supers);

kiss_ilos_class_t* kiss_make_ilos_class(const kiss_obj* const name, const kiss_obj* const supers)
{
    kiss_ilos_class_t* class = Kiss_GC_Malloc(sizeof(kiss_ilos_class_t));
    class->type      = KISS_ILOS_CLASS;
    class->class     = KISS_NIL;
    class->slots     = KISS_NIL;
    class->name      = name;
    class->abstractp = KISS_NIL;
    class->cpl       = kiss_cons(class, kiss_compute_cpl(supers));

    return class;
}

kiss_generic_function_t* kiss_make_generic_function(const kiss_obj* const name) {
    kiss_generic_function_t* gf = Kiss_GC_Malloc(sizeof(kiss_generic_function_t));
    gf->type               = KISS_GENERIC_FUNCTION;
    gf->name               = name;
    gf->lambda_list        = KISS_NIL;
    gf->method_combination = KISS_NIL;
    gf->around_methods     = KISS_NIL;
    gf->before_methods     = KISS_NIL;
    gf->primary_methods    = KISS_NIL;
    gf->after_methods      = KISS_NIL;
    return gf;
}

kiss_method_t* kiss_make_method(const kiss_obj* const name) {
     kiss_method_t* m = Kiss_GC_Malloc(sizeof(kiss_method_t));
     m->type         = KISS_METHOD;
     m->name         = (kiss_obj*)name;
     m->lambda       = KISS_NIL;
     m->specializers = KISS_NIL;
     m->qualifier    = KISS_NIL;
     return m;
}

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
kiss_ilos_obj_t KISS_ILOS_CLASS_built_in_class;

kiss_ilos_obj_t KISS_ILOS_CLASS_object = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_object, // name
     KISS_T, // abstractp
     KISS_NIL, // cpl
};


// <built-in-class>
kiss_cons_t KISS_ILOS_cpl01 = {
     KISS_CONS,
     NULL,        // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_object,
     KISS_NIL,
};

kiss_ilos_obj_t KISS_ILOS_CLASS_built_in_class = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_built_in_class, // name
     KISS_T, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01, // cpl
};

// <character>
kiss_ilos_obj_t KISS_ILOS_CLASS_character = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_character, // name
     KISS_NIL, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01, // cpl
};

// <symbol>
kiss_ilos_obj_t KISS_ILOS_CLASS_symbol = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_symbol, // name
     KISS_NIL, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01, // cpl
};

// <list>
kiss_ilos_obj_t KISS_ILOS_CLASS_list = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_list, // name
     KISS_T, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01, // cpl
};

// <cons>
kiss_cons_t KISS_ILOS_cpl02 = {
     KISS_CONS,
     NULL,        // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_list,
     (kiss_obj*)&KISS_ILOS_cpl01,
};

kiss_ilos_obj_t KISS_ILOS_CLASS_cons = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_cons, // name
     KISS_NIL, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl02, // cpl
};

// <null>
kiss_cons_t KISS_ILOS_cpl03 = {
     KISS_CONS,
     NULL,        // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_symbol,
     (kiss_obj*)&KISS_ILOS_cpl02,
};

kiss_ilos_obj_t KISS_ILOS_CLASS_null = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_null, // name
     KISS_T, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl03, // cpl
};

// <standard-class>
kiss_ilos_obj_t KISS_ILOS_CLASS_standard_class = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_standard_class, // name
     KISS_T, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01, // cpl
};

// <standard-object> spec. p.13
// The class named <standard-object> is an instance of the class
// <standard-class> and is a superclass of every class that is an
// instance of <standard-class> except itself. 
kiss_ilos_obj_t KISS_ILOS_CLASS_standard_object = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_standard_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_standard_object, // name
     KISS_T, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01, // cpl
};

// <stream>
kiss_ilos_obj_t KISS_ILOS_CLASS_stream = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_stream, // name
     KISS_NIL, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01, // cpl
};

// <function>
kiss_ilos_obj_t KISS_ILOS_CLASS_function = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_function, // name
     KISS_NIL, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01, // cpl
};

// <generic-function>
kiss_cons_t KISS_ILOS_cpl04 = {
     KISS_CONS,
     NULL,        // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_function,
     (kiss_obj*)&KISS_ILOS_cpl02,
};

kiss_ilos_obj_t KISS_ILOS_CLASS_generic_function = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_generic_function, // name
     KISS_T, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl04, // cpl
};

// <standard-generic-function>
kiss_cons_t KISS_ILOS_cpl05 = {
     KISS_CONS,
     NULL,        // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_generic_function,
     (kiss_obj*)&KISS_ILOS_cpl04,
};

kiss_ilos_obj_t KISS_ILOS_CLASS_standard_generic_function = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_standard_generic_function, // name
     KISS_NIL, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl05, // cpl
};

// <standard-method>
kiss_ilos_obj_t KISS_ILOS_CLASS_standard_method = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_standard_method, // name
     KISS_NIL, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01, // cpl
};

// <number>
kiss_ilos_obj_t KISS_ILOS_CLASS_number = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_number, // name
     KISS_T, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01, // cpl
};

// <integer>
kiss_cons_t KISS_ILOS_cpl06 = {
     KISS_CONS,
     NULL,        // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_number,
     (kiss_obj*)&KISS_ILOS_cpl01,
};

kiss_ilos_obj_t KISS_ILOS_CLASS_integer = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_integer, // name
     KISS_NIL, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl06, // cpl
};

// <float>
kiss_ilos_obj_t KISS_ILOS_CLASS_float = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_float, // name
     KISS_NIL, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl06, // cpl
};

// <basic-array>
kiss_ilos_obj_t KISS_ILOS_CLASS_basic_array = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_basic_array, // name
     KISS_T, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl01, // cpl
};

// <basic-array*>
kiss_cons_t KISS_ILOS_cpl07 = {
     KISS_CONS,
     NULL,        // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_basic_array,
     (kiss_obj*)&KISS_ILOS_cpl01,
};

kiss_ilos_obj_t KISS_ILOS_CLASS_basic_array_s = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_basic_array_s, // name
     KISS_T, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl07, // cpl
};

// <general-array*>
kiss_cons_t KISS_ILOS_cpl08 = {
     KISS_CONS,
     NULL,        // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_basic_array_s,
     (kiss_obj*)&KISS_ILOS_cpl07,
};

kiss_ilos_obj_t KISS_ILOS_CLASS_general_array_s = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_general_array_s, // name
     KISS_NIL, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl08, // cpl
};

// <basic-vector>
kiss_ilos_obj_t KISS_ILOS_CLASS_basic_vector = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_basic_vector, // name
     KISS_T, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl07, // cpl
};

// <general-vector>
kiss_cons_t KISS_ILOS_cpl09 = {
     KISS_CONS,
     NULL,        // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_basic_vector,
     (kiss_obj*)&KISS_ILOS_cpl07,
};

kiss_ilos_obj_t KISS_ILOS_CLASS_general_vector = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_general_vector, // name
     KISS_NIL, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl09, // cpl
};

// <string>
kiss_ilos_obj_t KISS_ILOS_CLASS_string = {
     KISS_ILOS_CLASS, // type
     NULL,          // gc_ptr
     (kiss_obj*)&KISS_ILOS_CLASS_built_in_class, // class
     KISS_NIL, // slots
     (kiss_obj*)&KISS_Sc_string, // name
     KISS_NIL, // abstractp
     (kiss_obj*)&KISS_ILOS_cpl09, // cpl
};

/// --------------------------------------

kiss_obj* kiss_ilos_obj_p(const kiss_obj* const obj) {
    if (KISS_IS_ILOS_OBJ(obj)) { return KISS_T; }
    else                       { return KISS_NIL; }
}

/* special operator: (class class-name) -> <class>
   Returns the class object that corresponds to the class named CLASS-NAME.
   On error, signal <undefined-entity> see spec. p.119 */
kiss_obj* kiss_class(const kiss_obj* const name) {
     kiss_obj* class = kiss_gethash(name, KISS_Skiss_classes.var, KISS_NIL);
     if (class != KISS_NIL) return class;
     Kiss_Err(L"Undefined Class: ~S", name);
}

/* function: (class-of obj) -> <class>
   Returns the class of which the given OBJ is a direct instance.
   OBJ may be any ISLISP object. */
kiss_obj* kiss_class_of(const kiss_obj* const obj) {
	  switch (KISS_OBJ_TYPE(obj)) {
	  case KISS_CONS:
	       return kiss_class((kiss_obj*)&KISS_Sc_cons);
	  case KISS_SYMBOL:
               return kiss_class((kiss_obj*)(obj == KISS_NIL ? &KISS_Sc_null : &KISS_Sc_symbol));
	  case KISS_CHARACTER:
               return kiss_class((kiss_obj*)&KISS_Sc_character);
	  case KISS_FIXNUM:
          case KISS_BIGNUM:
	       return kiss_class((kiss_obj*)&KISS_Sc_integer);
	  case KISS_FLOAT:
	       return kiss_class((kiss_obj*)&KISS_Sc_float);
	  case KISS_STRING:
	       return kiss_class((kiss_obj*)&KISS_Sc_string);
	  case KISS_GENERAL_VECTOR:
	       return kiss_class((kiss_obj*)&KISS_Sc_general_vector);
	  case KISS_GENERAL_ARRAY_S:
               return kiss_class((kiss_obj*)&KISS_Sc_general_array_s);
	  case KISS_HASH_TABLE:
               return kiss_class((kiss_obj*)&KISS_Sc_hash_table);
	  case KISS_STREAM:
               return kiss_class((kiss_obj*)&KISS_Sc_stream);
	  case KISS_LFUNCTION:
	  case KISS_LMACRO:
	  case KISS_CFUNCTION:
	  case KISS_CMACRO:
               return kiss_class((kiss_obj*)&KISS_Sc_function);
	  case KISS_ILOS_OBJ:
          case KISS_ILOS_CLASS:
               return ((kiss_ilos_obj_t*)obj)->class;
	  case KISS_CATCHER:
	  case KISS_BLOCK:
	  case KISS_CLEANUP:
	  case KISS_TAGBODY:
	       fwprintf(stderr, L"class-of: unexpected internal built-in primitive type = %d\n",
                        KISS_OBJ_TYPE(obj));
	       exit(EXIT_FAILURE);
	  default:
	       fwprintf(stderr, L"class-of: unknown primitive object type = %d\n",
                        KISS_OBJ_TYPE(obj));
	       exit(EXIT_FAILURE);
	  }
}

kiss_ilos_class_t* Kiss_Class(const kiss_obj* const obj) {
     if (KISS_IS_ILOS_CLASS(obj)) {
          return (kiss_ilos_class_t*)obj;
     } else {
          Kiss_Err(L"Not a class object: ~S", obj);
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
     kiss_obj* binding = kiss_assoc(name, ilos_obj->slots);
     if (binding == KISS_NIL) {
          binding = kiss_cons(name, value);
          ilos_obj->slots = kiss_cons(binding, ilos_obj->slots);
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
     slot-spec  ::= slot-name | (slot-name slot-opt *)
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
     Kiss_Symbol(name);
     Kiss_List(supers);
     Kiss_List(slot_specs);
     kiss_ilos_class_t* class = kiss_make_ilos_class(name, supers);

     kiss_obj* abstractp = kiss_assoc((kiss_obj*)&KISS_Skw_abstractp, class_opts);
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
     class->metaclass = metaclass;
     kiss_c_puthash(name, class, KISS_Skiss_classes.var);
     return name;
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
kiss_obj* kiss_instancp(const kiss_obj* const obj, const kiss_obj* const class) {
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
     kiss_symbol_t* name = Kiss_Symbol(func_spec);
     kiss_generic_function_t* gf = kiss_make_generic_function(func_spec);
     gf->lambda_list = Kiss_Lambda_List(lambda_list);
     
     name->fun = (kiss_obj*)gf;
     return (kiss_obj*)name;
}

kiss_obj* kiss_collect_specilizers(const kiss_obj* const parameter_profile) {
     kiss_obj* head = (kiss_obj*)kiss_cons(KISS_NIL, KISS_NIL);
     kiss_obj* tail = head;
     for (const kiss_obj* p = parameter_profile; KISS_IS_CONS(p); p = KISS_CDR(p)) {
          kiss_obj* obj = KISS_CAR(p);
          kiss_obj* class = (kiss_obj*)&KISS_ILOS_CLASS_object;
          if (KISS_IS_CONS(obj)) {
               class = Kiss_Class(Kiss_Symbol(kiss_cadr(obj)));
          } else if (obj == (kiss_obj*)&KISS_Skw_rest || obj == (kiss_obj*)&KISS_Samp_rest) {
               break;
          }
          kiss_set_cdr(kiss_cons(class, KISS_NIL), &tail);
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
          kiss_set_cdr(kiss_cons(var, KISS_NIL), &tail);
     }
     return (kiss_obj*)KISS_CDR(head);
}

int kiss_cmp_qualifiers(const kiss_obj* q1, const kiss_obj* const q2) {
     const kiss_obj* p2 = q2;
     for (const kiss_obj* p1 = q1; KISS_IS_CONS(p1); p1 = KISS_CDR(p1)) {
          kiss_obj* c1 = KISS_CAR(p1);
          if (!KISS_IS_CONS(p2)) {
               Kiss_Err(L"kiss_cmp_qualifiers: two qualifiers of different length: ~S, ~S",
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

kiss_obj* kiss_add_method(kiss_generic_function_t* const gf, const kiss_method_t* const m) {
     kiss_obj* q = m->qualifier;
     kiss_obj* list = KISS_NIL;
     if (q == KISS_NIL) {
          list = gf->primary_methods;
     } else if (q == (kiss_obj*)KISS_Skw_around) {
          list = gf->around_methods;
     } else if (q == (kiss_obj*)KISS_Skw_before) {
          list = gf->before_methods;
     } else if (q == (kiss_obj*)KISS_Skw_after) {
          list = gf->after_methods;
     } else {
          Kiss_Err(L"unknown method qualifier: ~S", q);
     }

     kiss_obj* head = kiss_cons(KISS_NIL, KISS_NIL);
     kiss_obj* tail = head;
     
}

/* defining operator: (defmethod func-spec method-qualifier* parameter-profile form*) -> <symbol>
   func-spec ::= identifier | (setf identifier)
   method-qualifier ::= identifier | keyword
   parameter-profile ::= ({var | (var parameter-specializer-name)}* [{&rest | :rest} var])
   parameter-specializer-name ::= class-name
   class-name ::= identifier
   var ::= identifier                                                          */
kiss_obj* kiss_defmethod(const kiss_obj* const func_spec, const kiss_obj* const rest) {
     kiss_symbol_t* name = Kiss_Symbol(func_spec);
     const kiss_obj* p = rest;
     const kiss_obj* qualifier = kiss_car(rest);
     if (KISS_IS_CONS(qualifier)) {
          qualifier = KISS_NIL;
     } else {
          Kiss_Symbol(qualifier);
          if (qualifier != (kiss_obj*)KISS_Skw_around &&
              qualifier != (kiss_obj*)KISS_Skw_before &&
              qualifier != (kiss_obj*)KISS_Skw_after &&
              qualifier != KISS_NIL)
          {
               Kiss_Err(L"Invalid method qualifier: ~S", qualifier);
          }
          p = kiss_cdr(rest);
     }

     // skip other qualifiers
     for (; KISS_IS_CONSP(p) && KISS_IS_SYMBOL(KISS_CAR(p)); p = KISS_CDR(p)) {}
     
     const kiss_obj* parameter_profile = Kiss_List(kiss_car(p));
     const kiss_obj* specializers = kiss_collect_specilizers(parameter_profile);
     const kiss_obj* lambda_list  = kiss_collect_lambda_list(parameter_profile);
     const kiss_obj* const forms = kiss_cdr(p);
     Kiss_Lambda_List(lambda_list);
     kiss_generic_function_t* gf = Kiss_Generic_Function(name->fun);
     kiss_obj* lambda = kiss_cons((kiss_obj*)&KISS_Slambda, kiss_cons(lambda_list, forms));
     kiss_method_t* method = kiss_make_method((kiss_obj*)name);
     method->lambda = lambda;
     method->specializers = specializers;
     method->qualifier = qualifier;

     return func_spec;
}
