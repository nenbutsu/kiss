/*  -*- coding: utf-8 -*-
  ilos.c --- defines ILOS mechanism of ISLisp processor KISS.

  Copyright (C) 2017, 2018 Yuji Minejima.

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


kiss_obj* kiss_make_ilos_obj(const kiss_obj* const class) {
    kiss_ilos_obj_t* p = Kiss_GC_Malloc(sizeof(kiss_ilos_obj_t));
    p->type = KISS_ILOS_OBJ;
    p->class = (kiss_obj*)class;
    p->slots = KISS_NIL;
    return (kiss_obj*)p;
}

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
kiss_obj* kiss_compute_cpl(const kiss_obj* const class, const kiss_obj* const supers) {
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

