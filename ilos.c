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

/*
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

kiss_symbol_t KISS_Skiss_classes, KISS_Skw_class;

void kiss_init_ilos(void) {
     KISS_Skiss_classes.var = kiss_create_hash_table(KISS_NIL);
}

kiss_obj* kiss_make_ilos_obj(const kiss_obj* const class) {
    kiss_ilos_obj_t* p = Kiss_GC_Malloc(sizeof(kiss_ilos_obj_t));
    p->type = KISS_ILOS_OBJ;
    p->class = class;
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

kiss_obj* Kiss_Class(const kiss_obj* const obj) {
     kiss_obj* metaclass = kiss_class_of(obj);
     if (metaclass == kiss_class((kiss_obj*)&KISS_Sc_built_in_class) ||
         metaclass == kiss_class((kiss_obj*)&KISS_Sc_standard_class)) {
          return obj;
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

kiss_obj* kiss_set_slotref(const kiss_obj* const value, kiss_obj* const obj, kiss_obj* const name)
{
     kiss_ilos_obj* const ilos_obj = Kiss_ILOS_Obj(obj);
     const kiss_obj* const binding = kiss_assoc(name, ilos_obj->slots);
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
     Kiss_Class(sub);
     Kiss_Class(super);
     kiss_obj* cpl = kiss_slotref(sub, kiss_symbol(L":cpl"));
     return kiss_member(super, cpl) != KISS_NIL ? KISS_T : KISS_NIL;
}
