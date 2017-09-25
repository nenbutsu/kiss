/*  -*- coding: utf-8 -*-
  oo_obj.c --- defines the object mechanism of ISLisp processor KISS.

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

kiss_hash_table_t* Kiss_Classes = NULL;
kiss_ilos_class_t* Kiss_Built_In_Class = NULL;
kiss_ilos_class_t* Kiss_Standard_Class = NULL;


void kiss_init_ilos(void) {
     Kiss_Classes = (kiss_hash_table_t*)kiss_create_hash_table(KISS_NIL);

     kiss_puthash((kiss_obj*)&KISS_Sc_object, (kiss_obj*)kiss_make_ilos_class(KISS_Sc_object), (kiss_obj*)Kiss_Classes);

     Kiss_Built_In_Class = kiss_make_ilos_class(KISS_Sc_built_in_class);
     kiss_puthash((kiss_obj*)&KISS_Sc_built_in_class, (kiss_obj*)Kiss_Built_In_Class, (kiss_obj*)Kiss_Classes);

     Kiss_Standard_Class = kiss_make_ilos_class(KISS_Sc_standard_class);
     kiss_puthash((kiss_obj*)&KISS_Sc_standard_class, (kiss_obj*)Kiss_Standard_Class, (kiss_obj*)Kiss_Classes);
     
}

kiss_ilos_class_t* kiss_make_ilos_class(kiss_symbol_t* class_name) {
     kiss_ilos_class_t* p = Kiss_GC_Malloc(sizeof(kiss_ilos_class_t));
     p->type = KISS_ILOS_CLASS;
     p->class = NULL;
     p->slots = NULL;
     p->slot_specs = NULL;
     p->name = class_name;
     p->supers = NULL;
     p->cpl = NULL;
     return p;
}

kiss_obj* kiss_make_object(kiss_obj* plist) {
    kiss_object_t* p = Kiss_GC_Malloc(sizeof(kiss_object_t));
    p->type = KISS_ILOS_OBJ;
    p->plist = plist;
    return (kiss_obj*)p;
}

/* special operator: (class class-name) -> <class>
   Returns the class object that corresponds to the class named CLASS-NAME.
   On error, signal <undefined-entity> see spec. p.119 */
kiss_obj* kiss_class(const kiss_obj* const name) {
     kiss_obj* class = kiss_c_gethash((kiss_obj*)Kiss_Symbol(name), Kiss_Classes, KISS_NIL);
     if (class != KISS_NIL) {
          return class;
     } else {
          kiss_c_funcall(L"kiss::signal-undefined-entity-error", kiss_c_list(4, name, (kiss_obj*)&KISS_Sclass, KISS_NIL));
          fwprintf(stderr, L"class: internal error. shouldn't reach here\n");
          exit(EXIT_FAILURE);
     }
}

/* function: (class-of obj) -> <class>
   Returns the class of which the  given obj is a direct instance.
   obj may be any ISLISP object. */
kiss_obj* kiss_class_of(const kiss_obj* const obj) {
	  switch (KISS_OBJ_TYPE(obj)) {
	  case KISS_CONS:
	       return kiss_class((kiss_obj*)KISS_Sc_cons);
	  case KISS_SYMBOL:
               return kiss_class((kiss_obj*)(obj == KISS_NIL ? KISS_Sc_null : KISS_Sc_symbol));
	  case KISS_CHARACTER:
               return kiss_class((kiss_obj*)KISS_Sc_character);
	  case KISS_FIXNUM:
          case KISS_BIGNUM:
	       return kiss_class((kiss_obj*)KISS_Sc_integer);
	  case KISS_FLOAT:
	       return kiss_class((kiss_obj*)KISS_Sc_float);
	  case KISS_STRING:
	       return kiss_class((kiss_obj*)KISS_Sc_string);
	  case KISS_GENERAL_VECTOR:
	       return kiss_class((kiss_obj*)KISS_Sc_general_vector);
	  case KISS_GENERAL_ARRAY_S:
               return kiss_class((kiss_obj*)KISS_Sc_general_array_s);
	  case KISS_HASH_TABLE:
               return kiss_class((kiss_obj*)KISS_Sc_hash_table);
	  case KISS_STREAM:
               return kiss_class((kiss_obj*)KISS_Sc_stream);
	  case KISS_FUNCTION:
	  case KISS_MACRO:
	  case KISS_CFUNCTION:
	  case KISS_CMACRO:
               return kiss_class((kiss_obj*)KISS_Sc_function);
	  case KISS_ILOS_OBJ:
	  case KISS_ILOS_CLASS:
               return ((kiss_ilos_obj_t*)obj)->class;
	  case KISS_CATCHER:
	  case KISS_BLOCK:
	  case KISS_CLEANUP:
	  case KISS_TAGBODY:
	       fwprintf(stderr, L"class-of: unexpected internal built-in primitive type = %d\n", KISS_OBJ_TYPE(obj));
	       exit(EXIT_FAILURE);
	  default:
	       fwprintf(stderr, L"class-of: unknown primitive object type = %d\n", KISS_OBJ_TYPE(obj));
	       exit(EXIT_FAILURE);
	  }
}

/* function: (subclassp subclass superclass) -> boolean
   Returns t if the class SUBCLASS is a subclass of the class SUPERCLASS;
   otherwise, returns nil. An error shall be signaled if either SUBCLASS or
   SUPERCLASS is not a class object (error-id. domain-error). */
kiss_obj* kiss_subclassp(const kiss_obj* const subclass, const kiss_obj* const superclass) {
     kiss_ilos_class_t* sub = Kiss_ILOS_Class(subclass);
     Kiss_ILOS_Class(superclass);
     kiss_obj* cpl = sub->cpl;
     if (cpl == KISS_NIL) return KISS_NIL;
     return (kiss_member(subclass, KISS_CDR(cpl)) == KISS_NIL ? KISS_NIL : KISS_T);
}

/* function: (instancep obj class) -> boolean
   Returns t if OBJ is an instance (directly or otherwise) of the class
   CLASS; otherwise, returns nil OBJ may be any ISLISP object. An error
   shall be signaled if class is not a class object (error-id. domain-error ). */
kiss_obj* kiss_instancep(const kiss_obj* const obj, const kiss_obj* const class) {
     Kiss_ILOS_Class(class);
     kiss_obj* c = kiss_class_of(obj);
     if (c == class) return KISS_T;
     return (kiss_member(c, ((kiss_ilos_class_t*)c)->cpl) == KISS_NIL ? KISS_NIL : KISS_T);
}

/* special operator: (assure class-name form) → object
   Evaluates FORM. If FORM returns, the returned value is returned by the assure form.
   In addition, this form specify that the value of form is of the class specified
   by CLASS-NAME (which must be the name of an existing class).
   In an assure special form, an error shall be signaled if the value of form is not
   of the class or a subclass of the class designated by class-name (error-id. domain-error). */
kiss_obj* kiss_assure(const kiss_obj* const class_name, const kiss_obj* const form) {
     kiss_obj* result = kiss_eval(form);
     kiss_ilos_class_t* class = (kiss_ilos_class_t*)kiss_class(class_name);
     if (kiss_instancep(result, (kiss_obj*)class) != KISS_NIL) {
          return result;
     }
     Kiss_Domain_Error(result, Kiss_String(class->name)->str);
}

/* spec. p. 51
   Let C1, . . . , Cn be the direct superclasses of C in the order defined in
   the defclass defining form for C. Let P1, . . ., Pn be the class precedence
   lists for C1, . . . , Cn, respectively. Define P . Q on class precedence
   lists P and Q to be the two lists appended. Then the class precedence
   list for C is C . P1 . . . . Pn with duplicate classes removed by
   repeated application of the following rule: If a class appears twice in
   the resulting class precedence list, the leftmost occurrence is removed. */
static kiss_obj* compute_cpl(kiss_obj* class, kiss_obj* supers) {
     kiss_obj* cpl = KISS_NIL;
     for (kiss_obj* p = supers; KISS_IS_CONS(p); p = KISS_CDR(p)) {
          kiss_push(((kiss_ilos_class_t*)KISS_CAR(p))->cpl, &cpl);
     }
     cpl = kiss_nreverse(cpl);
     cpl = kiss_cons(class, kiss_append(cpl));
     kiss_obj* result = KISS_NIL;
     for (kiss_obj* p = cpl; KISS_IS_CONS(p); p = KISS_CDR(p)) {
          if (kiss_member(KISS_CAR(p), KISS_CDR(p)) == KISS_NIL) {
               kiss_push(KISS_CAR(p), &result);
          }
     }
     return kiss_nreverse(result);
}

/* defining operator: (defclass class-name (sc-name*) (slot-spec*) class-opt*) -> <symbol>
      class-name ::= identifier
      sc-name ::= identifier
      slot-spec ::= slot-name | (slot-name slot-opt *)
      slot-name ::= identifier
      slot-opt ::= :reader reader-function-name |
                   :writer writer-function-name |
                   :accessor reader-function-name |
                   :boundp boundp-function-name |
                   :initform form |
                   :initarg initarg-name
      initarg-name ::= identifier
      reader-function-name ::= identifier
      writer-function-name ::= identifier
      boundp-function-name ::= identifier
      class-opt ::= (:metaclass class-name) | (:abstractp abstract-flag)
      abstractp ::= t | nil */
kiss_obj* kiss_defclass(kiss_obj* class_name, kiss_obj* sc_names, kiss_obj* slot_specs, kiss_obj* class_options)
{
     kiss_ilos_class_t* class = kiss_c_gethash((kiss_obj*)Kiss_Symbol(name), Kiss_Classes, KISS_NIL);
     if (class == KISS_NIL) {
          class= kiss_make_ilos_class((kiss_symbol_t*)class_name);
     }
     class->supers = kiss_c_mapc1((kiss_cf1_t)kiss_class, Kiss_Proper_List(sc_names));
     class->cpl = compute_cpl(class, class->supers);
     
}
