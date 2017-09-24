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

void kiss_init_ilos(void) {
     Kiss_Classes = (kiss_hash_table_t*)kiss_create_hash_table(KISS_NIL);
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



//wchar_t* kiss_type_to_class_name_str(kiss_type t) {
//     wchar_t* class_name_str = NULL;
//     switch (t) {
//     case KISS_CONS:      class_name_str = L"<cons>";      break;
//     case KISS_SYMBOL:    class_name_str = L"<symbol>";    break;
//     case KISS_CHARACTER: class_name_str = L"<character>"; break;
//     case KISS_FIXNUM:    class_name_str = L"<integer>";   break;
//     case KISS_BIGNUM:    class_name_str = L"<integer>";   break;
//     case KISS_FLOAT:     class_name_str = L"<float>";     break;
//     case KISS_STRING:    class_name_str = L"<string>";    break;
//     case KISS_GENERAL_VECTOR:
//	  class_name_str = L"<general-vector>"; break;
//     case KISS_GENERAL_ARRAY_S:
//	  class_name_str = L"<general-array*>"; break;
//     case KISS_STREAM:    class_name_str = L"<stream>";    break;
//     case KISS_FUNCTION:  class_name_str = L"<function>";  break;
//     case KISS_CFUNCTION: class_name_str = L"<function>";  break;
//     case KISS_MACRO:     class_name_str = L"<macro>";     break;
//     case KISS_CMACRO:    class_name_str = L"<macro>";     break;
//     default:
//	  fwprintf(stderr, L"kiss_type_to_class_name_str: unexped kiss_type %d\n", t);
//	  exit(EXIT_FAILURE);
//     }
//     return class_name_str;
//}
//
//kiss_obj* kiss_type_to_class_name(kiss_type t) {
//     wchar_t* class_name = kiss_type_to_class_name_str(t);
//     return kiss_symbol(class_name);
//}
