/*  -*- coding: utf-8 -*-
  ilos.c --- defines the ILOS mechanism of ISLisp processor KISS.

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
kiss_symbol_t KISS_Sk_classes, KISS_Skw_class;

kiss_obj* kiss_make_ilos_obj(kiss_obj* plist) {
    kiss_ilos_obj_t* p = Kiss_GC_Malloc(sizeof(kiss_ilos_obj_t));
    p->type = KISS_ILOS_OBJ;
    p->plist = plist;
    return (kiss_obj*)p;
}

kiss_obj* kiss_object_p(kiss_obj* obj) {
    if (KISS_IS_ILOS_OBJ(obj)) { return KISS_T; }
    else                     { return KISS_NIL; }
}

kiss_obj* kiss_ilos_obj_plist(const kiss_obj* const obj) {
    kiss_ilos_obj_t* p = Kiss_ILOS_Obj(obj);
    return p->plist;
}

kiss_obj* kiss_set_ilos_obj_plist(const kiss_obj* const plist, kiss_obj* const obj) {
    kiss_ilos_obj_t* p = Kiss_ILOS_Obj(obj);
    p->plist = (kiss_obj*)plist;
    return obj;
}

kiss_obj* kiss_oref(const kiss_obj* const obj, const kiss_obj* const property) {
    return kiss_plist_get(kiss_ilos_obj_plist(obj), property);
}

kiss_obj* kiss_set_oref(const kiss_obj* const value, kiss_obj* const obj, const kiss_obj* const property) {
    kiss_obj* plist = kiss_plist_put(kiss_ilos_obj_plist(obj), property, value);
    return kiss_set_ilos_obj_plist(plist, obj);    
}

kiss_obj* kiss_k_class(const kiss_obj* const name) {
     kiss_obj* class = kiss_gethash(name, KISS_Sk_classes.var, KISS_NIL);
     if (class != KISS_NIL) return class;
     Kiss_Err(L"Undefined Class: ~S", name);
}

/* function: (class-of obj) -> <class>
   Returns the class of which the  given obj is a direct instance.
   obj may be any ISLISP object. */
kiss_obj* kiss_class_of(const kiss_obj* const obj) {
	  switch (KISS_OBJ_TYPE(obj)) {
	  case KISS_CONS:
	       return kiss_k_class((kiss_obj*)&KISS_Sc_cons);
	  case KISS_SYMBOL:
               return kiss_k_class((kiss_obj*)(obj == KISS_NIL ? &KISS_Sc_null : &KISS_Sc_symbol));
	  case KISS_CHARACTER:
               return kiss_k_class((kiss_obj*)&KISS_Sc_character);
	  case KISS_FIXNUM:
          case KISS_BIGNUM:
	       return kiss_k_class((kiss_obj*)&KISS_Sc_integer);
	  case KISS_FLOAT:
	       return kiss_k_class((kiss_obj*)&KISS_Sc_float);
	  case KISS_STRING:
	       return kiss_k_class((kiss_obj*)&KISS_Sc_string);
	  case KISS_GENERAL_VECTOR:
	       return kiss_k_class((kiss_obj*)&KISS_Sc_general_vector);
	  case KISS_GENERAL_ARRAY_S:
               return kiss_k_class((kiss_obj*)&KISS_Sc_general_array_s);
	  case KISS_HASH_TABLE:
               return kiss_k_class((kiss_obj*)&KISS_Sc_hash_table);
	  case KISS_STREAM:
               return kiss_k_class((kiss_obj*)&KISS_Sc_stream);
	  case KISS_LFUNCTION:
	  case KISS_LMACRO:
	  case KISS_CFUNCTION:
	  case KISS_CSPECIAL:
               return kiss_k_class((kiss_obj*)&KISS_Sc_function);
	  case KISS_ILOS_OBJ:
               return kiss_oref(obj, (kiss_obj*)&KISS_Skw_class);
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
