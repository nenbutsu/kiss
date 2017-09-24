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

kiss_obj* kiss_make_object(kiss_obj* plist) {
    kiss_object_t* p = Kiss_GC_Malloc(sizeof(kiss_object_t));
    p->type = KISS_ILOS_OBJ;
    p->plist = plist;
    return (kiss_obj*)p;
}

kiss_obj* kiss_object_p(kiss_obj* obj) {
    if (KISS_IS_OBJECT(obj)) { return KISS_T; }
    else                     { return KISS_NIL; }
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
//     case KISS_GENERAL_ARRAY:
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
