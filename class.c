/*  -*- coding: utf-8 -*-
  class.c --- defines the class mechanism of ISLisp processor KISS.

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

wchar_t* kiss_type_to_class_name_str(kiss_type t) {
     wchar_t* class_name_str = NULL;
     switch (t) {
     case KISS_CONS:      class_name_str = L"<cons>";      break;
     case KISS_SYMBOL:    class_name_str = L"<symbol>";    break;
     case KISS_CHARACTER: class_name_str = L"<character>"; break;
     case KISS_INTEGER:   class_name_str = L"<integer>";   break;
     case KISS_FLOAT:     class_name_str = L"<float>";     break;
     case KISS_STRING:    class_name_str = L"<string>";    break;
     case KISS_STREAM:    class_name_str = L"<stream>";    break;
     case KISS_FUNCTION:  class_name_str = L"<function>";  break;
     case KISS_CFUNCTION: class_name_str = L"<function>";  break;
     case KISS_MACRO:     class_name_str = L"<macro>";     break;
     case KISS_CMACRO:    class_name_str = L"<macro>";     break;
     default:
	  fwprintf(stderr, L"kiss_type_to_class_name_str: unexped kiss_type %d\n", t);
	  abort();
     }
     return class_name_str;
}

kiss_obj* kiss_type_to_class_name(kiss_type t) {
     wchar_t* class_name = kiss_type_to_class_name_str(t);
     kiss_symbol(class_name);
}
