/*  -*- coding: utf-8 -*-
  convert.c --- defines the function 'convert' of ISLisp processor KISS.

  Copyright (C) 2017, 2018 Yuji Minejima <yuji@minejima.jp>

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

kiss_obj* kiss_convert(const kiss_obj* const obj, const kiss_obj* const class_name) {
     switch (KISS_OBJ_TYPE(obj)) {
     case KISS_CHARACTER:
          if (class_name == (kiss_obj*)&KISS_Sc_character) {
               return (kiss_obj*)obj;
          } else if (class_name == (kiss_obj*)&KISS_Sc_integer) {
               return kiss_make_fixnum(kiss_wchar(obj));
          } else {
               goto error;
          }
     default:
	  goto error;
     }

error:
     Kiss_Err(L"Cannot convert ~S to ~S", obj, class_name);
}
