/*  -*- coding: utf-8 -*-
  repl.c --- defines the read, eval and print mechanism of ISLisp processor KISS.

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

kiss_obj* kiss_load(const kiss_obj* const filename) {
     size_t saved_heap_top = Kiss_Heap_Top;
     kiss_obj* in = kiss_open_input_file(filename, KISS_NIL);
     kiss_obj* form = kiss_c_read(in, KISS_NIL, KISS_EOS);
     while (form != KISS_EOS) {
	  kiss_eval(form);
	  form = kiss_c_read(in, KISS_NIL, KISS_EOS);
     }
     Kiss_Heap_Top = saved_heap_top;
     return KISS_T;
}
