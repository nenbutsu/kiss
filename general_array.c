/*  -*- coding: utf-8 -*-
  general_array.c --- defines the general array mechanism of ISLisp processor KISS.

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

/*
  <object>
     |
     +--> <basic-array>
              |
              +--> <basic-array*>
              |        |
              |        +--> <general-array*>
              |
              +--> <basic-vector>
                       |
                       +--> <general-vector>
                       +--> <string>
 */

static kiss_general_vector_t* kiss_make_general_array(size_t n, kiss_obj* obj) {
    kiss_general_vector_t* p = Kiss_Malloc(sizeof(kiss_general_vector_t));
    kiss_obj** v = Kiss_Malloc(n * sizeof(kiss_obj*));
    size_t i;
    p->type = KISS_GENERAL_VECTOR;
    p->v = v;
    p->n = n;
    for (i = 0; i < n; i++) { v[i] = obj; }
    return p;
}

