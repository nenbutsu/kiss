/*  -*- coding: utf-8 -*-
  misc.c --- defines Miscellaneous operators of ISLisp processor KISS.

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

kiss_obj* kiss_identity(kiss_obj* obj) {
     return obj;
}

kiss_obj* kiss_get_universal_time(void) {
     time_t t = time(NULL);
     if (t == (time_t)(-1)) { Kiss_System_Error(); }
     // https://stackoverflow.com/questions/8805832/number-of-seconds-from-1st-january-1900-to-start-of-unix-epoch
     return kiss_plus2(kiss_make_integer((kiss_ptr_int)t), kiss_make_integer(2208988800));
}

