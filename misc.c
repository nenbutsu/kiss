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

kiss_obj* kiss_get_internal_run_time(void) {
     clock_t t;

     t = clock();
     if (t == (clock_t)(-1)) {
          Kiss_System_Error();
     }
     return kiss_make_integer((kiss_ptr_int)t);
}

kiss_obj* kiss_get_internal_real_time(void) {
     clock_t t;
     struct tms time;

     t = times(&time);
     if (t == (clock_t)(-1)) {
          Kiss_System_Error();
     }
     return kiss_make_integer((kiss_ptr_int)t);
}

kiss_obj* kiss_internal_time_units_per_second(void) {
     return kiss_make_integer((kiss_ptr_int)CLOCKS_PER_SEC);
}

kiss_obj* kiss_time(kiss_obj* form) {
     kiss_obj* run_start = kiss_get_internal_run_time();
     kiss_obj* real_start = kiss_get_internal_real_time();

     kiss_obj* result = kiss_eval(form);

     kiss_obj* run_end = kiss_get_internal_run_time();
     kiss_obj* real_end = kiss_get_internal_real_time();
     kiss_format(kiss_standard_output(), (kiss_obj*)kiss_make_string(L"~S~%"),
                 kiss_c_list(1, result));
     kiss_format(kiss_standard_output(), (kiss_obj*)kiss_make_string(L"~&Real time: ~S~&"),
                 kiss_c_list(1, kiss_minus(real_end, kiss_c_list(1, real_start))));
     kiss_format(kiss_standard_output(), (kiss_obj*)kiss_make_string(L"~&Run time : ~S~&"),
                 kiss_c_list(1, kiss_minus(run_end, kiss_c_list(1, run_start))));
     return result;
}
