/*  -*- coding: utf-8 -*-
  setf.c --- defines the setf mechanism of ISLisp processor KISS.

  Copyright (C) 2017, 2018 Yuji Minejima <yuji@minejima.jp>.

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

/* special operator: (setf place form) → <object>
   
*/
kiss_obj* kiss_setf(const kiss_obj* const place, const kiss_obj* const form) {
     if (KISS_IS_SYMBOL(place)) {
          return kiss_setq(place, form);
     }
     if (!KISS_IS_CONS(place)) {
          Kiss_Err(L"setf: invalid place form: ~S", place);
     }
     const kiss_obj* const op = KISS_CAR(place);
}
