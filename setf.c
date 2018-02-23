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
   https://nenbutsu.github.io/ISLispHyperDraft/islisp-v23.html#s_setf */
kiss_obj* kiss_setf(const kiss_obj* const place, const kiss_obj* const form) {
     switch (KISS_OBJ_TYPE(place)) {
     case KISS_SYMBOL:
          return kiss_setq(place, form);
     case KISS_CONS: {
          const kiss_symbol_t* const op = Kiss_Symbol(KISS_CAR(place));
          const kiss_symbol_t* const setf_op = op->setf;
          if (!setf_op) {
               Kiss_Err(L"setf: invalid place form: ~S", place);
          }
          kiss_obj* head = kiss_cons(KISS_NIL, KISS_NIL);
          kiss_obj* tail = (kiss_obj*)head;
          for (const kiss_obj* p = KISS_CDR(place); KISS_IS_CONS(p); p = KISS_CDR(p)) {
               set_cdr(kiss_cons(kiss_eval(KISS_CAR(p)), KISS_NIL), tail);
          }
          set_car(kiss_eval(form), head);
          return kiss_funcall(kiss_function((kiss_obj*)f), head);
     }
     default:
          Kiss_Err(L"setf: invalid place form: ~S", place);
     }
}
