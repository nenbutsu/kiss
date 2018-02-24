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
          return kiss_setq((kiss_obj*)place, (kiss_obj*)form);
     case KISS_CONS: {
          const kiss_symbol_t* const op = Kiss_Symbol(KISS_CAR(place));
          if (!op->setf) {
               Kiss_Err(L"setf: invalid place form: ~S", place);
          }
          const kiss_obj* const setf_op = Kiss_Symbol(op->setf)->fun;
          const int setf_op_is_special = KISS_IS_CSPECIAL(setf_op);
          kiss_obj* head = kiss_cons(KISS_NIL, KISS_NIL);
          kiss_obj* tail = (kiss_obj*)head;
          kiss_obj* obj = KISS_NIL;
          for (const kiss_obj* p = KISS_CDR(place); KISS_IS_CONS(p); p = KISS_CDR(p)) {
               obj = setf_op_is_special ? KISS_CAR(p) : kiss_eval(KISS_CAR(p));
               kiss_set_cdr(kiss_cons(obj, KISS_NIL), tail);
               tail = KISS_CDR(tail);
          }
          obj = setf_op_is_macro ? form : kiss_eval(form);
          kiss_set_car(obj, head);

          switch (KISS_OBJ_TYPE(setf_op->fun)) {
          case KISS_CFUNCTION: case KISS_CSPECIAL:
               return kiss_cf_invoke((kiss_cfunction_t*)setf_op, head);
          case KISS_LFUNCTION:
               return kiss_lf_invoke((kiss_function_t*)setf_op, head);
          case KISS_LMACRO:
               fwprintf(stderr, L"lisp macro cannot be a setf operater");
               exit(EXIT_FAILURE);
          case KISS_CMACRO:
               Kiss_Err(L"C macro cannot be a setf operater");
               exit(EXIT_FAILURE);
          case KISS_GENERIC_FUNCTION:
               fwprintf(stderr, L"GF setf operator not implemented yet");
               break;
          default:
               fwprintf(stderr, L"Invalid setf operator %p", f);
               exit(EXIT_FAILURE);
          }
          
     }
     default:
          Kiss_Err(L"setf: invalid place form: ~S", place);
     }
}
