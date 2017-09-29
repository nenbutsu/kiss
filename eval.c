/*  -*- coding: utf-8 -*-
  eval.c --- defines the evaluation mechanism of ISLisp processor KISS.

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

extern kiss_gc_obj* Kiss_Heap_Stack[];

static kiss_obj* kiss_eval_args(kiss_obj* args);

static kiss_obj* kiss_invoke(kiss_obj* f, kiss_obj* args) {
     kiss_obj* result = KISS_NIL;
     size_t saved_heap_top = Kiss_Heap_Top;
     switch (KISS_OBJ_TYPE(f)) {
     case KISS_CFUNCTION:
	  result = kiss_cinvoke((kiss_cfunction_t*)f, kiss_eval_args(args));
	  break;
     case KISS_CMACRO:
	  result = kiss_cinvoke((kiss_cfunction_t*)f, args);
	  break;
     case KISS_FUNCTION:
	  result = kiss_linvoke((kiss_function_t*)f, kiss_eval_args(args));
	  break;
     case KISS_MACRO: {
	  kiss_obj* form = kiss_linvoke((kiss_function_t*)f, args);
	  result = kiss_eval(form);
	  break;
     }
     case KISS_ILOS_OBJ:
	  if (kiss_c_funcall(L"generic-function-p", kiss_c_list(1, f)) == KISS_T) {
	       /* fwprintf(stderr, L"calling generic-function\n"); fflush(stderr); */
	       result = kiss_c_funcall(L"generic-function-invoke",
				      kiss_c_list(2, f, kiss_eval_args(args)));
	  } else {
	       result = kiss_method_invoke(f);
	  }
	  break;
     default:
	  fwprintf(stderr, L"Can't invoke function like object %p", f);
	  exit(EXIT_FAILURE);
     }
     assert(saved_heap_top <= Kiss_Heap_Top);
     if (saved_heap_top < Kiss_Heap_Top) {
	  if (KISS_IS_GC_OBJ(result)) {
	       Kiss_Heap_Stack[saved_heap_top++] = (kiss_gc_obj*)result;
	  }
	  Kiss_Heap_Top = saved_heap_top;
     }
     //fwprintf(stderr, L"Kiss_Heap_Top = %lu\n", Kiss_Heap_Top);
     return result;
}

static kiss_obj* kiss_eval_compound_form(kiss_cons_t* p) {
     kiss_obj* op = p->car;
     switch (KISS_OBJ_TYPE(op)) {
     case KISS_SYMBOL: {
	  kiss_obj* f = kiss_fun_ref((kiss_symbol_t*)op);
	  return kiss_invoke(f, p->cdr);
     }
     case KISS_CONS: {
	  kiss_obj* f = (kiss_obj*)kiss_make_function(NULL, op);
	  return kiss_invoke(f, p->cdr);
     }
     default: Kiss_Err(L"Invalid compound expression ~S", p);
     }
     exit(EXIT_FAILURE); // not reach here
}

kiss_obj* kiss_eval(const kiss_obj* const form) {
     switch (KISS_OBJ_TYPE(form)) {
     case KISS_CONS:
          return kiss_eval_compound_form((kiss_cons_t*)Kiss_Proper_List(form));
     case KISS_SYMBOL:
          return kiss_var_ref((kiss_symbol_t*)form);
     default: /* self-evaluating object. */
          return (kiss_obj*)form;
     }
}

kiss_obj* kiss_eval_body(const kiss_obj* body) {
     kiss_obj* result = KISS_NIL;
     for (body = Kiss_Proper_List(body); KISS_IS_CONS(body); body = KISS_CDR(body)) {
	  result = kiss_eval(KISS_CAR(body));
     }
     return result;
}

static kiss_obj* kiss_eval_args(kiss_obj* args) {
    kiss_obj* stack = KISS_NIL;
    for (args = Kiss_Proper_List(args); KISS_IS_CONS(args); args = KISS_CDR(args)) {
	kiss_push(kiss_eval(KISS_CAR(args)), &stack);
    }
    return kiss_nreverse(stack);
}


