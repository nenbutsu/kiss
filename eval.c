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

static kiss_obj* kiss_eval_args(kiss_obj* args);

static kiss_obj* kiss_invoke(kiss_obj* f, kiss_obj* args) {
     switch (KISS_OBJ_TYPE(f)) {
     case KISS_CFUNCTION:
	  return kiss_cinvoke((kiss_cfunction_t*)f, kiss_eval_args(args));
     case KISS_CMACRO:
	  return kiss_cinvoke((kiss_cfunction_t*)f, args);
     case KISS_FUNCTION:
	  return kiss_linvoke((kiss_function_t*)f, kiss_eval_args(args));
     case KISS_MACRO: {
	  kiss_obj* form = kiss_linvoke((kiss_function_t*)f, args);
	  return kiss_eval(form);
     }
     case KISS_OO_OBJ: {
	  if (kiss_cfuncall(L"generic-function-p", kiss_clist(1, f)) == KISS_T) {
	       /* fwprintf(stderr, "calling generic-function\n"); fflush(stderr); */
	       return kiss_cfuncall(L"generic-function-invoke",
				    kiss_clist(2, f, kiss_eval_args(args)));
	  } else {
	       return kiss_method_invoke(f);
	  }

     }
     default:
	  Kiss_Err(L"Can't invoke function like object ~S", f);
     }
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
}

kiss_obj* kiss_eval(kiss_obj* form) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* result;
    switch (KISS_OBJ_TYPE(form)) {
    case KISS_CONS: {
	result = kiss_eval_compound_form((kiss_cons_t*)Kiss_Proper_List(form));
	break;
    }
    case KISS_SYMBOL: {
	result = kiss_var_ref((kiss_symbol_t*)form);
	break;
    }
    default: /* self-evaluating object. */
	result = form;
	break;
    }
    return result;
}

kiss_obj* kiss_gc_eval(kiss_obj* form) {
     kiss_environment_t* env = Kiss_Get_Environment();
     size_t saved_heap_index = env->heap_index;
     kiss_obj* result;
     switch (KISS_OBJ_TYPE(form)) {
     case KISS_CONS: {
	  result = kiss_eval_compound_form((kiss_cons_t*)Kiss_Proper_List(form));
	  break;
     }
     case KISS_SYMBOL: {
	  result = kiss_var_ref((kiss_symbol_t*)form);
	  break;
     }
     default: /* self-evaluating object. */
	  result = form;
	  break;
     }
     env->heap_index = saved_heap_index;
     return result;
}

kiss_obj* kiss_eval_body(kiss_obj* body) {
     kiss_environment_t* env = Kiss_Get_Environment();
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

kiss_obj* kiss_load(kiss_obj* filename) {
    kiss_obj* in = kiss_open_input_file(filename, KISS_NIL);
    kiss_obj* form;
    for (form = kiss_cread(in, KISS_NIL, KISS_EOS); form != KISS_EOS;
	 form = kiss_cread(in, KISS_NIL, KISS_EOS))
    {
	kiss_gc_eval(form);
    }
    return KISS_T;
}
