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

static inline kiss_obj* kiss_eval_args(const kiss_obj* const args) {
     kiss_cons_t head;
     kiss_init_cons(&head, KISS_NIL, KISS_NIL);
     kiss_obj* p = (kiss_obj*)&head;
     for (const kiss_obj* q = args; KISS_IS_CONS(q); q = KISS_CDR(q)) {
          kiss_set_cdr(kiss_cons(kiss_eval(KISS_CAR(q)), KISS_NIL), p);
          p = KISS_CDR(p);
     }
     return KISS_CDR((kiss_obj*)&head);
}

kiss_obj* kiss_invoke(const kiss_obj* const f, kiss_obj* const args) {
     kiss_environment_t* env = Kiss_Get_Environment();
     kiss_obj* result = KISS_NIL;
     size_t saved_heap_top = Kiss_Heap_Top;
     kiss_obj* saved_call_stack = env->call_stack;
     kiss_push(f, &(env->call_stack));
     Kiss_Proper_List(args);
     switch (KISS_OBJ_TYPE(f)) {
     case KISS_CFUNCTION:
	  result = kiss_cf_invoke((kiss_cfunction_t*)f, kiss_eval_args(args));
	  break;
     case KISS_CMACRO:
	  result = kiss_cf_invoke((kiss_cfunction_t*)f, args);
	  break;
     case KISS_FUNCTION:
	  result = kiss_lf_invoke((kiss_function_t*)f, kiss_eval_args(args));
	  break;
     case KISS_MACRO: {
	  kiss_obj* form = kiss_lf_invoke((kiss_function_t*)f, args);
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
     env->call_stack = saved_call_stack;
     return result;
}

