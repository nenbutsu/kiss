/*  -*- coding: utf-8 -*-
  repl.c --- defines the read, eval and print mechanism of ISLisp processor KISS.

  Copyright (C) 2017, 2018, 2019 Yuji Minejima <yuji@minejima.jp>

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

int kiss_read_eval_print_loop(void) {
     kiss_environment_t* env = Kiss_Get_Environment();
     kiss_obj* form;

     while (1) {
	  kiss_dynamic_environment_t saved_dynamic_env = env->dynamic_env;
	  kiss_lexical_environment_t saved_lexical_env = env->lexical_env;
	  size_t saved_heap_top = Kiss_Heap_Top;
	  if (setjmp(env->top_level) == 0) {
               //fwprintf(stderr, L"Kiss_Heap_Top = %ld\n", Kiss_Heap_Top);
	       fflush(stdout);
               
               env->call_stack = KISS_NIL;

               fwprintf(stdout, L"\nKISS>"); fflush(stdout);
               form = kiss_c_read(kiss_standard_input(), KISS_NIL, KISS_EOS);
               if (form == KISS_EOS) {
                    break;
               }

               env->call_stack = KISS_NIL;
	       kiss_obj* result = kiss_eval(form);
	       kiss_format_fresh_line(kiss_standard_output());
	       kiss_print(result);
	       fflush(stdout);
	  } else {
	       kiss_obj* result = env->throw_result;
	       if (!KISS_IS_STRING(result)) {
		    fwprintf(stderr, L"\nKISS| Internal error. Kiss_Err threw non-string obj.\n");
		    fwprintf(stderr, L"\n");
		    exit(EXIT_FAILURE);
	       } else {
		    kiss_string_t* msg = (kiss_string_t*)result;
		    fwprintf(stderr, L"\nKISS| %ls\n", msg->str);
                    if (KISS_IS_CONS(form)) {
                         fwprintf(stderr, L"KISS| ");
                         kiss_format_object(kiss_error_output(), kiss_car(kiss_reverse(env->error_call_stack)), KISS_NIL);
                    }
		    fflush(stderr);
		    fflush(stdout);
                    kiss_obj* c;
                    do {
                         c = kiss_c_read_char(kiss_standard_input(), KISS_NIL, KISS_EOS);
                    } while (c != KISS_EOS && kiss_C_wchar_t(c) != L'\n');
                    if (c == KISS_EOS) { break; }
		    env->dynamic_env = saved_dynamic_env;
		    env->lexical_env = saved_lexical_env;
                    env->call_stack = KISS_NIL;
	       }
	  }
	  Kiss_Heap_Top = saved_heap_top;
     }
     return 0;
}
