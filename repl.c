/*  -*- coding: utf-8 -*-
  repl.c --- defines the read, eval and print mechanism of ISLisp processor KISS.

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

static wchar_t* libraries[] = {
     L"cons.lisp",
     L"character.lisp",
     L"stream.lisp",
     L"sequence.lisp",
     L"control.lisp",
     L"number.lisp",
     L"string.lisp",
     L"oo_obj.lisp",
     L"class.lisp",
     L"built_in_classes.lisp",
     L"generic_function.lisp",
     L"gf_invoke.lisp",
     L"built_in_conditions.lisp",
     L"condition.lisp",
     L"format_oo_object.lisp",
     L"init.lisp",
     NULL,
};

kiss_obj* kiss_re(kiss_obj* in) {
     return kiss_cread(in, KISS_NIL, KISS_EOS);
}

kiss_obj* kiss_load(kiss_obj* filename) {
     kiss_environment_t* env = Kiss_Get_Environment();
     size_t saved_heap_index = env->heap_index;
     kiss_obj* in = kiss_open_input_file(filename, KISS_NIL);
     kiss_obj* form = kiss_re(in);
     while (form != KISS_EOS) {
	  kiss_eval(form);
	  form = kiss_re(in);
     }
     env->heap_index = saved_heap_index;
     return KISS_T;
}

void kiss_load_library(wchar_t* name) {
     kiss_environment_t* env = Kiss_Get_Environment();
     if (setjmp(env->top_level) == 0) {
	  fwprintf(stderr, L"loading %ls ... ", name);
	  fflush(stderr);
	  kiss_load((kiss_obj*)kiss_make_string(name));
	  fwprintf(stderr, L"done \n");
	  fflush(stderr);
     } else {
	  kiss_obj* result = env->throw_result;
	  if (!KISS_IS_STRING(result)) {
	       fwprintf(stderr, L"\nKISS| Internal error. Kiss_Err threw non-string object.\n");
	       fwprintf(stderr, L"\n");
	  } else {
	       fwprintf(stderr, L"initialization failed\n"); fflush(stderr);
	       kiss_string_t* msg = (kiss_string_t*)result;
	       fwprintf(stderr, L"\nKISS| ");
	       fwprintf(stderr, L"%ls\n", msg->str);
	  }
	  exit(1);
     }
}

int kiss_read_eval_print_loop(void) {
     kiss_environment_t* env = Kiss_Get_Environment();
     kiss_obj* form;
     kiss_lexical_environment_t saved_lexical_env;
     kiss_dynamic_environment_t saved_dynamic_env;
     size_t i, saved_heap_index;

     for (i = 0; libraries[i] != NULL; i++) {
	  kiss_load_library(libraries[i]);
     }

     while (1) {
	  saved_dynamic_env = env->dynamic_env;
	  saved_lexical_env = env->lexical_env;
	  saved_heap_index = env->heap_index;
	  if (setjmp(env->top_level) == 0) {
	       fwprintf(stdout, L"\nKISS>");
	       fflush(stdout);

	       form = kiss_cread(kiss_standard_input(), KISS_NIL, KISS_EOS);

	       if (form == KISS_EOS) break;

	       kiss_print(kiss_eval(form));
	       fflush(stdout);
	  } else {
	       kiss_obj* result = env->throw_result;
	       if (!KISS_IS_STRING(result)) {
		    fwprintf(stderr, L"\nKISS| Internal error. Kiss_Err threw non-string obj.\n");
		    fwprintf(stderr, L"\n");
		    exit(1);
	       } else {
		    kiss_string_t* msg = (kiss_string_t*)result;
		    fwprintf(stderr, L"\nKISS| ");
		    fwprintf(stderr, L"%ls\n", msg->str);
		    fflush(stderr);
		    fflush(stdout);
		    wint_t c;
		    while ((c = getwchar()) != L'\n' && c != WEOF);
		    env->dynamic_env = saved_dynamic_env;
		    env->lexical_env = saved_lexical_env;
	       }
	  }
	  env->heap_index = saved_heap_index;
     }
}
