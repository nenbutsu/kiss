/*  -*- coding: utf-8 -*-
  repl.c --- defines the read, eval, print mechanism of ISLisp processor KISS.

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

int kiss_read_eval_print_loop(void) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_lexical_environment_t saved_lexical_env;
    kiss_dynamic_environment_t saved_dynamic_env;
    kiss_obj* form;
    saved_lexical_env = env->lexical_env;
    saved_dynamic_env = env->dynamic_env;
    while (1) {
	if (setjmp(env->top_level) == 0) {
	    env->lexical_env = saved_lexical_env;
	    env->dynamic_env = saved_dynamic_env;

	    env->call_nest = 0;
	    
	    fprintf(stdout, "\nKISS>");
	    fflush(stdout);
	    form = kiss_cread(kiss_standard_input(), KISS_NIL, KISS_EOS);
	    /* fprintf(stderr, "read working\n");fflush(stderr); */
	    if (form == KISS_EOS) break;
	    kiss_print(kiss_eval(form));
	    /* kiss_format_object(kiss_standard_output(), form, KISS_T); */
	    /* format_object(standard_output(), form, T); */
	    fflush(stdout);
	} else {
	    kiss_obj* result = env->throw_result;
	    if (result->type != KISS_STRING) {
		fprintf(stderr, "\nKISS| Internal error. Kiss_Err threw non-string object.\n");
		fprintf(stderr, "%s\n");
		exit(1);
	    } else {
		kiss_string_t* msg = (kiss_string_t*)result;
		fprintf(stderr, "\nKISS| ");
		fprintf(stderr, "%s\n", msg->str);
		fflush(stderr);
		fflush(stdin);
		fflush(stdout);
	    }
	}
    }
}
