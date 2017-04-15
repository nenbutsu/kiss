/*  -*- coding: utf-8 -*-
  init.c --- defines the initialization mechanism of ISLisp processor KISS.

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

void kiss_init_error_catcher(void) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* tag = kiss_symbol("kiss::error-tag");
    kiss_catcher_t* c = kiss_make_catcher(tag, env->top_level);
    env->dynamic_env.jumpers = kiss_cons((kiss_obj*)c,
					 env->dynamic_env.jumpers);
}

static char* kiss_libraries[] = {
    "cons.lisp",
    "character.lisp",
    "sequence.lisp",
    "control.lisp",
    "number.lisp",
    "format_oo_object.lisp",
    "string.lisp",
    "object.lisp",
    "class.lisp",
    "built_in_classes.lisp",
    "generic_function.lisp",
    "gf_invoke.lisp",
    "built_in_conditions.lisp",
/*    "format.lisp", */
    "condition.lisp",
    NULL,
};

void kiss_initialize(void) {
    /* fprintf(stderr, "start init: environment, symbols, streams\n"); fflush(stderr); */
    kiss_environment_t* env;
    /*    char* current_locale = setlocale(LC_ALL, ""); */
    kiss_init_environment();
    kiss_init_symbols();
    kiss_init_streams();
    kiss_init_error_catcher();
    /*
    if (current_locale == NULL) {
	fprintf(stderr, "setlocale error\n");
	exit(1);
    }
    fprintf(stderr, "current locale = %s\n", current_locale);
    */
/*    GC_INIT(); */

    env = Kiss_Get_Environment();
    if (setjmp(env->top_level) == 0) {
	size_t i;
	for (i = 0; kiss_libraries[i] != NULL; i++) {
	    fprintf(stderr, "loading %s ... ", kiss_libraries[i]);
	    fflush(stderr);
	    kiss_load((kiss_obj*)kiss_make_string(kiss_libraries[i]));
	    fprintf(stderr, "done \n");
	    fflush(stderr);
	}
    } else {
	kiss_obj* result = env->throw_result;
	if (result->type != KISS_STRING) {
	    fprintf(stderr, "\nKISS| Internal error. Kiss_Err threw non-string object.\n");
	    fprintf(stderr, "%s\n");
	} else {
	    fprintf(stderr, "initialization failed\n"); fflush(stderr);
	    kiss_string_t* msg = (kiss_string_t*)result;
	    fprintf(stderr, "\nKISS| ");
	    fprintf(stderr, "%s\n", msg->str);
	}
	exit(1);
    }

}
