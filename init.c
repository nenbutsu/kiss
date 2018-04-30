/*  -*- coding: utf-8 -*-
  init.c --- defines the initialization mechanism of ISLisp processor KISS.

  Copyright (C) 2017, 2018 Yuji Minejima <yuji@minejima.jp>

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
    kiss_obj* tag = kiss_symbol(L"kiss::error");
    kiss_catcher_t* c = kiss_make_catcher(tag, env->top_level);
    env->dynamic_env.jumpers = kiss_cons((kiss_obj*)c, env->dynamic_env.jumpers);
}

static wchar_t* libraries[] = {
     L"lisp/setf.lisp",
     L"lisp/cons.lisp",
     L"lisp/character.lisp",
     L"lisp/stream.lisp",
     L"lisp/sequence.lisp",
     L"lisp/control.lisp",
     L"lisp/string.lisp",
//     L"lisp/ilos.lisp",
     L"lisp/condition.lisp",
     L"lisp/init.lisp",
     NULL,
};

void kiss_load_library(wchar_t* name) {
     kiss_environment_t* env = Kiss_Get_Environment();
     if (setjmp(env->top_level) == 0) {
	  fwprintf(stderr, L"loading %ls ... ", name); fflush(stderr);
	  kiss_load((kiss_obj*)kiss_make_string(name)); // emits garbage
	  fwprintf(stderr, L"done \n");
	  fflush(stderr);
     } else {
	  kiss_obj* result = env->throw_result;
	  if (!KISS_IS_STRING(result)) {
	       fwprintf(stderr, L"\nKISS| Internal error. Kiss_Err threw non-string object.\n");
	       fwprintf(stderr, L"\n");
               fflush(stderr);
	  } else {
	       fwprintf(stderr, L"initialization failed\n");
	       kiss_string_t* msg = (kiss_string_t*)result;
	       fwprintf(stderr, L"\nKISS| ");
	       fwprintf(stderr, L"%ls\n", msg->str);
               fflush(stderr);
	  }
	  exit(EXIT_FAILURE);
     }
}

void kiss_initialize(void) {

     assert(sizeof(long int) == sizeof(kiss_gc_obj*));
     
     setlocale (LC_ALL, "");
     fwide(stdin,  1); // wide oriented
     fwide(stdout, 1); // wide oriented
     fwide(stderr, 1); // wide oriented
     fwprintf(stderr, L"LOCALE = %s\n", setlocale(LC_ALL, NULL));
     kiss_init_environment();
     kiss_init_symbols();
     kiss_init_streams();
     kiss_init_ilos();
     kiss_init_error_catcher();

     for (size_t i = 0; libraries[i] != NULL; i++) {
	  kiss_load_library(libraries[i]);
     }
     
}
