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

static char* libraries[] = {
     "cons_l.lisp",
     "character_l.lisp",
     "stream_l.lisp",
     "sequence_l.lisp",
     "control_l.lisp",
     "number_l.lisp",
     "string_l.lisp",
     "oo_obj_l.lisp",
     "class_l.lisp",
     "built_in_classes.lisp",
     "generic_function.lisp",
     "gf_invoke_l.lisp",
     "built_in_conditions.lisp",
     "condition.lisp",
     "format_oo_object.lisp",
     "init.lisp",
     NULL,
};

kiss_obj* kiss_load(kiss_obj* filename) {
     size_t saved_heap_top = Kiss_Heap_Top;
     kiss_obj* in = kiss_open_input_file(filename, KISS_NIL);
     kiss_obj* form = kiss_c_read(in, KISS_NIL, KISS_EOS);
     while (form != KISS_EOS) {
	  kiss_eval(form);
	  form = kiss_c_read(in, KISS_NIL, KISS_EOS);
     }
     Kiss_Heap_Top = saved_heap_top;
     return KISS_T;
}

void kiss_load_library(char* name) {
     kiss_environment_t* env = Kiss_Get_Environment();
     if (setjmp(env->top_level) == 0) {
	  fprintf(stderr, "loading %s ... ", name);
	  fflush(stderr);
          wchar_t* buf = kiss_mbstowcs(name);
	  kiss_load((kiss_obj*)kiss_make_string(buf));
          free(buf);
	  fprintf(stderr, "done \n");
	  fflush(stderr);
     } else {
	  kiss_obj* result = env->throw_result;
	  if (!KISS_IS_STRING(result)) {
	       fprintf(stderr, "\nKISS| Internal error. Kiss_Err threw non-string object.\n");
	       fprintf(stderr, "\n");
	  } else {
	       fprintf(stderr, "initialization failed\n"); fflush(stderr);
	       kiss_string_t* msg = (kiss_string_t*)result;
	       fprintf(stderr, "\nKISS| ");
               char* buf = kiss_wcstombs(msg->str);
	       fprintf(stderr, "%s\n", buf);
               free(buf);
	  }
	  exit(EXIT_FAILURE);
     }
}

static char *line_read = (char *)NULL;

wchar_t* rl_gets () {
     /* If the buffer has already been allocated,
        return the memory to the free pool. */
     if (line_read) {
          free (line_read);
          line_read = (char *)NULL;
     }

     /* Get a line from the user. */
     line_read = readline ("KISS>");

     /* If the line has any text in it,
        save it on the history. */
     if (line_read && *line_read)
          add_history (line_read);

     return kiss_mbstowcs(line_read);
}

int kiss_read_eval_print_loop(void) {
     kiss_environment_t* env = Kiss_Get_Environment();
     kiss_obj* form;
     kiss_lexical_environment_t saved_lexical_env;
     kiss_dynamic_environment_t saved_dynamic_env;
     size_t i, saved_heap_top;

     for (i = 0; libraries[i] != NULL; i++) {
	  kiss_load_library(libraries[i]);
     }


     while (1) {
	  saved_dynamic_env = env->dynamic_env;
	  saved_lexical_env = env->lexical_env;
	  saved_heap_top = Kiss_Heap_Top;
	  if (setjmp(env->top_level) == 0) {
	       fflush(stdout);

               wchar_t* str = NULL;
               if (isatty(fileno(stdin))) {
                    str = rl_gets();
               } else {
                    fprintf(stdout, "\nKISS>");
                    fflush(stdout);
                    str = ((kiss_string_t*)kiss_c_read_line(kiss_standard_input(), KISS_T, KISS_EOS))->str;
               }
               if (str == NULL) break;
               kiss_obj* in = kiss_create_string_input_stream((kiss_obj*)kiss_make_string(str));
               free(str);
               form = kiss_c_read(in, KISS_T, KISS_EOS);

	       kiss_obj* result = kiss_eval(form);
	       kiss_format_fresh_line(kiss_standard_output());
	       kiss_print(result);
	       fflush(stdout);
	  } else {
	       kiss_obj* result = env->throw_result;
	       if (!KISS_IS_STRING(result)) {
		    fprintf(stderr, "\nKISS| Internal error. Kiss_Err threw non-string obj.\n");
		    fprintf(stderr, "\n");
		    exit(1);
	       } else {
		    kiss_string_t* msg = (kiss_string_t*)result;
		    fprintf(stderr, "\nKISS| ");
                    char* s = kiss_wcstombs(msg->str);
		    fprintf(stderr, "%s\n", s);
                    free(s);
		    fflush(stderr);
		    fflush(stdout);
		    env->dynamic_env = saved_dynamic_env;
		    env->lexical_env = saved_lexical_env;
	       }
	  }
	  Kiss_Heap_Top = saved_heap_top;
     }
     return 0;
}
