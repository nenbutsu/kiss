/*  -*- coding: utf-8 -*-
  gf_invoke.c --- defines the generic function invocation mechanism of ISLisp processor KISS.

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

kiss_lexical_environment_t Kiss_Null_Lexical_Env = {
    KISS_NIL, /* vars */
    KISS_NIL, /* funcs */
    KISS_NIL, /* jumpers */
};

/* local function: (call-next-method) -> <object> */
/* local function: (next-method-p) -> boolean */
static kiss_obj* kiss_true(void) { return KISS_T; }
kiss_cfunction_t KISS_CFtrue = {
    KISS_CFUNCTION, /* type */
    NULL,      /* name */
    kiss_true,      /* C function name */
    0,         /* minimum argument number */
    0,         /* maximum argument number */
};

static kiss_obj* kiss_false(void) { return KISS_NIL; }
kiss_cfunction_t KISS_CFfalse = {
    KISS_CFUNCTION, /* type */
    NULL,      /* name */
    kiss_false,    /* C function name */
    0,         /* minimum argument number */
    0,         /* maximum argument number */
};

kiss_obj* kiss_next_method_error(void) {
    Kiss_Err(L"Next method doesn't exist");
}
kiss_cfunction_t KISS_CFnext_method_error = {
    KISS_CFUNCTION,          /* type */
    NULL,               /* name */
    kiss_next_method_error, /* C function name */
    0,                  /* minimum argument number */
    0,                  /* maximum argument number */
};

static void kiss_bind_methodargs(kiss_obj* m) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj *binding, *next;
    /* fwprintf(stderr, "bind_methodargs\n"); fflush(stderr); */
    /* kiss_print(kiss_plist_get(kiss_object_plist(m),
       kiss_symbol(":lambda-list"))); */
    kiss_bind_funargs(kiss_object_plist_get(m, kiss_symbol(L":lambda-list")),
		      kiss_object_plist_get(m, kiss_symbol(L":args")));
    next = kiss_object_plist_get(m, kiss_symbol(L":next"));
    if (next != KISS_NIL) {
	binding = kiss_cons(kiss_symbol(L"next-method-p"),
			    (kiss_obj*)&KISS_CFtrue);
	env->lexical_env.funs = kiss_cons(binding, env->lexical_env.funs);
	binding = kiss_cons(kiss_symbol(L"call-next-method"), next);
	env->lexical_env.funs = kiss_cons(binding, env->lexical_env.funs);
    } else {
	binding = kiss_cons(kiss_symbol(L"next-method-p"),
			    (kiss_obj*)&KISS_CFfalse);
	env->lexical_env.funs = kiss_cons(binding, env->lexical_env.funs);
	binding = kiss_cons(kiss_symbol(L"call-next-method"),
			    (kiss_obj*)&KISS_CFnext_method_error);
	env->lexical_env.funs = kiss_cons(binding, env->lexical_env.funs);
    }
}

void kiss_call_methods(kiss_obj* methods);
kiss_obj* kiss_method_invoke(kiss_obj* m) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_lexical_environment_t saved_lexical_env = env->lexical_env;
    kiss_obj* result;

    kiss_call_methods(kiss_object_plist_get(m, kiss_symbol(L":before")));
    env->lexical_env = Kiss_Null_Lexical_Env;
    kiss_bind_methodargs(m);
    result = kiss_eval_body(kiss_object_plist_get(m, kiss_symbol(L":body")));
    kiss_call_methods(kiss_object_plist_get(m, kiss_symbol(L":after")));
    env->lexical_env = saved_lexical_env;
    return result;
}

void kiss_call_methods(kiss_obj* methods) {
    for (methods = Kiss_List(methods); KISS_IS_CONS(methods); methods = KISS_CDR(methods))
    {
	kiss_method_invoke(KISS_CAR(methods));
    }
}


