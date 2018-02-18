/*  -*- coding: utf-8 -*-
  gf_invoke.c --- defines the generic function invocation mechanism of ISLisp processor KISS.

  Copyright (C) 2017, 2018 Yuji Minejima (yuji@minejima.jp).

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

kiss_obj* kiss_next_method_p(void) {
     kiss_environment_t* env = Kiss_Get_Environment();
     return KISS_IS_CONS(env->gf_invocation.next_methods) ? KISS_T : KISS_NIL;
}

kiss_obj* kiss_call_next_method(void) {
     
}


kiss_lexical_environment_t Kiss_Null_Lexical_Env = {
    KISS_NIL, /* vars */
    KISS_NIL, /* funcs */
    KISS_NIL, /* jumpers */
};

/* local function: (call-next-method) -> <object> */
/* local function: (next-method-p) -> boolean */
static kiss_obj* kiss_true(void) { return KISS_T; }
kiss_cfunction_t KISS_CFtrue = {
    KISS_CFUNCTION,        /* type */
    NULL,                  /* name */
    (kiss_cf_t*)kiss_true, /* C function name */
    0,                     /* minimum argument number */
    0,                     /* maximum argument number */
};

static kiss_obj* kiss_false(void) { return KISS_NIL; }
kiss_cfunction_t KISS_CFfalse = {
    KISS_CFUNCTION,         /* type */
    NULL,                   /* name */
    (kiss_cf_t*)kiss_false, /* C function name */
    0,                      /* minimum argument number */
    0,                      /* maximum argument number */
};

kiss_obj* kiss_next_method_error(void) {
    Kiss_Err(L"Next method doesn't exist");
}
kiss_cfunction_t KISS_CFnext_method_error = {
    KISS_CFUNCTION,                     /* type */
    NULL,                               /* name */
    (kiss_cf_t*)kiss_next_method_error, /* C function name */
    0,                                  /* minimum argument number */
    0,                                  /* maximum argument number */
};

static void kiss_bind_methodargs(const kiss_obj* const m) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj *binding, *next;
    /* fwprintf(stderr, L"bind_methodargs\n"); fflush(stderr); */
    /* kiss_print(kiss_plist_get(kiss_ilos_obj_plist(m),
       kiss_symbol(":lambda-list"))); */
    kiss_bind_funargs((kiss_obj*)kiss_symbol(L"{generic-function}"),
                      kiss_oref(m, kiss_symbol(L":lambda-list")),
		      kiss_oref(m, kiss_symbol(L":args")));
    next = kiss_oref(m, kiss_symbol(L":next"));
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

void kiss_call_methods(kiss_obj* methods) {
     Kiss_List(methods);
     for (; KISS_IS_CONS(methods); methods = KISS_CDR(methods)) {
          kiss_method_invoke(KISS_CAR(methods));
     }
}

kiss_obj* kiss_method_invoke(const kiss_obj* const m) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_lexical_environment_t saved_lexical_env = env->lexical_env;
    kiss_obj* result;

    kiss_call_methods(kiss_oref(m, kiss_symbol(L":before")));
    env->lexical_env = Kiss_Null_Lexical_Env;
    kiss_bind_methodargs(m);
    result = kiss_eval_body(kiss_oref(m, kiss_symbol(L":body")));
    kiss_call_methods(kiss_oref(m, kiss_symbol(L":after")));

    env->lexical_env = saved_lexical_env;
    return result;
}



