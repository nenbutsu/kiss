/*  -*- coding: utf-8 -*-
  function.c --- defines the function mechanism of ISLisp processor KISS.

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

kiss_function_t* kiss_make_function(kiss_symbol_t* name, kiss_obj* lambda) {
    kiss_function_t* p = Kiss_GC_Malloc(sizeof(kiss_function_t));
    kiss_environment_t* env = Kiss_Get_Environment();
    p->type = KISS_FUNCTION;
    p->name = name;
    p->lambda = KISS_NIL;
    p->lexical_env = env->lexical_env;
    p->lambda = Kiss_Lambda_Expression(lambda); // might gc
    return p;
}

static kiss_function_t* kiss_make_macro(kiss_symbol_t* name, kiss_obj* lambda) {
    kiss_function_t* p = kiss_make_function(name, lambda);
    p->type = KISS_MACRO;
    return p;
}

kiss_obj* kiss_simple_function_p(kiss_obj* obj) {
    if (KISS_IS_FUNCTION(obj) || KISS_IS_CFUNCTION(obj)) {
	return KISS_T;
    } else {
	return KISS_NIL;
    }
}

void kiss_bind_funargs(kiss_obj* params, kiss_obj* args) {
    kiss_environment_t* env = Kiss_Get_Environment();
    int nparam = kiss_c_length(params);
    int narg = kiss_c_length(args);
    int rest = kiss_member((kiss_obj*)&KISS_Skw_rest,  params) != KISS_NIL ||
         kiss_member((kiss_obj*)&KISS_Samp_rest, params) != KISS_NIL;
    if (!rest && nparam < narg) {
	Kiss_Err(L"Too many arguments ~S ~S", params, args);
    }
    
    if ((!rest && nparam > narg) || (rest && nparam - 2 > narg)) {
	Kiss_Err(L"Too few arguments ~S ~S", params, args);
    }
    while (KISS_IS_CONS(params)) {
	kiss_obj* p = KISS_CAR(params);
	kiss_obj* a = KISS_CAR(args);
	if (p == (kiss_obj*)&KISS_Samp_rest || p == (kiss_obj*)&KISS_Skw_rest) {
	    params = KISS_CDR(params);
	    p = KISS_CAR(params);
	    a = kiss_copy_list(args);
	}
	kiss_push(kiss_cons(p, a), &env->lexical_env.vars);
	args = KISS_CDR(args);
	params = KISS_CDR(params);
    }
}

kiss_obj* kiss_linvoke(kiss_function_t* fun, kiss_obj* args) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* body = KISS_CDDR(fun->lambda);
    kiss_lexical_environment_t saved_lexical_env = env->lexical_env;
    kiss_obj* result;
    env->lexical_env = fun->lexical_env;
    kiss_bind_funargs(kiss_cadr(fun->lambda), args);
    result = kiss_eval_body(body);
    env->lexical_env = saved_lexical_env;
    return result;
}

/* special operator: (lambda lambda-list form*) -> <function> */
kiss_obj* kiss_lambda(kiss_obj* params, kiss_obj* body) {
    kiss_obj* lambda =
         kiss_c_list(3, (kiss_obj*)&KISS_Slambda, params,
		   kiss_c_append(2, kiss_c_list(2, &KISS_Sblock, &KISS_Slambda), body));
    /* (lambda () . body) -> (lambda () (block lambda . body)) */
    return (kiss_obj*)kiss_make_function(NULL, lambda);
}

/* special operator: (defun function-name lambda-list form*) -> <symbol> */
kiss_obj* kiss_defun(kiss_obj* name, kiss_obj* params, kiss_obj* body) {
    kiss_symbol_t* fname = Kiss_Symbol(name);
    kiss_obj* lambda = kiss_c_list(3, &KISS_Slambda, params,
				  kiss_c_append(2, kiss_c_list(2, &KISS_Sblock, name), body));
    /* (defun foo () . body) -> (defun foo () (block foo . body)) */
    fname->fun = (kiss_obj*)kiss_make_function(fname, lambda);
    return name;
}

/* special operator: (defmacro macro-name lambda-list form*) -> <symbol> */
kiss_obj* kiss_defmacro(kiss_obj* name, kiss_obj* params, kiss_obj* body) {
    kiss_symbol_t* fname = Kiss_Symbol(name);
    kiss_obj* lambda = kiss_c_list(3, &KISS_Slambda, params,
				  kiss_c_append(2, kiss_c_list(2, &KISS_Sblock, name), body));
    /* (defmacro foo () . body) -> (defmacro foo () (block foo . body)) */
    fname->fun = (kiss_obj*)kiss_make_macro(fname, lambda);
    return name;
}

kiss_obj* kiss_fun_ref(kiss_symbol_t* name) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* binding = kiss_assoc((kiss_obj*)name, env->lexical_env.funs);
    if (KISS_IS_CONS(binding)) { return KISS_CDR(binding); }
    if (name->fun == NULL) { Kiss_Err(L"Unbound function ~S", name); }
    return name->fun;
}

/* special operator: (function function-name) -> <function>
     An error shall be signaled if no binding has been established for the
     identifier in the function namespace of current lexical environment
     (see §9.2) (error-id. undefined-function). The consequences are
     undefined if the function-name names a macro or special form. */
kiss_obj* kiss_function(kiss_obj* name) { return kiss_fun_ref(Kiss_Symbol(name)); }


/* function: (funcall function obj*) -> <object> */
kiss_obj* kiss_funcall(const kiss_obj* const f, const kiss_obj* const args) {
     switch (KISS_OBJ_TYPE(f)) {
     case KISS_CFUNCTION:
	  return kiss_cf_invoke((kiss_cfunction_t*)f, (kiss_obj*)args);
     case KISS_FUNCTION:
	  return kiss_linvoke((kiss_function_t*)f, (kiss_obj*)args);
     case KISS_ILOS_OBJ:
	  if (kiss_c_funcall(L"generic-function-p", kiss_c_list(1, f)) == KISS_T) {
	       return kiss_c_funcall(L"generic-function-invoke", kiss_c_list(2, f, args));
	  }
	  // fall through
     default:
	  Kiss_Err(L"Not a function ~S", f);
     }
    
}

kiss_obj* kiss_c_funcall(wchar_t* function_name, kiss_obj* args) {
    return kiss_funcall(kiss_symbol_function(kiss_symbol(function_name)), args);
}

/*
  args := (list) || (arg list)   || (arg1 arg2 list)    || ...
  result = list  || (arg . list) || (arg1 arg2  . list) || ...
 */
static kiss_cons_t* kiss_flatten_apply_args(kiss_cons_t* args) {
    kiss_cons_t* p = args;
    kiss_cons_t* head = (kiss_cons_t*)kiss_cons(KISS_NIL, KISS_NIL);
    kiss_cons_t* tail = head;
    while (KISS_IS_CONS(p->cdr)) { /* loop ends at last cons */
	tail->cdr = kiss_cons(KISS_CAR(p), KISS_NIL);
	tail = (kiss_cons_t*)tail->cdr;
	p = (kiss_cons_t*)p->cdr;
    }
    tail->cdr = Kiss_Proper_List(p->car);
    return (kiss_cons_t*)head->cdr;
}

/* function: (apply function obj* list) -> <object> */
kiss_obj* kiss_apply(kiss_obj* f, kiss_obj* obj, kiss_obj* rest) {
    kiss_obj* args = (kiss_obj*)kiss_flatten_apply_args((kiss_cons_t*)kiss_cons(obj, rest));
    return kiss_funcall(f, args);
}

/* special operator:
   (flet ((function-name lambda-list form*)*) body-form*) -> <object> */
kiss_obj* kiss_flet(kiss_obj* fspecs, kiss_obj* body) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* saved_funs = env->lexical_env.funs;
    kiss_obj* stack = KISS_NIL;
    kiss_obj* result;
    for (fspecs = Kiss_Proper_List(fspecs); KISS_IS_CONS(fspecs);
	 fspecs = KISS_CDR(fspecs))
    {
	kiss_obj* spec = Kiss_Proper_List(KISS_CAR(fspecs));
	kiss_symbol_t* name = Kiss_Symbol(kiss_car(spec));
	kiss_obj* lambda = kiss_cons((kiss_obj*)&KISS_Slambda, kiss_cdr(spec));
	kiss_function_t* fun = kiss_make_function(name, lambda);
	kiss_push(kiss_cons((kiss_obj*)name, (kiss_obj*)fun), &stack);
    }
    env->lexical_env.funs = kiss_c_append(2, stack, env->lexical_env.funs);
    result = kiss_eval_body(body);
    env->lexical_env.funs = saved_funs;
    return result;
}

/* special operator
   (labels ((function-name lambda-list form*)*) body-form*) -> <object>

   the scope of an identifier FUNCTION-NAME is the whole labels special
   form (excluding nested scopes, if any);

   Example:
   (labels ((evenp (n) (if (= n 0) t   (oddp (- n 1))))
            (oddp (n)  (if (= n 0) nil (evenp (- n 1)))))
      (evenp 88)) => t   
*/
kiss_obj* kiss_labels(kiss_obj* fspecs, kiss_obj* body) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* saved_funs = env->lexical_env.funs;
    kiss_obj* stack = KISS_NIL;
    kiss_obj* result;
    Kiss_Proper_List(fspecs);
    for (fspecs = Kiss_Proper_List(fspecs); KISS_IS_CONS(fspecs);
	 fspecs = KISS_CDR(fspecs))
    {
	kiss_obj* spec = Kiss_Proper_List(KISS_CAR(fspecs));
	kiss_symbol_t* name = Kiss_Symbol(kiss_car(spec));
	kiss_obj* lambda = kiss_cons((kiss_obj*)&KISS_Slambda, kiss_cdr(spec));
	/* temporarily store lambda expression instead of function object */
	kiss_push(kiss_cons((kiss_obj*)name, lambda), &stack); 
    }

    /* make local function names visible to the following make_function call
       which captures the current lexical environment. */
    env->lexical_env.funs = kiss_c_append(2, stack, env->lexical_env.funs);
    while (KISS_IS_CONS(stack)) {
	kiss_cons_t* binding = (kiss_cons_t*)KISS_CAR(stack);
	kiss_function_t* fun =
	    kiss_make_function((kiss_symbol_t*)binding->car, binding->cdr);
	/* write function object over lambda expr.*/
	binding->cdr = (kiss_obj*)fun;
	stack = KISS_CDR(stack);
    }
    result = kiss_eval_body(body);
    env->lexical_env.funs = saved_funs;
    return result;
}

