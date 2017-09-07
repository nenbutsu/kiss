/*  -*- coding: utf-8 -*-
  variable.c --- defines the variable handling mechanism of ISLisp processor KISS.

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

kiss_obj* kiss_var_ref(kiss_symbol_t* name) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* binding = kiss_assoc((kiss_obj*)name, env->lexical_env.vars);
    if (KISS_IS_CONS(binding)) { return KISS_CDR(binding); }
    if (name->var == NULL) { Kiss_Unbound_Variable_Error((kiss_obj*)name); }
    return name->var;
}


/* special operator: (setq var form) -> <object> */
kiss_obj* kiss_setq(kiss_obj* name, kiss_obj* form) {
     kiss_environment_t* env = Kiss_Get_Environment();
     kiss_symbol_t* symbol = Kiss_Symbol(name);
     kiss_obj* binding = kiss_assoc(name, env->lexical_env.vars);
     if (KISS_IS_CONS(binding)) {
	  kiss_obj* value = kiss_eval(form);
	  ((kiss_cons_t*)binding)->cdr = value;
	  return value;
     } else if (symbol->var == NULL) {
	  Kiss_Unbound_Variable_Error(name);
     }
     kiss_obj* value;
     value = kiss_eval(form);
     symbol->var = value;
     return value;
}

/* defining operator: (defglobal name form) -> <symbol> */
kiss_obj* kiss_defglobal(kiss_obj* name, kiss_obj* form) {
    kiss_symbol_t* symbol = Kiss_Symbol(name);
    kiss_obj* value = kiss_eval(form);
    symbol->var = value;
    return name;
}

/* defining operator: (defconstant name form) -> <symbol> */
kiss_obj* kiss_defconstant(kiss_obj* name, kiss_obj* form) {
    kiss_symbol_t* symbol = Kiss_Symbol(name);
    kiss_obj* value = kiss_eval(form);
    symbol->var = value;
    symbol->flags = symbol->flags | KISS_CONSTANT_VAR;
    return name;
}

/* special operator: (let ((var form)*) body-form*) -> <object> */
kiss_obj* kiss_let(kiss_obj* vspecs, kiss_obj* body) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* saved_lexical_vars = env->lexical_env.vars;
    kiss_obj* stack = KISS_NIL;
    kiss_obj* result;
    Kiss_Proper_List(vspecs);
    for (vspecs = Kiss_Proper_List(vspecs); KISS_IS_CONS(vspecs); vspecs = KISS_CDR(vspecs)) {
	kiss_cons_t* spec = Kiss_Proper_List_2(KISS_CAR(vspecs));
	kiss_symbol_t* name = Kiss_Symbol(KISS_CAR(spec));
	kiss_obj* value = kiss_eval(KISS_CADR(spec));
	kiss_push(kiss_cons((kiss_obj*)name, value), &stack);
    }
    env->lexical_env.vars = kiss_cappend(2, stack, env->lexical_env.vars);
    result = kiss_eval_body(body);
    env->lexical_env.vars = saved_lexical_vars;
    return result;
}

/* special operator: (let* ((var form)*) body-form*) -> <object> */
kiss_obj* kiss_let_s(kiss_obj* vspecs, kiss_obj* body) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* saved_lexical_vars = env->lexical_env.vars;
    kiss_obj* result;
    for (vspecs = Kiss_Proper_List(vspecs); KISS_IS_CONS(vspecs); vspecs = KISS_CDR(vspecs)) {
	kiss_cons_t* spec = Kiss_Proper_List_2(KISS_CAR(vspecs));
	kiss_symbol_t* name = Kiss_Symbol(KISS_CAR(spec));
	kiss_obj* value = kiss_eval(KISS_CADR(spec));
	kiss_push(kiss_cons((kiss_obj*)name, value), &env->lexical_env.vars);
    }
    result = kiss_eval_body(body);
    env->lexical_env.vars = saved_lexical_vars;
    return result;
}


/* defining operator: (defdynamic name form) -> <symbol> */
kiss_obj* kiss_defdynamic(kiss_obj* name, kiss_obj* form) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* value = kiss_eval(form);
    kiss_obj* binding = kiss_assoc(name, env->global_dynamic_vars);
    if (KISS_IS_CONS(binding)) {
	((kiss_cons_t*)binding)->cdr = value;
    } else {
	kiss_push(kiss_cons(name, value) ,&(env->global_dynamic_vars));
    }
	
    return name;
}

/* special operator: (dynamic var) -> <object> */
kiss_obj* kiss_dynamic(kiss_obj* name) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* binding = kiss_assoc(name, env->dynamic_env.vars);
    if (KISS_IS_CONS(binding)) { return KISS_CDR(binding); }
    binding = kiss_assoc(name, env->global_dynamic_vars);
    if (KISS_IS_CONS(binding)) { return KISS_CDR(binding); }
    Kiss_Unbound_Variable_Error((kiss_obj*)name);
    exit(EXIT_FAILURE); // not reach here     
}

/* special operator: (set-dynamic form var) -> <object>
  This special form denotes an assignment to a dynamic variable. This
  form can appear anywhere that (dynamic var) can appear.  form is
  evaluated and the result of the evaluation is used to change the
  dynamic binding of var.  An error shall be signaled if var has no
  dynamic value (error-id. unbound-variable). setf of dynamic can be
  used only for modifying bindings, and not for establishing them.
 */
kiss_obj* kiss_set_dynamic(kiss_obj* form, kiss_obj* name) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* value = kiss_eval(form);
    kiss_obj* binding = kiss_assoc(name, env->dynamic_env.vars);
    if (KISS_IS_CONS(binding)) {
	kiss_set_cdr(value, binding);
	return value;
    }
    binding = kiss_assoc(name, env->global_dynamic_vars);
    if (KISS_IS_CONS(binding)) {
	kiss_set_cdr(value, binding);
	return value;
    }
    Kiss_Unbound_Variable_Error((kiss_obj*)name);
    exit(EXIT_FAILURE); // not reach here     
}


/* special operator: (dynamic-let ((var form)*) body-form*) -> <object> */
kiss_obj* kiss_dynamic_let(kiss_obj* vspecs, kiss_obj* body) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* saved_dynamic_vars = env->dynamic_env.vars;
    kiss_obj* stack = KISS_NIL;
    kiss_obj* result;
    Kiss_Proper_List(vspecs);
    for (vspecs = Kiss_Proper_List(vspecs); KISS_IS_CONS(vspecs); vspecs = KISS_CDR(vspecs)) {
	kiss_cons_t* spec = Kiss_Proper_List_2(KISS_CAR(vspecs));
	kiss_symbol_t* name = Kiss_Symbol(KISS_CAR(spec));
	kiss_obj* value = kiss_eval(KISS_CADR(spec));
	kiss_push(kiss_cons((kiss_obj*)name, value), &stack);
    }
    env->dynamic_env.vars = kiss_cappend(2, stack, env->dynamic_env.vars);
    result = kiss_eval_body(body);
    env->dynamic_env.vars = saved_dynamic_vars;
    return result;
}
