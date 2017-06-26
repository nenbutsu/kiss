/*  -*- coding: utf-8 -*-
  control.c --- defines the control mechanism of ISLisp processor KISS.

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

kiss_catcher_t* kiss_make_catcher(kiss_obj* tag, jmp_buf jmp) {
    kiss_catcher_t* p = Kiss_GC_Malloc(sizeof(kiss_catcher_t));
    kiss_environment_t* env = Kiss_Get_Environment();
    p->type = KISS_CATCHER;
    p->tag = tag;
    p->jmp = jmp;
    p->dynamic_env = env->dynamic_env;
    return p;
}

static kiss_block_t* kiss_make_block(kiss_symbol_t* name, jmp_buf jmp) {
    kiss_block_t* p = Kiss_GC_Malloc(sizeof(kiss_block_t));
    kiss_environment_t* env = Kiss_Get_Environment();
    p->type = KISS_BLOCK;
    p->name = name;
    p->jmp  = jmp;
    p->dynamic_env = env->dynamic_env;
    return p;
}

static kiss_tagbody_t* kiss_make_tagbody(kiss_symbol_t* tag, jmp_buf jmp, kiss_obj* body) {
    kiss_tagbody_t* p = Kiss_GC_Malloc(sizeof(kiss_tagbody_t));
    kiss_environment_t* env = Kiss_Get_Environment();
    p->type = KISS_TAGBODY;
    p->tag = tag;
    p->jmp = jmp;
    p->dynamic_env = env->dynamic_env;
    p->body = body;
    return p;
}

static kiss_cleanup_t* kiss_make_cleanup(kiss_obj* body) {
    kiss_cleanup_t* p = Kiss_GC_Malloc(sizeof(kiss_cleanup_t));
    kiss_environment_t* env = Kiss_Get_Environment();
    p->type = KISS_CLEANUP;
    p->body = body;
    p->lexical_env = env->lexical_env;
    p->dynamic_env = env->dynamic_env;
    return p;
}


/* special operator: (catch tag-form form*) → <object> */
kiss_obj* kiss_catch(kiss_obj* tag_form, kiss_obj* body) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* tag = kiss_eval(tag_form);
    kiss_lexical_environment_t saved_lexical_env = env->lexical_env;
    kiss_dynamic_environment_t saved_dynamic_env = env->dynamic_env;
    kiss_obj* result;
    jmp_buf jmp;
    if (setjmp(jmp) == 0) {
	kiss_catcher_t* c = kiss_make_catcher(tag, jmp);
	env->dynamic_env.jumpers = kiss_cons((kiss_obj*)c, env->dynamic_env.jumpers);
	/* make c visible when jumping back to c itself */
	c->dynamic_env.jumpers = env->dynamic_env.jumpers; 
	result = kiss_eval_body(body);
    } else {
	result = env->throw_result;
    }
    env->lexical_env = saved_lexical_env;
    env->dynamic_env = saved_dynamic_env;
    return result;
}

static kiss_catcher_t* kiss_catcher_ref(kiss_obj* tag) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* p;
    for (p = env->dynamic_env.jumpers; KISS_IS_CONS(p); p = KISS_CDR(p)) {
	kiss_obj* jmp = KISS_CAR(p);
	if (KISS_IS_CATCHER(jmp)) {
	    kiss_catcher_t* catcher = (kiss_catcher_t*)jmp;
	    if (catcher->tag == tag) { return catcher; }
	}
    }
    Kiss_Catcher_Not_Found_Error(tag);
}

static void kiss_eval_cleanups(kiss_obj* stop, kiss_obj* jumpers) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* p = env->dynamic_env.jumpers;
    while(KISS_CAR(p) != stop) {
	kiss_obj* x = KISS_CAR(p);
	if (KISS_IS_CLEANUP(x)) {
	    kiss_cleanup_t* cleanup = (kiss_cleanup_t*)x;
	    env->dynamic_env = cleanup->dynamic_env;
	    env->dynamic_env.jumpers = jumpers;
	    env->lexical_env = cleanup->lexical_env;
	    kiss_eval_body(cleanup->body);
	}
	p = KISS_CDR(p);
    }
}

/* special operator: (throw tag-form result-form) transfers control and data */
kiss_obj* kiss_throw(kiss_obj* tag_form, kiss_obj* result_form) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* tag = kiss_eval(tag_form);
    kiss_obj* result = kiss_eval(result_form);
    kiss_catcher_t* catcher = kiss_catcher_ref(tag);
    kiss_eval_cleanups((kiss_obj*)catcher, catcher->dynamic_env.jumpers);
    env->throw_result = result;
    longjmp(catcher->jmp, 1);
}

/* special operator: (unwind-protect form cleanup-form*) → <object> */
kiss_obj* kiss_unwind_protect(kiss_obj* protected_form, kiss_obj* cleanup_body) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* result;
    kiss_cleanup_t* cleanup = kiss_make_cleanup(cleanup_body);
    env->dynamic_env.jumpers = kiss_cons((kiss_obj*)cleanup, env->dynamic_env.jumpers);
    result = kiss_eval(protected_form);
    kiss_eval_body(cleanup_body);
    return result;
}


/* special operator: (block name form*) → <object>*/
kiss_obj* kiss_block(kiss_obj* name, kiss_obj* body) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_lexical_environment_t saved_lexical_env = env->lexical_env;
    kiss_dynamic_environment_t saved_dynamic_env = env->dynamic_env;
    kiss_obj* result;
    kiss_block_t* b;
    jmp_buf jmp;
    if (setjmp(jmp) == 0) {
	b = kiss_make_block(Kiss_Symbol(name), jmp);
	env->dynamic_env.jumpers = kiss_cons((kiss_obj*)b, env->dynamic_env.jumpers);
	/* make b visible when jumping back to b itself */
	b->dynamic_env.jumpers = env->dynamic_env.jumpers;
	env->lexical_env.jumpers = kiss_cons((kiss_obj*)b, env->lexical_env.jumpers);
	result = kiss_eval_body(body);
    } else {
	result = env->block_result;
    }
    env->lexical_env = saved_lexical_env;
    env->dynamic_env = saved_dynamic_env;
    return result;
}

static kiss_block_t* kiss_block_ref(kiss_symbol_t* name) {
     kiss_environment_t* env = Kiss_Get_Environment();
     kiss_obj* p;
     for (p = env->dynamic_env.jumpers; KISS_IS_CONS(p); p = KISS_CDR(p)) {
	  kiss_obj* x = KISS_CAR(p);
	  if (KISS_IS_BLOCK(x)) {
	       kiss_block_t* block = (kiss_block_t*)x;
	       if (block->name == name) {
		    if (kiss_member((kiss_obj*)block, env->lexical_env.jumpers) != KISS_NIL) {
			 return block;
		    } else {
			 goto error;
		    }
	       }
	  }
     }
error:
     Kiss_Block_Not_Found_Error((kiss_obj*)name);
}

/* special operator: (return-from name result-form) transfers control and data
 */
kiss_obj* kiss_return_from(kiss_obj* name, kiss_obj* result_form) {
     kiss_environment_t* env = Kiss_Get_Environment();
     kiss_obj* result = kiss_eval(result_form);
     kiss_block_t* block = kiss_block_ref(Kiss_Symbol(name));
     kiss_eval_cleanups((kiss_obj*)block, block->dynamic_env.jumpers);
     env->block_result = result;
     longjmp(block->jmp, 1);
}


static kiss_obj* kiss_remove_tags(kiss_obj* p) {
    kiss_obj* stack = KISS_NIL;
    for (p = Kiss_Proper_List(p); KISS_IS_CONS(p); p = KISS_CDR(p)) {
	kiss_obj* x = KISS_CAR(p);
	if (!KISS_IS_SYMBOL(x)) { kiss_push(x, &stack); }
    }
    return kiss_nreverse(stack);
}

static kiss_obj* kiss_make_tagbodies(kiss_obj* p, jmp_buf jmp) {
    kiss_obj* stack = KISS_NIL;
    for(p = Kiss_Proper_List(p); KISS_IS_CONS(p); p = KISS_CDR(p)) {
	if (KISS_IS_SYMBOL(KISS_CAR(p))) {
	    kiss_symbol_t* tag = (kiss_symbol_t*)KISS_CAR(p);
	    kiss_tagbody_t* tagbody =
		kiss_make_tagbody(tag, jmp, kiss_remove_tags(KISS_CDR(p)));
	    kiss_push((kiss_obj*)tagbody, &stack);
	}
    }
    return stack;
}

/* special operator: (tagbody {tagbody-tag | form}*) → <object>
   executes the forms sequentially from left to right, discarding their
   values. If the execution of the last form completes normally, nil is
   returned by the tagbody special form.
 */
kiss_obj* kiss_tagbody(kiss_obj* args) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* body = kiss_remove_tags(args);
    kiss_lexical_environment_t saved_lexical_env = env->lexical_env;
    kiss_dynamic_environment_t saved_dynamic_env = env->dynamic_env;
    kiss_lexical_environment_t tagbody_lexical_env = env->lexical_env;
    kiss_dynamic_environment_t tagbody_dynamic_env = env->dynamic_env;
    kiss_obj* result;
    jmp_buf jmp;
    kiss_obj* stack = kiss_make_tagbodies(args, jmp);
    kiss_obj* p = stack;
    tagbody_lexical_env.jumpers = kiss_cappend(2, stack,
					       tagbody_lexical_env.jumpers);
    tagbody_dynamic_env.jumpers = kiss_cappend(2, stack,
					       tagbody_dynamic_env.jumpers);
    for (; KISS_IS_CONS(p); p = KISS_CDR(p)) {
	kiss_tagbody_t* t = KISS_CAR(p);
	t->dynamic_env.jumpers = tagbody_dynamic_env.jumpers;
    }
    while (1) {
	env->lexical_env = tagbody_lexical_env;
	env->dynamic_env = tagbody_dynamic_env;
	if (setjmp(jmp) == 0) {
	    kiss_eval_body(body);
	    break;
	} else {
	    body = env->current_tagbody->body;
	}
    }
    env->lexical_env = saved_lexical_env;
    env->dynamic_env = saved_dynamic_env;
    return KISS_NIL;
}

static kiss_tagbody_t* kiss_tagbody_ref(kiss_symbol_t* tag) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* p;
    for (p = env->dynamic_env.jumpers; KISS_IS_CONS(p); p = KISS_CDR(p)) {
	kiss_obj* jmp = KISS_CAR(p);
	if (KISS_IS_TAGBODY(jmp)) {
	    kiss_tagbody_t* tagbody = (kiss_tagbody_t*)jmp;
	    if (tagbody->tag == tag) {
		if (kiss_member((kiss_obj*)tagbody, env->lexical_env.jumpers)
		    != KISS_NIL)
		{
		    return tagbody;
		} else {
		    goto error;
		}
	    }
	}
    }
    error:
    Kiss_Tagbody_Not_Found_Error((kiss_obj*)tag);
}

/* special operator: (go tagbody-tag) transfers control */
kiss_obj* kiss_go(kiss_obj* tag) {
     kiss_environment_t* env = Kiss_Get_Environment();
     kiss_tagbody_t* tagbody = kiss_tagbody_ref(Kiss_Symbol(tag));
     kiss_eval_cleanups((kiss_obj*)tagbody, tagbody->dynamic_env.jumpers);
     env->current_tagbody = tagbody;
     longjmp(tagbody->jmp, 1);
}

/* special operator: (quote obj) → <object> */
kiss_obj* kiss_quote(kiss_obj* obj) { return obj; }

/* special operator: (if test-form then-form [else-form]) → <object> */
kiss_obj* kiss_if(kiss_obj* test_form, kiss_obj* then_form, kiss_obj* rest) {
    if (kiss_eval(test_form) != KISS_NIL) {
	return kiss_eval(then_form);
    } else {
	return kiss_eval_body(rest);
    }
}

/* function: (eq obj1 obj2) → boolean
   the consequences are implementation defined if both obj1 and obj2 are
   numbers or both are characters.*/
kiss_obj* kiss_eq(kiss_obj* obj1, kiss_obj* obj2) {
    if (obj1 == obj2) { return KISS_T; }
    else              { return KISS_NIL; }
}

/* function: (eql obj1 obj2) → boolean
   the meaning for numbers and characters is defined as follows:
   • If obj1 and obj2 are numbers, eql tests whether they are direct
   instances of the same class and have the same value.  If an
   implementation supports positive and negative zeros as distinct
   values, then (eql 0.0 -0.0) returns nil. When the syntax -0.0 is read
   and it is interpreted as the value 0.0 then (eql 0.0 -0.0) returns t.
   • If obj1 and obj2 are characters, eql tests whether they are the same
   character (see char=).
 */
kiss_obj* kiss_eql(kiss_obj* obj1, kiss_obj* obj2) {
     if (KISS_IS_INTEGER(obj1) && KISS_IS_INTEGER(obj2)) {
	  if (((kiss_integer_t*)obj1)->i == ((kiss_integer_t*)obj2)->i) {
	       return KISS_T;
	  } else {
	       return KISS_NIL;
	  }
     }
     if (KISS_IS_FLOAT(obj1) && KISS_IS_FLOAT(obj2)) {
	  if (((kiss_float_t*)obj1)->f == ((kiss_float_t*)obj2)->f) {
	       return KISS_T;
	  } else {
	       return KISS_NIL;
	  }
     }
     if (KISS_IS_CHARACTER(obj1) && KISS_IS_CHARACTER(obj2)) {
	  if (((kiss_character_t*)obj1)->c == ((kiss_character_t*)obj2)->c) {
	       return KISS_T;
	  } else {
	       return KISS_NIL;
	  }
     }
     return kiss_eq(obj1, obj2);
}

/* special operator: (progn form*) → <object> */
kiss_obj* kiss_progn(kiss_obj* body) { return kiss_eval_body(body); }


