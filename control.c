/*  -*- coding: utf-8 -*-
  control.c --- defines the control mechanism of ISLisp processor KISS.

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


/* special operator: (catch tag-form form*) -> <object> */
kiss_obj* kiss_catch(kiss_obj* tag_form, kiss_obj* body) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* tag = kiss_eval(tag_form);
    kiss_lexical_environment_t saved_lexical_env = env->lexical_env;
    kiss_dynamic_environment_t saved_dynamic_env = env->dynamic_env;
    kiss_obj* saved_call_stack = env->call_stack;
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
    env->call_stack = saved_call_stack;
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
    exit(EXIT_FAILURE); // not reach here
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

/* special operator: (unwind-protect form cleanup-form*) -> <object> */
kiss_obj* kiss_unwind_protect(kiss_obj* protected_form, kiss_obj* cleanup_body) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* result;
    kiss_cleanup_t* cleanup = kiss_make_cleanup(cleanup_body);
    kiss_obj* saved_call_stack = env->call_stack;
    env->dynamic_env.jumpers = kiss_cons((kiss_obj*)cleanup, env->dynamic_env.jumpers);
    result = kiss_eval(protected_form);
    env->call_stack = saved_call_stack;
    kiss_eval_body(cleanup_body);
    return result;
}


/* special operator: (block name form*) -> <object>*/
kiss_obj* kiss_block(kiss_obj* name, kiss_obj* body) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_lexical_environment_t saved_lexical_env = env->lexical_env;
    kiss_dynamic_environment_t saved_dynamic_env = env->dynamic_env;
    kiss_obj* saved_call_stack = env->call_stack;
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
    env->call_stack = saved_call_stack;
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

/* special operator: (tagbody {tagbody-tag | form}*) -> <object>
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
    jmp_buf jmp;
    kiss_obj* stack = kiss_make_tagbodies(args, jmp);
    kiss_obj* p = stack;
    tagbody_lexical_env.jumpers = kiss_c_append(2, stack,
						tagbody_lexical_env.jumpers);
    tagbody_dynamic_env.jumpers = kiss_c_append(2, stack,
						tagbody_dynamic_env.jumpers);
    for (; KISS_IS_CONS(p); p = KISS_CDR(p)) {
	kiss_tagbody_t* t = KISS_CAR(p);
	t->dynamic_env.jumpers = tagbody_dynamic_env.jumpers;
    }
    kiss_obj* tagbody_call_stack = env->call_stack;
    while (1) {
	env->lexical_env = tagbody_lexical_env;
	env->dynamic_env = tagbody_dynamic_env;
        env->call_stack  = tagbody_call_stack;
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
    exit(EXIT_FAILURE); // not reach here
}

/* special operator: (go tagbody-tag) transfers control */
kiss_obj* kiss_go(kiss_obj* tag) {
     kiss_environment_t* env = Kiss_Get_Environment();
     kiss_tagbody_t* tagbody = kiss_tagbody_ref(Kiss_Symbol(tag));
     kiss_eval_cleanups((kiss_obj*)tagbody, tagbody->dynamic_env.jumpers);
     env->current_tagbody = tagbody;
     longjmp(tagbody->jmp, 1);
}

/* special operator: (quote obj) -> <object> */
kiss_obj* kiss_quote(kiss_obj* obj) { return obj; }

/* special operator: (if test-form then-form [else-form]) -> <object> */
kiss_obj* kiss_if(kiss_obj* test_form, kiss_obj* then_form, kiss_obj* rest) {
    if (kiss_eval(test_form) != KISS_NIL) {
	return kiss_eval(then_form);
    } else {
	return kiss_eval_body(rest);
    }
}

/* special operator: (progn form*) -> <object> */
kiss_obj* kiss_progn(kiss_obj* body) { return kiss_eval_body(body); }


/* special operator: (and form*) -> <object>
   and is the sequential logical `and'. FORMS are evaluated
   from left to right until either one of them evaluates to nil or else
   none are left. If one of them evaluates to nil, then nil is returned
   from the and; otherwise, the value of the last evaluated form is returned. */
kiss_obj* kiss_and(const kiss_obj* const forms) {
     kiss_obj* result = KISS_T;
     for (const kiss_obj* p = forms; KISS_IS_CONS(p); p = KISS_CDR(p)) {
          kiss_obj* obj = KISS_CAR(p);
          result = kiss_eval(obj);
          if (result == KISS_NIL) { return KISS_NIL; }
     }
     return result;
}

/* special operator: (or form*) -> <object>
   or is the sequential logical `or'. forms are evaluated from
   left to right until either one of them evaluates to a non-nil value or
   else none are left. If one of them evaluates to a non-nil value,
   then this non-nil value is returned, otherwise nil is returned. */
kiss_obj* kiss_or(const kiss_obj* const forms) {
     for (const kiss_obj* p = forms; KISS_IS_CONS(p); p = KISS_CDR(p)) {
          kiss_obj* obj = KISS_CAR(p);
          kiss_obj* result = kiss_eval(obj);
          if (result != KISS_NIL) { return result; }
     }
     return KISS_NIL;
}


/* special operator: (while test-form body-form*) -> <null>
   Iterates while the test-form returns a true value. Specifically:
     1. test-form is evaluated, producing a value Vt.
     2. If Vt is nil, then the while form immediately returns nil.
     3. Otherwise, if Vt is non-nil, the forms body-form* are evaluated 
        sequentially (from left to right).
     4. Upon successful completion of the body-forms*, the while form begins
        again with step 1. */
kiss_obj* kiss_while(const kiss_obj* const test_form, const kiss_obj* const body) {
     kiss_obj* result = kiss_eval(test_form);
     while (result != KISS_NIL) {
          kiss_eval_body(body);
          result = kiss_eval(test_form);
     }
     return KISS_NIL;
}



 /* function: (equal obj1 obj2) → boolean
    Tests whether OBJ1 and OBJ2 are isomorphic—i.e., whether OBJ1 and OBJ2 denote
    the same structure with equivalent values. 
    equal returns t if the test was satisfied, and nil if not. 

    Specifically:
    If OBJ1 and OBJ2 are direct instances of the same class, equal returns t if they are eql.
    Otherwise (if they are direct instances of the same class but not eql),
    the result is t if one of the following cases applies:

    lists: either obj1 and obj2 are both the empty list (i.e., nil), or

    (and (equal (car obj1) (car obj2))
         (equal (cdr obj1) (cdr obj2))) holds;

    basic arrays:

    (equal (array-dimensions obj1)
           (array-dimensions obj2))

    holds and for every valid reference (aref obj1 ind1 ...indn)

    (equal (aref obj1 ind1 … indn)
           (aref obj2 ind1 … indn)) is satisfied.

    Otherwise the value is nil.

    OBJ1 and OBJ2 may be any ISLISP objects. */
kiss_obj* kiss_equal(const kiss_obj* const obj1, const kiss_obj* const obj2) {
     switch (KISS_OBJ_TYPE(obj1)) {
     case KISS_CHARACTER:
     case KISS_FIXNUM:
     case KISS_BIGNUM:
     case KISS_FLOAT:
     case KISS_SYMBOL:
     case KISS_STREAM:
     case KISS_HASH_TABLE:
     case KISS_LFUNCTION:
     case KISS_LMACRO:
     case KISS_CFUNCTION:
     case KISS_CSPECIAL:
     case KISS_CATCHER:
     case KISS_BLOCK:
     case KISS_CLEANUP:
     case KISS_TAGBODY:
     case KISS_ILOS_OBJ:
          return kiss_eql(obj1, obj2);
     case KISS_CONS:
          if (!KISS_IS_CONS(obj2)) return KISS_NIL;
          return (kiss_equal(KISS_CAR(obj1), KISS_CAR(obj2)) == KISS_T &&
                  kiss_equal(KISS_CDR(obj1), KISS_CDR(obj2)) == KISS_T ? KISS_T : KISS_NIL);
     case KISS_STRING: {
          if (!KISS_IS_STRING(obj2)) return KISS_NIL;
          size_t n = ((kiss_string_t*)obj1)->n;
          if (((kiss_string_t*)obj2)->n != n) return KISS_NIL;
          wchar_t* s1 = ((kiss_string_t*)obj1)->str;
          wchar_t* s2 = ((kiss_string_t*)obj2)->str;
          for (size_t i = 0; i < n; i++) {
               if (s1[i] != s2[i]) return KISS_NIL;
          }
          return KISS_T;
     }
     case KISS_GENERAL_VECTOR: {
          if (!KISS_IS_GENERAL_VECTOR(obj2)) return KISS_NIL;
          size_t n = ((kiss_general_vector_t*)obj1)->n;
          if (((kiss_general_vector_t*)obj2)->n != n) return KISS_NIL;
          kiss_obj** v1 = ((kiss_general_vector_t*)obj1)->v;
          kiss_obj** v2 = ((kiss_general_vector_t*)obj2)->v;
          for (size_t i = 0; i < n; i++) {
               if (kiss_equal(v1[i], v2[i]) == KISS_NIL) return KISS_NIL;
          }
          return KISS_T;
     }
     case KISS_GENERAL_ARRAY_S:
          if (!KISS_IS_GENERAL_ARRAY_S(obj2)) return KISS_NIL;
          if (kiss_equal(kiss_array_dimensions(obj1), kiss_array_dimensions(obj2)) == KISS_NIL)
               return KISS_NIL;
          return kiss_equal(((kiss_general_array_t*)obj1)->vector,
                            ((kiss_general_array_t*)obj2)->vector);
     default:
          fwprintf(stderr, L"equal: unknown primitive object type = %d\n", KISS_OBJ_TYPE(obj1));
          exit(EXIT_FAILURE);
     }
}

/* special operator: (cond (test form*)*) -> <object>
   Executing the prepared cond, the clauses (test form*) are scanned
   sequentially and in each case the TEST is evaluated; when a TEST delivers
   a non-nil value the scanning process stops and all FORMS associated
   with the corresponding clause are sequentially evaluated and the value
   of the last one is returned. If no TEST is true, then nil is returned.
   If no FORM exists for the successful test then the value of this test
   is returned. */
kiss_obj* kiss_cond(const kiss_obj* const clauses) {
     for (const kiss_obj* p = clauses; KISS_IS_CONS(p); p = KISS_CDR(p)) {
          const kiss_obj* clause = KISS_CAR(p);
          const kiss_obj* const test = kiss_car(clause);
          kiss_obj* result = kiss_eval(test);
          if (result != KISS_NIL) {
               kiss_obj* body = kiss_cdr(clause);
               if (body == KISS_NIL) return result;
               return kiss_eval_body(body);
          }
     }
     return KISS_NIL;
}

/* special operator: (case-using predform keyform ((key*) form*)* [(t form*)]) → object
   The case and case-using special forms, called case forms, provide a mechanism to execute
   a matching clause from a series of clauses based on the value of a dispatching form KEYFORM.

   The clause to be executed is identified by a set of KEYS. A KEY can be any object.
   If the keylist of the last clause is t the associated clause is executed if no key matches
   the KEYFORM.

   KEYFORM is a form to be computed at the beginning of execution of the case form. 
   If the result of evaluating KEYFORM is equivalent to a KEY, then the FORMS, if any,
   in the corresponding clause are evaluated sequentially and the value of the last one 
   is returned as value of the whole case form. case determines match equivalence by using eql;
   case-using match determines equivalence by using the result of evaluating PREDFORM. 
   PREDFORM must be a boolean or quasi-boolean function that accepts two arguments, 
   the value returned by KEYFORM and KEY. If no FORM exists for a matching key, the case
   form evaluates to nil. If the value of KEYFORM is different from every KEY, and there is
   a default clause, its FORMS, if any, are evaluated sequentially, and the value of the last
   one is the result of the case form.

   The same KEY (as determined by the match predicate) may occur only once in a case form. */
kiss_obj* kiss_case_using(const kiss_obj* const predform, const kiss_obj* const keyform, const kiss_obj* const clauses) {
     kiss_obj* predicate = kiss_eval(predform);
     kiss_obj* key = kiss_eval(keyform);
     for (const kiss_obj* p = clauses; KISS_IS_CONS(p); p = KISS_CDR(p)) {
          kiss_obj* clause = KISS_CAR(p);
          kiss_obj* key_list = kiss_car(clause);
          if (key_list == KISS_T || kiss_member_using(predicate, key, key_list) != KISS_NIL) {
               return kiss_eval_body(kiss_cdr(clause));
          }
     }
     return KISS_NIL;
}

kiss_obj* kiss_case(const kiss_obj* const keyform, const kiss_obj* const clauses) {
     return kiss_case_using(kiss_function((kiss_obj*)&KISS_Seql), keyform, clauses);
}


kiss_obj* kiss_prog1(const kiss_obj* const form1, const kiss_obj* const forms) {
     kiss_obj* result = kiss_eval(form1);
     kiss_eval_body(forms);
     return result;
}
