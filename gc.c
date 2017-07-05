/*  -*- coding: utf-8 -*-
  gc.c --- defines the garbage collection mechanism of ISLisp processor KISS.

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

static int GCing = 0;

static size_t GC_Counter = 0;

#define HEAP_STACK_SIZE (1024 * 100)
kiss_gc_obj* Kiss_Heap_Stack[HEAP_STACK_SIZE];
static kiss_gc_obj* GC_Objects = NULL;

kiss_obj* kiss_gc(void);

/* An error shall be signaled if the requested memory cannot be allocated
   (error-id. <storage-exhausted>). */
void* Kiss_Malloc(size_t size) {
    void* p = malloc(size);
    if (p == NULL) { Kiss_System_Error(); }
    return p;
}

void* Kiss_GC_Malloc(size_t size) {
    void* p = Kiss_Malloc(size);
    kiss_environment_t* env = Kiss_Get_Environment();

    GC_Counter += size;
    if (GC_Counter > 1024 * 1024) {
	 kiss_gc();
	 GC_Counter = 0;
    }

    Kiss_Heap_Stack[env->heap_index++] = p;
    assert(env->heap_index < HEAP_STACK_SIZE);
    ((kiss_gc_obj*)p)->gc_next = GC_Objects;
    GC_Objects = p;
    ((kiss_gc_obj*)p)->gc_flag = env->gc_flag;
    return p;
}

static int gc_marked(kiss_gc_obj* obj) {
     kiss_environment_t* env = Kiss_Get_Environment();
     return (obj->gc_flag != env->gc_flag);
}


void kiss_gc_mark_obj_flag(kiss_gc_obj* obj) {
     kiss_environment_t* env = Kiss_Get_Environment();
     if (env->gc_flag) {
	  obj->gc_flag = 0;
     } else {
	  obj->gc_flag = 1;
     }
}

void kiss_gc_mark_obj(kiss_obj* obj);

void kiss_gc_mark_lexical_environment(kiss_lexical_environment_t* lexical_env) {
     kiss_gc_mark_obj(lexical_env->vars);
     kiss_gc_mark_obj(lexical_env->funs);
     kiss_gc_mark_obj(lexical_env->jumpers);
}

void kiss_gc_mark_dynamic_environment(kiss_dynamic_environment_t* dynamic_env) {
     kiss_gc_mark_obj(dynamic_env->vars);
     kiss_gc_mark_obj(dynamic_env->jumpers);
}

void kiss_gc_mark_cons(kiss_cons_t* obj) {
     if (gc_marked((kiss_gc_obj*)obj)) { return; }
     kiss_gc_mark_obj_flag((kiss_gc_obj*)obj);
     
     kiss_gc_mark_obj(obj->car);
     kiss_gc_mark_obj(obj->cdr);
}

void kiss_gc_mark_general_vector(kiss_general_vector_t* obj) {
     size_t i;
     if (gc_marked((kiss_gc_obj*)obj)) { return; }
     kiss_gc_mark_obj_flag((kiss_gc_obj*)obj);
     for (i = 0; i < obj->n; i++) {
	  kiss_gc_mark_obj(obj->v[i]);
     }
}

void kiss_gc_mark_symbol(kiss_symbol_t* symbol) {
     if (gc_marked((kiss_gc_obj*)symbol)) { return; }
     kiss_gc_mark_obj_flag((kiss_gc_obj*)symbol);
     
     kiss_gc_mark_obj(symbol->var);
     kiss_gc_mark_obj(symbol->fun);
     kiss_gc_mark_obj(symbol->plist);
}

void kiss_gc_mark_function(kiss_function_t* f) {
     if (gc_marked((kiss_gc_obj*)f)) { return; }
     kiss_gc_mark_obj_flag((kiss_gc_obj*)f);
     kiss_gc_mark_obj(f->lambda);
     kiss_gc_mark_lexical_environment(&(f->lexical_env));
}

void kiss_gc_mark_cfunction(kiss_cfunction_t* f) {
     kiss_gc_mark_obj((kiss_obj*)(f->name));
}

void kiss_gc_mark_catcher(kiss_catcher_t* catcher) {
     if (gc_marked((kiss_gc_obj*)catcher)) { return; }
     kiss_gc_mark_obj_flag((kiss_gc_obj*)catcher);
     kiss_gc_mark_obj(catcher->tag);
     kiss_gc_mark_dynamic_environment(&(catcher->dynamic_env));
}

void kiss_gc_mark_block(kiss_block_t* block) {
     if (gc_marked((kiss_gc_obj*)block)) { return; }
     kiss_gc_mark_obj_flag((kiss_gc_obj*)block);
     kiss_gc_mark_obj((kiss_obj*)block->name);
     kiss_gc_mark_dynamic_environment(&(block->dynamic_env));
}

void kiss_gc_mark_cleanup(kiss_cleanup_t* cleanup) {
     if (gc_marked((kiss_gc_obj*)cleanup)) { return; }
     kiss_gc_mark_obj_flag((kiss_gc_obj*)cleanup);
     kiss_gc_mark_obj(cleanup->body);
     kiss_gc_mark_lexical_environment(&(cleanup->lexical_env));
     kiss_gc_mark_dynamic_environment(&(cleanup->dynamic_env));
}

void kiss_gc_mark_tagbody(kiss_tagbody_t* tagbody) {
     if (gc_marked((kiss_gc_obj*)tagbody)) { return; }
     kiss_gc_mark_obj_flag((kiss_gc_obj*)tagbody);
     kiss_gc_mark_obj((kiss_obj*)tagbody->tag);
     kiss_gc_mark_dynamic_environment(&(tagbody->dynamic_env));
     kiss_gc_mark_obj(tagbody->body);
}

void kiss_gc_mark_stream(kiss_stream_t* obj) {
     if (gc_marked((kiss_gc_obj*)obj)) { return; }
     kiss_gc_mark_obj_flag((kiss_gc_obj*)obj);
     if (KISS_IS_STRING_STREAM(obj)) {
	  kiss_string_stream_t* str_stream = (kiss_string_stream_t*)obj;
	  kiss_gc_mark_obj(str_stream->list);
     }
}

void kiss_gc_mark_oo_obj(kiss_oo_obj_t* obj) {
     if (gc_marked((kiss_gc_obj*)obj)) { return; }
     kiss_gc_mark_obj_flag((kiss_gc_obj*)obj);
     kiss_gc_mark_obj(obj->plist);
}

void kiss_gc_mark_obj(kiss_obj* obj) {
     if (obj == NULL) {
	  return;
     } else {
	  /* fwprintf(stderr, L"type = %d\n", KISS_OBJ_TYPE(obj)); */
	  switch (KISS_OBJ_TYPE(obj)) {
	  case KISS_CONS:
	       kiss_gc_mark_cons((kiss_cons_t*)obj);
	       break;
	  case KISS_SYMBOL:
	       kiss_gc_mark_symbol((kiss_symbol_t*)obj);
	       break;
	  case KISS_CHARACTER:
	  case KISS_INTEGER:
	  case KISS_FLOAT:
	  case KISS_STRING:
	       if (gc_marked((kiss_gc_obj*)obj)) { return; }
	       kiss_gc_mark_obj_flag((kiss_gc_obj*)obj);
	       break;
	  case KISS_GENERAL_VECTOR:
	       kiss_gc_mark_general_vector((kiss_general_vector_t*)obj);
	  case KISS_STREAM:
	       kiss_gc_mark_stream((kiss_stream_t*)obj);
	       break;
	  case KISS_FUNCTION:
	  case KISS_MACRO:
	       kiss_gc_mark_function((kiss_function_t*)obj);
	       break;
	  case KISS_CFUNCTION:
	  case KISS_CMACRO:
	       kiss_gc_mark_cfunction((kiss_cfunction_t*)obj);
	       break;
	  case KISS_CATCHER:
	       kiss_gc_mark_catcher((kiss_catcher_t*)obj);
	       break;
	  case KISS_BLOCK:
	       kiss_gc_mark_block((kiss_block_t*)obj);
	       break;
	  case KISS_CLEANUP:
	       kiss_gc_mark_cleanup((kiss_cleanup_t*)obj);
	       break;
	  case KISS_TAGBODY:
	       kiss_gc_mark_tagbody((kiss_tagbody_t*)obj);
	       break;
	  case KISS_OO_OBJ:
	       kiss_gc_mark_oo_obj((kiss_oo_obj_t*)obj);
	       break;
	  default:
	       fwprintf(stderr, L"GC unknown object type = %d\n", KISS_OBJ_TYPE(obj));
	       abort();
	  }
     }
}

void kiss_gc_mark(void) {
     kiss_environment_t* env = Kiss_Get_Environment();
     size_t i;
     kiss_obj* obj;

     kiss_gc_mark_lexical_environment(&(env->lexical_env));
     kiss_gc_mark_dynamic_environment(&(env->dynamic_env));
     kiss_gc_mark_obj((kiss_obj*)(env->lexeme_chars));
     kiss_gc_mark_obj((kiss_obj*)(env->throw_result));
     kiss_gc_mark_obj((kiss_obj*)(env->block_result));
     kiss_gc_mark_obj((kiss_obj*)(env->current_tagbody));
     kiss_gc_mark_obj((kiss_obj*)(env->global_dynamic_vars));
     kiss_gc_mark_obj((kiss_obj*)(env->features));
     for (i = 0; i < env->heap_index; i++) {
	  obj = (kiss_obj*)Kiss_Heap_Stack[i];
	  kiss_gc_mark_obj(obj);
     }
     for (i = 0; i < Kiss_Symbol_Number; i++) {
	  obj = (kiss_obj*)Kiss_Symbols[i];
	  kiss_gc_mark_obj(obj);
     }
}

void kiss_gc_free_symbol(kiss_symbol_t* obj) {
     free(obj->name);
     free(obj);
}

void kiss_gc_free_string(kiss_string_t* obj) {
     free(obj->str);
     free(obj);
}

void kiss_gc_free_stream(kiss_stream_t* obj) {
     if (KISS_IS_FILE_STREAM(obj) && (((kiss_file_stream_t*)obj)->file_ptr)) {
	  fclose(((kiss_file_stream_t*)obj)->file_ptr);
     }
     free(obj);
}

void kiss_gc_free_obj(kiss_gc_obj* obj) {
     if (obj == NULL) {
	  return;
     } else {
	  switch (KISS_OBJ_TYPE(obj)) {
	  case KISS_SYMBOL:
	       kiss_gc_free_symbol((kiss_symbol_t*)obj);
	       break;
	  case KISS_STRING:
	       kiss_gc_free_string((kiss_string_t*)obj);
	       break;
	  case KISS_STREAM:
	       kiss_gc_free_stream((kiss_stream_t*)obj);
	       break;
	  case KISS_CONS:
	  case KISS_CHARACTER:
	  case KISS_INTEGER:
	  case KISS_FLOAT:
	  case KISS_GENERAL_VECTOR:
	  case KISS_FUNCTION:
	  case KISS_MACRO:
	  case KISS_CFUNCTION:
	  case KISS_CMACRO:
	  case KISS_CATCHER:
	  case KISS_BLOCK:
	  case KISS_CLEANUP:
	  case KISS_TAGBODY:
	  case KISS_OO_OBJ:
	       free(obj);
	       break;
	  default:
	       fwprintf(stderr, L"GC unknown object type = %d\n", KISS_OBJ_TYPE(obj));
	       abort();
	  }
     }
}

void kiss_gc_sweep(void) {
     kiss_gc_obj** prev = &GC_Objects;
     kiss_gc_obj* obj = GC_Objects;
     while (obj != NULL) {
	  if (gc_marked(obj)) {
	       prev = &(obj->gc_next);
	       obj = obj->gc_next;
	  } else {
	       kiss_gc_obj* tmp;
	       *prev = obj->gc_next;
	       tmp = obj;
	       obj = obj->gc_next;
	       kiss_gc_free_obj(tmp);
	  }
     }
}

kiss_obj* kiss_gc_info(void) {
     kiss_environment_t* env = Kiss_Get_Environment();
     fwprintf(stderr, L"heap_index = %d\n", env->heap_index);
     return KISS_NIL;
}

kiss_obj* kiss_gc(void) {
     kiss_environment_t* env = Kiss_Get_Environment();
     assert(!GCing);
     GCing = 1;
     //fwprintf(stderr, L"GC entered\n");
     //fwprintf(stderr, L"heap_index = %d\n", env->heap_index);
     //fwprintf(stderr, L"gc_flag = %d\n", env->gc_flag);
     //fwprintf(stderr, L"gc_mark\n");
     kiss_gc_mark();
     //fwprintf(stderr, L"gc_sweep\n");
     kiss_gc_sweep();
     env->gc_flag = env->gc_flag ? 0 : 1;
     //fwprintf(stderr, L"GC leaving\n\n");
     GCing = 0;
     return KISS_NIL;
}

