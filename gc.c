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

size_t Kiss_Heap_Top = 0;
kiss_ptr_int Kiss_GC_Flag = 0;

static int Kiss_GCing = 0;
size_t Kiss_GC_Amount = 0;
kiss_gc_obj* Kiss_Heap_Stack[KISS_HEAP_STACK_SIZE];
void* Kiss_GC_Objects = NULL;

#define gc_flag(x)  ((kiss_ptr_int)((kiss_ptr_int)x & 1))

kiss_obj* kiss_gc(void);


static inline int is_marked(kiss_gc_obj* const restrict obj) {
     return gc_flag(obj->gc_ptr) != Kiss_GC_Flag;
}


static inline void mark_flag(kiss_gc_obj* const restrict obj) {
     obj->gc_ptr = Kiss_GC_Flag ? (void*)(((kiss_ptr_int)obj->gc_ptr) & (~0<<1)) :
                                  (void*)(((kiss_ptr_int)obj->gc_ptr) | 1);
}

void kiss_gc_mark_obj(kiss_obj* obj);

static inline
void kiss_gc_mark_lexical_environment(kiss_lexical_environment_t* const lexical_env) {
     kiss_gc_mark_obj(lexical_env->vars);
     kiss_gc_mark_obj(lexical_env->funs);
     kiss_gc_mark_obj(lexical_env->jumpers);
}

static inline
void kiss_gc_mark_dynamic_environment(kiss_dynamic_environment_t* const dynamic_env) {
     kiss_gc_mark_obj(dynamic_env->vars);
     kiss_gc_mark_obj(dynamic_env->jumpers);
}

static inline
void kiss_gc_mark_cons(kiss_cons_t* const obj) {
     if (is_marked((kiss_gc_obj*)obj)) { return; }
     mark_flag((kiss_gc_obj*)obj);
     
     kiss_gc_mark_obj(obj->car);
     kiss_gc_mark_obj(obj->cdr);
}

static inline
void kiss_gc_mark_general_vector(kiss_general_vector_t* const obj) {
     if (is_marked((kiss_gc_obj*)obj)) { return; }
     mark_flag((kiss_gc_obj*)obj);
     for (size_t i = 0; i < obj->n; i++) {
	  kiss_gc_mark_obj(obj->v[i]);
     }
}

static inline
void kiss_gc_mark_general_array(kiss_general_array_t* const obj) {
     if (is_marked((kiss_gc_obj*)obj)) { return; }
     mark_flag((kiss_gc_obj*)obj);
     kiss_gc_mark_obj(obj->vector);
}

static inline
void kiss_gc_mark_hash_table(kiss_hash_table_t* const obj) {
     if (is_marked((kiss_gc_obj*)obj)) { return; }
     mark_flag((kiss_gc_obj*)obj);
     kiss_gc_mark_general_vector(obj->vector);
     kiss_gc_mark_obj(obj->test);
     kiss_gc_mark_obj(obj->weakness);
     kiss_gc_mark_obj(obj->rehash_size);
     kiss_gc_mark_obj(obj->rehash_threshold);
}

static inline
void kiss_gc_mark_symbol(kiss_symbol_t* const symbol) {
     if (is_marked((kiss_gc_obj*)symbol)) { return; }
     mark_flag((kiss_gc_obj*)symbol);
     
     kiss_gc_mark_obj(symbol->var);
     kiss_gc_mark_obj(symbol->fun);
     kiss_gc_mark_obj(symbol->plist);
}

static inline
void kiss_gc_mark_function(kiss_function_t* const f) {
     if (is_marked((kiss_gc_obj*)f)) { return; }
     mark_flag((kiss_gc_obj*)f);
     kiss_gc_mark_obj(f->lambda);
     kiss_gc_mark_lexical_environment(&(f->lexical_env));
}

static inline
void kiss_gc_mark_cfunction(kiss_cfunction_t* const f) {
     kiss_gc_mark_obj((kiss_obj*)(f->name));
}

static inline
void kiss_gc_mark_catcher(kiss_catcher_t* const catcher) {
     if (is_marked((kiss_gc_obj*)catcher)) { return; }
     mark_flag((kiss_gc_obj*)catcher);
     kiss_gc_mark_obj(catcher->tag);
     kiss_gc_mark_dynamic_environment(&(catcher->dynamic_env));
}

static inline
void kiss_gc_mark_block(kiss_block_t* const block) {
     if (is_marked((kiss_gc_obj*)block)) { return; }
     mark_flag((kiss_gc_obj*)block);
     kiss_gc_mark_obj((kiss_obj*)block->name);
     kiss_gc_mark_dynamic_environment(&(block->dynamic_env));
}

static inline
void kiss_gc_mark_cleanup(kiss_cleanup_t* const cleanup) {
     if (is_marked((kiss_gc_obj*)cleanup)) { return; }
     mark_flag((kiss_gc_obj*)cleanup);
     kiss_gc_mark_obj(cleanup->body);
     kiss_gc_mark_lexical_environment(&(cleanup->lexical_env));
     kiss_gc_mark_dynamic_environment(&(cleanup->dynamic_env));
}

static inline
void kiss_gc_mark_tagbody(kiss_tagbody_t* const tagbody) {
     if (is_marked((kiss_gc_obj*)tagbody)) { return; }
     mark_flag((kiss_gc_obj*)tagbody);
     kiss_gc_mark_obj((kiss_obj*)tagbody->tag);
     kiss_gc_mark_dynamic_environment(&(tagbody->dynamic_env));
     kiss_gc_mark_obj(tagbody->body);
}

static inline
void kiss_gc_mark_stream(kiss_stream_t* const obj) {
     if (is_marked((kiss_gc_obj*)obj)) { return; }
     mark_flag((kiss_gc_obj*)obj);
     if (KISS_IS_STRING_STREAM(obj)) {
	  kiss_string_stream_t* const str_stream = (kiss_string_stream_t*)obj;
	  kiss_gc_mark_obj(str_stream->list);
     }
}

static inline
void kiss_gc_mark_oo_obj(kiss_object_t* const obj) {
     if (is_marked((kiss_gc_obj*)obj)) { return; }
     mark_flag((kiss_gc_obj*)obj);
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
	  case KISS_FIXNUM:
               break;
          case KISS_BIGNUM:
	  case KISS_FLOAT:
	  case KISS_STRING:
	       if (is_marked((kiss_gc_obj*)obj)) { return; }
	       mark_flag((kiss_gc_obj*)obj);
	       break;
	  case KISS_GENERAL_VECTOR:
	       kiss_gc_mark_general_vector((kiss_general_vector_t*)obj);
               break;
	  case KISS_GENERAL_ARRAY_S:
	       kiss_gc_mark_general_array((kiss_general_array_t*)obj);
               break;
	  case KISS_HASH_TABLE:
	       kiss_gc_mark_hash_table((kiss_hash_table_t*)obj);
               break;
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
	  case KISS_ILOS_OBJ:
	       kiss_gc_mark_oo_obj((kiss_object_t*)obj);
	       break;
	  default:
	       fwprintf(stderr, L"gc_mark: unknown primitive object type = %d\n", KISS_OBJ_TYPE(obj));
	       exit(EXIT_FAILURE);
	  }
     }
}

void kiss_gc_mark(void) {
     kiss_environment_t* env = Kiss_Get_Environment();

     kiss_gc_mark_lexical_environment(&(env->lexical_env));
     kiss_gc_mark_dynamic_environment(&(env->dynamic_env));
     kiss_gc_mark_obj((kiss_obj*)(env->lexeme_chars));
     kiss_gc_mark_obj((kiss_obj*)(env->throw_result));
     kiss_gc_mark_obj((kiss_obj*)(env->block_result));
     kiss_gc_mark_obj((kiss_obj*)(env->current_tagbody));
     kiss_gc_mark_obj((kiss_obj*)(env->global_dynamic_vars));
     kiss_gc_mark_obj((kiss_obj*)(Kiss_Features));
     for (size_t i = 0; i < Kiss_Heap_Top; i++) {
	  kiss_obj* obj = (kiss_obj*)Kiss_Heap_Stack[i];
	  kiss_gc_mark_obj(obj);
     }
     for (size_t i = 0; i < Kiss_Symbol_Number; i++) {
	  kiss_obj* obj = (kiss_obj*)Kiss_Symbols[i];
	  kiss_gc_mark_obj(obj);
     }
     kiss_gc_mark_hash_table(Kiss_Symbol_Hash_Table);
}

static inline
void kiss_gc_free_symbol(kiss_symbol_t* const obj) {
     free(obj->name);
     free(obj);
}

static inline
void kiss_gc_free_bignum(kiss_bignum_t* const obj) {
     mpz_clear(obj->mpz);
     free(obj);
}

static inline
void kiss_gc_free_float(kiss_float_t* const obj) {
     mpf_clear(obj->mpf);
     free(obj);
}

static inline
void kiss_gc_free_string(kiss_string_t* const obj) {
     free(obj->str);
     free(obj);
}

static inline
void kiss_gc_free_stream(kiss_stream_t* const obj) {
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
	  case KISS_CHARACTER:
	  case KISS_FIXNUM:
               break;
          case KISS_BIGNUM:
               kiss_gc_free_bignum((kiss_bignum_t*)obj);
               break;
	  case KISS_FLOAT:
               kiss_gc_free_float((kiss_float_t*)obj);
               break;
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
	  case KISS_GENERAL_VECTOR:
	  case KISS_GENERAL_ARRAY_S:
          case KISS_HASH_TABLE:
	  case KISS_FUNCTION:
	  case KISS_MACRO:
	  case KISS_CFUNCTION:
	  case KISS_CMACRO:
	  case KISS_CATCHER:
	  case KISS_BLOCK:
	  case KISS_CLEANUP:
	  case KISS_TAGBODY:
	  case KISS_ILOS_OBJ:
	       free(obj);
	       break;
	  default:
	       fwprintf(stderr, L"gc_free: unknown object type = %d\n", KISS_OBJ_TYPE(obj));
	       exit(EXIT_FAILURE);
	  }
     }
}

void kiss_gc_sweep(void) {
     void** prev = &Kiss_GC_Objects;
     kiss_gc_obj* obj = kiss_gc_ptr(Kiss_GC_Objects);
     while (obj != NULL) {
	  if (is_marked(obj)) {
               prev = &(obj->gc_ptr);
	       obj = kiss_gc_ptr(obj->gc_ptr);
          } else {
               kiss_gc_obj* tmp = obj;
	       obj = kiss_gc_ptr(obj->gc_ptr);
	       *prev = (void*)((kiss_ptr_int)obj | (Kiss_GC_Flag ? 0 : 1));
	       kiss_gc_free_obj(tmp);
	  }
     }
}

kiss_obj* kiss_gc_info(void) {
     fwprintf(stderr, L"Kiss_Heap_Top = %ld\n", Kiss_Heap_Top);
     fwprintf(stderr, L"Kiss_GC_Flag = %ld\n", Kiss_GC_Flag);
     return KISS_NIL;
}

kiss_obj* kiss_gc(void) {
     assert(!Kiss_GCing);
     Kiss_GCing = 1;
     //fwprintf(stderr, L"GC entered\n");
     //fwprintf(stderr, L"gc_mark\n");
     kiss_gc_mark();
     //fwprintf(stderr, L"gc_sweep\n");
     kiss_gc_sweep();
     Kiss_GC_Flag = Kiss_GC_Flag ? 0 : 1;
     //fwprintf(stderr, L"GC leaving\n\n");
     Kiss_GCing = 0;
     return KISS_NIL;
}
