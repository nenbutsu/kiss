/*  -*- coding: utf-8 -*-
  hash_table.c --- defines the class mechanism of ISLisp processor KISS.

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

kiss_obj* kiss_create_hash_table(kiss_obj* args) {
     kiss_hash_table_t* p = Kiss_GC_Malloc(sizeof(kiss_hash_table_t));
     p->type = KISS_HASH_TABLE;
     p->n = 0;
     p->vector = NULL;
     p->test = KISS_NIL;
     p->weakness = KISS_NIL;
     p->rehash_size = KISS_NIL;
     p->rehash_threshold = KISS_NIL;
     kiss_obj* size = kiss_plist_get(args, kiss_symbol(L":size"));
     if (size == KISS_NIL)
          size = kiss_make_fixnum(1024);
     p->vector = (kiss_general_vector_t*)kiss_create_general_vector(size, KISS_NIL);

     kiss_obj* test = kiss_plist_get(args, kiss_symbol(L":test"));
     p->test = (test == KISS_NIL ? kiss_function(kiss_symbol(L"eql")) : test);

     kiss_obj* weakness = kiss_plist_get(args, kiss_symbol(L":weakness"));
     p->weakness = weakness;
     
     kiss_obj* rehash_size = kiss_plist_get(args, kiss_symbol(L":rehash_size"));
     if (rehash_size == KISS_NIL) {
          kiss_float_t* f = kiss_make_float();
          mpf_set_d(f->mpf, 1.5);
          rehash_size = (kiss_obj*)f;
     }
     p->rehash_size = rehash_size;

     kiss_obj* rehash_threshold = kiss_plist_get(args, kiss_symbol(L":rehash_size"));
     if (rehash_threshold == KISS_NIL) {
          kiss_float_t* f = kiss_make_float();
          mpf_set_d(f->mpf, 0.8);
          rehash_threshold = (kiss_obj*)f;
     }
     p->rehash_threshold = rehash_threshold;

     return (kiss_obj*)p;
}

kiss_ptr_int kiss_hash_string(kiss_string_t* str, kiss_hash_table_t* table) {
     size_t n = 0;
     for (wchar_t* wcs = str->str; *wcs != L'\0'; wcs++) {
          n += *wcs;
     }
     n %= table->vector->n;
     return n;
}

kiss_ptr_int kiss_hash_symbol(kiss_symbol_t* symbol, kiss_hash_table_t* table) {
     size_t n = kiss_ptr_int(symbol);
     n %= table->vector->n;
     return n;
}

kiss_ptr_int kiss_hash(kiss_obj* obj, kiss_hash_table_t* table) {
     switch (KISS_OBJ_TYPE(obj)) {
     case KISS_STRING:
          return kiss_hash_string((kiss_string_t*)obj, table);
          break;
     case KISS_SYMBOL:
          return kiss_hash_symbol((kiss_symbol_t*)obj, table);
          break;
     default:
          Kiss_Err(L"kiss_compute_hash: unimplemented primitive type = %d", KISS_OBJ_TYPE(obj));
     }
     exit(EXIT_FAILURE);
}

kiss_obj* kiss_gethash(kiss_obj* key, kiss_obj* table, kiss_obj* rest) {
     kiss_hash_table_t* hash_table = Kiss_Hash_Table(table);
     kiss_obj* default_value = (KISS_IS_CONS(rest) ? KISS_CAR(rest) : KISS_NIL);
     kiss_ptr_int k = kiss_hash(key, hash_table);
     kiss_obj* list = hash_table->vector->v[k];
     kiss_obj* p = kiss_assoc_using(hash_table->test, key, list);
     if (p == KISS_NIL)
          return default_value;
     else
          return kiss_cdr(p);
}

kiss_obj* kiss_puthash(kiss_obj* key, kiss_obj* value, kiss_obj* table) {
     kiss_hash_table_t* hash_table = Kiss_Hash_Table(table);
     kiss_ptr_int k = kiss_hash(key, hash_table);
     kiss_obj* alist = hash_table->vector->v[k];
     kiss_obj* p = kiss_assoc_using(hash_table->test, key, alist);
     if (p == KISS_NIL)
          hash_table->vector->v[k] = kiss_cons(kiss_cons(key, value), alist);
     else
          kiss_set_cdr(value, p);
     return KISS_NIL;
}
