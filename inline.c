/*  -*- coding: utf-8 -*-
  inline.c --- defines the inline functions of ISLisp processor KISS.

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

extern inline
void* Kiss_Malloc(size_t const size);

extern inline
void* Kiss_GC_Malloc(size_t const size);

extern inline
kiss_obj* kiss_make_integer(kiss_ptr_int i);

extern inline
kiss_cons_t* Kiss_Cons(const kiss_obj* const obj);

extern inline
kiss_ptr_int Kiss_Fixnum(const kiss_obj* obj);

extern inline
kiss_bignum_t* Kiss_Bignum(const kiss_obj* const obj);

extern inline
kiss_obj* Kiss_Integer(const kiss_obj* const obj);

extern inline
kiss_obj* Kiss_Non_Zero_Integer(const kiss_obj* const obj);

extern inline
kiss_obj* Kiss_Non_Negative_Integer(const kiss_obj* const obj);

extern inline
kiss_obj* Kiss_Number(const kiss_obj* const obj);

extern inline
kiss_obj* Kiss_Non_Zero_Number(const kiss_obj* const obj);

extern inline
kiss_obj* Kiss_Positive_Number(const kiss_obj* const obj);

extern inline
kiss_obj* Kiss_Non_Negative_Number(const kiss_obj* const obj);
     
extern inline
kiss_ptr_int Kiss_Non_Negative_Fixnum(const kiss_obj* const obj);

extern inline
kiss_ptr_int Kiss_Non_Zero_Fixnum(const kiss_obj* const obj);

extern inline
kiss_obj* Kiss_General_Array(const kiss_obj* const obj);

extern inline
kiss_generic_function_t* Kiss_Generic_Function(const kiss_obj* const obj);

extern inline
kiss_metho_t* Kiss_Method(const kiss_obj* const obj);

extern inline
kiss_hash_table_t* Kiss_Hash_Table(const kiss_obj* const obj);
     
extern inline
kiss_obj* Kiss_Sequence(const kiss_obj* const obj);

extern inline
kiss_ilos_obj_t* Kiss_ILOS_Obj(const kiss_obj* const obj);

extern inline
kiss_obj* Kiss_Basic_Array(const kiss_obj* const obj);

extern inline
kiss_obj* Kiss_List(const kiss_obj* const obj);

extern inline
kiss_obj* Kiss_Proper_List(const kiss_obj* const obj);

extern inline
kiss_cons_t* Kiss_Proper_List_2(const kiss_obj* const obj);

extern inline
kiss_float_t* Kiss_Float(const kiss_obj* const obj);

extern inline
wchar_t Kiss_Character(const kiss_obj* const obj);

extern inline
kiss_symbol_t* Kiss_Symbol(const kiss_obj* const obj);

extern inline
kiss_string_t* Kiss_String(const kiss_obj* const obj);

extern inline
kiss_stream_t* Kiss_Stream(const kiss_obj* const obj);

extern inline
kiss_general_vector_t* Kiss_General_Vector(const kiss_obj* const obj);

extern inline
kiss_general_array_t* Kiss_General_Array_S(const kiss_obj* const obj);

extern inline
kiss_function_t* Kiss_LFunction(const kiss_obj* const obj);

extern inline
kiss_function_t* Kiss_LMacro(const kiss_obj* const obj);

extern inline
kiss_cfunction_t* Kiss_CFunction(const kiss_obj* const obj);

extern inline
kiss_cfunction_t* Kiss_CMacro(const kiss_obj* const obj);

extern inline
kiss_obj* kiss_not(const kiss_obj* const obj);

extern inline
kiss_obj* kiss_eq(const kiss_obj* const obj1, const kiss_obj* const obj2);

extern inline
kiss_obj* kiss_eql(const kiss_obj* const obj1, const kiss_obj* const obj2);

extern inline
kiss_obj* kiss_streamp(kiss_obj* obj);

extern inline
kiss_obj* kiss_input_stream_p(kiss_obj* p);

extern inline
kiss_obj* kiss_output_stream_p(kiss_obj* p);

extern inline
kiss_obj* kiss_standard_input(void);

extern inline
kiss_obj* kiss_standard_output(void);

extern inline
kiss_obj* kiss_error_output(void);

extern inline
kiss_obj* kiss_consp(const kiss_obj* const obj);

extern inline
kiss_obj* kiss_car(const kiss_obj* const p);

extern inline
kiss_obj* kiss_cdr(const kiss_obj* const p);

extern inline
kiss_obj* kiss_cadr(const kiss_obj* const p);

extern inline
kiss_obj* kiss_cddr(const kiss_obj* const p);

extern inline
kiss_obj* kiss_caddr(const kiss_obj* const p);

extern inline
kiss_obj* kiss_null(const kiss_obj* const p);

extern inline
kiss_obj* kiss_listp(const kiss_obj* const p);

extern inline
kiss_obj* kiss_nreverse(kiss_obj* p);

extern inline
kiss_obj* kiss_assoc(const kiss_obj* const obj, kiss_obj* const alist);

extern inline
kiss_obj* kiss_assoc_using(const kiss_obj* test, const kiss_obj* const obj, kiss_obj* const alist);

extern inline
kiss_obj* kiss_last(const kiss_obj* const list, const kiss_obj* const rest);

extern inline
kiss_obj* kiss_nconc(kiss_obj* const lists);

extern inline
kiss_cons_t* kiss_init_cons(kiss_cons_t* const p, const kiss_obj* const left, const kiss_obj* const right);

extern inline
kiss_obj* kiss_cons(const kiss_obj* const car, const kiss_obj* const cdr);

extern inline
void kiss_push(const kiss_obj* const elm, kiss_obj** const list);

extern inline
size_t kiss_c_length(const kiss_obj* const p);

extern inline
kiss_obj* kiss_set_car(const kiss_obj* const obj, kiss_obj* const cons);

extern inline
kiss_obj* kiss_set_cdr(const kiss_obj* const obj, kiss_obj* const cons);

extern inline
kiss_obj* kiss_create_list(const kiss_obj* const i, const kiss_obj* const rest);

extern inline
kiss_obj* kiss_copy_list(const kiss_obj* p);

extern inline
kiss_obj* kiss_list(kiss_obj* const p);

extern inline
kiss_obj* kiss_c_list(int nargs, ...);

extern inline
void kiss_copy_list_to_consarray(const kiss_obj* const list, kiss_cons_t* const pointer);


extern inline
kiss_obj* kiss_c_mapcar1(const kiss_cf1_t f, const kiss_obj* list);

extern inline
kiss_obj* kiss_c_mapc1(const kiss_cf1_t f, const kiss_obj* const list);

extern inline
kiss_obj* kiss_mapcar1(const kiss_obj* const f, const kiss_obj* const list);

extern inline
kiss_obj* kiss_mapcar(const kiss_obj* const function, const kiss_obj* const list1, const kiss_obj* const rest);

extern inline
kiss_obj* kiss_mapcan(const kiss_obj* const function, const kiss_obj* const list1, const kiss_obj* const rest);

extern inline
kiss_obj* kiss_mapc1(const kiss_obj* const function, const kiss_obj* const list);
     
extern inline
kiss_obj* kiss_mapc(const kiss_obj* const function, const kiss_obj* const list1, const kiss_obj* const rest);

extern inline
kiss_obj* kiss_maplist1(const kiss_obj* const function, const kiss_obj* const list);

extern inline
kiss_obj* kiss_maplist(const kiss_obj* const function, const kiss_obj* const list1, const kiss_obj* const rest);

extern inline
kiss_obj* kiss_mapcon(const kiss_obj* const function, const kiss_obj* const list1, const kiss_obj* const rest);

extern inline
kiss_obj* kiss_mapl1(const kiss_obj* const function, const kiss_obj* const list);

extern inline
kiss_obj* kiss_mapl(const kiss_obj* const function, const kiss_obj* const list1, const kiss_obj* const rest);

extern inline
kiss_obj* kiss_append_s(kiss_obj* p);

extern inline
kiss_obj* kiss_append(kiss_obj* const p);

extern inline
kiss_obj* kiss_c_append(int nargs, ...);

extern inline
kiss_obj* kiss_reverse(kiss_obj* p);

extern inline
kiss_obj* kiss_member(const kiss_obj* const obj, kiss_obj* const list);

extern inline
kiss_obj* kiss_member_using(const kiss_obj* const predicate, kiss_obj* const obj, kiss_obj* const list);

extern inline
kiss_obj* kiss_plist_member (kiss_obj* plist, const kiss_obj* const property);

extern inline
kiss_obj* kiss_plist_remove(kiss_obj* plist, const kiss_obj* const property);

extern inline
kiss_obj* kiss_plist_get (kiss_obj* plist, const kiss_obj* const property);

extern inline
kiss_obj* kiss_plist_put(kiss_obj* plist, const kiss_obj* const property, const kiss_obj* const value);

extern inline
kiss_obj* kiss_plist_mapc(const kiss_obj* const function, const kiss_obj* const plist);

extern inline
kiss_obj* kiss_eval_compound_form(kiss_cons_t* p);

extern inline
kiss_obj* kiss_eval(const kiss_obj* const form);

extern inline
kiss_obj* kiss_eval_body(const kiss_obj* const body);
