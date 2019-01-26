/*  -*- coding: utf-8 -*-
  convert.c --- defines the function 'convert' of ISLisp processor KISS.

  Copyright (C) 2017, 2018, 2019 Yuji Minejima <yuji@minejima.jp>

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

kiss_obj* kiss_convert(const kiss_obj* obj, const kiss_obj* const class_name) {
     obj = kiss_eval(obj);
     kiss_k_class(class_name);
     switch (KISS_OBJ_TYPE(obj)) {
     case KISS_CHARACTER:
          if (class_name == (kiss_obj*)&KISS_Sc_character) {
               return (kiss_obj*)obj;
          } else if (class_name == (kiss_obj*)&KISS_Sc_integer) {
               return kiss_make_fixnum(kiss_C_wchar_t(obj));
          } else if (class_name == (kiss_obj*)&KISS_Sc_symbol) {
               const kiss_string_t* const str = kiss_chars_to_str(kiss_cons(obj, KISS_NIL));
               return kiss_intern((kiss_obj*)str);
          } else if (class_name == (kiss_obj*)&KISS_Sc_string) {
               Kiss_Err(L"Cannot convert character to string, use (create-string 1 ~S)", obj);
          } else {
               goto error;
          }
     case KISS_FIXNUM:
          if (class_name == (kiss_obj*)&KISS_Sc_integer) {
               return (kiss_obj*)obj;
          } else if (class_name == (kiss_obj*)&KISS_Sc_character) {
               return kiss_make_char(kiss_C_integer(obj));
          } else if (class_name == (kiss_obj*)&KISS_Sc_float) {
               return kiss_float(obj);
          } else if (class_name == (kiss_obj*)&KISS_Sc_string) {
               kiss_obj* out = kiss_create_string_output_stream();
               kiss_format_fixnum(out, obj, kiss_make_fixnum(10));
               return kiss_get_output_stream_string(out);
          } else {
               goto error;
          }
     case KISS_BIGNUM:
          if (class_name == (kiss_obj*)&KISS_Sc_integer) {
               return (kiss_obj*)obj;
          } else if (class_name == (kiss_obj*)&KISS_Sc_character) {
               return kiss_make_char(mpz_get_si(((kiss_bignum_t*)obj)->mpz));
          } else if (class_name == (kiss_obj*)&KISS_Sc_float) {
               return kiss_float(obj);
          } else if (class_name == (kiss_obj*)&KISS_Sc_string) {
               kiss_obj* out = kiss_create_string_output_stream();
               kiss_format_bignum(out, obj, kiss_make_fixnum(10));
               return kiss_get_output_stream_string(out);
          } else {
               goto error;
          }
     case KISS_FLOAT:
          if (class_name == (kiss_obj*)&KISS_Sc_float) {
               return (kiss_obj*)obj;
          } else if (class_name == (kiss_obj*)&KISS_Sc_integer) {
               Kiss_Err(L"Cannot convert float to <integer>, consider using floor, ceiling, round, or truncate", obj, class_name);
          } else if (class_name == (kiss_obj*)&KISS_Sc_string) {
               kiss_obj* out = kiss_create_string_output_stream();
               kiss_format_float(out, obj);
               return kiss_get_output_stream_string(out);
          } else {
               goto error;
          }
     case KISS_SYMBOL:
          if (class_name == (kiss_obj*)&KISS_Sc_symbol) {
               return (kiss_obj*)obj;
          } else if (class_name == (kiss_obj*)&KISS_Sc_string) {
               kiss_symbol_t* symbol = (kiss_symbol_t*)obj;
               return (kiss_obj*)kiss_make_string(symbol->name);
          } else if (obj == KISS_NIL) {
               if (class_name == (kiss_obj*)&KISS_Sc_list) {
                    return (kiss_obj*)obj;
               } else if (class_name == (kiss_obj*)&KISS_Sc_general_vector) {
                    return kiss_list_to_vec(obj);
               } else {
                    goto error;
               }
          } else {
               goto error;
          }
     case KISS_STRING:
          if (class_name == (kiss_obj*)&KISS_Sc_string) {
               return (kiss_obj*)obj;
          } else if (class_name == (kiss_obj*)&KISS_Sc_integer ||
                     class_name == (kiss_obj*)&KISS_Sc_float)
          {
               return kiss_c_parse_number(obj);
          } else if (class_name == (kiss_obj*)&KISS_Sc_symbol) {
               return kiss_intern(obj);
          } else if (class_name == (kiss_obj*)&KISS_Sc_general_vector) {
               return kiss_str_to_vec(obj);
          } else if (class_name == (kiss_obj*)&KISS_Sc_list) {
               return kiss_str_to_chars((kiss_string_t*)obj);
          } else {
               goto error;
          }
     case KISS_GENERAL_VECTOR:
          if (class_name == (kiss_obj*)&KISS_Sc_general_vector) {
               return (kiss_obj*)obj;
          } else if (class_name == (kiss_obj*)&KISS_Sc_list) {
               return kiss_vec_to_list(obj);
          } else {
               goto error;
          }
     case KISS_CONS:
          if (class_name == (kiss_obj*)&KISS_Sc_list) {
               return (kiss_obj*)obj;
          } else if (class_name == (kiss_obj*)&KISS_Sc_general_vector) {
               return kiss_list_to_vec(obj);
          } else {
               goto error;
          }
     default:
	  goto error;
     }

error:
     Kiss_Err(L"Cannot convert ~S to ~S", obj, class_name);
}
