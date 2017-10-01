/*  -*- coding: utf-8 -*- 
  cinvoke.c --- defines the C language function call mechanism of ISLisp processor KISS.

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

kiss_obj* kiss_cf_invoke(const kiss_cfunction_t* const cfun, kiss_obj* args) {
     long int min = cfun->min_args; /* minimum number of args required */
     long int max = cfun->max_args; /* maximum number of args permitted
                                       -1 means any number of args is permitted
				    */
     long int narg = kiss_c_length(args);
     if (narg < min) { Kiss_Err(L"Too few arguments ~S", cfun->name); }
     if (max >= 0 && narg > max) { Kiss_Err(L"Too many arguments ~S", cfun->name); }
     if (min == max) { /* exact number of argumets must be given  */
          switch (min) {
          case 0:
               return ((kiss_cf0_t)cfun->fun)();
          case 1:
               return ((kiss_cf1_t)cfun->fun)(KISS_CAR(args));
          case 2:
               return ((kiss_cf2_t)cfun->fun)(KISS_CAR(args), kiss_cadr(args));
          case 3: {
               kiss_obj* const x1 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x2 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x3 = KISS_CAR(args);
               return ((kiss_cf3_t)cfun->fun)(x1, x2, x3);
          }
          case 4: {
               kiss_obj* const x1 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x2 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x3 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x4 = KISS_CAR(args);
               return ((kiss_cf4_t)cfun->fun)(x1, x2, x3, x4);
          }
          case 5: {
               kiss_obj* const x1 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x2 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x3 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x4 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x5 = KISS_CAR(args);
               return ((kiss_cf5_t)cfun->fun)(x1, x2, x3, x4, x5);
          }
          case 6: {
               kiss_obj* const x1 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x2 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x3 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x4 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x5 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x6 = KISS_CAR(args);
               return ((kiss_cf6_t)cfun->fun)(x1, x2, x3, x4, x5, x6);
          }
          case 7: {
               kiss_obj* const x1 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x2 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x3 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x4 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x5 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x6 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x7 = KISS_CAR(args);
               return ((kiss_cf7_t)cfun->fun)(x1, x2, x3, x4, x5, x6, x7);
          }
          case 8: {
               kiss_obj* const x1 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x2 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x3 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x4 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x5 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x6 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x7 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x8 = KISS_CAR(args);
               return ((kiss_cf8_t)cfun->fun)(x1, x2, x3, x4, x5, x6, x7, x8);
          }
          case 9: {
               kiss_obj* const x1 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x2 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x3 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x4 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x5 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x6 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x7 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x8 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x9 = KISS_CAR(args);
               return ((kiss_cf9_t)cfun->fun)(x1, x2, x3, x4, x5, x6, x7, x8, x9);
          }
          case 10: {
               kiss_obj* const x1 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x2 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x3 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x4 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x5 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x6 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x7 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x8 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x9 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x10 = KISS_CAR(args);
               return ((kiss_cf10_t)cfun->fun)(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10);
          }
          default:
               Kiss_Err(L"cf_invoke| Internal error. the number of arguments given to a C function exceeds supported value: ~S", cfun->name);
          }
     } else { /* min_args number of args must be given, more than that are treated as :rest */
          switch (min) {
          case 0:
               return ((kiss_cf1_t)cfun->fun)(args);
          case 1:
               return ((kiss_cf2_t)cfun->fun)(KISS_CAR(args), KISS_CDR(args));
          case 2: {
               kiss_obj* const x1 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x2 = KISS_CAR(args); args = KISS_CDR(args);
               return ((kiss_cf3_t)cfun->fun)(x1, x2, args);
          }
          case 3: {
               kiss_obj* const x1 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x2 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x3 = KISS_CAR(args); args = KISS_CDR(args);
               return ((kiss_cf4_t)cfun->fun)(x1, x2, x3, args);
          }
          case 4: {
               kiss_obj* const x1 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x2 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x3 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x4 = KISS_CAR(args); args = KISS_CDR(args);
               return ((kiss_cf5_t)cfun->fun)(x1, x2, x3, x4, args);
          }
          case 5: {
               kiss_obj* const x1 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x2 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x3 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x4 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x5 = KISS_CAR(args); args = KISS_CDR(args);
               return ((kiss_cf6_t)cfun->fun)(x1, x2, x3, x4, x5, args);
          }
          case 6: {
               kiss_obj* const x1 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x2 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x3 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x4 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x5 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x6 = KISS_CAR(args); args = KISS_CDR(args);
               return ((kiss_cf7_t)cfun->fun)(x1, x2, x3, x4, x5, x6, args);
          }
          case 7: {
               kiss_obj* const x1 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x2 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x3 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x4 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x5 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x6 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x7 = KISS_CAR(args); args = KISS_CDR(args);
               return ((kiss_cf8_t)cfun->fun)(x1, x2, x3, x4, x5, x6, x7, args);
          }
          case 8: {
               kiss_obj* const x1 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x2 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x3 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x4 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x5 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x6 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x7 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x8 = KISS_CAR(args); args = KISS_CDR(args);
               return ((kiss_cf9_t)cfun->fun)(x1, x2, x3, x4, x5, x6, x7, x8,
                                              args);
          }
          case 9: {
               kiss_obj* const x1 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x2 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x3 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x4 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x5 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x6 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x7 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x8 = KISS_CAR(args); args = KISS_CDR(args);
               kiss_obj* const x9 = KISS_CAR(args); args = KISS_CDR(args);
               return ((kiss_cf10_t)cfun->fun)(x1, x2, x3, x4, x5, x6, x7, x8, x9, args);
          }
          default:
               Kiss_Err(L"cf_invoke| Internal error. the number of arguments given to a C function exceeds supported value: ~S", cfun->name);
          }
     }
     // not reach here
}

