/*  -*- coding: utf-8 -*-
  number.c --- defines the number handling mechanism of ISLisp processor KISS.

  Copyright (C) 2017, 2018 Yuji Minejima (yuji@minejima.jp).

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

kiss_bignum_t* kiss_make_bignum(kiss_ptr_int i) {
    kiss_bignum_t* p = Kiss_GC_Malloc(sizeof(kiss_bignum_t));
    p->type = KISS_BIGNUM;
    mpz_init_set_si(p->mpz, i);
    return p;
}

kiss_float_t* kiss_make_float(double f) {
    kiss_float_t* p = Kiss_GC_Malloc(sizeof(kiss_float_t));
    p->type = KISS_FLOAT;
    p->f = f;
    return p;
}

/* function: (integerp obj) -> boolean
   Returns t if obj is an integer otherwise, returns nil.*/
kiss_obj* kiss_integerp(const kiss_obj* const obj) {
     return KISS_IS_INTEGER(obj) ? KISS_T : KISS_NIL;
}

kiss_obj* kiss_fixnump(const kiss_obj* const obj) {
     return KISS_IS_FIXNUM(obj) ? KISS_T : KISS_NIL;
}

kiss_obj* kiss_bignump(const kiss_obj* const obj) {
     return KISS_IS_BIGNUM(obj) ? KISS_T : KISS_NIL;
}

/* function: (floatp obj) -> boolean
   Returns t if obj is an float otherwise, returns nil.*/
kiss_obj* kiss_floatp(const kiss_obj* const obj) {
     return KISS_IS_FLOAT(obj) ? KISS_T : KISS_NIL;
}

/* function: (numberp obj) -> boolean
   Returns t if obj is a number (instance of class <number>);
   otherwise, returns nil.
   The obj may be any ISLISP object. */
kiss_obj* kiss_numberp(const kiss_obj* const obj) {
     if (kiss_integerp(obj) == KISS_T || kiss_floatp(obj) == KISS_T) {
          return KISS_T;
     } else {
          return KISS_NIL;
     }
}


/* function: (float x) -> <float>
   Returns X itself if it is an instance of the class <float> and returns 
   a floating-point approximation of X otherwise. An error shall be signaled if
   X is not a number (error-id. domain-error). */
kiss_obj* kiss_float(kiss_obj* x) {
     Kiss_Number(x);
     switch (KISS_OBJ_TYPE(x)) {
     case KISS_FIXNUM: {
          return (kiss_obj*)kiss_make_float(kiss_ptr_int(x));
     }
     case KISS_BIGNUM: {
          return (kiss_obj*)kiss_make_float(mpz_get_d(((kiss_bignum_t*)x)->mpz));
     }
     case KISS_FLOAT:
          return x;
     default:
          fwprintf(stderr, L"kiss_float: unknown primitive number type = %d", KISS_OBJ_TYPE(x));
          exit(EXIT_FAILURE);
     }
}

static int is_valid_float_textual_representation(wchar_t* p) {
     if (*p == L'\0') { return 0; }
     if (*p == L'-') { p++; }
     if (*p == L'\0') { return 0; }
     if (wcschr(L"0123456789", *p) == NULL) { return 0; }
     while (*p != L'\0' && wcschr(L"0123456789", *p)) { p++; }
     if (*p == L'\0') { return 0; }
     if (*p == L'.') {
          p++;
          if (*p == L'\0') { return 0; }
          if (wcschr(L"0123456789", *p) == NULL) { return 0; }
          while (*p != L'\0' && wcschr(L"0123456789", *p)) { p++; }
          if (*p == L'\0') { return 1; } // [s]dd … d.dd … d
     }
     if (*p != L'e' && *p != L'E') { return 0; }
     p++;
     if (*p == L'\0') { return 0; }
     if (*p == L'-' || *p == L'+') { p++; }
     if (*p == L'\0') { return 0; }
     if (wcschr(L"0123456789", *p) == NULL) { return 0; }
     while (*p != L'\0' && wcschr(L"0123456789", *p)) { p++; }
     if (*p == '\0') { return 1; }
     return 0;
}

kiss_obj* kiss_c_parse_number(kiss_obj* obj) {
     kiss_string_t* str = Kiss_String(obj);
     wchar_t* p = str->str;
     if (*p == L'+') { p++; }
     int base = 10;

     if (*p == L'#') {
          p++;
          switch (*p) {
          case L'b': case L'B':
               base = 2;
               break;
          case L'x': case L'X':
               base = 16;
               break;
          case L'o': case L'O':
               base = 8;
               break;
          default:
               return NULL;
          }
          p++;
     }
     
     wchar_t* tail = NULL;
     long int i = wcstol(p, &tail, base);
     if (tail == p + wcslen(p) && i <= KISS_PTR_INT_MAX) {
          return (kiss_obj*)kiss_make_fixnum(i);
     }
     
     char* s = kiss_wcstombs(p);
     kiss_bignum_t* z = kiss_make_bignum(0);
     int result = mpz_set_str(z->mpz, s, base);
     free(s);
     if (result == 0) {
          return (kiss_obj*)z;
     }
     if (base != 10) { return NULL; }

     if (!is_valid_float_textual_representation(p)) { return NULL; }
     wchar_t * tailptr = NULL;
     double f = wcstod(p, &tailptr);
     if (*tailptr != L'\0') {
          fwprintf(stderr, L"parse-string: cannot parse valid float %ls\n", p);
          exit(EXIT_FAILURE);
     }
     kiss_float_t* x = kiss_make_float(f);
     return (kiss_obj*)x;
}

/* function: (parse-number string) -> <number>
   The characters belonging to STRING are scanned (as if by read) and if the resulting
   lexeme is the textual representation of a number, the number it represents is returned.
   An error shall be signaled if STRING is not a string (error-id. domain-error).
   An error shall be signaled if STRING is not the textual representation of a number
   (error-id. cannot-parse-number) */
kiss_obj* kiss_parse_number(kiss_obj* obj) {
     kiss_obj* p = kiss_c_parse_number(obj);
     if (p == NULL) { Kiss_Cannot_Parse_Number_Error(obj); }
     return p;
}

static inline
kiss_obj* kiss_plus2_fixnum2 (const kiss_obj* const a, const kiss_obj* const b) {
     const kiss_ptr_int i1 = kiss_ptr_int(a);
     const kiss_ptr_int i2 = kiss_ptr_int(b);
     if ((i1 >= 0 && i2 <=0) || (i1 <= 0 && i2 >=0)) {
          return (kiss_obj*)kiss_make_fixnum(i1 + i2);
     } else if (i1 > 0 && i2 > 0) {
          kiss_ptr_int n = KISS_PTR_INT_MAX - i2;
          if (i1 <= n) {
               return (kiss_obj*)kiss_make_fixnum(i1 + i2);
          } else {
               kiss_bignum_t* z = kiss_make_bignum(i1);
               mpz_add_ui(z->mpz, z->mpz, i2);
               return (kiss_obj*)z;
          }
     } else if (i1 < 0 && i2 < 0) {
          kiss_ptr_int n = KISS_PTR_INT_MIN - i2;
          if (i1 >= n) {
               return (kiss_obj*)kiss_make_fixnum(i1 + i2);
          } else {
               kiss_bignum_t* z1 = kiss_make_bignum(i1);
               kiss_bignum_t* z2 = kiss_make_bignum(i2);
               mpz_add(z1->mpz, z1->mpz, z2->mpz);
               return (kiss_obj*)z1;
          }
     } else {
          fwprintf(stderr, L"kiss_plus2_fixnum2: internal error");
          exit(EXIT_FAILURE);
     }
}

static inline
kiss_obj* kiss_plus2_fixnum_bignum (kiss_obj* a, kiss_obj* b) {
     kiss_bignum_t* z1 = kiss_make_bignum(kiss_ptr_int(a));
     kiss_bignum_t* z2 = (kiss_bignum_t*)b;
     mpz_add(z1->mpz, z1->mpz, z2->mpz);
     return (kiss_obj*)z1;
}

static inline
kiss_obj* kiss_plus2_fixnum_float(kiss_obj* a, kiss_obj* b) {
     kiss_float_t* f1 = kiss_make_float(kiss_ptr_int(a));
     kiss_float_t* f2 = (kiss_float_t*)b;
     f1->f += f2->f;
     return (kiss_obj*)f1;
}

static inline
kiss_obj* kiss_plus2_bignum2(kiss_obj* a, kiss_obj* b) {
     kiss_bignum_t* z1 = (kiss_bignum_t*)a;
     kiss_bignum_t* z2 = (kiss_bignum_t*)b;
     kiss_bignum_t* p = kiss_make_bignum(0);
     mpz_add(p->mpz, z1->mpz, z2->mpz);
     return (kiss_obj*)p;
}

static inline
kiss_obj* kiss_plus2_bignum_float(kiss_obj* a, kiss_obj* b) {
     kiss_bignum_t* z1 = (kiss_bignum_t*)a;
     kiss_float_t* p = kiss_make_float(mpz_get_d(z1->mpz));
     kiss_float_t* f2 = (kiss_float_t*)b;
     p->f += f2->f;
     return (kiss_obj*)p;
}

static inline
kiss_obj* kiss_plus2_float2(kiss_obj* a, kiss_obj* b) {
     kiss_float_t* f1 = (kiss_float_t*)a;
     kiss_float_t* f2 = (kiss_float_t*)b;
     kiss_float_t* p = kiss_make_float(f1->f + f2->f);
     return (kiss_obj*)p;
}

kiss_obj* kiss_plus2(kiss_obj* a, kiss_obj* b) {
     Kiss_Number(a);
     Kiss_Number(b);
     switch (KISS_OBJ_TYPE(a)) {
     case KISS_FIXNUM:
          switch (KISS_OBJ_TYPE(b)) {
          case KISS_FIXNUM:
               return kiss_plus2_fixnum2(a, b);
          case KISS_BIGNUM:
               return kiss_plus2_fixnum_bignum(a, b);
          case KISS_FLOAT:
               return kiss_plus2_fixnum_float(a, b);
          default:
               fwprintf(stderr, L"kiss_plus2: unexpected primitive number type = %d",
                        KISS_OBJ_TYPE(b));
               exit(EXIT_FAILURE);
          }
          break;
     case KISS_BIGNUM:
          switch (KISS_OBJ_TYPE(b)) {
          case KISS_FIXNUM:
               return kiss_plus2_fixnum_bignum(b, a);
          case KISS_BIGNUM:
               return kiss_plus2_bignum2(a, b);
          case KISS_FLOAT:
               return kiss_plus2_bignum_float(a, b);
          default:
               fwprintf(stderr, L"kiss_plus2: unexpected primitive number type = %d",
                        KISS_OBJ_TYPE(b));
               exit(EXIT_FAILURE);
          }
          break;
     case KISS_FLOAT:
          switch (KISS_OBJ_TYPE(b)) {
          case KISS_FIXNUM:
               return kiss_plus2_fixnum_float(b, a);
          case KISS_BIGNUM:
               return kiss_plus2_bignum_float(b, a);
          case KISS_FLOAT:
               return kiss_plus2_float2(a, b);
          default:
               fwprintf(stderr, L"kiss_plus2: unexpected primitive number type = %d",
                        KISS_OBJ_TYPE(b));
               exit(EXIT_FAILURE);
          }
          break;
     default:
          fwprintf(stderr, L"kiss_plus2: unexpected primitive number type = %d",
                   KISS_OBJ_TYPE(a));
          exit(EXIT_FAILURE);
     }
}

/* function: (+ x*) -> <number> 
   The function + returns the sum of its arguments. 
   If all arguments are integers, the result is an integer.
   If any argument is a float, the result is a float.
   When given no arguments, + returns 0. 
   An error shall be signaled if any X is not a number (error-id. domain-error). */
kiss_obj* kiss_plus(kiss_obj* list) {
     if (list == KISS_NIL) { return kiss_make_fixnum(0); }
     kiss_obj* p = KISS_CAR(list);
     list = KISS_CDR(list);
     while (list != KISS_NIL) {
          p = kiss_plus2(p, KISS_CAR(list));
          list = KISS_CDR(list);
     }
     return p;
}

kiss_obj* kiss_flip_sign(kiss_obj* obj) {
     Kiss_Number(obj);
     switch (KISS_OBJ_TYPE(obj)) {
     case KISS_FIXNUM: {
          kiss_ptr_int i = - kiss_ptr_int(obj);
          if (i <= KISS_PTR_INT_MAX && i >= KISS_PTR_INT_MIN) {
               return kiss_make_fixnum(i);
          } else {
               kiss_bignum_t* z = kiss_make_bignum(i);
               return (kiss_obj*)z;
          }
     }
     case KISS_BIGNUM: {
          kiss_bignum_t* z = kiss_make_bignum(0);
          mpz_set(z->mpz, ((kiss_bignum_t*)obj)->mpz);
          mpz_neg(z->mpz, z->mpz);
          return (kiss_obj*)z;
     }
     case KISS_FLOAT: {
          kiss_float_t* f = kiss_make_float( - ((kiss_float_t*)obj)->f);
          return (kiss_obj*)f;
     }
     default:
          fwprintf(stderr, L"kiss_flisp_sign: unexpected primitive number type = %d",
                   KISS_OBJ_TYPE(obj));
          exit(EXIT_FAILURE);
     }
}


/* function: (- x+) -> <number> 
   Given one argument, X, this function returns its additive inverse.
   An error shall be signaled if X is not a number (error-id. domain-error).
   If an implementation supports a -0.0 that is distinct from 0.0 ,then (- 0.0)
   returns -0.0 ;in implementations where -0.0 and 0.0 are not distinct, (- 0.0)
   returns 0.0. */
kiss_obj* kiss_minus(kiss_obj* number, kiss_obj* rest) {
     if (rest == KISS_NIL) {
          return kiss_flip_sign(number);
     }
     for (kiss_obj* p = rest; p != KISS_NIL; p = KISS_CDR(p)) {
          number = kiss_plus2(number, kiss_flip_sign(KISS_CAR(p)));
     }
     return number;     
}

static inline
kiss_obj* kiss_multiply2_fixnum_bignum(kiss_obj* a, kiss_obj* b) {
     kiss_bignum_t* z = kiss_make_bignum(kiss_ptr_int(a));
     mpz_mul(z->mpz, z->mpz, ((kiss_bignum_t*)b)->mpz);
     return (kiss_obj*)z;
}

static inline
kiss_obj* kiss_multiply2_fixnum2 (kiss_obj* a, kiss_obj* b) {
     kiss_ptr_int i1 = kiss_ptr_int(a);
     kiss_ptr_int i2 = kiss_ptr_int(b);

     if (i1 == 0 || i2 == 0) {
          return (kiss_obj*)kiss_make_fixnum(0);
     }
     
     int minus = 1;
     if ((i1 < 0 && i2 < 0) || (i1 > 0 && i2 > 0)) {
          minus = 0;
     }

     if (i1 < 0) {
          if (i1 < -KISS_PTR_INT_MAX) {
               return kiss_multiply2_fixnum_bignum(b, (kiss_obj*)kiss_make_bignum(i1));
          }
          i1 = -i1;
     }
     if (i2 < 0) {
          if (i2 < -KISS_PTR_INT_MAX) {
               return kiss_multiply2_fixnum_bignum(a, (kiss_obj*)kiss_make_bignum(i2));
          }
          i2 = -i2;
     }

     kiss_ptr_int q = KISS_PTR_INT_MAX / i1;
     if (i2 <= q) {
          return (kiss_obj*)kiss_make_fixnum(i1 * i2 * (minus ? -1 : 1));
     } else {
          kiss_bignum_t* z = kiss_make_bignum(i1);
          mpz_mul_si(z->mpz, z->mpz, i2);
          if (minus) {
               mpz_neg(z->mpz, z->mpz);
          }
          return (kiss_obj*)z;
     }
}

static inline
kiss_obj* kiss_multiply2_fixnum_float(kiss_obj* a, kiss_obj* b) {
     kiss_float_t* f = kiss_make_float(((kiss_float_t*)b)->f);
     f->f *= kiss_ptr_int(a);
     return (kiss_obj*)f;
}

static inline
kiss_obj* kiss_multiply2_bignum2(kiss_obj* a, kiss_obj* b) {
     kiss_bignum_t* z = kiss_make_bignum(0);
     mpz_set(z->mpz, ((kiss_bignum_t*)a)->mpz);
     mpz_mul(z->mpz, z->mpz, ((kiss_bignum_t*)b)->mpz);
     return (kiss_obj*)z;
}

static inline
kiss_obj* kiss_multiply2_bignum_float(kiss_obj* a, kiss_obj* b) {
     kiss_float_t* f = kiss_make_float(((kiss_float_t*)b)->f);
     f->f *= mpz_get_d(((kiss_bignum_t*)a)->mpz);
     return (kiss_obj*)f;
}

static inline
kiss_obj* kiss_multiply2_float2(kiss_obj* a, kiss_obj* b) {
     kiss_float_t* f = kiss_make_float(((kiss_float_t*)a)->f);
     f->f *= ((kiss_float_t*)b)->f;
     return (kiss_obj*)f;
}

static inline
kiss_obj* kiss_multiply2(kiss_obj* a, kiss_obj* b) {
     Kiss_Number(a);
     Kiss_Number(b);
     switch (KISS_OBJ_TYPE(a)) {
     case KISS_FIXNUM:
          switch (KISS_OBJ_TYPE(b)) {
          case KISS_FIXNUM:
               return kiss_multiply2_fixnum2(a, b);
          case KISS_BIGNUM:
               return kiss_multiply2_fixnum_bignum(a, b);
          case KISS_FLOAT:
               return kiss_multiply2_fixnum_float(a, b);
          default:
               fwprintf(stderr, L"kiss_multiply2: unexpected primitive number type = %d",
                        KISS_OBJ_TYPE(b));
               exit(EXIT_FAILURE);
          }
          break;
     case KISS_BIGNUM:
          switch (KISS_OBJ_TYPE(b)) {
          case KISS_FIXNUM:
               return kiss_multiply2_fixnum_bignum(b, a);
          case KISS_BIGNUM:
               return kiss_multiply2_bignum2(a, b);
          case KISS_FLOAT:
               return kiss_multiply2_bignum_float(a, b);
          default:
               fwprintf(stderr, L"kiss_multiply2: unexpected primitive number type = %d",
                        KISS_OBJ_TYPE(b));
               exit(EXIT_FAILURE);
          }
          break;
     case KISS_FLOAT:
          switch (KISS_OBJ_TYPE(b)) {
          case KISS_FIXNUM:
               return kiss_multiply2_fixnum_float(b, a);
          case KISS_BIGNUM:
               return kiss_multiply2_bignum_float(b, a);
          case KISS_FLOAT:
               return kiss_multiply2_float2(a, b);
          default:
               fwprintf(stderr, L"kiss_multiply2: unexpected primitive number type = %d",
                        KISS_OBJ_TYPE(b));
               exit(EXIT_FAILURE);
          }
          break;
     default:
          fwprintf(stderr, L"kiss_multiply2: unexpected primitive number type = %d",
                   KISS_OBJ_TYPE(a));
          exit(EXIT_FAILURE);
     }
}
/* function: (* x*) -> <number> 
   The function * returns the product of its arguments. 
   If all arguments are integers, the result is an integer.
   If any argument is a float, the result is a float.
   When given no arguments, * returns 1. 
   An error shall be signaled if any x is not a number (error-id. domain-error). */
kiss_obj* kiss_multiply(kiss_obj* list) {
     if (list == KISS_NIL) { return kiss_make_fixnum(1); }
     kiss_obj* p = KISS_CAR(list);
     list = KISS_CDR(list);
     while (list != KISS_NIL) {
          p = kiss_multiply2(p, KISS_CAR(list));
          list = KISS_CDR(list);
     }
     return p;
}


/* function: (= x y) -> boolean 
   Returns t if X has the same mathematical value as Y ;otherwise, returns nil.
   An error shall be signaled if either X or Y is not a number (error-id. domain-error).
   Note: = differs from eql because = compares only the mathematical values of its arguments,
   whereas eql also compares the representations. */
kiss_obj* kiss_num_eq(const kiss_obj* const a, const kiss_obj* const b) {
     Kiss_Number(a);
     Kiss_Number(b);
     switch (KISS_OBJ_TYPE(a)) {
     case KISS_FIXNUM:
          switch (KISS_OBJ_TYPE(b)) {
          case KISS_FIXNUM:
               return (kiss_ptr_int)a == (kiss_ptr_int)b ? KISS_T : KISS_NIL;
          case KISS_BIGNUM:
               return mpz_cmp_si(((kiss_bignum_t*)b)->mpz, kiss_ptr_int(a)) == 0 ?
                    KISS_T : KISS_NIL;
          case KISS_FLOAT:
               return (((kiss_float_t*)b)->f == kiss_ptr_int(a)) ? KISS_T : KISS_NIL;
          default:
               fwprintf(stderr, L"kiss_num_eq: unexpected primitive number type = %d",
                        KISS_OBJ_TYPE(b));
               exit(EXIT_FAILURE);
          }
          break;
     case KISS_BIGNUM:
          switch (KISS_OBJ_TYPE(b)) {
          case KISS_FIXNUM:
               return mpz_cmp_si(((kiss_bignum_t*)a)->mpz, kiss_ptr_int(b)) == 0 ?
                    KISS_T : KISS_NIL;
          case KISS_BIGNUM:
               return mpz_cmp(((kiss_bignum_t*)a)->mpz, ((kiss_bignum_t*)b)->mpz) == 0 ?
                    KISS_T : KISS_NIL;
          case KISS_FLOAT:
               return mpz_cmp_d(((kiss_bignum_t*)a)->mpz, ((kiss_float_t*)b)->f) == 0 ?
                    KISS_T : KISS_NIL;
          default:
               fwprintf(stderr, L"kiss_num_eq: unexpected primitive number type = %d",
                        KISS_OBJ_TYPE(b));
               exit(EXIT_FAILURE);
          }
          break;
     case KISS_FLOAT:
          switch (KISS_OBJ_TYPE(b)) {
          case KISS_FIXNUM:
               return ((kiss_float_t*)a)->f == kiss_ptr_int(b) ? KISS_T : KISS_NIL;
          case KISS_BIGNUM:
               return mpz_cmp_d(((kiss_bignum_t*)b)->mpz, ((kiss_float_t*)a)->f) == 0 ?
                    KISS_T : KISS_NIL;
          case KISS_FLOAT:
               return ((kiss_float_t*)a)->f == ((kiss_float_t*)b)->f ? KISS_T : KISS_NIL;
               break;
          default:
               fwprintf(stderr, L"kiss_num_eq: unexpected primitive number type = %d",
                        KISS_OBJ_TYPE(b));
               exit(EXIT_FAILURE);
          }
          break;
     default:
          fwprintf(stderr, L"kiss_num_eq: unexpected primitive number type = %d",
                   KISS_OBJ_TYPE(a));
          exit(EXIT_FAILURE);
     }
}

/* function: (/= x1 x2) -> boolean
   Returns t if x1 and x2 have mathematically distinct values; otherwise,
   returns nil. An error shall be signaled if either x1 or x2 is not a number
   (error-id. domain-error ). */
kiss_obj* kiss_num_neq(const kiss_obj* const x, const kiss_obj* const y) {
     return kiss_num_eq(x, y) == KISS_T ? KISS_NIL : KISS_T;
}

/* function: (< x1 x2) -> boolean 
   < returns t if X1 is less than X2.
   The mathematical values of the arguments are compared. 
   An error shall be signaled if either X1 or X2 is not a number (error-id. domain-error). */
kiss_obj* kiss_num_lessthan(const kiss_obj* const a, const kiss_obj* const b) {
     Kiss_Number(a);
     Kiss_Number(b);
     switch (KISS_OBJ_TYPE(a)) {
     case KISS_FIXNUM:
          switch (KISS_OBJ_TYPE(b)) {
          case KISS_FIXNUM:
               return kiss_ptr_int(a) < kiss_ptr_int(b) ? KISS_T : KISS_NIL;
          case KISS_BIGNUM:
               return mpz_cmp_si(((kiss_bignum_t*)b)->mpz, kiss_ptr_int(a)) > 0 ?
                    KISS_T : KISS_NIL;
          case KISS_FLOAT:
               return kiss_ptr_int(a) < ((kiss_float_t*)b)->f ? KISS_T : KISS_NIL;
          default:
               fwprintf(stderr, L"kiss_num_lessthan: unexpected primitive number type = %d",
                        KISS_OBJ_TYPE(b));
               exit(EXIT_FAILURE);
          }
          break;
     case KISS_BIGNUM:
          switch (KISS_OBJ_TYPE(b)) {
          case KISS_FIXNUM:
               return mpz_cmp_si(((kiss_bignum_t*)a)->mpz, kiss_ptr_int(b)) < 0 ?
                    KISS_T : KISS_NIL;
          case KISS_BIGNUM:
               return mpz_cmp(((kiss_bignum_t*)a)->mpz, ((kiss_bignum_t*)b)->mpz) < 0 ?
                    KISS_T : KISS_NIL;
          case KISS_FLOAT:
               return mpz_cmp_d(((kiss_bignum_t*)a)->mpz, ((kiss_float_t*)b)->f) < 0 ?
                    KISS_T : KISS_NIL;
          default:
               fwprintf(stderr, L"kiss_plus2: unexpected primitive number type = %d",
                        KISS_OBJ_TYPE(b));
               exit(EXIT_FAILURE);
          }
          break;
     case KISS_FLOAT:
          switch (KISS_OBJ_TYPE(b)) {
          case KISS_FIXNUM:
               return ((kiss_float_t*)a)->f < kiss_ptr_int(b) ?
                    KISS_T : KISS_NIL;
          case KISS_BIGNUM:
               return mpz_cmp_d(((kiss_bignum_t*)b)->mpz, ((kiss_float_t*)a)->f) > 0 ?
                    KISS_T : KISS_NIL;
          case KISS_FLOAT:
               return ((kiss_float_t*)a)->f <  ((kiss_float_t*)b)->f ? KISS_T : KISS_NIL;
               break;
          default:
               fwprintf(stderr, L"kiss_num_lessthan: unexpected primitive number type = %d",
                        KISS_OBJ_TYPE(b));
               exit(EXIT_FAILURE);
          }
          break;
     default:
          fwprintf(stderr, L"kiss_num_lessthan: unexpected primitive number type = %d",
                   KISS_OBJ_TYPE(a));
          exit(EXIT_FAILURE);
     }
}

/* function: (<= x1 x2) -> boolean */
kiss_obj* kiss_num_lessthan_eq(const kiss_obj* const a, const kiss_obj* const b) {
     if (kiss_num_lessthan(a, b) == KISS_T || kiss_num_eq(a, b) == KISS_T) {
          return KISS_T;
     } else {
          return KISS_NIL;
     }
}

/* function: (> x1 x2) -> boolean */
kiss_obj* kiss_num_greaterthan(const kiss_obj* const a, const kiss_obj* const b) {
     return kiss_num_lessthan_eq(a, b) == KISS_T ? KISS_NIL : KISS_T;
}

/* function: (>= x1 x2) -> boolean */
kiss_obj* kiss_num_greaterthan_eq(const kiss_obj* const a, const kiss_obj* const b) {
     return kiss_num_lessthan(a, b) == KISS_T ? KISS_NIL : KISS_T;
}

kiss_obj* kiss_fixnum_if_possible(const kiss_obj* const obj) {
     if (KISS_IS_BIGNUM(obj)) {
          kiss_bignum_t* z = (kiss_bignum_t*)obj;
          return (kiss_obj*)((mpz_cmp_si(z->mpz, KISS_PTR_INT_MAX) <= 0 &&
                              mpz_cmp_si(z->mpz, KISS_PTR_INT_MIN) >= 0) ?
                             kiss_make_fixnum(mpz_get_si(z->mpz)) : obj);
     } else {
          return (kiss_obj*)obj;
     }
}

static inline
kiss_obj* kiss_div_fixnum(kiss_obj* a, kiss_obj* b) {
     kiss_ptr_int i1 = kiss_ptr_int(a);
     kiss_ptr_int i2 = kiss_ptr_int(b);
     kiss_ptr_int q = i1 / i2;
     kiss_ptr_int r = i1 % i2;
     if (i1 >= 0) { // (div 14 3) => 4
          if (i2 > 0) {
               return (kiss_obj*)kiss_make_fixnum(q);
          } else {  // (div 14 -3) => -5
               return q == 0 || r == 0 ?
                    (kiss_obj*)kiss_make_fixnum(q) :
                    (kiss_obj*)kiss_make_fixnum(q - 1);
          }
     } else if (i1 < 0) {
          if (i2 > 0) { // (div -14 3) => -5
               return q == 0 || r == 0 ?
                    (kiss_obj*)kiss_make_fixnum(q) :
                    (kiss_obj*)kiss_make_fixnum(q - 1);
          } else {      // (div -14 -3) => 4
               return (kiss_obj*)kiss_make_fixnum(q);
          }
     } else {
          fwprintf(stderr, L"kiss_div_fixnum: logical error i1 = %ld, i2 = %ld", i1, i2);
          exit(EXIT_FAILURE);
     }
}

/* function: (div z1 z2) -> <integer>
   div returns the greatest integer less than or equal to the quotient of Z1 and Z2.
   An error shall be signaled if Z2 is zero (error-id. division-by-zero). 
   An error shall be signaled if either z1 or z2 is not an integer (error-id. domain-error). */
kiss_obj* kiss_div(kiss_obj* a, kiss_obj* b) {
     Kiss_Integer(a);
     Kiss_Non_Zero_Integer(b);
     switch (KISS_OBJ_TYPE(a)) {
     case KISS_FIXNUM: {
          switch (KISS_OBJ_TYPE(b)) {
          case KISS_FIXNUM:
               return kiss_div_fixnum(a, b);
          case KISS_BIGNUM: {
               kiss_ptr_int i1 = kiss_ptr_int(a);
               kiss_bignum_t* z1 = kiss_make_bignum(i1);
               kiss_bignum_t* z2 = (kiss_bignum_t*)b;
               mpz_fdiv_q(z1->mpz, z1->mpz, z2->mpz);
               return (kiss_obj*)z1;
          }
          default:
               fwprintf(stderr, L"kiss_div: unexpected primitive integer type = %d",
                        KISS_OBJ_TYPE(b));
               exit(EXIT_FAILURE);
          }
          break;
     }
     case KISS_BIGNUM:
          switch (KISS_OBJ_TYPE(b)) {
          case KISS_FIXNUM: {
               kiss_ptr_int i2 = kiss_ptr_int(b);
               kiss_bignum_t* z1 = (kiss_bignum_t*)a;
               kiss_bignum_t* z2 = kiss_make_bignum(i2);
               mpz_fdiv_q(z2->mpz, z1->mpz, z2->mpz);
               return (kiss_obj*)z2;
          }
          case KISS_BIGNUM: {
               kiss_bignum_t* z1 = (kiss_bignum_t*)a;
               kiss_bignum_t* z2 = (kiss_bignum_t*)b;
               kiss_bignum_t* q  = kiss_make_bignum(0);
               mpz_fdiv_q(q->mpz, z1->mpz, z2->mpz);
               return (kiss_obj*)q;
          }
          default:
               fwprintf(stderr, L"kiss_div: unexpected primitive integer type = %d",
                        KISS_OBJ_TYPE(b));
               exit(EXIT_FAILURE);
          }
          break;
     default:
          fwprintf(stderr, L"kiss_div: unexpected primitive integer type = %d",
                   KISS_OBJ_TYPE(a));
          exit(EXIT_FAILURE);
     }
}


static inline
kiss_obj* kiss_mod_fixnum(kiss_obj* a, kiss_obj* b) {
     kiss_ptr_int i1 = kiss_ptr_int(a);
     kiss_ptr_int i2 = kiss_ptr_int(b);
     kiss_ptr_int q = i1 / i2;
     kiss_ptr_int r = i1 % i2;
     if (i1 >= 0) { // (div 14 3) => 2
          if (i2 > 0) {
               return (kiss_obj*)kiss_make_fixnum(r);
          } else {  // (div 14 -3) => -1
               return q == 0 || r == 0 ?
                    (kiss_obj*)kiss_make_fixnum(r) :
                    (kiss_obj*)kiss_make_fixnum(r + i2);
          }
     } else if (i1 < 0) {
          if (i2 > 0) { // (div -14 3) => 1
               return q == 0 || r == 0 ?
                    (kiss_obj*)kiss_make_fixnum(r) :
                    (kiss_obj*)kiss_make_fixnum(r + i2);
          } else {      // (div -14 -3) => -2
               return (kiss_obj*)kiss_make_fixnum(r);
          }
     } else {
          fwprintf(stderr, L"kiss_div_fixnum: logical error i1 = %ld, i2 = %ld", i1, i2);
          exit(EXIT_FAILURE);
     }
}

/* function: (mod z1 z2) -> <integer>
   mod returns the remainder of the integer division of z1 by z2.
   The sign of the result is the sign of z2.
   The result lies between 0 (inclusive) and z2 (exclusive),
   and the difference of z1 and this result is divisible by z2 without remainder. */
kiss_obj* kiss_mod(kiss_obj* a, kiss_obj* b) {
     Kiss_Integer(a);
     Kiss_Non_Zero_Integer(b);
     switch (KISS_OBJ_TYPE(a)) {
     case KISS_FIXNUM: {
          switch (KISS_OBJ_TYPE(b)) {
          case KISS_FIXNUM:
               return kiss_mod_fixnum(a, b);
          case KISS_BIGNUM: {
               kiss_ptr_int i1 = kiss_ptr_int(a);
               kiss_bignum_t* z1 = kiss_make_bignum(i1);
               kiss_bignum_t* z2 = (kiss_bignum_t*)b;
               mpz_fdiv_r(z1->mpz, z1->mpz, z2->mpz);
               return (kiss_obj*)z1;
          }
          default:
               fwprintf(stderr, L"kiss_div: unexpected primitive integer type = %d",
                        KISS_OBJ_TYPE(b));
               exit(EXIT_FAILURE);
          }
          break;
     }
     case KISS_BIGNUM:
          switch (KISS_OBJ_TYPE(b)) {
          case KISS_FIXNUM: {
               kiss_ptr_int i2 = kiss_ptr_int(b);
               kiss_bignum_t* z1 = (kiss_bignum_t*)a;
               kiss_bignum_t* z2 = kiss_make_bignum(i2);
               mpz_fdiv_r(z2->mpz, z1->mpz, z2->mpz);
               return (kiss_obj*)z2;
          }
          case KISS_BIGNUM: {
               kiss_bignum_t* z1 = (kiss_bignum_t*)a;
               kiss_bignum_t* z2 = (kiss_bignum_t*)b;
               kiss_bignum_t* q  = kiss_make_bignum(0);
               mpz_fdiv_r(q->mpz, z1->mpz, z2->mpz);
               return (kiss_obj*)q;
          }
          default:
               fwprintf(stderr, L"kiss_div: unexpected primitive integer type = %d",
                        KISS_OBJ_TYPE(b));
               exit(EXIT_FAILURE);
          }
          break;
     default:
          fwprintf(stderr, L"kiss_div: unexpected primitive integer type = %d",
                   KISS_OBJ_TYPE(a));
          exit(EXIT_FAILURE);
     }
}

/* function: (gcd z1 z2) -> <integer>
   gcd returns the greatest common divisor of its integer arguments. 
   The result is a non-negative integer. 
   For nonzero arguments the greatest common divisor is the largest integer z 
   such that z1 and z2 are integral multiples of z.
   An error shall be signaled if either z1 or z2 is not an integer 
   (error-id. domain-error).
*/
kiss_obj* kiss_gcd(kiss_obj* z1, kiss_obj* z2) {
     fwprintf(stderr, L"kiss_gcd: not implemented");
     exit(EXIT_FAILURE);
}

/* function: (lcm z1 z2) -> <integer>
   lcm returns the least common multiple of its integer arguments.
   gcd and lcm satisfies: (= (* (gcd m n) (lcm m n)) (abs (* m n)))
   That is, the evaluation of the above form always return t.
   An error shall be signaled if either z1 or z2 is not an integer
   (error-id. domain-error).
*/
kiss_obj* kiss_lcm(kiss_obj* z1, kiss_obj* z2) {
     fwprintf(stderr, L"kiss_lcm: not implemented");
     exit(EXIT_FAILURE);
}


/* function: (abs x) -> <number>
   The function abs returns the absolute value of its argument.
   An error shall be signaled if x is not a number (error-id. domain-error) */
kiss_obj* kiss_abs(kiss_obj* x) {
     Kiss_Number(x);
     switch (KISS_OBJ_TYPE(x)) {
     case KISS_FIXNUM: {
          kiss_ptr_int i = kiss_ptr_int(x);
          if (i < 0) {
               if (i == KISS_PTR_INT_MIN) {
                    kiss_bignum_t* z = kiss_make_bignum(i);
                    mpz_neg(z->mpz, z->mpz);
                    return (kiss_obj*)z;
               } else {
                    return (kiss_obj*)kiss_make_fixnum(-i);
               }
          } else {
               return x;
          }
     }
     case KISS_BIGNUM: {
          kiss_bignum_t* z = kiss_make_bignum(0);
          mpz_abs(z->mpz, ((kiss_bignum_t*)x)->mpz);
          return (kiss_obj*)z;
     }
     case KISS_FLOAT: {
          kiss_float_t* f = kiss_make_float(fabs(((kiss_float_t*)x)->f));
          return (kiss_obj*)f;
     }
     default:
          fwprintf(stderr, L"kiss_abs: unknown primitive number type = %d", KISS_OBJ_TYPE(x));
          exit(EXIT_FAILURE);
     }
}

/* function: (exp x) -> <number>
   Returns e raised to the power x, where e is the base of the natural logarithm.
   An error shall be signaled if x is not a number (error-id. domain-error).
*/
kiss_obj* kiss_exp(kiss_obj* x) {
     fwprintf(stderr, L"kiss_exp: not implemented");
     exit(EXIT_FAILURE);
}

/* function: (floor x) -> <integer> 
   Returns the greatest integer less than or equal to x.
   Thatis, x is truncated towards negative infinity.
   An error shall be signaled if x is not a number (error-id. domain-error).
*/
kiss_obj* kiss_floor(kiss_obj* x) {
     fwprintf(stderr, L"kiss_exp: not implemented");
     exit(EXIT_FAILURE);
}

/* function: (ceiling x) -> <integer> 
   Returns the smallest integer that is not smaller than x. 
   Thatis, x is truncated towards positive infinity.
   An error shall be signaled if x is not a number (error-id. domain-error).   
*/
kiss_obj* kiss_ceiling(kiss_obj* x) {
     fwprintf(stderr, L"kiss_ceiling: not implemented");
     exit(EXIT_FAILURE);
}

/* function: (truncate x) -> <integer> 
   Returns the integer between 0 and x(inclusive) that is nearest to x.
   Thatis, x is truncated towards zero.
   An error shall be signaled if x is not a number (error-id. domain-error).
*/
kiss_obj* kiss_truncate(kiss_obj* x) {
     fwprintf(stderr, L"kiss_truncate: not implemented");
     exit(EXIT_FAILURE);
}

/* function: (round x) -> <integer> 
   Returns the integer nearest to x. If x is exactly halfway between two integers,
   the even one is chosen. 
   An error shall be signaled if x is not a number (error-id. domain-error).
*/
kiss_obj* kiss_round(kiss_obj* x) {
     Kiss_Err(L"c library function 'roundevenf' is not supported as of this writing.");
     /*
     Kiss_Number(x);
     if (KISS_IS_INTEGER(x)) {
	  return (kiss_obj*)kiss_make_integer(roundevenf(Kiss_Integer(x)->i));
     } else {
	  return (kiss_obj*)kiss_make_integer(roundevenf(Kiss_Float(x)->f));
     }
     */
}



/* function: (log x) -> <number>
   Returns the natural logarithm of x.
   An error shall be signaled if x is not a positive number (error-id. domain-error).
*/
kiss_obj* kiss_log(kiss_obj* x) {
     fwprintf(stderr, L"kiss_log: not implemented");
     exit(EXIT_FAILURE);
}

/* function: (sin x) -> <number>
   The function sin returns the sine of x. 
   x must be given in radians.
*/
kiss_obj* kiss_sin(kiss_obj* x) {
     fwprintf(stderr, L"kiss_sin: not implemented");
     exit(EXIT_FAILURE);
}

/* function: (cos x) -> <number>
   The function cos returns the cosine of x. 
   x must be given in radians.
*/
kiss_obj* kiss_cos(kiss_obj* x) {
     fwprintf(stderr, L"kiss_cos: not implemented");
     exit(EXIT_FAILURE);
}

/* function: (tan x) -> <number>
   The function tan returns the tangent of x. 
   x must be given in radians.
*/
kiss_obj* kiss_tan(kiss_obj* x) {
     fwprintf(stderr, L"kiss_tan: not implemented");
     exit(EXIT_FAILURE);
}

/* function: (max x+) -> <number>
   The function max returns the greatest (closest to positive infinity) of its arguments.
   The comparison is done by >.
   An error shall be signaled if any x is not a number (error-id. domain-error).*/
kiss_obj* kiss_max(const kiss_obj* x, const kiss_obj* const rest) {
     Kiss_Number(x);
     for (const kiss_obj* p = rest; p != KISS_NIL; p = kiss_cdr(p)) {
          if (kiss_num_greaterthan(kiss_car(p), x) == KISS_T) {
               x = KISS_CAR(p);
          }
     }
     return (kiss_obj*)x;
}

/* function: (min x+) -> <number>
   The function min returns the least (closest to negative infinity) of its arguments.
   The comparison is done by <.
   An error shall be signaled if any x is not a number (error-id. domain-error).*/
kiss_obj* kiss_min(const kiss_obj* x, const kiss_obj* const rest) {
     Kiss_Number(x);
     for (const kiss_obj* p = rest; p != KISS_NIL; p = kiss_cdr(p)) {
          if (kiss_num_lessthan(kiss_car(p), x) == KISS_T) {
               x = KISS_CAR(p);
          }
     }
     return (kiss_obj*)x;
}
