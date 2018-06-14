/*  -*- coding: utf-8 -*-
  string.c --- defines the string handling mechanism of ISLisp processor KISS.

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

extern inline
void kiss_init_string(kiss_string_t* str, wchar_t* name, size_t n);

extern inline
kiss_string_t* kiss_make_string(const wchar_t* const s);


/* function: (create-string i [initial-character]) -> <string>
   Returns a string of length I. If INITIAL-CHARACTER is given, then the characters of
   the new string are initialized with this character, otherwise the initialization is
   implementation defined. An error shall be signaled if the requested string cannot be
   allocated (error-id. cannot-create-string).
   An error shall be signaled if I is not a non-negative integer or if INITIAL-CHARACTER
   is not a character (error-id. domain-error). */
kiss_obj* kiss_create_string(const kiss_obj* const i, const kiss_obj* const rest) {
    kiss_ptr_int n = Kiss_Non_Negative_Fixnum(i);
    wchar_t c = rest == KISS_NIL ? L' ' : Kiss_Character(KISS_CAR(rest));
    wchar_t* s = Kiss_Malloc(sizeof(wchar_t) * (n + 1));
    for (size_t j = 0; j < n; j++) { s[j] = c; }
    s[n] = L'\0';

    kiss_string_t* const p = Kiss_GC_Malloc(sizeof(kiss_string_t));
    p->type = KISS_STRING;
    p->str = s;
    p->n = n;
    return (kiss_obj*)p;
}

/* function: (stringp obj) -> boolean 
   Returns t if OBJ is a string (instance of class string);
   otherwise, returns nil. OBJ may be any LISP object. */
inline kiss_obj* kiss_stringp(const kiss_obj* const obj) {
     return KISS_IS_STRING(obj) ? KISS_T : KISS_NIL;
}


/* function: (string= string1 string2) -> quasi-boolean
   Tests whether STRING1 is the same string as STRING2.
   Two strings are string= if they are of the same length l, and if for every i,
   where 0 <= i < l, (char= (elt string1 i ) (elt string2 i)) holds.
   if the test is satisfied, an implementation-defined non-nil value is returned; otherwise,
   nil is returned. */
kiss_obj* kiss_string_eq(const kiss_obj* const str1, const kiss_obj* const str2) {
     kiss_string_t* s1 = Kiss_String(str1);
     kiss_string_t* s2 = Kiss_String(str2);
     if (s1->n != s2->n)
          return KISS_NIL;

     size_t i = 0;
     size_t n = s1->n;
     while (i < n && s1->str[i] == s2->str[i]) { i++; }
     return  (i == n ? KISS_T : KISS_NIL);
}

/* function: (string/= string1 string2) -> quasi-boolean
   Two strings are string/= if and only if they are not string=.
   if the test is satisfied, an implementation-defined non-nil value is returned; otherwise,
   nil is returned. */
kiss_obj* kiss_string_neq(const kiss_obj* const str1, const kiss_obj* const str2) {
     return (kiss_string_eq(str1, str2) == KISS_NIL ? KISS_T : KISS_NIL);
}

kiss_string_t* kiss_chars_to_str(const kiss_obj* const chars) {
     kiss_string_t* str = (kiss_string_t*)kiss_create_string(kiss_length(chars), KISS_NIL);
     wchar_t* s = str->str;
     for (const kiss_obj* p = chars; KISS_IS_CONS(p); p = KISS_CDR(p)) {
          *s = Kiss_Character(KISS_CAR(p));
          s++;
     }
     *s = L'\0';
     return str;
}

kiss_obj* kiss_str_to_chars(const kiss_string_t* const str) {
    kiss_obj* chars = KISS_NIL;
    for (size_t n = str->n; n > 0; n--) {
	kiss_push(kiss_make_char(str->str[n-1]), &chars);
    }
    return chars;
}

/* function: (string-append string*) -> <string>
   Returns a single string containing a sequence of characters that results from
   appending the sequences of characters of each of the STRINGs, 
   or "" if given no strings.
   An error shall be signaled if any STRING is not a string (error-id. domain-error).
   This function does not modify its arguments. 
   It is implementation defined whether and when the result shares structure withn its
   STRING arguments.
   An error shall be signaled if the string cannot be allocated 
   (error-id. cannot-create-string). */
kiss_obj* kiss_string_append(const kiss_obj* const rest) {
     long int n = 0;
     for (const kiss_obj* args = rest; KISS_IS_CONS(args); args = KISS_CDR(args)) {
	  n += kiss_c_length(KISS_CAR(args));
     }
     kiss_string_t* str = (kiss_string_t*)kiss_create_string((kiss_obj*)kiss_make_fixnum(n), KISS_NIL);

     wchar_t* p = str->str;
     for (const kiss_obj* args = rest; KISS_IS_CONS(args); args = KISS_CDR(args)) {
	  wcscpy(p, Kiss_String(KISS_CAR(args))->str);
	  p += kiss_c_length(KISS_CAR(args));
     }
     return (kiss_obj*)str;
}
